
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_BPP;
// ================================================================
// Exports

export mkNOVA_BPC_L0_BPP;
export mkNOVA_BPC_L1_BPP;
export mkNOVA_BPC_L2_BPP;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import ConfigReg    :: *;
import RegFile      :: *;

import Vector::*;

// ----------------
// BSV additional libs

import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;
import CPU_Globals :: *;

import NOVA_Decls :: *;
import NOVA_Utils :: *;
import NOVA_BrPredCplx_IFC     :: *;

module mkNOVA_BPC_GNRL_BPP (NOVA_BPC_GNRL_BPP_IFC#(odly, impl, bpp_hf_entries, req_t, rsp_t, updt_req_t, updt_rsp_t))
  provisos(
    Alias#(req_t, BPC_BPP_REQ_t#(btb_id_t)),
    Alias#(rsp_t, BPC_BPP_RSP_t#(bpp_sig_t)),
    Alias#(updt_req_t, BPC_BPP_UPDT_REQ_t),
    Alias#(updt_rsp_t, BPC_BPP_UPDT_RSP_t),
    Alias#(bpp_sig_t, Bit#(bpp_id_hw)),
    Bits#(BPC_BPP_REQ_t#(btb_id_t), req_bits),
    Add#(extra_ght, bpp_id_hw, NOVA_CFG_BPC_BHT_W),
    Add#(extra_pc,  bpp_id_hw, IFetch_HAddr_w),
    Log#(bpp_hf_entries,bpp_id_hw));

  // ----------------
  // Instances
  Vector#(2, RegFile#(bpp_sig_t, Vector#(NOVA_CFG_BPC_FETCH_HW,IFC_PHTE_t))) 
                            pht <- replicateM(mkRegFileFull());

  GPCvt #(req_t)         req         <- mkGPCvt;
  GPCvt #(updt_req_t)    updt_req    <- mkGPCvt;
  RWire#(rsp_t)          rsp_wire      <- mkRWireSBR;
  RWire#(updt_rsp_t)     updt_rsp_wire <- mkRWireSBR;

  // ----------------
  // States


  // ----------------
  // Rules 

  function Bool predict(IFC_PHTE_t val);
    return val >= 2'b10;
  endfunction

  function IFC_PHTE_t updt(IFC_PHTE_t val, Bool taken);
    IFC_PHTE_t res = val;
    if (taken)
      res = val == 2'b11 ? 2'b11 : val + 2'b01;
    else
      res = val == 2'b00 ? 2'b00 : val - 2'b01;
    return res;
  endfunction

  rule handle_lkup;
    let val = req.first();
    rsp_t rspv = unpack(fromInteger(valueOf(0)));
    Vector#(2, Bit#(NOVA_CFG_BPC_FETCH_HW)) res_taken  = replicate('b0);
    Vector#(2, Bit#(NOVA_CFG_BPC_FETCH_HW)) res_btb_valid = replicate('b0);
    Vector#(2, Bit#(NOVA_CFG_BPC_FETCH_HW)) res_is_brcc   = replicate('b0);
    Vector#(2, bpp_sig_t) res_bpp_idx = replicate('b0);

    for (Integer i = 0; i < 2; i=i+1)
    begin
      bpp_sig_t idx1 = truncate(pack(val.btb_req.pc_h[i]));
      bpp_sig_t idx2 = truncate(pack(val.ght));
      bpp_sig_t idx = idx1 ^ idx2;
      res_bpp_idx[i] = idx;
      Vector#(NOVA_CFG_BPC_FETCH_HW,IFC_PHTE_t) phts = pht[i].sub(idx);

      for (Integer j = 0; j < valueOf(NOVA_CFG_BPC_FETCH_HW); j=j+1)
        begin
          Bool taken = False;
          Bool is_brcc = False;
          Bool btb_valid = False;
          case (val.btb_rsp.btb_info[i].br_class[j]) matches
            BC_NO:      // not a branch or jump
              begin
              end
            BC_BRCC:    // conditional branch, brcc can be currently not predicated if not mapped in BTB
              begin
                is_brcc = True;
                if (predict(phts[j]))
                begin
                  taken = True;
                  btb_valid = True;
                end
              end
            BC_BRUC:    // un-conditional branch
              begin
                taken = True;
                btb_valid = isValid(val.btb_rsp.btb_map[i].target_pos[j]);
              end
            BC_JMP:     // unconditional jump
              begin
                taken = True;
                btb_valid = isValid(val.btb_rsp.btb_map[i].target_pos[j]);
              end
            //BC_BRNT:    // conditional branch mostly not taken, not currently predicted
            //BC_LOOP:    // Special BRCC: Small loop
            BC_CALL:    // Special JMP: func call
              begin
                taken = True;
              end
            BC_RET:     // Special JMP: func ret
              begin
                taken = True;
              end
            BC_CONT:    // Special BRCC: countinue in a loop
              begin
                taken = True;
              end
            BC_IND:     // Special JMP: indirect target other than func ret
              begin
                taken = True;
              end
          endcase
          res_taken[i][j]  = pack(taken);
          res_btb_valid[i][j] = pack(btb_valid);
          res_is_brcc[i][j] = pack(is_brcc);
        end
    end
    
    Bit#(NOVA_CFG_BPC_FETCH_W) vector  = 'b0;
    Bit#(NOVA_CFG_BPC_FETCH_W) branchs = 'b0;
    if (msb(val.pc_os) == 1'b0)
    begin
      vector = {res_taken[1], res_taken[0]};
      branchs = {res_is_brcc[1], res_is_brcc[0]};
    end else begin
      vector = {res_taken[0], res_taken[1]};
      branchs = {res_is_brcc[0], res_is_brcc[1]};
    end

    IFetch_HF_POS_t start = truncate(val.pc_os);
    Maybe#(IFetch_LAddr_t) end_pos   = Invalid;

    rspv.bp_sig = 0;
    for (Integer j = 0; j < valueOf(NOVA_CFG_BPC_FETCH_W); j=j+1)
    if (!isValid(end_pos) && fromInteger(j) >= start)
    begin
      if (vector[j] == 1'b1)
      begin
        end_pos = tagged Valid fromInteger(j);
        rspv.brcc_taken = branchs[j] == 1'b1;
      end
      rspv.brcc_cnt = rspv.brcc_cnt + zeroExtend(branchs[j]);
    end

    rspv.pc_os_end = end_pos.Valid;
    bpp_sig_t sig = unpack('b0);
    if (msb(val.pc_os) == 1'b0)
    begin
      sig = res_bpp_idx[msb(rspv.pc_os_end)];
    end else begin
      sig = res_bpp_idx[~msb(rspv.pc_os_end)];
      rspv.pc_os_end = rspv.pc_os_end ^ (1 << (valueOf(NOVA_CFG_BPC_FETCH_AW)-1));
    end
    rspv.taken = isValid(end_pos);
    if (rspv.taken)
      rspv.bp_sig = sig;
    rsp_wire.wset(rspv);
  endrule

  rule handle_updt;
    let val = updt_req.first();
    BPC_BPP_UPDT_RSP_t rspv = unpack('b0);
    bpp_sig_t idx1 = truncate(pack(val.pc_h));
    bpp_sig_t idx2 = truncate(pack(val.ght));
    bpp_sig_t idx = idx1 ^ idx2;
    IFetch_HF_POS_t lsb_os = truncate(val.pc_os);
    Vector#(NOVA_CFG_BPC_FETCH_HW,IFC_PHTE_t) phts = pht[msb(val.pc_os)].sub(idx);
    phts[lsb_os] = updt(phts[lsb_os], val.taken);
    pht[msb(val.pc_os)].upd(idx, phts);
    updt_rsp_wire.wset(rspv);
  endrule

if (valueOf(odly) == 0)
begin
  GPCvt #(rsp_t)            rsp         <- mkGPCvt;
  GPCvt #(updt_rsp_t)       updt_rsp    <- mkGPCvt;

  rule rl_rsp_odly if (rsp_wire.wget() matches tagged Valid .rspd);
    rsp.enq(rspd);
  endrule

  rule rl_updt_rsp_odly if (updt_rsp_wire.wget() matches tagged Valid .updt_rspv);
    updt_rsp.enq(updt_rspv);
  endrule

  rule accept_lkup if (rsp.deq_ready());
    req.deq();
  endrule

  rule accept_updt if (updt_rsp.deq_ready());
    updt_req.deq();
  endrule

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req, rsp);
  interface updt_server = toGPServer(updt_req, updt_rsp);

end else begin
  Vector#(odly, FIFOF#(rsp_t))      rsp_stages      <- replicateM(mkPipelineFIFOF); 
  Vector#(odly, FIFOF#(updt_rsp_t)) updt_rsp_stages <- replicateM(mkPipelineFIFOF); 

  for (Integer i = 1; i < valueOf(odly); i=i+1)
  begin

  rule rl_rsp_odly;
    let a = rsp_stages[i-1].first();
    rsp_stages[i].enq(a);
    rsp_stages[i-1].deq();
  endrule

  rule rl_updt_rsp_odly;
    let a = updt_rsp_stages[i-1].first();
    updt_rsp_stages[i].enq(a);
    updt_rsp_stages[i-1].deq();
  endrule

  end

  rule accept_lkup if (rsp_wire.wget() matches tagged Valid .rspd &&& rsp_stages[0].notFull());
    req.deq();
    rsp_stages[0].enq(rspd);
  endrule

  rule accept_updt if (updt_rsp_wire.wget() matches tagged Valid .updt_rspv &&& updt_rsp_stages[0].notFull());
    updt_req.deq();
    updt_rsp_stages[0].enq(updt_rspv);
  endrule

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req, rsp_stages[valueOf(odly)-1]);
  interface updt_server = toGPServer(updt_req, updt_rsp_stages[valueOf(odly)-1]);
end
  // ----------------
  // method

endmodule: mkNOVA_BPC_GNRL_BPP

(* synthesize *)
module mkNOVA_BPC_L0_BPP (NOVA_BPC_L0_BPP_IFC);
  function module#(NOVA_BPC_L0_BPP_IFC) mkNOVA_BPC_L0_BPP_f = mkNOVA_BPC_GNRL_BPP;
  let  m <- mkNOVA_BPC_L0_BPP_f;
  return m;
endmodule

(* synthesize *)
module mkNOVA_BPC_L1_BPP (NOVA_BPC_L1_BPP_IFC);
  function module#(NOVA_BPC_L1_BPP_IFC) mkNOVA_BPC_L1_BPP_f = mkNOVA_BPC_GNRL_BPP;
  let  m <- mkNOVA_BPC_L1_BPP_f;
  return m;
endmodule

(* synthesize *)
module mkNOVA_BPC_L2_BPP (NOVA_BPC_L2_BPP_IFC);
  function module#(NOVA_BPC_L2_BPP_IFC) mkNOVA_BPC_L2_BPP_f = mkNOVA_BPC_GNRL_BPP;
  let  m <- mkNOVA_BPC_L2_BPP_f;
  return m;
endmodule

endpackage
