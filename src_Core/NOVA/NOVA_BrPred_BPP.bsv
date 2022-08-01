
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

module mkNOVA_BPC_GNRL_BPP (NOVA_BPC_GNRL_BPP_IFC#(odly, impl, bpp_entries, req_t, rsp_t, updt_req_t, updt_rsp_t))
  provisos(
    Alias#(req_t, BPC_BPP_REQ_t#(btb_id_t)),
    Alias#(rsp_t, BPC_BPP_RSP_t#(bpp_sig_t)),
    Alias#(updt_req_t, BPC_BPP_UPDT_REQ_t),
    Alias#(updt_rsp_t, BPC_BPP_UPDT_RSP_t),
    Alias#(bpp_sig_t, Bit#(bpp_id_w)),
    Bits#(BPC_BPP_REQ_t#(btb_id_t), req_bits),
    Add#(extra_ght, bpp_id_w, NOVA_CFG_BPC_BHT_W),
    Add#(extra_pc,  bpp_id_w, IFetch_HAddr_w),
    Log#(bpp_entries,bpp_id_w));

  // ----------------
  // Instances
  RegFile#(bpp_sig_t, Vector#(NOVA_CFG_BPC_FETCH_W,IFC_PHTE_t))
                         pht <- mkRegFileFull();
  Wire#(updt_rsp_t)      updt_rsp_wire <- mkWire;
  Wire#(Vector#(NOVA_CFG_BPC_FETCH_W,IFC_PHTE_t)) pht_wire <- mkWire;
  Wire#(bpp_sig_t)                                idx_wire <- mkWire;

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

  function Maybe#(PC_t) sel_btb_pc(IFetch_LAddr_t pc_os_end, 
                                BPC_BTB_MAP_ENTRY_t btb_map, 
                                BPC_BTB_ADDR_ENTRY_t btb_addr
                            );
    
    let target_pos = btb_map.target_pos;
    let target_pcs = btb_addr.target_pc;
    Maybe#(PC_t) target_pc = Invalid;
    if (target_pos[pc_os_end] matches tagged Valid .pos)
      target_pc = target_pcs[pos];
    return target_pc;
  endfunction

  function rsp_t fn_handle_lkup(req_t val, Vector#(NOVA_CFG_BPC_FETCH_W,IFC_PHTE_t) phts);
    rsp_t rspv = unpack(fromInteger(valueOf(0)));
    Bit#(NOVA_CFG_BPC_FETCH_W)  res_taken     = 'b0;
    Bit#(NOVA_CFG_BPC_FETCH_W)  res_is_brcc   = 'b0;

    for (Integer j = 0; j < valueOf(NOVA_CFG_BPC_FETCH_W); j=j+1)
    begin
      Bool taken = False;
      Bool is_brcc = False;
      if (isValid(val.btb_rsp.btb_id))
      case (val.btb_rsp.btb_info.br_class[j]) matches
        BC_NO:      // not a branch or jump
          begin
          end
        BC_BRCC:    // conditional branch, brcc can be currently not predicated if not mapped in BTB
          begin
            is_brcc = True;
            taken = predict(phts[j]);
          end
        BC_LOOP:    // Loop
          begin
            is_brcc = True;
            taken = True;
          end
        BC_BRUC:    // un-conditional branch
          begin
            taken = True;
          end
        BC_JMP:     // unconditional jump
          begin
            taken = True;
          end
        //BC_BRNT:    // conditional branch mostly not taken, not currently predicted
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
      res_taken[j]   = pack(taken);
      res_is_brcc[j] = pack(is_brcc);
    end
    
    Bit#(NOVA_CFG_BPC_FETCH_W) takens  = res_taken;
    Bit#(NOVA_CFG_BPC_FETCH_W) brccs   = res_is_brcc;

    IFetch_LAddr_t start = truncate(val.pc_os);
    Maybe#(IFetch_LAddr_t) end_pos   = Invalid;
    
    rspv.bp_sig = 0;
    for (Integer j = 0; j < valueOf(NOVA_CFG_BPC_FETCH_W); j=j+1)
      if (!isValid(end_pos) && fromInteger(j) >= start)
      begin
        if (takens[j] == 1'b1)
        begin
          end_pos = tagged Valid fromInteger(j);
          rspv.brcc_taken = brccs[j] == 1'b1;
        end
        rspv.brcc_cnt = rspv.brcc_cnt + zeroExtend(brccs[j]);
      end

    rspv.pc_os_end = isValid(end_pos) ? end_pos.Valid : fromInteger(-1);
    IFetch_LAddr_t lsb_os = truncate(rspv.pc_os_end);
    rspv.br_class = val.btb_rsp.btb_info.br_class[lsb_os];
    rspv.taken = isValid(end_pos);
    rspv.target_pc = sel_btb_pc(rspv.pc_os_end, val.btb_rsp.btb_map, val.btb_rsp.btb_addr);
    return rspv;
  endfunction

  function bpp_sig_t fn_get_idx(IFetch_HAddr_t pc_h, BPC_BHT_t ght);
    bpp_sig_t idx1 = truncate(pack(pc_h));
    bpp_sig_t idx2 = truncate(pack(ght));
    bpp_sig_t idx = idx1 ^ idx2;
    return idx;
  endfunction

  let updt_req_put =
  (interface Put#(updt_req_t);
    method Action put(updt_req_t val);
      updt_rsp_t rspv = unpack('b0);
      let idx = fn_get_idx(val.pc_h, val.ght);
      Vector#(NOVA_CFG_BPC_FETCH_W,IFC_PHTE_t) phts = pht.sub(idx);
      phts[val.pc_os] = updt(phts[val.pc_os], val.taken);
      pht.upd(idx, phts);
      updt_rsp_wire <= rspv;
    endmethod
  endinterface);
  
  let updt_rsp_get = 
  (interface Get#(updt_rsp_t);
    method ActionValue#(updt_rsp_t) get();
      return updt_rsp_wire;
    endmethod
  endinterface);

  function Vector#(NOVA_CFG_BPC_FETCH_W,IFC_PHTE_t) fn_rd_pht(BPC_BPP_LKUP_REQ_t val);
    Vector#(NOVA_CFG_BPC_FETCH_W,IFC_PHTE_t) pht_rd;
    let idx = fn_get_idx(val.btb_req.pc_h, val.ght);
    pht_rd = pht.sub(idx);
    return pht_rd;
  endfunction

if (valueOf(odly) == 0)
begin
  Wire#(rsp_t)           rsp_wire      <- mkWire;

  let prereq_put =
  (interface Put#(req_t);
    method Action put(BPC_BPP_LKUP_REQ_t val);
      pht_wire <= fn_rd_pht(val);
      idx_wire <= fn_get_idx(val.btb_req.pc_h, val.ght);
    endmethod
  endinterface);

  let req_put =
  (interface Put#(req_t);
    method Action put(req_t val);
      let rspv = fn_handle_lkup(val, pht_wire);
      rspv.bp_sig = idx_wire;
      rsp_wire <= rspv;
    endmethod
  endinterface);

  let rsp_get = 
  (interface Get#(rsp_t);
    method ActionValue#(rsp_t) get();
      return rsp_wire;
    endmethod
  endinterface);

  // ----------------
  // Interfaces
  interface updt_server  = toGPServer(updt_req_put, updt_rsp_get);
  interface pre_lkup_put = toPut(prereq_put);
  interface lkup_server  = toGPServer(req_put, rsp_get);
end else begin
  Vector#(odly, FIFOF#(Tuple2#(Vector#(NOVA_CFG_BPC_FETCH_W,IFC_PHTE_t), bpp_sig_t)))
                pht_rd_stages      <- replicateM(mkPipelineFIFOF); 

  FIFOF#(rsp_t) rsp_fifo      <- mkPipelineFIFOF;

  let prereq_put =
  (interface Put#(req_t);
    method Action put(BPC_BPP_LKUP_REQ_t val);
      let pht_rd = fn_rd_pht(val);
      let idx = fn_get_idx(val.btb_req.pc_h, val.ght);
      pht_rd_stages[0].enq(tuple2(pht_rd, idx));
    endmethod
  endinterface);

  let req_put =
  (interface Put#(req_t);
    method Action put(req_t val);
      let rspv = fn_handle_lkup(val, pht_wire);
      rspv.bp_sig = idx_wire;
      rsp_fifo.enq(rspv);
    endmethod
  endinterface);

  for (Integer i = 1; i < valueOf(odly); i=i+1)
  begin

  rule rl_pht_odly;
    let a = pht_rd_stages[i-1].first();
    pht_rd_stages[i].enq(a);
    pht_rd_stages[i-1].deq();
  endrule

  end

  rule rl_pht_rd;
    let a = pht_rd_stages[valueOf(odly)-1].first();
    pht_rd_stages[valueOf(odly)-1].deq();
    pht_wire <= tpl_1(a);
    idx_wire <= tpl_2(a);
  endrule

  // ----------------
  // Interfaces
  interface updt_server  = toGPServer(updt_req_put, updt_rsp_get);
  interface pre_lkup_put = toPut(prereq_put);
  interface lkup_server  = toGPServer(req_put, rsp_fifo);
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
