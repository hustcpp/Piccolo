
// ================================================================
// Branch Prediction Complex

package NOVA_BrPredCplx;
// ================================================================
// Exports

export mkNOVA_BrPredCplx;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import ConfigReg    :: *;

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

(* synthesize *)
module mkNOVA_BPC_BPQ (NOVA_BPC_BPQ_IFC);
  // ----------------
  // Instances
  FIFOF #(IFC_BPC_BRF_Pack_t)  ifc_brf_fifo <- mkFIFOF;
  RWire #(IFC_BPC_BRF_Pack_t)  flush        <- mkRWireSBR;
  GPCvt #(IFC_BPC_BPQ_Pack_t)  deq_val      <- mkGPCvt;
  GPCvt #(IFC_BPC_BPQ_Pack_t)  enq_val      <- mkGPCvt;
  PulseWire                    set_full     <- mkPulseWire;

  // ----------------
  // States
  Vector#(NOVA_CFG_BPC_BPQ_ENTRIES, Reg#(IFC_BPC_BPQ_Pack_t)) q <- replicateM(mkRegA(unpack(fromInteger(valueOf(0)))));
  Reg#(BPQ_PTR_t)   rd_ptr <- mkRegA(0);
  Reg#(BPQ_PTR_t)   wr_ptr <- mkRegA(0);
  Reg#(Bool)        full   <- mkRegA(False);
  
  Bool notEmpty = rd_ptr != wr_ptr || full;
  IFC_BPC_BPQ_Pack_t top = q[rd_ptr];

  // ----------------
  // Rules 
  rule rl_read_q if (notEmpty);
    if (flush.wget() matches tagged Valid .vl &&& vl.bp_id == top.bp_id)
      ifc_brf_fifo.enq(vl);
    else
      deq_val.enq(top);
  endrule

  rule rl_write_q;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) m;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) res = 0;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) rd_mask = (1<<(rd_ptr+1))-1;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) wr_mask = (1<<(wr_ptr+1))-1;
    let vl = fromMaybe(?, flush.wget());
    for (Integer i = 0; i < valueOf(NOVA_CFG_BPC_BPQ_ENTRIES); i=i+1)
      m[i] = pack(vl.bp_id == q[i].bp_id);

    if (wr_ptr <= rd_ptr)
    begin
      if ((~rd_mask & m) != 0)
          res = ~rd_mask & m;
      else
          res = wr_mask & m;
    end else begin
      res = ~rd_mask & wr_mask & m;
    end

    BPQ_PTR_t wr_ptr_w   = 0;
    for (Integer i = valueOf(NOVA_CFG_BPC_BPQ_ENTRIES)-1; i >= 0 ; i=i-1)
      if (res[i] == 1'b1)
        wr_ptr_w = fromInteger(i);

    if (isValid(flush.wget()))
    begin
        if (full)
            wr_ptr_w = rd_ptr;
    end else
        wr_ptr_w = wr_ptr;
    
    if (!full)
    begin
      if (enq_val.hsked())
        q[wr_ptr_w] <= enq_val.first();
    end

    if ((!full && enq_val.hsked()) || isValid(flush.wget()))
    begin
        wr_ptr <= wr_ptr_w+1;
        if (wr_ptr_w+1 == rd_ptr)
          set_full.send();
    end
  endrule

  rule rl_handle_wr;
    if (!full)
      enq_val.deq();
  endrule

  rule rl_set_full;
    if (deq_val.hsked)
    begin
      full <= False;
      rd_ptr <= rd_ptr+1;
    end else if (set_full)
      full <= True;
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface ifc_brf_intf = toGet(ifc_brf_fifo);
  interface flush_intf   = toPut(flush);
  interface enq_intf     = toPut(enq_val);
  interface ifc_deq_intf = toGet(deq_val);
endmodule: mkNOVA_BPC_BPQ

module mkNOVA_BPC_GNRL_BTB (NOVA_BPC_GNRL_BTB_IFC#(odly, impl, btb_hf_entries, btb_asso, req_t, rsp_t, updt_req_t, updt_rsp_t))
  provisos(
    Alias#(req_t, BPC_BTB_REQ_t),
    Alias#(rsp_t, BPC_BTB_RSP_t#(btb_id_t)),
    Alias#(updt_req_t, BPC_BTB_UPDT_REQ_t#(btb_id_t)),
    Alias#(updt_rsp_t, BPC_BTB_UPDT_RSP_t#(btb_id_t)),
    Alias#(btb_id_t, Bit#(btb_id_hw)),
    Log#(btb_hf_entries,btb_id_hw),
    Mul#(btb_asso, btb_sets, btb_hf_entries),
    Log#(btb_asso, btb_asso_id_w),
    Add#(btb_asso_id_w, btb_set_idx_w, btb_id_hw),
    Max#(1, btb_set_idx_w, btb_set_idx_w1),
    Alias#(btb_set_idx_t, Bit#(btb_set_idx_w)),
    Alias#(btb_set_idx1_t, Bit#(btb_set_idx_w1)),
    Alias#(btb_asso_id_t, Bit#(btb_asso_id_w)));

  // ----------------
  // Instances
  Vector#(2, Vector#(btb_sets, Vector#(btb_asso, Reg#(Maybe#(BPC_BTB_INFO_ENTRY_t)))))
    btb_info <- replicateM(replicateM(replicateM(mkRegA(Invalid))));
  Vector#(2, Vector#(btb_sets, Vector#(btb_asso, Reg#(BPC_BTB_MAP_ENTRY_t))))
    btb_map  <- replicateM(replicateM(replicateM(mkRegA(unpack(fromInteger(valueOf(0)))))));
  Vector#(2, Vector#(btb_sets, Vector#(btb_asso, Reg#(BPC_BTB_ADDR_ENTRY_t))))
    btb_addr <- replicateM(replicateM(replicateM(mkRegA(unpack(fromInteger(valueOf(0)))))));
  GPCvt #(req_t)            req         <- mkGPCvt;
  GPCvt #(updt_req_t)       updt_req    <- mkGPCvt;
  Vector#(2, LRU#(btb_asso))               lru <- replicateM(mkLRU);
  Vector#(2, Wire#(Bit#(TLog#(btb_asso)))) lruv <- replicateM(mkWire);
  RWire#(rsp_t)      rsp_wire <- mkRWireSBR;
  RWire#(updt_rsp_t) updt_rsp_wire <- mkRWireSBR;

  // ----------------
  // States

  // ----------------
  // Rules 
  rule handle_lkup if (req.enq_valid());
    rsp_t rspd = unpack(fromInteger(valueOf(0)));
    let val = req.first();
    for (Integer i = 0; i < 2; i=i+1)
    begin
      btb_set_idx1_t idx = 'b0;
      if (valueOf(btb_set_idx_w) != 0)
        idx = val.pc_h[i][valueOf(btb_set_idx_w)-1:0];
      Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t)) btb_info_rd = readVReg(btb_info[i][idx]);
      Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t)  btb_map_rd = readVReg(btb_map[i][idx]);
      Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t) btb_addr_rd = readVReg(btb_addr[i][idx]);

      for (Integer j = 0; j < valueOf(btb_asso); j=j+1)
        if (btb_info_rd[j] matches tagged Valid .val2 &&& val2.pc_h == val.pc_h[i])
        begin
          btb_asso_id_t asso_id = fromInteger(j);
          btb_id_t btb_id = 0;
          if (valueOf(btb_set_idx_w) == 0)
            btb_id = zeroExtend(asso_id);
          else begin
            btb_set_idx_t idx_l = idx[valueOf(btb_set_idx_w)-1:0];
            btb_id = {idx_l, asso_id};
          end
          rspd.btb_info[i] = val2;
          rspd.btb_addr[i] = btb_addr_rd[j];
          rspd.btb_map[i]  = btb_map_rd[j];
          rspd.btb_id[i] = tagged Valid btb_id;
        end
    end
    for (Integer i = 0; i < 2; i=i+1)
      if (rspd.btb_id[i] matches tagged Valid .btb_id2)
      begin
        btb_asso_id_t asso_id = btb_id2[valueOf(btb_asso_id_w)-1:0];
        lru[i].access(1 << asso_id);
      end
    rsp_wire.wset(rspd);
  endrule

  function Tuple2#(BPC_BTB_MAP_ENTRY_t, BPC_BTB_ADDR_ENTRY_t) updt_nxt_entry(
        BPC_BTB_UPDT_ADDR_REQ_ENTRY_t#(btb_id_t) updt,
        BPC_BTB_MAP_ENTRY_t  old_map,
        BPC_BTB_ADDR_ENTRY_t old_addr
        );
    BPC_BTB_ADDR_ENTRY_t nxt_addr = old_addr;
    BPC_BTB_MAP_ENTRY_t  nxt_map  = old_map;

    for (Integer j = 0; j < valueOf(NOVA_CFG_BPC_PRED_HW); j=j+1)
      if (updt.e.target_pc[j] matches tagged Valid .pc2) 
      begin
        let pc_os = updt.target_pos[j];
        nxt_map.target_pos[pc_os] = tagged Valid fromInteger(j);
        nxt_addr.target_pc[j] = tagged Valid pc2;
      end
      
    return tuple2(nxt_map, nxt_addr);
  endfunction

  rule handle_updt if (updt_req.enq_valid());
    let val = updt_req.first();
    updt_rsp_t updt_rspv = unpack(fromInteger(valueOf(0)));

    for (Integer i = 0; i < 2; i=i+1)
    begin
      Maybe#(btb_asso_id_t) inv_btb_id = Invalid;
      Maybe#(btb_asso_id_t) rpl_btb_id = Invalid;
      btb_asso_id_t new_btb_id = 'b0;
      btb_asso_id_t alc_btb_id = 'b0;

      btb_set_idx1_t idx = 0;
      if (valueOf(btb_set_idx_w) != 0)
        idx = val.d[i].Valid.info.pc_h[valueOf(btb_set_idx_w)-1:0];
      Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t)) btb_info_rd = readVReg(btb_info[i][idx]);

      for (Integer j = 0; j < valueOf(btb_asso); j=j+1)
        if (btb_info_rd[j] matches Invalid)
          inv_btb_id = tagged Valid fromInteger(j);

      if (inv_btb_id matches tagged Valid .val2)
        new_btb_id = val2;
      else begin
        new_btb_id = unpack(lruv[i]);
        rpl_btb_id = tagged Valid new_btb_id;
      end

      if (val.d[i] matches tagged Valid .val2)
      begin
        Bool write_en = False;
        if (val2.btb_id matches tagged Valid .val3)
        begin
          btb_asso_id_t asso_id = truncate(val3);
          let info_todo = btb_info_rd[asso_id];
          alc_btb_id = asso_id;
          write_en = info_todo matches tagged Valid .entry &&& entry.pc_h == val2.info.pc_h ? True : False;
        end else begin
          alc_btb_id = new_btb_id;
          if (rpl_btb_id matches tagged Valid .rpl_btb_id_v)
          begin
            if (valueOf(btb_set_idx_w) == 0)
            begin
              btb_id_t rpl_btb_id2 = zeroExtend(rpl_btb_id_v);
              updt_rspv.rpl_btb_id[i] = tagged Valid rpl_btb_id2;
            end else begin
              btb_set_idx_t idx_l = idx[valueOf(btb_set_idx_w)-1:0];
              btb_id_t rpl_btb_id2 = {idx_l, rpl_btb_id_v};
              updt_rspv.rpl_btb_id[i] = tagged Valid rpl_btb_id2;
            end
          end
          write_en = True;
        end

        if (write_en)
          btb_info[i][idx][alc_btb_id] <= tagged Valid val2.info;
      end

      if (val.a[i] matches tagged Valid .val2)
      begin
        BPC_BTB_MAP_ENTRY_t  map_old   = unpack(fromInteger(valueOf(0)));
        BPC_BTB_ADDR_ENTRY_t addr_old  = unpack(fromInteger(valueOf(0)));
        btb_asso_id_t           wr_btb_id = alc_btb_id;
        Bool                    write_en2 = True;

        idx = 'b0;
        if (valueOf(btb_set_idx_w) != 0)
          idx = val2.pc_h[valueOf(btb_set_idx_w)-1:0];
        Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t)  btb_map_rd = readVReg(btb_map[i][idx]);
        Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t) btb_addr_rd = readVReg(btb_addr[i][idx]);

        if (val2.btb_id matches tagged Valid .btb_id2)
        begin
          btb_asso_id_t asso_id = truncate(btb_id2);
          addr_old = btb_addr_rd[asso_id];
          map_old  = btb_map_rd[asso_id];
          let info_old = btb_info_rd[asso_id];
          wr_btb_id = asso_id;
          write_en2 = info_old matches tagged Valid .entry &&& entry.pc_h == val2.pc_h ? True : False;
        end

        if (write_en2)
        begin
          match {.nxt_map, .nxt_addr} = updt_nxt_entry(val2, map_old, addr_old);
          btb_addr[i][idx][wr_btb_id] <= nxt_addr;
          btb_map[i][idx][wr_btb_id]  <= nxt_map;
        end
      end
    end
    updt_rsp_wire.wset(updt_rspv);
  endrule
  
  rule rd_lru;
    for (Integer i = 0; i < 2; i=i+1)
      lruv[i] <= lru[i].lru(fromInteger(-1));
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

endmodule: mkNOVA_BPC_GNRL_BTB

(* synthesize *)
module mkNOVA_BPC_L0_BTB (NOVA_BPC_L0_BTB_IFC);
  function module#(NOVA_BPC_L0_BTB_IFC) mkNOVA_BPC_L0_BTB_f = mkNOVA_BPC_GNRL_BTB;
  let  m <- mkNOVA_BPC_L0_BTB_f;
  return m;
endmodule

(* synthesize *)
module mkNOVA_BPC_L1_BTB (NOVA_BPC_L1_BTB_IFC);
  function module#(NOVA_BPC_L1_BTB_IFC) mkNOVA_BPC_L1_BTB_f = mkNOVA_BPC_GNRL_BTB;
  let  m <- mkNOVA_BPC_L1_BTB_f;
  return m;
endmodule

(* synthesize *)
module mkNOVA_BPC_L2_BTB (NOVA_BPC_L2_BTB_IFC);
  function module#(NOVA_BPC_L2_BTB_IFC) mkNOVA_BPC_L2_BTB_f = mkNOVA_BPC_GNRL_BTB;
  let  m <- mkNOVA_BPC_L2_BTB_f;
  return m;
endmodule

(* synthesize *)
module mkNOVA_BPC_L0_BPP (NOVA_BPC_L0_BPP_IFC);
  // ----------------
  // Instances
  Vector#(2, Vector#(NOVA_CFG_L0_BPP_HF_ENTRIES, Vector#(NOVA_CFG_BPC_FETCH_HW,Reg#(IFC_PHTE_t)))) 
                            pht <- replicateM(replicateM(replicateM(mkRegA('b0))));

  GPCvt #(BPC_L0_BPP_REQ_t)         req         <- mkGPCvt;
  GPCvt #(BPC_L0_BPP_RSP_t)         rsp         <- mkGPCvt;
  GPCvt #(BPC_BPP_UPDT_REQ_t)       updt_req    <- mkGPCvt;
  GPCvt #(BPC_BPP_UPDT_RSP_t)       updt_rsp    <- mkGPCvt;

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
    BPC_L0_BPP_RSP_t rspv = unpack(fromInteger(valueOf(0)));
    Vector#(2, Bit#(NOVA_CFG_BPC_FETCH_HW)) res_taken  = replicate('b0);
    Vector#(2, Bit#(NOVA_CFG_BPC_FETCH_HW)) res_btb_valid = replicate('b0);
    Vector#(2, Bit#(NOVA_CFG_BPC_FETCH_HW)) res_is_brcc   = replicate('b0);
    Vector#(2, L0_BPP_HF_IDX_t) res_bpp_idx = replicate('b0);

    for (Integer i = 0; i < 2; i=i+1)
    begin
      L0_BPP_HF_IDX_t idx = truncate(pack(val.btb_req.pc_h[i])) ^ truncate(pack(val.ght));
      res_bpp_idx[i] = idx;
      Vector#(NOVA_CFG_BPC_FETCH_HW,IFC_PHTE_t) phts = readVReg(pht[i][idx]);

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

    rspv.bp_sig = Invalid;
    for (Integer j = 0; j < valueOf(NOVA_CFG_BPC_FETCH_W); j=j+1)
    if (!isValid(end_pos) && fromInteger(j) >= start)
    begin
      if (vector[j] == 1'b1)
      begin
        end_pos = tagged Valid fromInteger(j);
      end else begin
        rspv.untaken_brcc_cnt = rspv.untaken_brcc_cnt + zeroExtend(branchs[j]);
      end
    end

    rspv.pc_os_end = end_pos.Valid;
    IFetch_LAddr_t sig = 'b0;
    if (msb(val.pc_os) == 1'b0)
    begin
      L0_BPP_HF_IDX_t high = res_bpp_idx[msb(rspv.pc_os_end)];
      sig = {high, msb(rspv.pc_os_end)};
    end else begin
      L0_BPP_HF_IDX_t high = res_bpp_idx[~msb(rspv.pc_os_end)];
      sig = {high, ~msb(rspv.pc_os_end)};
    end
    if (isValid(end_pos))
      rspv.bp_sig = tagged Valid sig;
  endrule

  rule handle_updt;
    let val = updt_req.first();
    BPC_BPP_UPDT_RSP_t rspv = unpack('b0);
    L0_BPP_HF_IDX_t idx = truncate(pack(val.pc_h)) ^ truncate(pack(val.ght));
    IFetch_HF_POS_t lsb_os = truncate(val.pc_os);
    Vector#(NOVA_CFG_BPC_FETCH_HW,IFC_PHTE_t) phts = readVReg(pht[msb(val.pc_os)][idx]);
    pht[msb(val.pc_os)][idx][lsb_os] <= updt(pht[msb(val.pc_os)][idx][lsb_os], val.taken);
    updt_rsp.enq(rspv);
  endrule

  rule accept_lkup;
    req.deq();
  endrule

  rule accept_updt;
    updt_req.deq();
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req, rsp);
  interface updt_server = toGPServer(updt_req, updt_rsp);
endmodule: mkNOVA_BPC_L0_BPP


(* synthesize *)
module mkNOVA_BPC_L1_BPP (NOVA_BPC_L1_BPP_IFC);
  // ----------------
  // Instances

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
endmodule: mkNOVA_BPC_L1_BPP

(* synthesize *)
module mkNOVA_BPC_L2_BPP (NOVA_BPC_L2_BPP_IFC);
  // ----------------
  // Instances

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
endmodule: mkNOVA_BPC_L2_BPP

(* synthesize *)
module mkNOVA_BPC_RAS (NOVA_BPC_RAS_IFC);
  // ----------------
  // Instances

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
endmodule: mkNOVA_BPC_RAS

(* synthesize *)
module mkNOVA_BPC_ITA (NOVA_BPC_ITA_IFC);
  // ----------------
  // Instances

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
endmodule: mkNOVA_BPC_ITA

(* synthesize *)
module mkNOVA_BPC_LOOP (NOVA_BPC_LOOP_IFC);
  // ----------------
  // Instances

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
endmodule: mkNOVA_BPC_LOOP

(* synthesize *)
module mkNOVA_BPC_CTRL (NOVA_BPC_CTRL_IFC);
  // ----------------
  // Instances
  FIFOF #(BPC_IFC_FBU_Pack_t)  ifc_fbu_fifo <- mkFIFOF;
  Vector#(NOVA_CFG_BRU_N, FIFOF #(EXU_BPC_BCU_Pack_t))  exu_bcu_fifos <- replicateM(mkFIFOF);
  FIFOF #(ROB_BPC_CMT_Pack_t)  rob_cmt_fifo <- mkFIFOF;
  FIFOF #(ROB_BPC_EXCP_Pack_t) rob_excp_fifo <- mkFIFOF;
  FIFOF #(BPC_IFC_ITBF_Pack_t) itb_flush_fifo <- mkFIFOF;

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface ifc_fbu_intf = toPut(ifc_fbu_fifo);
  interface exu_bcu_intfs = map(toGet, exu_bcu_fifos);
  interface rob_cmt_intf  = toPut(rob_cmt_fifo);
  interface rob_excp_intf = toPut(rob_excp_fifo);
  interface itb_flush_intf = toPut(itb_flush_fifo);
endmodule: mkNOVA_BPC_CTRL

(* synthesize *)
module mkNOVA_BrPredCplx (NOVA_BrPredCplx_IFC);
  // ----------------
  // Instances
  NOVA_BPC_BPQ_IFC     bpq    <- mkNOVA_BPC_BPQ;
  NOVA_BPC_L0_BTB_IFC  l0_btb <- mkNOVA_BPC_L0_BTB;
  NOVA_BPC_L1_BTB_IFC  l1_btb <- mkNOVA_BPC_L1_BTB;
  NOVA_BPC_L2_BTB_IFC  l2_btb <- mkNOVA_BPC_L2_BTB;
  NOVA_BPC_L0_BPP_IFC  l0_bpp <- mkNOVA_BPC_L0_BPP;
  NOVA_BPC_L1_BPP_IFC  l1_bpp <- mkNOVA_BPC_L1_BPP;
  NOVA_BPC_L2_BPP_IFC  l2_bpp <- mkNOVA_BPC_L2_BPP;
  NOVA_BPC_RAS_IFC     ras    <- mkNOVA_BPC_RAS;
  NOVA_BPC_ITA_IFC     ita    <- mkNOVA_BPC_ITA;
  NOVA_BPC_LOOP_IFC    loop   <- mkNOVA_BPC_LOOP;
  NOVA_BPC_CTRL_IFC    ctrl   <- mkNOVA_BPC_CTRL;

  mkConnection(ctrl.bpq_flush_intf, bpq.flush_intf);
  mkConnection(ctrl.bpq_enq_intf,   bpq.enq_intf);

  mkConnection(ctrl.l0_btb_client,  l0_btb.lkup_server);
  mkConnection(ctrl.l1_btb_client,  l1_btb.lkup_server);
  mkConnection(ctrl.l2_btb_client,  l2_btb.lkup_server);

  mkConnection(ctrl.l0_bpp_client,  l0_bpp.lkup_server);
  mkConnection(ctrl.l1_bpp_client,  l1_bpp.lkup_server);
  mkConnection(ctrl.l2_bpp_client,  l2_bpp.lkup_server);

  mkConnection(ctrl.l0_btb_updt_client,  l0_btb.updt_server);
  mkConnection(ctrl.l1_btb_updt_client,  l1_btb.updt_server);
  mkConnection(ctrl.l2_btb_updt_client,  l2_btb.updt_server);

  mkConnection(ctrl.l0_bpp_updt_client,  l0_bpp.updt_server);
  mkConnection(ctrl.l1_bpp_updt_client,  l1_bpp.updt_server);
  mkConnection(ctrl.l2_bpp_updt_client,  l2_bpp.updt_server);

  mkConnection(ctrl.ita_lkup_client,  ita.lkup_server);
  mkConnection(ctrl.ita_alloc,  ita.alloc);
  mkConnection(ctrl.ita_cmt,  ita.cmt);

  mkConnection(ctrl.ita_lkup_client,  ita.lkup_server);
  mkConnection(ctrl.ita_alloc,  ita.alloc);
  mkConnection(ctrl.ita_cmt,  ita.cmt);

  mkConnection(ctrl.loop_lkup_client,  loop.lkup_server);
  mkConnection(ctrl.loop_alloc,  loop.alloc);
  mkConnection(ctrl.loop_cmt,  loop.cmt);

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces

  interface ifc_bpq_intf   = bpq.ifc_deq_intf;
  interface ifc_brf_intf   = bpq.ifc_brf_intf;
  interface itb_flush_intf = ctrl.itb_flush_intf;
  interface ifc_fbu_intf   = ctrl.ifc_fbu_intf;
  interface exu_bcu_intfs  = ctrl.exu_bcu_intfs;
  interface rob_cmt_intf   = ctrl.rob_cmt_intf;
  interface rob_excp_intf  = ctrl.rob_excp_intf;

endmodule: mkNOVA_BrPredCplx

endpackage
