
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_BTB;
// ================================================================
// Exports

export mkNOVA_BPC_L0_BTB;
export mkNOVA_BPC_L1_BTB;
export mkNOVA_BPC_L2_BTB;

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
    Add#(btb_max_set_idx, 1, btb_sets),
    Max#(1, btb_set_idx_w, btb_set_idx_w1),
    Alias#(btb_set_idx_t, Bit#(btb_set_idx_w)),
    Alias#(btb_set_idx1_t, Bit#(btb_set_idx_w1)),
    Alias#(btb_asso_id_t, Bit#(btb_asso_id_w)));

  // ----------------
  // Instances
  Vector#(2, RegFile#(btb_set_idx1_t, Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t))))
    btb_info <- replicateM(mkRegFile(0, fromInteger(valueOf(btb_max_set_idx))));
  Vector#(2, RegFile#(btb_set_idx1_t, Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t)))
    btb_map  <- replicateM(mkRegFile(0, fromInteger(valueOf(btb_max_set_idx))));
  Vector#(2, RegFile#(btb_set_idx1_t, Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t)))
    btb_addr <- replicateM(mkRegFile(0, fromInteger(valueOf(btb_max_set_idx))));
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
      Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t)) btb_info_rd = btb_info[i].sub(idx);
      Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t)  btb_map_rd = btb_map[i].sub(idx);
      Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t) btb_addr_rd = btb_addr[i].sub(idx);

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
      Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t)) btb_info_rd = btb_info[i].sub(idx);

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
        begin
          btb_info_rd[alc_btb_id] = tagged Valid val2.info;
          btb_info[i].upd(idx, btb_info_rd);
        end
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
        Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t)  btb_map_rd = btb_map[i].sub(idx);
        Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t) btb_addr_rd = btb_addr[i].sub(idx);

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
          btb_map_rd[wr_btb_id]  = nxt_map;
          btb_addr_rd[wr_btb_id] = nxt_addr;
          btb_addr[i].upd(idx, btb_addr_rd);
          btb_map[i].upd(idx, btb_map_rd);
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

endpackage
