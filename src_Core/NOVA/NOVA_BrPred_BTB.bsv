
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

module mkNOVA_BPC_GNRL_BTB (NOVA_BPC_GNRL_BTB_IFC#(odly, impl, btb_entries, btb_asso, req_t, rsp_t, updt_req_t, updt_rsp_t))
  provisos(
    Alias#(req_t, BPC_BTB_REQ_t),
    Alias#(rsp_t, BPC_BTB_RSP_t#(btb_id_t)),
    Alias#(updt_req_t, BPC_BTB_UPDT_REQ_t#(btb_id_t)),
    Alias#(updt_rsp_t, BPC_BTB_UPDT_RSP_t#(btb_id_t)),
    Alias#(btb_id_t, Bit#(btb_id_w)),
    Log#(btb_entries,btb_id_w),
    Mul#(btb_asso, btb_sets, btb_entries),
    Log#(btb_asso, btb_asso_id_w),
    Add#(btb_asso_id_w, btb_set_idx_w, btb_id_w),
    Add#(btb_max_set_idx, 1, btb_sets),
    Max#(1, btb_set_idx_w, btb_set_idx_w1),
    Alias#(btb_set_idx_t, Bit#(btb_set_idx_w)),
    Alias#(btb_set_idx1_t, Bit#(btb_set_idx_w1)),
    Alias#(btb_asso_id_t, Bit#(btb_asso_id_w)));

  // ----------------
  // Instances
  RegFile#(btb_set_idx1_t, Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t)))
    btb_info <- mkRegFile(0, fromInteger(valueOf(btb_max_set_idx)));
  RegFile#(btb_set_idx1_t, Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t))
    btb_map  <- mkRegFile(0, fromInteger(valueOf(btb_max_set_idx)));
  RegFile#(btb_set_idx1_t, Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t))
    btb_addr <- mkRegFile(0, fromInteger(valueOf(btb_max_set_idx)));
  LRU#(btb_asso)               lru  <- mkLRU;
  Wire#(Bit#(TLog#(btb_asso))) lruv <- mkWire;
  Wire#(updt_rsp_t) updt_rsp_wire <- mkWire;

  // ----------------
  // States

  // ----------------
  // Rules 
  function ActionValue#(rsp_t) fn_handle_lkup(req_t val);
  actionvalue
    rsp_t rspd = unpack(fromInteger(valueOf(0)));
    btb_set_idx1_t idx = 'b0;
    if (valueOf(btb_set_idx_w) != 0)
      idx = val.pc_h[valueOf(btb_set_idx_w)-1:0];
    Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t)) btb_info_rd = btb_info.sub(idx);
    Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t)  btb_map_rd = btb_map.sub(idx);
    Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t) btb_addr_rd = btb_addr.sub(idx);

    for (Integer j = 0; j < valueOf(btb_asso); j=j+1)
      if (btb_info_rd[j] matches tagged Valid .val2 &&& val2.pc_h == val.pc_h)
      begin
        btb_asso_id_t asso_id = fromInteger(j);
        btb_id_t btb_id = 0;
        if (valueOf(btb_set_idx_w) == 0)
          btb_id = zeroExtend(asso_id);
        else begin
          btb_set_idx_t idx_l = idx[valueOf(btb_set_idx_w)-1:0];
          btb_id = {idx_l, asso_id};
        end
        rspd.btb_info = val2;
        rspd.btb_addr = btb_addr_rd[j];
        rspd.btb_map  = btb_map_rd[j];
        rspd.btb_id   = tagged Valid btb_id;
      end
    if (rspd.btb_id matches tagged Valid .btb_id2)
    begin
      btb_asso_id_t asso_id = btb_id2[valueOf(btb_asso_id_w)-1:0];
      lru.access(1 << asso_id);
    end
    return rspd;
  endactionvalue
  endfunction

  function Tuple2#(BPC_BTB_MAP_ENTRY_t, BPC_BTB_ADDR_ENTRY_t) updt_nxt_entry(
        BPC_BTB_UPDT_ADDR_REQ_ENTRY_t#(btb_id_t) updt,
        BPC_BTB_MAP_ENTRY_t  old_map,
        BPC_BTB_ADDR_ENTRY_t old_addr
        );
    BPC_BTB_ADDR_ENTRY_t nxt_addr = old_addr;
    BPC_BTB_MAP_ENTRY_t  nxt_map  = old_map;

    for (Integer j = 0; j < valueOf(NOVA_CFG_BPC_PRED_W); j=j+1)
      if (updt.e.target_pc[j] matches tagged Valid .pc2) 
      begin
        let pc_os = updt.target_pos[j];
        nxt_map.target_pos[pc_os] = tagged Valid fromInteger(j);
        nxt_addr.target_pc[j] = tagged Valid pc2;
      end
      
    return tuple2(nxt_map, nxt_addr);
  endfunction

  let updt_req_put =
  (interface Put#(updt_req_t);
    method Action put(updt_req_t val);
    updt_rsp_t updt_rspv = unpack(fromInteger(valueOf(0)));

    Maybe#(btb_asso_id_t) inv_btb_id = Invalid;
    Maybe#(btb_asso_id_t) rpl_btb_id = Invalid;
    btb_asso_id_t new_btb_id = 'b0;
    btb_asso_id_t alc_btb_id = 'b0;

    btb_set_idx1_t idx = 0;
    if (valueOf(btb_set_idx_w) != 0)
      idx = val.d.Valid.info.pc_h[valueOf(btb_set_idx_w)-1:0];
    Vector#(btb_asso, Maybe#(BPC_BTB_INFO_ENTRY_t)) btb_info_rd = btb_info.sub(idx);

    for (Integer j = 0; j < valueOf(btb_asso); j=j+1)
      if (btb_info_rd[j] matches Invalid)
        inv_btb_id = tagged Valid fromInteger(j);

    if (inv_btb_id matches tagged Valid .val2)
      new_btb_id = val2;
    else begin
      new_btb_id = unpack(lruv);
      rpl_btb_id = tagged Valid new_btb_id;
    end

    if (val.d matches tagged Valid .val2)
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
            updt_rspv.rpl_btb_id = tagged Valid rpl_btb_id2;
          end else begin
            btb_set_idx_t idx_l = idx[valueOf(btb_set_idx_w)-1:0];
            btb_id_t rpl_btb_id2 = {idx_l, rpl_btb_id_v};
            updt_rspv.rpl_btb_id = tagged Valid rpl_btb_id2;
          end
        end
        write_en = True;
      end

      if (write_en)
      begin
        btb_info_rd[alc_btb_id] = tagged Valid val2.info;
        btb_info.upd(idx, btb_info_rd);
      end
    end

    if (val.a matches tagged Valid .val2)
    begin
      BPC_BTB_MAP_ENTRY_t  map_old   = unpack(fromInteger(valueOf(0)));
      BPC_BTB_ADDR_ENTRY_t addr_old  = unpack(fromInteger(valueOf(0)));
      btb_asso_id_t           wr_btb_id = alc_btb_id;
      Bool                    write_en2 = True;

      idx = 'b0;
      if (valueOf(btb_set_idx_w) != 0)
        idx = val2.pc_h[valueOf(btb_set_idx_w)-1:0];
      Vector#(btb_asso, BPC_BTB_MAP_ENTRY_t)  btb_map_rd = btb_map.sub(idx);
      Vector#(btb_asso, BPC_BTB_ADDR_ENTRY_t) btb_addr_rd = btb_addr.sub(idx);

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
        btb_addr.upd(idx, btb_addr_rd);
        btb_map.upd(idx, btb_map_rd);
      end
    end
    updt_rsp_wire <= updt_rspv;
    endmethod
  endinterface);

  let updt_rsp_get = 
  (interface Get#(updt_rsp_t);
    method ActionValue#(updt_rsp_t) get();
      return updt_rsp_wire;
    endmethod
  endinterface);

  rule rd_lru;
    lruv <= lru.lru(fromInteger(-1));
  endrule
if (valueOf(odly) == 0)
begin
  Wire#(rsp_t)      rsp_wire <- mkWire;

  let req_put =
  (interface Put#(req_t);
    method Action put(req_t val);
      let rspd <- fn_handle_lkup(val);
      rsp_wire <= rspd;
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
  interface lkup_server = toGPServer(req_put, rsp_get);
  interface updt_server = toGPServer(updt_req_put, updt_rsp_get);

end else begin
  Vector#(odly, FIFOF#(rsp_t))      rsp_stages      <- replicateM(mkPipelineFIFOF); 

  for (Integer i = 1; i < valueOf(odly); i=i+1)
  begin
  rule rl_rsp_odly;
    let a = rsp_stages[i-1].first();
    rsp_stages[i].enq(a);
    rsp_stages[i-1].deq();
  endrule
  end

  let req_put =
  (interface Put#(req_t);
    method Action put(req_t val);
      let rspd <- fn_handle_lkup(val);
      rsp_stages[0].enq(rspd);
    endmethod
  endinterface);

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req_put, rsp_stages[valueOf(odly)-1]);
  interface updt_server = toGPServer(updt_req_put, updt_rsp_get);
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
