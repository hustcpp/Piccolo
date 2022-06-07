
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_LOOP;
// ================================================================
// Exports

export mkNOVA_BPC_LOOP;

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

typedef  Bit #(14)      Loop_Cnt_t;
typedef struct {
  Loop_Cnt_t            max_cnt;
  Loop_Cnt_t            cur_cnt;
  Bool                  trained;
  Bit#(2)               conf; // increase each correct loop exit prediction. decrease when need to find a replacement
} BPC_LOOP_Pack_t
deriving (FShow, Bits);

typedef TSub#(NOVA_CFG_LOOP_OSQ_ENTRIES, 1)  NOVA_CFG_LOOP_OSQ_ENTRIES_MAX;

(* synthesize *)
module mkNOVA_BPC_LOOP (NOVA_BPC_LOOP_IFC);
  // ----------------
  // Instances
  SpCache#(NOVA_CFG_LOOP_ENTRIES, NOVA_CFG_LOOP_ASSO, BPC_LOOP_Pack_t, PC_t, LOOP_SET_ID_t, LOOP_ASSO_ID_t) 
                            cnt_cache <- mkSpCache;
  Vector#(NOVA_CFG_LOOP_OSQ_ENTRIES, Reg#(LOOP_MAP_ID_t)) 
                            osq_id_r    <- replicateM(mkRegA(unpack(fromInteger(0))));
  Reg#(Bit#(NOVA_CFG_LOOP_OSQ_ENTRIES)) 
                            osq_flush_r <- mkRegA(0);
  FIFOMgr#(NOVA_CFG_LOOP_OSQ_ENTRIES, LOOP_OSQ_ID_t)
                            osq_mgr   <- mkFIFOMgr;
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Reg#(PC_t))
                            map_pc_r  <- replicateM(mkRegA(0));
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Reg#(Loop_Cnt_t))
                            map_cnt_r <- replicateM(mkRegA(0));
  FreeQueMgr#(NOVA_CFG_LOOP_MAP_ENTRIES, LOOP_MAP_ID_t)
                            map_mgr   <- mkFreeQueMgr;

  GPCvt #(BPC_SPLBP_REQ_t)                req_agent   <- mkGPCvt;
  GPCvt #(BPC_LOOP_RSP_t)                 rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_SPLBP_ALLOC_t)              alloc_agent <- mkGPCvt;
  GPCvt #(BPC_LOOP_CMT_t)                 cmt_agent   <- mkGPCvt;

  RWire#(LOOP_MAP_ID_t)                   new_map_id_w <- mkRWireSBR;
  RWire#(LOOP_MAP_ID_t)                   inc_map_id_w <- mkRWireSBR;

  // ----------------
  // States

  Bool                                  osq_full  = osq_mgr.is_full;
  LOOP_OSQ_ID_t                         osq_rd    = osq_mgr.get_rd;
  LOOP_OSQ_ID_t                         osq_wr    = osq_mgr.get_wr;

  Vector#(NOVA_CFG_LOOP_OSQ_ENTRIES, LOOP_MAP_ID_t)
                 osq_id       = readVReg(osq_id_r);
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, PC_t)
                 map_pc       = readVReg(map_pc_r);
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Wire#(Loop_Cnt_t))
                 map_cnt      <- replicateM(mkWire);
  Bit#(NOVA_CFG_LOOP_OSQ_ENTRIES) osq_flush = osq_flush_r;
  Bool           retire_flush = osq_flush[osq_rd] == 1'b1;
  LOOP_MAP_ID_t  retire_id    = osq_id[osq_rd];

  // ----------------
  // Rules 
  rule rl_reg_read;
    for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_MAP_ENTRIES); i=i+1)
      map_cnt[i] <= map_cnt_r[i];
  endrule

  rule rl_handle_alloc;
    let           reqv = alloc_agent.first();
    PC_t          pc   = {reqv.pc_h, reqv.pc_os};
    let cache_data = cnt_cache.rd_data(pc);
    if (!isValid(cache_data))
    begin
      // allocate a new cache entries
      let new_cnt_cache_entry = BPC_LOOP_Pack_t {
        max_cnt : 0,
        cur_cnt : 0,
        trained : False,
        conf    : 0 
      };
      cnt_cache.write_data(pc, new_cnt_cache_entry);
    end
  endrule

  rule rl_handle_lkup;
    let            reqv = req_agent.first();
    BPC_LOOP_RSP_t rspv = unpack(fromInteger(0));
    PC_t           pc   = {reqv.pc_h, reqv.pc_os};
    let cache_data = cnt_cache.rd_data(pc);
    Maybe#(Loop_Cnt_t) map_data = Invalid;
    req_agent.deq();

    LOOP_MAP_ID_t map_id = unpack(fromInteger(0));
    for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_MAP_ENTRIES); i=i+1)
      if (map_pc[i] == pc)
      begin
        map_data = tagged Valid map_cnt[i];
        map_id = fromInteger(i);
      end

    if (!isValid(map_data))
    begin 
      // allocate a new map entries
      let new_map_id <- map_mgr.get_entry();
      new_map_id_w.wset(new_map_id);
      map_id = new_map_id;
      map_pc_r[new_map_id]  <= pc;
    end else
      inc_map_id_w.wset(map_id);

    if (cache_data matches tagged Valid .cache_data_v)
    begin
      Loop_Cnt_t cnt = cache_data_v.cur_cnt;
      if (map_data matches tagged Valid .map_data_v)
        cnt = cnt + map_data_v;
      if (cnt == cache_data_v.max_cnt)
        rspv.taken = True;
      if (!cache_data_v.trained) 
        // in training, always jump
        rspv.taken = True;
      rspv.id = tuple2(map_id, osq_wr);
    end
    osq_mgr.inc_wr();
    osq_id_r[osq_wr] <= map_id;
    rsp_agent.enq(rspv);
  endrule

  rule rl_handle_cmt;
    let           reqv = cmt_agent.first();
    Bit#(NOVA_CFG_LOOP_OSQ_ENTRIES) osq_flush_nxt = osq_flush;
    Bool osq_rd_inc = False;
    Loop_Cnt_t flush_cnt = 0;

    Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Loop_Cnt_t) map_cnt_nxt;
    for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_MAP_ENTRIES); i=i+1)
      map_cnt_nxt[i] = map_cnt[i];

    if (new_map_id_w.wget matches tagged Valid .new_map_id)
      map_cnt_nxt[new_map_id] = 1;

    if (inc_map_id_w.wget matches tagged Valid .inc_map_id)
      map_cnt_nxt[inc_map_id] = map_cnt[inc_map_id] + 1;

    if (reqv.flush matches tagged Valid .flush_id)
    begin
      match {.map_id, .osq_id} = flush_id;
      // mark osq entries as flushed
      for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_OSQ_ENTRIES); i=i+1)
      begin
        LOOP_OSQ_ID_t ii = fromInteger(i);
        if (osq_mgr.is_valid(ii, osq_id))
        begin
          osq_flush_nxt[i] = 1'b1;
          flush_cnt = flush_cnt + 1;
        end
      end
      // update map cnt
      map_cnt_nxt[map_id] = map_cnt[map_id] - flush_cnt;
    end

    if (reqv.commit || reqv.excp)
    begin
      osq_flush_nxt[osq_rd] = 1'b0;
      if (!retire_flush)
      begin
        if (reqv.commit)
        begin
          cmt_agent.deq();
        end
        else if (reqv.excp)
        begin
          cmt_agent.deq();
        end
      end 

      // freeup osq rd
      osq_rd_inc = True;

      // push pop retire loop
      if (reqv.commit && !retire_flush)
      begin
        // update map cnt
        map_cnt_nxt[retire_id] = map_cnt[retire_id] - 1;
      end
    end else begin
      cmt_agent.deq();
      if (retire_flush)
      begin
        osq_flush_nxt[osq_rd] = 1'b0;
        osq_rd_inc = True;
      end 
    end
    
    if (osq_rd_inc)
        osq_mgr.inc_rd_nb;
    writeVReg(map_cnt_r, map_cnt_nxt);
    osq_flush_r <= osq_flush_nxt;
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req_agent, rsp_agent);
  interface alloc       = toPut(alloc_agent);
  interface cmt         = toPut(cmt_agent);
endmodule: mkNOVA_BPC_LOOP

endpackage
