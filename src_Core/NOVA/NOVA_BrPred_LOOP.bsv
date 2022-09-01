
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
  Reg#(Bit#(NOVA_CFG_LOOP_MAP_ENTRIES))
                            map_trained_r <- mkSRegA(0);
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Reg#(Loop_Cnt_t))
                            map_max_r <- replicateM(mkSRegA(0));
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Reg#(Loop_Cnt_t))
                            map_cnt_r <- replicateM(mkSRegA(0)); // latest spec loop cnt in map
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Reg#(LOOP_OSQ_ID_t))
                            map_osq_r <- replicateM(mkSRegA(0)); // osq entries-1 for this map entries
  FreeQueMgr#(NOVA_CFG_LOOP_MAP_ENTRIES, LOOP_MAP_ID_t)
                            map_mgr   <- mkFreeQueMgr;

  FIFOF #(BPC_LOOP_RSP_t)                 rsp_agent   <- mkPipelineFIFOF;
  FIFOR #(BPC_LOOP_RSP_t)                 loop_rsp_rd <- mkFIFOR(rsp_agent);
  RWire#(BPC_LOOP_CMT_t)                  cmt_wire    <- mkRWireSBR;

  RWire#(LOOP_MAP_ID_t)                   new_map_id_w      <- mkRWireSBR;
  Wire#(Loop_Cnt_t)                       new_map_cnt_w     <- mkWire;
  Wire#(Loop_Cnt_t)                       new_map_max_w     <- mkWire;
  Wire#(Bool)                             new_map_trained_w <- mkWire;
  RWire#(LOOP_MAP_ID_t)                   inc_map_id_w      <- mkRWireSBR;

  // ----------------
  // States

  Bool                                  osq_full  = osq_mgr.is_full;
  LOOP_OSQ_ID_t                         osq_rd    = osq_mgr.get_rd;
  LOOP_OSQ_ID_t                         osq_wr    = osq_mgr.get_wr;

  Vector#(NOVA_CFG_LOOP_OSQ_ENTRIES, LOOP_MAP_ID_t)
                 osq_id       = readVReg(osq_id_r);
  Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, PC_t)
                 map_pc       = readVReg(map_pc_r);
  Bit#(NOVA_CFG_LOOP_OSQ_ENTRIES) osq_flush = osq_flush_r;
  Bool           retire_flush = osq_flush[osq_rd] == 1'b1;
  LOOP_MAP_ID_t  retire_id    = osq_id[osq_rd];

  // ----------------
  // Rules 
  let alloc_put =
  (interface Put#(BPC_SPLBP_ALLOC_t);
    method Action put(BPC_SPLBP_ALLOC_t reqv);
      PC_t          pc   = {reqv.pc_h, reqv.pc_os};
    endmethod
  endinterface);

  let req_put =
  (interface Put#(BPC_SPLBP_REQ_t);
    method Action put(BPC_SPLBP_REQ_t reqv);
    BPC_LOOP_RSP_t rspv = unpack(fromInteger(0));
    PC_t           pc   = {reqv.pc_h, reqv.pc_os};
    // search for cache for loop instr first
    let cache_data = cnt_cache.rd_data(pc);
    
    // search a full set map which take priority, as all counter inc/dec are maintained in this map
    LOOP_MAP_ID_t map_id = unpack(fromInteger(0));
    Maybe#(LOOP_MAP_ID_t) map_hit_id = Invalid;
    for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_MAP_ENTRIES); i=i+1)
      if (map_pc[i] == pc)
        map_hit_id = tagged Valid fromInteger(i);

    if (!isValid(map_hit_id))
    begin 
      // allocate a new map entries
      let new_map_id <- map_mgr.get_entry();
      new_map_id_w.wset(new_map_id);
      map_id = new_map_id;
      map_pc_r[new_map_id]  <= pc;
    end else begin
      map_id = map_hit_id.Valid;
      inc_map_id_w.wset(map_hit_id.Valid);
    end

    Loop_Cnt_t rd_cnt = 0;
    Loop_Cnt_t rd_max = 0;
    Bool       rd_trained = True;

    if (cache_data matches tagged Valid .cache_data_v)
    begin
      rd_cnt = cache_data_v.cur_cnt;
      rd_max = cache_data_v.max_cnt;
      rd_trained = cache_data_v.trained;

      // send the cur count to map
      new_map_cnt_w     <= cache_data_v.cur_cnt;
      new_map_max_w     <= cache_data_v.max_cnt;
      new_map_trained_w <= cache_data_v.trained;
    end else begin
      // else this is a new entry
      new_map_cnt_w     <= 1;
      new_map_max_w     <= 0;
      new_map_trained_w <= False;
    end

    if (map_hit_id matches tagged Valid .map_id_v)
    begin
      rd_cnt = map_cnt_r[map_id_v];
      rd_max = map_max_r[map_id_v];
      rd_trained = map_trained_r[map_id_v] == 1'b1;
    end

    if (rd_cnt == rd_max)
      rspv.taken = True;
    if (!rd_trained) 
      // in training, always jump
      rspv.taken = True;
    rspv.id = tuple2(map_id, osq_wr);
    // rspv.target_pc not set, LOOP does not predict target pc

    osq_mgr.inc_wr();
    osq_id_r[osq_wr] <= map_id;
    rsp_agent.enq(rspv);
    endmethod
  endinterface);

  let cmt_put =
  (interface Put#(BPC_LOOP_CMT_t);
    method Action put(BPC_LOOP_CMT_t reqv) if (!retire_flush);
      cmt_wire.wset(reqv);
    endmethod
  endinterface);

  rule rl_handle_cmt;
    Bit#(NOVA_CFG_LOOP_OSQ_ENTRIES) osq_flush_nxt = osq_flush;
    Bool osq_rd_inc = False;
    LOOP_OSQ_ID_t flush_cnt = 0;

    Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Loop_Cnt_t)    map_cnt_nxt;
    Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, Loop_Cnt_t)    map_max_nxt;
    Vector#(NOVA_CFG_LOOP_MAP_ENTRIES, LOOP_OSQ_ID_t) map_osq_nxt;
    Bit#(NOVA_CFG_LOOP_MAP_ENTRIES) map_trained_nxt = map_trained_r;

    for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_MAP_ENTRIES); i=i+1)
    begin
      map_cnt_nxt[i] = map_cnt_r[i];
      map_max_nxt[i] = map_max_r[i];
      map_osq_nxt[i] = map_osq_r[i];
    end

    if (new_map_id_w.wget matches tagged Valid .new_map_id)
    begin
      map_cnt_nxt[new_map_id]     = new_map_cnt_w;
      map_max_nxt[new_map_id]     = new_map_max_w;
      map_osq_nxt[new_map_id]     = 0;
      map_trained_nxt[new_map_id] = pack(new_map_trained_w);
    end

    if (inc_map_id_w.wget matches tagged Valid .inc_map_id)
    begin
      map_cnt_nxt[inc_map_id] = map_cnt_r[inc_map_id] + 1;
      map_osq_nxt[inc_map_id] = map_osq_r[inc_map_id] + 1;
    end

    if (cmt_wire.wget matches tagged Valid .reqv)
    begin
      if (reqv.flush matches tagged Valid .flush_id)
      begin
        match {.map_id, .osq_id} = flush_id;
        // mark osq entries as flushed
        for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_OSQ_ENTRIES); i=i+1)
        begin
          LOOP_OSQ_ID_t ii = fromInteger(i);
          if (osq_mgr.is_valid(ii, osq_id) && osq_flush[i] == 1'b0)
          begin
            osq_flush_nxt[i] = 1'b1;
            flush_cnt = flush_cnt + 1;
          end
        end
        // update map cnt
        map_cnt_nxt[map_id] = map_cnt_r[map_id] - zeroExtend(flush_cnt);
        map_osq_nxt[map_id] = map_osq_r[map_id] - flush_cnt;

        // if this is a mispred and the pc is in training, the cur cnt is the max cnt
        // need to update cache with this max cnt
        if (reqv.flush_mispred && map_trained_r[map_id] == 1'b0)
        begin
          map_max_nxt[map_id] = map_cnt_nxt[map_id];
          map_trained_nxt[map_id] = 1'b1;
        end
      end

      if (reqv.commit || reqv.excp)
      begin
        osq_flush_nxt[osq_rd] = 1'b0;
        // freeup osq rd
        osq_rd_inc = True;

        // push pop retire loop
        if (reqv.commit && !retire_flush)
        begin
          // update map cnt
          //map_cnt_nxt[retire_id] = map_cnt_r[retire_id] - 1;
          map_osq_nxt[retire_id] = map_osq_r[retire_id] - 1;
        end
      end
    end else begin
      if (retire_flush)
      begin
        osq_flush_nxt[osq_rd] = 1'b0;
        osq_rd_inc = True;
      end 
    end
    
    if (osq_rd_inc)
        osq_mgr.inc_rd_nb;
    writeVReg(map_cnt_r, map_cnt_nxt);
    writeVReg(map_max_r, map_max_nxt);
    writeVReg(map_osq_r, map_osq_nxt);
    map_trained_r <= map_trained_nxt;
    osq_flush_r <= osq_flush_nxt;

    // update cache if map is deallocated
    Maybe#(LOOP_MAP_ID_t) cache_upd_map_id = Invalid;
    for (Integer i = 0; i < valueOf(NOVA_CFG_LOOP_MAP_ENTRIES); i=i+1)
      if (map_osq_nxt[i] == 0)
        cache_upd_map_id = tagged Valid fromInteger(i);

    if (cache_upd_map_id matches tagged Valid .map_id)
    begin
      let cache_entry = BPC_LOOP_Pack_t {
          max_cnt : map_max_nxt[map_id],
          cur_cnt : map_cnt_nxt[map_id],
          trained : map_trained_nxt[map_id] == 1'b1
        };
      map_mgr.free_entry(map_id);
      cnt_cache.wr_data(map_pc[map_id], cache_entry);
    end
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_req    = toPut(req_put);
  interface lkup_rsp    = loop_rsp_rd;
  interface alloc       = toPut(alloc_put);
  interface cmt         = toPut(cmt_put);
endmodule: mkNOVA_BPC_LOOP

endpackage
