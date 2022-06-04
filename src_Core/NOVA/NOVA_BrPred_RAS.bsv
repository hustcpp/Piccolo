
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_RAS;
// ================================================================
// Exports

export mkNOVA_BPC_RAS;

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

typedef TSub#(NOVA_CFG_RAS_P_ENTRIES, 1) P_RAS_MAX;
typedef TSub#(NOVA_CFG_RAS_OSQ_ENTRIES, 1) OSQ_RAS_MAX;

(* synthesize *)
module mkNOVA_BPC_RAS (NOVA_BPC_RAS_IFC);
  // ----------------
  // Instances
  RegFile#(RAS_PID_t, PC_t)          pras_r <- mkRegFile(0, fromInteger(valueOf(P_RAS_MAX)));
  RegFile#(RAS_PID_t, RAS_PID_t)     pras_link_r <- mkRegFile(0, fromInteger(valueOf(P_RAS_MAX)));
  Reg#(Bit#(NOVA_CFG_RAS_P_ENTRIES)) pras_vld_r  <- mkRegA(0);
  Reg#(RAS_PID_t)                    pras_top_r  <- mkRegA(0);
  FreeQueMgr#(NOVA_CFG_RAS_P_ENTRIES, RAS_PID_t)
                                     pras_mgr    <- mkFreeQueMgr;

  RegFile#(RAS_OSQ_ID_t, RAS_PID_t)    osq_pid_r   <- mkRegFile(0, fromInteger(valueOf(OSQ_RAS_MAX)));
  Reg#(Bit#(NOVA_CFG_RAS_OSQ_ENTRIES)) osq_call_r  <- mkRegA(0);
  Reg#(Bit#(NOVA_CFG_RAS_OSQ_ENTRIES)) osq_flush_r <- mkRegA(0);
  FIFOMgr#(NOVA_CFG_RAS_OSQ_ENTRIES, RAS_OSQ_ID_t)
                                       osq_mgr     <- mkFIFOMgr;

  Que#(NOVA_CFG_RAS_CMT_ENTRIES, RAS_PID_t) cmt_ras   <- mkSizedQue;

  GPCvt #(BPC_SPLBP_REQ_t)                req_agent   <- mkGPCvt;
  GPCvt #(BPC_RAS_RSP_t)                  rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_SPLBP_ALLOC_t)              alloc_agent <- mkGPCvt;
  GPCvt #(BPC_RAS_CMT_t)                  cmt_agent   <- mkGPCvt;
  

  // ----------------
  // States

  // ----------------
  // Rules 
  Bool                                  osq_full  = osq_mgr.is_full;
  RAS_OSQ_ID_t                          osq_rd    = osq_mgr.get_rd;
  RAS_OSQ_ID_t                          osq_wr    = osq_mgr.get_wr;

  Bit#(NOVA_CFG_RAS_OSQ_ENTRIES) osq_call  = osq_call_r;
  Bit#(NOVA_CFG_RAS_OSQ_ENTRIES) osq_flush = osq_flush_r;
  RAS_PID_t              pras_top = pras_top_r;
  Bool      pras_full    = pras_mgr.is_full;
  Bool      pras_empty   = pras_mgr.is_empty;
  PC_t      pc_top       = pras_r.sub(pras_top);
  RAS_PID_t retire_pid   = osq_pid_r.sub(osq_rd);
  Bool      retire_call  = osq_call[osq_rd] == 1'b1;
  Bool      retire_flush = osq_flush[osq_rd] == 1'b1;

  function Action new_call(PC_t ret_pc);
  action
    RAS_PID_t pid <- pras_mgr.get_entry();
    pras_top_r <= pid;
    pras_r.upd(pid, ret_pc);
    osq_call_r[osq_wr]  <= 1'b1;
    osq_pid_r.upd(osq_wr, pid);
    osq_mgr.inc_wr();
  endaction
  endfunction

  function Action new_ret();
  action
    osq_call_r[osq_wr]  <= 1'b0;
    pras_top_r <= pras_link_r.sub(pras_top);
    osq_mgr.inc_wr();
  endaction
  endfunction

  (* descending_urgency = "rl_handle_alloc, rl_handle_lkup" *)

  rule rl_handle_alloc;
    let           reqv = alloc_agent.first();
    alloc_agent.deq();
    new_call(reqv.target_pc);
  endrule

  rule rl_handle_lkup;
    let           reqv = req_agent.first();
    BPC_RAS_RSP_t rspv = unpack(fromInteger(0));
    req_agent.deq();
    new_ret();
    rspv.taken      = True && !pras_empty;
    rspv.id         = osq_wr;
    rspv.target_pc  = pc_top;
    rsp_agent.enq(rspv);
  endrule

  rule rl_handle_cmt;
    let           reqv = cmt_agent.first();
    Bit#(NOVA_CFG_RAS_OSQ_ENTRIES) osq_flush_nxt = osq_flush;
    Bool osq_rd_inc = False;
    Bit#(NOVA_CFG_RAS_P_ENTRIES) free_entry = 0;

    if (reqv.flush matches tagged Valid .flush_id)
    begin
      // mark osq entries as flushed
      for (Integer i = 0; i < valueOf(NOVA_CFG_RAS_P_ENTRIES); i=i+1)
      begin
        RAS_OSQ_ID_t ii = fromInteger(i);
        if (osq_mgr.is_valid(ii, flush_id))
          osq_flush_nxt[i] = 1'b1;
      end
    end
    

    if (reqv.commit || reqv.excp)
    begin
      if (retire_flush)
      begin
        osq_flush_nxt[osq_rd] = 1'b0;
      end 
      else if (reqv.commit)
      begin
        cmt_agent.deq();
      end
      else if (reqv.excp)
      begin
        cmt_agent.deq();
      end

      // freeup osq rd
      osq_rd_inc = True;
      if (retire_flush || reqv.excp)
        free_entry[retire_pid] = 1'b1;

      // push pop retire ras
      if (reqv.commit && !retire_flush)
      begin
        if (retire_call)
        begin
          if (cmt_ras.full())
          begin
            // freeup end of commit ras
            free_entry[cmt_ras.last()] = 1'b1;
            cmt_ras.deq_last();
          end
          // push ras
          cmt_ras.enq(retire_pid);
        end else begin
          // pop and freeup ras
          free_entry[cmt_ras.front()] = 1'b1;
          cmt_ras.deq_front();
        end
      end
    end else begin
      cmt_agent.deq();
      if (retire_flush)
      begin
        osq_flush_nxt[osq_rd] = 1'b0;

        // freeup osq rd
        free_entry[retire_pid] = 1'b1;
        osq_rd_inc = True;
      end 
    end
    
    if (osq_rd_inc)
        osq_mgr.inc_rd;
    pras_mgr.free_multi_entry(free_entry);
    osq_flush_r <= osq_flush_nxt;
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req_agent, rsp_agent);
  interface alloc       = toPut(alloc_agent);
  interface cmt         = toPut(cmt_agent);
endmodule: mkNOVA_BPC_RAS

endpackage
