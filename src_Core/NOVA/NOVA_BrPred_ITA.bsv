
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_ITA;
// ================================================================
// Exports

export mkNOVA_BPC_ITA;

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

(* synthesize *)
module mkNOVA_BPC_ITA (NOVA_BPC_ITA_IFC);
  // ----------------
  // Instances
  SpCache#(NOVA_CFG_ITA_ENTRIES, NOVA_CFG_ITA_ASSO, PC_t, ITA_IDX_t, ITA_SET_ID_t, ITA_ASSO_ID_t) 
                            cache <- mkSpCache;

  GPCvt #(BPC_SPLBP_REQ_t)                req_agent   <- mkGPCvt;
  GPCvt #(BPC_ITA_RSP_t)                  rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_SPLBP_ALLOC_t)              alloc_agent <- mkGPCvt;
  GPCvt #(BPC_ITA_CMT_t)                  cmt_agent   <- mkGPCvt;

  // ----------------
  // States

  // ----------------
  // Rules 
  rule rl_handle_alloc;
    let           reqv = alloc_agent.first();
    PC_t          pc   = {reqv.pc_h, reqv.pc_os};
    ITA_IDX_t     idx  = truncate(pc) ^ truncate(reqv.ght);
    cache.wr_data(idx, reqv.target_pc);
  endrule

  rule rl_handle_lkup;
    let            reqv = req_agent.first();
    BPC_ITA_RSP_t  rspv = unpack(fromInteger(0));
    PC_t           pc   = {reqv.pc_h, reqv.pc_os};
    ITA_IDX_t     idx  = truncate(pc) ^ truncate(reqv.ght);
    let cache_data = cache.rd_data(idx);
    rspv.taken = isValid(cache_data); // indicate ITA has a predication
    rspv.target_pc = cache_data.Valid;
    req_agent.deq();
    rsp_agent.enq(rspv);
  endrule

  rule rl_handle_cmt;
    let           reqv = cmt_agent.first();
    cmt_agent.deq();
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req_agent, rsp_agent);
  interface alloc       = toPut(alloc_agent);
  interface cmt         = toPut(cmt_agent);
endmodule: mkNOVA_BPC_ITA

endpackage
