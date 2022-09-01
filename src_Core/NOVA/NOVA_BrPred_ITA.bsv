
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

  FIFOF #(BPC_ITA_RSP_t)                  rsp_agent   <- mkPipelineFIFOF;
  FIFOR #(BPC_ITA_RSP_t)                  ita_rsp_rd  <- mkFIFOR(rsp_agent);

  // ----------------
  // States

  // ----------------
  // Rules 
  let alloc_put =
  (interface Put#(BPC_SPLBP_ALLOC_t);
    method Action put(BPC_SPLBP_ALLOC_t reqv);
    PC_t          pc   = {reqv.pc_h, reqv.pc_os};
    ITA_IDX_t     idx  = truncate(pc) ^ truncate(reqv.ght);
    cache.wr_data(idx, reqv.target_pc);
    endmethod
  endinterface);

  let req_put =
  (interface Put#(BPC_SPLBP_REQ_t);
    method Action put(BPC_SPLBP_REQ_t reqv);
    BPC_ITA_RSP_t  rspv = unpack(fromInteger(0));
    PC_t           pc   = {reqv.pc_h, reqv.pc_os};
    ITA_IDX_t     idx  = truncate(pc) ^ truncate(reqv.ght);
    let cache_data = cache.rd_data(idx);
    rspv.taken = isValid(cache_data); // indicate ITA has a predication
    rspv.target_pc = cache_data.Valid;
    rsp_agent.enq(rspv);
    endmethod
  endinterface);

  let cmt_put =
  (interface Put#(BPC_ITA_CMT_t);
    method Action put(BPC_ITA_CMT_t reqv);
    endmethod
  endinterface);

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_req    = toPut(req_put);
  interface lkup_rsp    = ita_rsp_rd;
  interface alloc       = toPut(alloc_put);
  interface cmt         = toPut(cmt_put);
endmodule: mkNOVA_BPC_ITA

endpackage
