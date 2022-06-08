
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

typedef  Bit #(14)      Loop_Cnt_t;
typedef struct {
  PC_t                  target;
} BPC_LOOP_Pack_t
deriving (FShow, Bits);

(* synthesize *)
module mkNOVA_BPC_ITA (NOVA_BPC_ITA_IFC);
  // ----------------
  // Instances
  SpCache#(NOVA_CFG_ITA_ENTRIES, NOVA_CFG_ITA_ASSO, PC_t, ITA_TAG_t, ITA_SET_ID_t, ITA_ASSO_ID_t) 
                            cache <- mkSpCache;

  GPCvt #(BPC_SPLBP_REQ_t)                req_agent   <- mkGPCvt;
  GPCvt #(BPC_ITA_RSP_t)                  rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_SPLBP_ALLOC_t)              alloc_agent <- mkGPCvt;
  GPCvt #(BPC_ITA_CMT_t)                  cmt_agent   <- mkGPCvt;

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req_agent, rsp_agent);
  interface alloc       = toPut(alloc_agent);
  interface cmt         = toPut(cmt_agent);
endmodule: mkNOVA_BPC_ITA

endpackage
