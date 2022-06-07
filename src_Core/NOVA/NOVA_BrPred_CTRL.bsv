
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_CTRL;
// ================================================================
// Exports

export mkNOVA_BPC_CTRL;

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

endpackage
