
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
import NOVA_BrPredCplx_IFC     :: *;


(* synthesize *)
module mkNOVA_BPC_BPQ (NOVA_BPC_BPQ_IFC);
   // ----------------
   // Instances
   FIFOF #(IFC_BPC_BPQ_Pack_t)  ifc_bpq_fifo <- mkFIFOF;
   FIFOF #(IFC_BPC_BRF_Pack_t)  ifc_brf_fifo <- mkFIFOF;


   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
   interface ifc_bpq_intf = toGet(ifc_bpq_fifo);
   interface ifc_brf_intf = toGet(ifc_brf_fifo);
endmodule: mkNOVA_BPC_BPQ

(* synthesize *)
module mkNOVA_BPC_L0_BTB (NOVA_BPC_L0_BTB_IFC);
   // ----------------
   // Instances
   FIFOF #(ROB_IFC_ITBF_Pack_t)  itb_flush_fifo <- mkFIFOF;

   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
   interface itb_flush_intf = toPut(itb_flush_fifo);
endmodule: mkNOVA_BPC_L0_BTB

(* synthesize *)
module mkNOVA_BPC_L0_BPP (NOVA_BPC_L0_BPP_IFC);
   // ----------------
   // Instances

   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
endmodule: mkNOVA_BPC_L0_BPP

(* synthesize *)
module mkNOVA_BPC_L1_BTB (NOVA_BPC_L1_BTB_IFC);
   // ----------------
   // Instances

   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
endmodule: mkNOVA_BPC_L1_BTB

(* synthesize *)
module mkNOVA_BPC_L1_BPP (NOVA_BPC_L1_BPP_IFC);
   // ----------------
   // Instances

   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
endmodule: mkNOVA_BPC_L1_BPP

(* synthesize *)
module mkNOVA_BPC_L2_BTB (NOVA_BPC_L2_BTB_IFC);
   // ----------------
   // Instances

   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
endmodule: mkNOVA_BPC_L2_BTB

(* synthesize *)
module mkNOVA_BPC_L2_BPP (NOVA_BPC_L2_BPP_IFC);
   // ----------------
   // Instances

   // ----------------
   // States

   // ----------------
   // Rules 

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

   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
   interface ifc_fbu_intf = toPut(ifc_fbu_fifo);
   interface exu_bcu_intfs = map(toGet, exu_bcu_fifos);
   interface rob_cmt_intf  = toPut(rob_cmt_fifo);
   interface rob_excp_intf = toPut(rob_excp_fifo);
endmodule: mkNOVA_BPC_CTRL

(* synthesize *)
module mkNOVA_BrPredCplx (NOVA_BrPredCplx_IFC);
   // ----------------
   // Instances
   NOVA_BPC_BPQ_IFC     bpq    <- mkNOVA_BPC_BPQ;
   NOVA_BPC_L0_BTB_IFC  l0_btb <- mkNOVA_BPC_L0_BTB;
   NOVA_BPC_L0_BPP_IFC  l0_bpp <- mkNOVA_BPC_L0_BPP;
   NOVA_BPC_L1_BTB_IFC  l1_btb <- mkNOVA_BPC_L1_BTB;
   NOVA_BPC_L1_BPP_IFC  l1_bpp <- mkNOVA_BPC_L1_BPP;
   NOVA_BPC_L2_BTB_IFC  l2_btb <- mkNOVA_BPC_L2_BTB;
   NOVA_BPC_L2_BPP_IFC  l2_bpp <- mkNOVA_BPC_L2_BPP;
   NOVA_BPC_RAS_IFC     ras    <- mkNOVA_BPC_RAS;
   NOVA_BPC_ITA_IFC     ita    <- mkNOVA_BPC_ITA;
   NOVA_BPC_LOOP_IFC    loop   <- mkNOVA_BPC_LOOP;
   NOVA_BPC_CTRL_IFC    ctrl   <- mkNOVA_BPC_CTRL;

   // ----------------
   // States

   // ----------------
   // Rules 

   // ----------------
   // Interfaces
   interface ifc_bpq_intf   = bpq.ifc_bpq_intf;
   interface ifc_brf_intf   = bpq.ifc_brf_intf;
   interface itb_flush_intf = l0_btb.itb_flush_intf;
   interface ifc_fbu_intf   = ctrl.ifc_fbu_intf;
   interface exu_bcu_intfs  = ctrl.exu_bcu_intfs;
   interface rob_cmt_intf   = ctrl.rob_cmt_intf;
   interface rob_excp_intf  = ctrl.rob_excp_intf;

endmodule: mkNOVA_BrPredCplx

endpackage
