
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
import NOVA_BrPred_BPQ         :: *;
import NOVA_BrPred_BTB         :: *;
import NOVA_BrPred_BPP         :: *;
import NOVA_BrPred_RAS         :: *;
import NOVA_BrPred_LOOP        :: *;
import NOVA_BrPred_ITA         :: *;
import NOVA_BrPred_CTRL        :: *;

(* synthesize *)
module mkNOVA_BrPredCplx (NOVA_BrPredCplx_IFC);
  // ----------------
  // Instances
  NOVA_BPC_BPQ_IFC     bpq    <- mkNOVA_BPC_BPQ;
  NOVA_BPC_L0_BTB_IFC  l0_btb <- mkNOVA_BPC_L0_BTB;
  NOVA_BPC_L1_BTB_IFC  l1_btb <- mkNOVA_BPC_L1_BTB;
  NOVA_BPC_L2_BTB_IFC  l2_btb <- mkNOVA_BPC_L2_BTB;
  NOVA_BPC_L0_BPP_IFC  l0_bpp <- mkNOVA_BPC_L0_BPP;
  NOVA_BPC_L1_BPP_IFC  l1_bpp <- mkNOVA_BPC_L1_BPP;
  NOVA_BPC_L2_BPP_IFC  l2_bpp <- mkNOVA_BPC_L2_BPP;
  NOVA_BPC_RAS_IFC     ras    <- mkNOVA_BPC_RAS;
  NOVA_BPC_ITA_IFC     ita    <- mkNOVA_BPC_ITA;
  NOVA_BPC_LOOP_IFC    loop   <- mkNOVA_BPC_LOOP;


  NOVA_BPC_CTRL_Int_IFC    ctrl_intf = (interface NOVA_BPC_CTRL_Int_IFC;
    interface bpq_flush_intf    = bpq.flush_intf;
    interface bpq_enq_intf      = bpq.enq_intf;

    interface l0_btb     = l0_btb.lkup_server;
    interface l1_btb     = l1_btb.lkup_server;
    interface l2_btb     = l2_btb.lkup_server;

    interface l0_bpp     = l0_bpp.lkup_server;
    interface l1_bpp     = l1_bpp.lkup_server;
    interface l2_bpp     = l2_bpp.lkup_server;

    interface l0_btb_updt = l0_btb.updt_server;
    interface l1_btb_updt = l1_btb.updt_server;
    interface l2_btb_updt = l2_btb.updt_server;

    interface l0_bpp_updt = l0_bpp.updt_server;
    interface l1_bpp_updt = l1_bpp.updt_server;
    interface l2_bpp_updt = l2_bpp.updt_server;

    interface l0_bpp_pre_lkup = l0_bpp.pre_lkup_put;
    interface l1_bpp_pre_lkup = l1_bpp.pre_lkup_put;
    interface l2_bpp_pre_lkup = l2_bpp.pre_lkup_put;

    interface ita_lkup   =  ita.lkup_server;
    interface ita_alloc  =  ita.alloc;
    interface ita_cmt    =  ita.cmt;

    interface ras_lkup   =  ras.lkup_server;
    interface ras_alloc  =  ras.alloc;
    interface ras_cmt    =  ras.cmt;

    interface loop_lkup  =  loop.lkup_server;
    interface loop_alloc =  loop.alloc;
    interface loop_cmt   =  loop.cmt;
  endinterface);

  NOVA_BPC_CTRL_IFC ctrl   <- mkNOVA_BPC_CTRL(ctrl_intf);
  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces

  interface ifc_bpq_intf   = bpq.ifc_deq_intf;
  interface ifc_brf_intf   = bpq.ifc_brf_intf;
  interface itb_flush_intf = ctrl.itb_flush_intf;
  interface ifc_fbu_intf   = ctrl.ifc_fbu_intf;
  interface rob_cmt_intf   = ctrl.rob_cmt_intf;
  interface rob_flush_intf = ctrl.rob_flush_intf;

endmodule: mkNOVA_BrPredCplx

endpackage
