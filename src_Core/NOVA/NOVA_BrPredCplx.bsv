
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
  NOVA_BPC_CTRL_IFC    ctrl   <- mkNOVA_BPC_CTRL;

  mkConnection(ctrl.bpq_flush_intf, bpq.flush_intf);
  mkConnection(ctrl.bpq_enq_intf,   bpq.enq_intf);

  mkConnection(ctrl.l0_btb_client,  l0_btb.lkup_server);
  mkConnection(ctrl.l1_btb_client,  l1_btb.lkup_server);
  mkConnection(ctrl.l2_btb_client,  l2_btb.lkup_server);

  mkConnection(ctrl.l0_bpp_client,  l0_bpp.lkup_server);
  mkConnection(ctrl.l1_bpp_client,  l1_bpp.lkup_server);
  mkConnection(ctrl.l2_bpp_client,  l2_bpp.lkup_server);

  mkConnection(ctrl.l0_btb_updt_client,  l0_btb.updt_server);
  mkConnection(ctrl.l1_btb_updt_client,  l1_btb.updt_server);
  mkConnection(ctrl.l2_btb_updt_client,  l2_btb.updt_server);

  mkConnection(ctrl.l0_bpp_updt_client,  l0_bpp.updt_server);
  mkConnection(ctrl.l1_bpp_updt_client,  l1_bpp.updt_server);
  mkConnection(ctrl.l2_bpp_updt_client,  l2_bpp.updt_server);

  mkConnection(ctrl.ita_lkup_client,  ita.lkup_server);
  mkConnection(ctrl.ita_alloc,  ita.alloc);
  mkConnection(ctrl.ita_cmt,  ita.cmt);

  mkConnection(ctrl.ita_lkup_client,  ita.lkup_server);
  mkConnection(ctrl.ita_alloc,  ita.alloc);
  mkConnection(ctrl.ita_cmt,  ita.cmt);

  mkConnection(ctrl.loop_lkup_client,  loop.lkup_server);
  mkConnection(ctrl.loop_alloc,  loop.alloc);
  mkConnection(ctrl.loop_cmt,  loop.cmt);

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
  interface exu_bcu_intfs  = ctrl.exu_bcu_intfs;
  interface rob_cmt_intf   = ctrl.rob_cmt_intf;
  interface rob_excp_intf  = ctrl.rob_excp_intf;

endmodule: mkNOVA_BrPredCplx

endpackage
