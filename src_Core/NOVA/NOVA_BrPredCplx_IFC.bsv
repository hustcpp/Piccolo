
package NOVA_BrPredCplx_IFC;

// ================================================================
// BSV library imports

import GetPut       :: *;
import ClientServer :: *;

import Vector::*;

// ================================================================
// Imports

import ISA_Decls       :: *;
import NOVA_Decls      :: *;

// ================================================================
// Type defines

typedef struct {
  IFetch_HAddr_t        pc_h;
  IFetch_LAddr_t        pc_os_start;
  IFetch_LAddr_t        pc_os_end;
  BP_ID_t               bp_id;
  Bool                  loop_start;
  Bool                  cross_boundry;
  Maybe#(ITB_BP_ID_t)   itb_bp_id;
} IFC_BPC_BPQ_Pack_t
deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
} IFC_BPC_BRF_Pack_t
deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
  Vector#(NOVA_CFG_BPC_FETCH_W, Br_Class_t) 
                        br_class;
  Bool                  miss;
  IFetch_LAddr_t        miss_pc_os;
  IFetch_HAddr_t        miss_pc_h;
  PC_t                  miss_pc_target;
} BPC_IFC_FBU_Pack_t
deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
  Br_Class_t            br_class;
  IFetch_LAddr_t        pc_os;
  IFetch_HAddr_t        pc_h;
  PC_t                  pc_target;
  Bool                  taken;
} EXU_BPC_BCU_Pack_t
deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
} ROB_BPC_CMT_Pack_t
deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
  Bit #(8)              excp_type;
} ROB_BPC_EXCP_Pack_t
deriving (FShow, Bits);

typedef struct {
  ITB_BP_ID_t           itb_bp_id;
} ROB_IFC_ITBF_Pack_t
deriving (FShow, Bits);

// ================================================================
// Interfaces

interface NOVA_BPC_BPQ_IFC;
   // branch prediction queue to IFC 
   interface Get #(IFC_BPC_BPQ_Pack_t)  ifc_bpq_intf;
   // branch flush to IFC
   interface Get #(IFC_BPC_BRF_Pack_t)  ifc_brf_intf;


   interface Put #(IFC_BPC_BPQ_Pack_t)  l0_enq;
   interface Put #(IFC_BPC_BPQ_Pack_t)  l1_flush;
   interface Put #(IFC_BPC_BPQ_Pack_t)  l2_flush;
endinterface

interface NOVA_BPC_L0_BTB_IFC;
   // inform ITB a L0 BTB is flushed
   interface Put #(ROB_IFC_ITBF_Pack_t) itb_flush_intf;
endinterface

interface NOVA_BPC_L0_BPP_IFC;
endinterface

interface NOVA_BPC_L1_BTB_IFC;
endinterface

interface NOVA_BPC_L1_BPP_IFC;
endinterface

interface NOVA_BPC_L2_BTB_IFC;
endinterface

interface NOVA_BPC_L2_BPP_IFC;
endinterface

interface NOVA_BPC_RAS_IFC;
endinterface

interface NOVA_BPC_ITA_IFC;
endinterface

interface NOVA_BPC_LOOP_IFC;
endinterface

interface NOVA_BPC_CTRL_IFC;
   interface Put #(BPC_IFC_FBU_Pack_t)  ifc_fbu_intf;
   interface Vector#(NOVA_CFG_BRU_N, Get#(EXU_BPC_BCU_Pack_t)) exu_bcu_intfs;
   interface Put #(ROB_BPC_CMT_Pack_t)  rob_cmt_intf;
   interface Put #(ROB_BPC_EXCP_Pack_t) rob_excp_intf;
endinterface

interface NOVA_BrPredCplx_IFC;
   // branch prediction queue to IFC 
   interface Get #(IFC_BPC_BPQ_Pack_t)  ifc_bpq_intf;

   // branch flush to IFC
   interface Get #(IFC_BPC_BRF_Pack_t)  ifc_brf_intf;

   // IFC branch target or decode update
   interface Put #(BPC_IFC_FBU_Pack_t)  ifc_fbu_intf;
   
   // EXU branch unit interfaces
   interface Vector#(NOVA_CFG_BRU_N, Get#(EXU_BPC_BCU_Pack_t)) exu_bcu_intfs;

   // ROB commit interface to free BPC resources
   interface Put #(ROB_BPC_CMT_Pack_t)  rob_cmt_intf;

   // ROB exception interface to redirect BPU
   interface Put #(ROB_BPC_EXCP_Pack_t) rob_excp_intf;

   // inform ITB a L0 BTB is flushed
   interface Put #(ROB_IFC_ITBF_Pack_t) itb_flush_intf;
endinterface

// ================================================================

endpackage
