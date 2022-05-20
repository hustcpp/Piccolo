
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
  Bool                  has_new_bp;  // if brcc in the fetch, new prediction is made with new bp_id
  Bool                  has_taken_brcc;
  Bool                  loop_start;
  Bool                  cross_boundry;
  Maybe#(L0_BTB_ID_t)   itb_l0_btb_id; // this prediction may not from l0 btb
  ITB_BP_SIG_t          itb_l0_bp_sig;
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
  L0_BTB_ID_t           itb_btb_id;
} BPC_IFC_ITBF_Pack_t
deriving (FShow, Bits);

typedef struct {
  IFetch_HAddr_t        pc_h;
  Vector#(NOVA_CFG_BPC_FETCH_HW, Br_Class_t) 
                        br_class;
} BPC_L0_BTB_INFO_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  Vector#(NOVA_CFG_BPC_FETCH_HW, Maybe#(BPQ_PRED_HF_POS_t))
                        target_pos;
} BPC_L0_BTB_MAP_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  Vector#(NOVA_CFG_BPC_PRED_HW, Maybe#(PC_t))
                        target_pc;
} BPC_L0_BTB_ADDR_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  Vector#(2, IFetch_HAddr_t) pc_h;
} BPC_BTB_REQ_t
deriving (FShow, Bits);

typedef struct {
  Vector#(2, Maybe#(btb_id_t))  btb_id;
  Vector#(2, BPC_L0_BTB_INFO_ENTRY_t)
                        btb_info;
  Vector#(2, BPC_L0_BTB_MAP_ENTRY_t)
                        btb_map;
  Vector#(2, BPC_L0_BTB_ADDR_ENTRY_t)
                        btb_addr;
} BPC_BTB_RSP_t#(type btb_id_t) 
deriving (FShow, Bits);
typedef BPC_BTB_RSP_t#(L0_BTB_HF_ID_t) BPC_L0_BTB_RSP_t;
typedef BPC_BTB_RSP_t#(L1_BTB_HF_ID_t) BPC_L1_BTB_RSP_t;
typedef BPC_BTB_RSP_t#(L2_BTB_HF_ID_t) BPC_L2_BTB_RSP_t;

typedef struct {
  BPC_BHT_t                 ght;
  IFetch_LAddr_t            pc_os;
  BPC_BTB_REQ_t             btb_req;
  BPC_BTB_RSP_t#(btb_id_t)  btb_rsp;
} BPC_BPP_REQ_t#(type btb_id_t)
deriving (FShow, Bits);
typedef BPC_BPP_REQ_t#(L0_BTB_HF_ID_t) BPC_L0_BPP_REQ_t;
typedef BPC_BPP_REQ_t#(L1_BTB_HF_ID_t) BPC_L1_BPP_REQ_t;
typedef BPC_BPP_REQ_t#(L2_BTB_HF_ID_t) BPC_L2_BPP_REQ_t;

typedef struct {
  Maybe#(sig_t)         bp_sig;
  IFetch_LAddr_t        pc_os_end;
  IFetch_LAddr_t        untaken_brcc_cnt;
} BPC_BPP_RSP_t#(type sig_t) 
deriving (FShow, Bits);
typedef BPC_BPP_RSP_t#(L0_BPP_SIG_t) BPC_L0_BPP_RSP_t;
typedef BPC_BPP_RSP_t#(L1_BPP_SIG_t) BPC_L1_BPP_RSP_t;
typedef BPC_BPP_RSP_t#(L2_BPP_SIG_t) BPC_L2_BPP_RSP_t;

typedef struct {
  Maybe#(L0_BTB_HF_ID_t)  btb_id;
  BPC_L0_BTB_INFO_ENTRY_t info;
  //BPC_L0_BTB_MAP_ENTRY_t  map;
} BPC_BTB_UPDT_INFO_REQ_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  Maybe#(L0_BTB_HF_ID_t)  btb_id;
  IFetch_HAddr_t          pc_h;
  BPC_L0_BTB_ADDR_ENTRY_t e;
  Vector#(NOVA_CFG_BPC_PRED_HW, IFetch_HF_POS_t)
                          target_pos;
} BPC_BTB_UPDT_ADDR_REQ_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  Vector#(2, Maybe#(BPC_BTB_UPDT_INFO_REQ_ENTRY_t)) d;
  Vector#(2, Maybe#(BPC_BTB_UPDT_ADDR_REQ_ENTRY_t)) a;
} BPC_BTB_UPDT_REQ_t
deriving (FShow, Bits);

typedef struct {
  Vector#(2, Maybe#(btb_id_t))     rpl_btb_id; // replaced btb id
} BPC_BTB_UPDT_RSP_t#(type btb_id_t)
deriving (FShow, Bits);
typedef BPC_BTB_UPDT_RSP_t#(L0_BTB_HF_ID_t) BPC_L0_BTB_UPDT_RSP_t;
typedef BPC_BTB_UPDT_RSP_t#(L1_BTB_HF_ID_t) BPC_L1_BTB_UPDT_RSP_t;
typedef BPC_BTB_UPDT_RSP_t#(L2_BTB_HF_ID_t) BPC_L2_BTB_UPDT_RSP_t;

typedef struct {
  IFetch_HAddr_t        pc_h;
  IFetch_LAddr_t        pc_os;
  BPC_BHT_t             ght;
  Bool                  taken;
  Br_Class_t            br_class;
} BPC_BPP_UPDT_REQ_t
deriving (FShow, Bits);

typedef struct {
  Bool                  dummy;
} BPC_BPP_UPDT_RSP_t
deriving (FShow, Bits);

typedef struct {
  Vector#(2, IFetch_HAddr_t) pc_h;
  IFetch_LAddr_t        pc_os;
  BPC_BHT_t             ght;
  Vector#(NOVA_CFG_BPC_FETCH_W, Br_Class_t) 
                        br_class;
} BPC_SPLBP_REQ_t
deriving (FShow, Bits);

typedef struct {
  Bool                  taken;
  IFetch_LAddr_t        pc_os;
  PC_t                  target_pc;
} BPC_SPLBP_RSP_t 
deriving (FShow, Bits);

typedef struct {
  IFetch_HAddr_t        pc_h;
  IFetch_LAddr_t        pc_os;
  BPC_BHT_t             ght;
  PC_t                  target_pc;
} BPC_SPLBP_ALLOC_t
deriving (FShow, Bits);

typedef struct {
  Bool                  flush;
} BPC_SPLBP_CMT_t
deriving (FShow, Bits);

typedef struct {
  PC_t                  excp_base;
} BPC_CFG_t
deriving (FShow, Bits);

typedef  Bit #(2)             IFC_PHTE_t;  // prediction history table entry

// ================================================================
// Interfaces

interface NOVA_BPC_BPQ_IFC;
  // branch flush to IFC
  interface Get #(IFC_BPC_BRF_Pack_t)  ifc_brf_intf;
  // handle branch flush from ctrl
  interface Put #(IFC_BPC_BRF_Pack_t)  flush_intf;
  // handle enqueue from ctrl
  interface Put #(IFC_BPC_BPQ_Pack_t)  enq_intf;
  // handle dequeue from ifc
  interface Get #(IFC_BPC_BPQ_Pack_t)  ifc_deq_intf;

endinterface

interface NOVA_BPC_L0_BTB_IFC;
  interface Server#(BPC_BTB_REQ_t, BPC_L0_BTB_RSP_t) lkup_server;
  interface Server#(BPC_BTB_UPDT_REQ_t, BPC_L0_BTB_UPDT_RSP_t) updt_server;
endinterface

interface NOVA_BPC_L0_BPP_IFC;
  interface Server#(BPC_L0_BPP_REQ_t, BPC_L0_BPP_RSP_t) lkup_server;
  interface Server#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t) updt_server;
endinterface

interface NOVA_BPC_L1_BTB_IFC;
  interface Server#(BPC_BTB_REQ_t, BPC_L1_BTB_RSP_t) lkup_server;
  interface Server#(BPC_BTB_UPDT_REQ_t, BPC_L1_BTB_UPDT_RSP_t) updt_server;
endinterface

interface NOVA_BPC_L1_BPP_IFC;
  interface Server#(BPC_L1_BPP_REQ_t, BPC_L1_BPP_RSP_t) lkup_server;
  interface Server#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t) updt_server;
endinterface

interface NOVA_BPC_L2_BTB_IFC;
  interface Server#(BPC_BTB_REQ_t, BPC_L2_BTB_RSP_t) lkup_server;
  interface Server#(BPC_BTB_UPDT_REQ_t, BPC_L2_BTB_UPDT_RSP_t) updt_server;
endinterface

interface NOVA_BPC_L2_BPP_IFC;
  interface Server#(BPC_L2_BPP_REQ_t, BPC_L2_BPP_RSP_t) lkup_server;
  interface Server#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t) updt_server;
endinterface

interface NOVA_BPC_RAS_IFC;
  interface Server#(BPC_SPLBP_REQ_t, BPC_SPLBP_RSP_t) lkup_server;
  interface Put#(BPC_SPLBP_ALLOC_t) alloc;
  interface Put#(BPC_SPLBP_CMT_t)   cmt;
endinterface

interface NOVA_BPC_ITA_IFC;
  interface Server#(BPC_SPLBP_REQ_t, BPC_SPLBP_RSP_t) lkup_server;
  interface Put#(BPC_SPLBP_ALLOC_t) alloc;
  interface Put#(BPC_SPLBP_CMT_t)   cmt;
endinterface

interface NOVA_BPC_LOOP_IFC;
  interface Server#(BPC_SPLBP_REQ_t, BPC_SPLBP_RSP_t) lkup_server;
  interface Put#(BPC_SPLBP_ALLOC_t) alloc;
  interface Put#(BPC_SPLBP_CMT_t)   cmt;
endinterface

interface NOVA_BPC_CTRL_IFC;
  interface Put #(BPC_IFC_FBU_Pack_t)  ifc_fbu_intf;
  interface Vector#(NOVA_CFG_BRU_N, Get#(EXU_BPC_BCU_Pack_t)) exu_bcu_intfs;
  interface Put #(ROB_BPC_CMT_Pack_t)  rob_cmt_intf;
  interface Put #(ROB_BPC_EXCP_Pack_t) rob_excp_intf;
  interface Put #(BPC_IFC_ITBF_Pack_t) itb_flush_intf;
  interface Get #(IFC_BPC_BRF_Pack_t)  bpq_flush_intf;
  interface Get #(IFC_BPC_BPQ_Pack_t)  bpq_enq_intf;

  interface Client#(BPC_BTB_REQ_t   , BPC_L0_BTB_RSP_t) l0_btb_client;
  interface Client#(BPC_BTB_REQ_t   , BPC_L1_BTB_RSP_t) l1_btb_client;
  interface Client#(BPC_BTB_REQ_t   , BPC_L2_BTB_RSP_t) l2_btb_client;

  interface Client#(BPC_L0_BPP_REQ_t, BPC_L0_BPP_RSP_t) l0_bpp_client;
  interface Client#(BPC_L1_BPP_REQ_t, BPC_L1_BPP_RSP_t) l1_bpp_client;
  interface Client#(BPC_L2_BPP_REQ_t, BPC_L2_BPP_RSP_t) l2_bpp_client;

  interface Client#(BPC_BTB_UPDT_REQ_t, BPC_L0_BTB_UPDT_RSP_t) l0_btb_updt_client;
  interface Client#(BPC_BTB_UPDT_REQ_t, BPC_L1_BTB_UPDT_RSP_t) l1_btb_updt_client;
  interface Client#(BPC_BTB_UPDT_REQ_t, BPC_L2_BTB_UPDT_RSP_t) l2_btb_updt_client;

  interface Client#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t)    l0_bpp_updt_client;
  interface Client#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t)    l1_bpp_updt_client;
  interface Client#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t)    l2_bpp_updt_client;

  interface Client#(BPC_SPLBP_REQ_t, BPC_SPLBP_RSP_t)    ras_lkup_client;
  interface Get#(BPC_SPLBP_ALLOC_t)                      ras_alloc;
  interface Get#(BPC_SPLBP_CMT_t)                        ras_cmt;

  interface Client#(BPC_SPLBP_REQ_t, BPC_SPLBP_RSP_t)    ita_lkup_client;
  interface Get#(BPC_SPLBP_ALLOC_t)                      ita_alloc;
  interface Get#(BPC_SPLBP_CMT_t)                        ita_cmt;

  interface Client#(BPC_SPLBP_REQ_t, BPC_SPLBP_RSP_t)    loop_lkup_client;
  interface Get#(BPC_SPLBP_ALLOC_t)                      loop_alloc;
  interface Get#(BPC_SPLBP_CMT_t)                        loop_cmt;
endinterface

interface NOVA_BrPredCplx_IFC;
  // branch prediction queue to IFC 
  interface Get #(IFC_BPC_BPQ_Pack_t)  ifc_bpq_intf;

  // branch flush to IFC
  interface Get #(IFC_BPC_BRF_Pack_t)  ifc_brf_intf;

  // IFC branch target or decode UPDT
  interface Put #(BPC_IFC_FBU_Pack_t)  ifc_fbu_intf;
  
  // EXU branch unit interfaces
  interface Vector#(NOVA_CFG_BRU_N, Get#(EXU_BPC_BCU_Pack_t)) exu_bcu_intfs;

  // ROB commit interface to free BPC resources
  interface Put #(ROB_BPC_CMT_Pack_t)  rob_cmt_intf;

  // ROB exception interface to redirect BPU
  interface Put #(ROB_BPC_EXCP_Pack_t) rob_excp_intf;

  // inform ITB a L0 BTB is flushed
  interface Put #(BPC_IFC_ITBF_Pack_t) itb_flush_intf;
  
  // external configuration
  interface Put #(BPC_CFG_t) bpc_cfg_intf;
endinterface

// ================================================================

endpackage
