
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

// IFetch to BrPred Que
typedef struct {
  IFetch_HAddr_t        pc_h;
  IFetch_LAddr_t        pc_os_start;
  IFetch_LAddr_t        pc_os_end;
  BP_ID_t               bp_id;
  Bool                  has_taken;  // if brcc in the fetch, new osq is allocated with new bp_id
  Bool                  has_taken_brcc;
  Bool                  loop_start;
  //Bool                  cross_boundry;
  Maybe#(L0_BTB_ID_t)   itb_l0_btb_id; // this prediction may not from l0 btb
  ITB_BP_SIG_t          itb_l0_bp_sig;
} IFC_BPC_BPQ_Pack_t
deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
} IFC_BPC_BRF_Pack_t
deriving (FShow, Bits);

// BrPred to Fetch Branch Unit
typedef struct {
  BP_ID_t               bp_id;
  Vector#(NOVA_CFG_BPC_FETCH_W, Br_Class_t) 
                        br_class;
  Bool                  misspred_target;  // btb target pc misspredict
  Bool                  misspred_class;   // btb branch/jump class misspredict
  PC_t                  pc_target;
} BPC_IFC_FBU_Pack_t
deriving (FShow, Bits);

//typedef struct {
//  BP_ID_t               bp_id;
//  Br_Class_t            br_class;
//  IFetch_LAddr_t        pc_os;
//  IFetch_HAddr_t        pc_h;
//  PC_t                  pc_target;
//  Bool                  taken;
//} EXU_BPC_BCU_Pack_t
//deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
  Bool                  excp;
  PC_t                  excp_pc;
} ROB_BPC_CMT_Pack_t
deriving (FShow, Bits);

typedef struct {
  BP_ID_t               bp_id;
  Bool                  misspred_pc;
  Bool                  misspred_dir;
  PC_t                  pc_target;
  Bool                  taken;
} ROB_BPC_FLUSH_Pack_t
deriving (FShow, Bits);

typedef struct {
  L0_BTB_ID_t           itb_btb_id;
} BPC_IFC_ITBF_Pack_t
deriving (FShow, Bits);

typedef struct {
  IFetch_HAddr_t        pc_h;
  Vector#(NOVA_CFG_BPC_FETCH_W, Br_Class_t) 
                        br_class;
} BPC_BTB_INFO_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  Vector#(NOVA_CFG_BPC_FETCH_W, Maybe#(BPQ_PRED_POS_t))
                        target_pos;
} BPC_BTB_MAP_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  Vector#(NOVA_CFG_BPC_PRED_W, Maybe#(PC_t))
                        target_pc;
} BPC_BTB_ADDR_ENTRY_t
deriving (FShow, Bits);

typedef struct {
  IFetch_HAddr_t pc_h;
  IFetch_LAddr_t pc_l;
} BPC_BTB_REQ_t
deriving (FShow, Bits);

typedef struct {
  Maybe#(btb_id_t)      btb_id;
  BPC_BTB_INFO_ENTRY_t  btb_info;
  BPC_BTB_MAP_ENTRY_t   btb_map;
  BPC_BTB_ADDR_ENTRY_t  btb_addr;
} BPC_BTB_RSP_t#(type btb_id_t) 
deriving (FShow, Bits);
typedef BPC_BTB_RSP_t#(L0_BTB_ID_t) BPC_L0_BTB_RSP_t;
typedef BPC_BTB_RSP_t#(L1_BTB_ID_t) BPC_L1_BTB_RSP_t;
typedef BPC_BTB_RSP_t#(L2_BTB_ID_t) BPC_L2_BTB_RSP_t;

typedef struct {
  BPC_BHT_t                 ght;
  BPC_BTB_REQ_t             btb_req;
} BPC_BPP_LKUP_REQ_t
deriving (FShow, Bits);

typedef struct {
  BPC_BTB_RSP_t#(btb_id_t)  btb_rsp;
} BPC_BPP_REQ_t#(type btb_id_t)
deriving (FShow, Bits);
typedef BPC_BPP_REQ_t#(L0_BTB_ID_t) BPC_L0_BPP_REQ_t;
typedef BPC_BPP_REQ_t#(L1_BTB_ID_t) BPC_L1_BPP_REQ_t;
typedef BPC_BPP_REQ_t#(L2_BTB_ID_t) BPC_L2_BPP_REQ_t;

typedef struct {
  Bool                  taken;        // has taken jump or brcc
  Bool                  brcc_taken;   // last one is taken brcc
  sig_t                 bp_sig;
  IFetch_LAddr_t        pc_os_end;
  IFetch_LAddr_t        brcc_cnt;     // total brcc in this fetch
  Br_Class_t            br_class;
  Maybe#(PC_t)          target_pc;
} BPC_BPP_RSP_t#(type sig_t) 
deriving (FShow, Bits);
typedef BPC_BPP_RSP_t#(L0_BPP_SIG_t) BPC_L0_BPP_RSP_t;
typedef BPC_BPP_RSP_t#(L1_BPP_SIG_t) BPC_L1_BPP_RSP_t;
typedef BPC_BPP_RSP_t#(L2_BPP_SIG_t) BPC_L2_BPP_RSP_t;

typedef struct {
  Maybe#(btb_id_t)  btb_id;
  BPC_BTB_INFO_ENTRY_t info;
} BPC_BTB_UPDT_INFO_REQ_ENTRY_t#(type btb_id_t)
deriving (FShow, Bits);

typedef struct {
  Maybe#(btb_id_t)        btb_id;
  IFetch_HAddr_t          pc_h;
  BPC_BTB_ADDR_ENTRY_t    e;
  Vector#(NOVA_CFG_BPC_PRED_W, IFetch_LAddr_t)
                          target_pos;
} BPC_BTB_UPDT_ADDR_REQ_ENTRY_t#(type btb_id_t)
deriving (FShow, Bits);

typedef struct {
  Maybe#(BPC_BTB_UPDT_INFO_REQ_ENTRY_t#(btb_id_t)) d; // update info part
  Maybe#(BPC_BTB_UPDT_ADDR_REQ_ENTRY_t#(btb_id_t)) a; // update target part
} BPC_BTB_UPDT_REQ_t#(type btb_id_t)
deriving (FShow, Bits);
typedef BPC_BTB_UPDT_REQ_t#(L0_BTB_ID_t) BPC_L0_BTB_UPDT_REQ_t;
typedef BPC_BTB_UPDT_REQ_t#(L1_BTB_ID_t) BPC_L1_BTB_UPDT_REQ_t;
typedef BPC_BTB_UPDT_REQ_t#(L2_BTB_ID_t) BPC_L2_BTB_UPDT_REQ_t;

typedef struct {
  Maybe#(btb_id_t)     rpl_btb_id; // replaced btb id
} BPC_BTB_UPDT_RSP_t#(type btb_id_t)
deriving (FShow, Bits);
typedef BPC_BTB_UPDT_RSP_t#(L0_BTB_ID_t) BPC_L0_BTB_UPDT_RSP_t;
typedef BPC_BTB_UPDT_RSP_t#(L1_BTB_ID_t) BPC_L1_BTB_UPDT_RSP_t;
typedef BPC_BTB_UPDT_RSP_t#(L2_BTB_ID_t) BPC_L2_BTB_UPDT_RSP_t;

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
  IFetch_HAddr_t        pc_h;
  IFetch_LAddr_t        pc_os;
  BPC_BHT_t             ght;
} BPC_SPLBP_REQ_t
deriving (FShow, Bits);

typedef struct {
  Bool                  taken;
  PC_t                  target_pc;
  id_t                  id;
} BPC_SPLBP_RSP_t#(type id_t)
deriving (FShow, Bits);
typedef BPC_SPLBP_RSP_t#(RAS_OSQ_ID_t)  BPC_RAS_RSP_t;
typedef BPC_SPLBP_RSP_t#(LOOP_ID_t)     BPC_LOOP_RSP_t;
typedef BPC_SPLBP_RSP_t#(ITA_OSQ_ID_t)  BPC_ITA_RSP_t;

typedef struct {
  IFetch_HAddr_t        pc_h;
  IFetch_LAddr_t        pc_os;
  BPC_BHT_t             ght;
  PC_t                  target_pc;
} BPC_SPLBP_ALLOC_t
deriving (FShow, Bits);

typedef struct {
  Bool                  excp;
  Bool                  commit;
  Bool                  flush_mispred; // flush is due to mispred of this unit or not
  Maybe#(id_t)          flush;
} BPC_SPLBP_CMT_t#(type id_t)
deriving (FShow, Bits);
typedef BPC_SPLBP_CMT_t#(RAS_OSQ_ID_t)  BPC_RAS_CMT_t;
typedef BPC_SPLBP_CMT_t#(LOOP_ID_t)     BPC_LOOP_CMT_t;
typedef BPC_SPLBP_CMT_t#(ITA_OSQ_ID_t)  BPC_ITA_CMT_t;

typedef struct {
  PC_t                  excp_base;
} BPC_CFG_t
deriving (FShow, Bits);

typedef  Bit #(2)             IFC_PHTE_t;  // prediction history table entry

// ================================================================
// Interfaces

interface NOVA_BPC_BPQ_IFC;
  // branch flush to IFC
  interface Get #(IFC_BPC_BRF_Pack_t)  brf_intf;
  // handle branch flush from ctrl
  interface Put #(IFC_BPC_BRF_Pack_t)  flush_intf;
  // handle enqueue from ctrl
  interface Put #(IFC_BPC_BPQ_Pack_t)  enq_intf;
  // handle dequeue from ifc
  interface Get #(IFC_BPC_BPQ_Pack_t)  deq_intf;

endinterface

interface NOVA_BPC_GNRL_BTB_IFC#(numeric type odly, 
                                 numeric type impl,
                                 numeric type btb_entries, 
                                 numeric type btb_asso, 
                                 type req_t, 
                                 type rsp_t, 
                                 type updt_req_t, 
                                 type updt_rsp_t
                                 );
  interface Server#(req_t, rsp_t) lkup_server;
  interface Server#(updt_req_t, updt_rsp_t) updt_server;
endinterface

interface NOVA_BPC_GNRL_BPP_IFC#(numeric type odly, 
                                 numeric type impl,
                                 numeric type btb_entries, 
                                 type req_t, 
                                 type rsp_t, 
                                 type updt_req_t, 
                                 type updt_rsp_t);
  interface Server#(req_t, rsp_t) lkup_server;
  interface Server#(updt_req_t, updt_rsp_t) updt_server;
  interface Put#(BPC_BPP_LKUP_REQ_t) pre_lkup_put;
endinterface

typedef NOVA_BPC_GNRL_BTB_IFC#(0, 0, NOVA_CFG_L0_BTB_ENTRIES, NOVA_CFG_L0_BTB_ENTRIES, BPC_BTB_REQ_t, BPC_L0_BTB_RSP_t, BPC_L0_BTB_UPDT_REQ_t, BPC_L0_BTB_UPDT_RSP_t) NOVA_BPC_L0_BTB_IFC;
typedef NOVA_BPC_GNRL_BTB_IFC#(1, 0, NOVA_CFG_L1_BTB_ENTRIES, 4,  BPC_BTB_REQ_t, BPC_L1_BTB_RSP_t, BPC_L1_BTB_UPDT_REQ_t, BPC_L1_BTB_UPDT_RSP_t) NOVA_BPC_L1_BTB_IFC;
typedef NOVA_BPC_GNRL_BTB_IFC#(2, 0, NOVA_CFG_L2_BTB_ENTRIES, 16, BPC_BTB_REQ_t, BPC_L2_BTB_RSP_t, BPC_L2_BTB_UPDT_REQ_t, BPC_L2_BTB_UPDT_RSP_t) NOVA_BPC_L2_BTB_IFC;

typedef NOVA_BPC_GNRL_BPP_IFC#(0, 0, NOVA_CFG_L0_BPP_ENTRIES, BPC_L0_BPP_REQ_t, BPC_L0_BPP_RSP_t, BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t) NOVA_BPC_L0_BPP_IFC;
typedef NOVA_BPC_GNRL_BPP_IFC#(1, 0, NOVA_CFG_L1_BPP_ENTRIES, BPC_L1_BPP_REQ_t, BPC_L1_BPP_RSP_t, BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t) NOVA_BPC_L1_BPP_IFC;
typedef NOVA_BPC_GNRL_BPP_IFC#(2, 0, NOVA_CFG_L2_BPP_ENTRIES, BPC_L2_BPP_REQ_t, BPC_L2_BPP_RSP_t, BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t) NOVA_BPC_L2_BPP_IFC;

interface NOVA_BPC_SPL_IFC#(type req_t, type rsp_t, type alloc_t, type cmt_t);
  interface Server#(req_t, rsp_t) lkup_server;
  interface Put#(alloc_t) alloc;
  interface Put#(cmt_t)   cmt;
endinterface

typedef NOVA_BPC_SPL_IFC#(BPC_SPLBP_REQ_t, BPC_RAS_RSP_t  , BPC_SPLBP_ALLOC_t, BPC_RAS_CMT_t  ) NOVA_BPC_RAS_IFC;
typedef NOVA_BPC_SPL_IFC#(BPC_SPLBP_REQ_t, BPC_ITA_RSP_t  , BPC_SPLBP_ALLOC_t, BPC_ITA_CMT_t  ) NOVA_BPC_ITA_IFC;
typedef NOVA_BPC_SPL_IFC#(BPC_SPLBP_REQ_t, BPC_LOOP_RSP_t , BPC_SPLBP_ALLOC_t, BPC_LOOP_CMT_t ) NOVA_BPC_LOOP_IFC;

interface NOVA_BPC_CTRL_IFC;
  interface Put #(BPC_IFC_FBU_Pack_t)  fbu_intf;
  interface Put #(ROB_BPC_CMT_Pack_t)  rob_cmt_intf;
  interface Put #(ROB_BPC_FLUSH_Pack_t) rob_flush_intf;
  interface Put #(BPC_IFC_ITBF_Pack_t) itb_flush_intf;
endinterface

interface NOVA_BPC_CTRL_Int_IFC;
  interface Put #(IFC_BPC_BRF_Pack_t)  bpq_flush_intf;
  interface Put #(IFC_BPC_BPQ_Pack_t)  bpq_enq_intf;

  interface Server#(BPC_BTB_REQ_t   , BPC_L0_BTB_RSP_t) l0_btb;
  interface Server#(BPC_BTB_REQ_t   , BPC_L1_BTB_RSP_t) l1_btb;
  interface Server#(BPC_BTB_REQ_t   , BPC_L2_BTB_RSP_t) l2_btb;

  interface Server#(BPC_L0_BPP_REQ_t, BPC_L0_BPP_RSP_t) l0_bpp;
  interface Server#(BPC_L1_BPP_REQ_t, BPC_L1_BPP_RSP_t) l1_bpp;
  interface Server#(BPC_L2_BPP_REQ_t, BPC_L2_BPP_RSP_t) l2_bpp;

  interface Server#(BPC_L0_BTB_UPDT_REQ_t, BPC_L0_BTB_UPDT_RSP_t) l0_btb_updt;
  interface Server#(BPC_L1_BTB_UPDT_REQ_t, BPC_L1_BTB_UPDT_RSP_t) l1_btb_updt;
  interface Server#(BPC_L2_BTB_UPDT_REQ_t, BPC_L2_BTB_UPDT_RSP_t) l2_btb_updt;

  interface Server#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t)    l0_bpp_updt;
  interface Server#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t)    l1_bpp_updt;
  interface Server#(BPC_BPP_UPDT_REQ_t, BPC_BPP_UPDT_RSP_t)    l2_bpp_updt;

  interface Put#(BPC_BPP_LKUP_REQ_t) l0_bpp_pre_lkup;
  interface Put#(BPC_BPP_LKUP_REQ_t) l1_bpp_pre_lkup;
  interface Put#(BPC_BPP_LKUP_REQ_t) l2_bpp_pre_lkup;

  interface Server#(BPC_SPLBP_REQ_t, BPC_RAS_RSP_t)      ras_lkup;
  interface Put#(BPC_SPLBP_ALLOC_t)                      ras_alloc;
  interface Put#(BPC_RAS_CMT_t)                          ras_cmt;

  interface Server#(BPC_SPLBP_REQ_t, BPC_ITA_RSP_t)      ita_lkup;
  interface Put#(BPC_SPLBP_ALLOC_t)                      ita_alloc;
  interface Put#(BPC_ITA_CMT_t)                          ita_cmt;

  interface Server#(BPC_SPLBP_REQ_t, BPC_LOOP_RSP_t)     loop_lkup;
  interface Put#(BPC_SPLBP_ALLOC_t)                      loop_alloc;
  interface Put#(BPC_LOOP_CMT_t)                         loop_cmt;
endinterface

interface NOVA_BrPredCplx_IFC;
  // branch prediction queue to IFC 
  interface Get #(IFC_BPC_BPQ_Pack_t)  bpq_intf;

  // branch flush to IFC
  interface Get #(IFC_BPC_BRF_Pack_t)  brf_intf;

  // IFC branch target or decode UPDT
  interface Put #(BPC_IFC_FBU_Pack_t)  fbu_intf;
  
  // ROB commit interface to free BPC resources
  interface Put #(ROB_BPC_CMT_Pack_t)  rob_cmt_intf;

  // ROB interface to redirect BPU (from exception, BP mispred)
  interface Put #(ROB_BPC_FLUSH_Pack_t) rob_flush_intf;

  // inform ITB a L0 BTB is flushed
  interface Put #(BPC_IFC_ITBF_Pack_t) itb_flush_intf;
  
  // external configuration
  interface Put #(BPC_CFG_t) bpc_cfg_intf;
endinterface

// ================================================================

endpackage
