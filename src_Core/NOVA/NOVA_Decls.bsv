
package NOVA_Decls;

import GetPut       :: *;

import ISA_Decls       :: *;

typedef  TSub#(XLEN, 1) PC_W;    // Raw (unsigned) register data
typedef  Bit #(PC_W)    PC_t;    // Raw (unsigned) register data

// ================================================================
// Configuration for the module

`ifdef NOVA_CFG_BPC_FETCH_W
typedef `NOVA_CFG_BPC_FETCH_W  NOVA_CFG_BPC_FETCH_W;
`else
typedef 8  NOVA_CFG_BPC_FETCH_W;
`endif
typedef TMul#(NOVA_CFG_BPC_FETCH_W,4)              NOVA_CFG_BPC_FETCH_BYTES;
typedef TSub#(TLog#(NOVA_CFG_BPC_FETCH_BYTES), 1)  NOVA_CFG_BPC_FETCH_AW;
typedef TSub#(PC_W, NOVA_CFG_BPC_FETCH_AW)         IFetch_HAddr_w;
typedef  Bit #(IFetch_HAddr_w)                     IFetch_HAddr_t;
typedef  Bit #(NOVA_CFG_BPC_FETCH_AW)              IFetch_LAddr_t;

`ifdef NOVA_CFG_RESET_PC
typedef `NOVA_CFG_RESET_PC  NOVA_CFG_RESET_PC;
`else
typedef 0  NOVA_CFG_RESET_PC;
`endif
typedef TDiv#(NOVA_CFG_RESET_PC,NOVA_CFG_BPC_FETCH_W) NOVA_CFG_RESET_PCH;
typedef TMul#(NOVA_CFG_RESET_PCH, NOVA_CFG_BPC_FETCH_W) NOVA_CFG_RESET_PC_ALIGN;
typedef TSub#(NOVA_CFG_RESET_PC, NOVA_CFG_RESET_PC_ALIGN) NOVA_CFG_RESET_PCL;

`ifdef NOVA_CFG_BPC_PRED_W
typedef `NOVA_CFG_BPC_PRED_W  NOVA_CFG_BPC_PRED_W;
`else
typedef 4  NOVA_CFG_BPC_PRED_W;
`endif
typedef TLog#(NOVA_CFG_BPC_PRED_W)       NOVA_CFG_BPC_PRED_ID_W;
typedef  Bit #(NOVA_CFG_BPC_PRED_ID_W)   BPQ_PRED_POS_t;

`ifdef NOVA_CFG_BPC_BHT_W
typedef `NOVA_CFG_BPC_BHT_W  NOVA_CFG_BPC_BHT_W;
`else
typedef 128  NOVA_CFG_BPC_BHT_W;
`endif
typedef  Bit #(NOVA_CFG_BPC_BHT_W)              BPC_BHT_t;

`ifdef NOVA_CFG_BPC_BHT_SV_W
typedef `NOVA_CFG_BPC_BHT_SV_W  NOVA_CFG_BPC_BHT_SV_W;
`else
typedef 256  NOVA_CFG_BPC_BHT_SV_W;
`endif
typedef  Bit #(NOVA_CFG_BPC_BHT_SV_W)           BPC_BHT_SV_t;
typedef TLog#(NOVA_CFG_BPC_BHT_SV_W)            NOVA_CFG_BPC_BHT_SV_IDW;
typedef  Bit #(NOVA_CFG_BPC_BHT_SV_IDW)         BPC_BHT_SV_ID_t;

`ifdef NOVA_CFG_BPC_BPQ_ENTRIES
typedef `NOVA_CFG_BPC_BPQ_ENTRIES  NOVA_CFG_BPC_BPQ_ENTRIES;
`else
typedef 8  NOVA_CFG_BPC_BPQ_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_BPC_BPQ_ENTRIES)  NOVA_CFG_BPC_BPQ_ID_W;
typedef  Bit #(NOVA_CFG_BPC_BPQ_ID_W)    BPQ_PTR_t;

`ifdef NOVA_CFG_BPC_BP_ID_NUM
typedef `NOVA_CFG_BPC_BP_ID_NUM  NOVA_CFG_BPC_BP_ID_NUM;
`else
typedef 64  NOVA_CFG_BPC_BP_ID_NUM;
`endif
typedef TLog#(NOVA_CFG_BPC_BP_ID_NUM)     NOVA_CFG_BPC_BP_ID_W;
typedef TSub#(NOVA_CFG_BPC_BP_ID_NUM, 1)  NOVA_CFG_BPC_BP_ID_MAX;
typedef  Bit #(NOVA_CFG_BPC_BP_ID_W)   BP_ID_t;

`ifdef NOVA_CFG_BRU_N
typedef `NOVA_CFG_BRU_N       NOVA_CFG_BRU_N;
`else
typedef 1  NOVA_CFG_BRU_N;
`endif

`ifdef NOVA_CFG_INST_TRACE_BUF_ENTRIES
typedef `NOVA_CFG_INST_TRACE_BUF_ENTRIES NOVA_CFG_INST_TRACE_BUF_ENTRIES;
`else
typedef 64  NOVA_CFG_INST_TRACE_BUF_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_INST_TRACE_BUF_ENTRIES) NOVA_CFG_INST_TRACE_BUF_ID_W;
typedef  Bit #(NOVA_CFG_INST_TRACE_BUF_ID_W)   ITB_ID_t;

`ifdef NOVA_CFG_L0_BTB_ENTRIES
typedef `NOVA_CFG_L0_BTB_ENTRIES NOVA_CFG_L0_BTB_ENTRIES;
`else
typedef 16  NOVA_CFG_L0_BTB_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L0_BTB_ENTRIES)     NOVA_CFG_L0_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L0_BTB_ID_W)       L0_BTB_ID_t;

`ifdef NOVA_CFG_L1_BTB_ENTRIES
typedef `NOVA_CFG_L1_BTB_ENTRIES NOVA_CFG_L1_BTB_ENTRIES;
`else
typedef 256  NOVA_CFG_L1_BTB_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L1_BTB_ENTRIES)     NOVA_CFG_L1_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L1_BTB_ID_W)       L1_BTB_ID_t;

`ifdef NOVA_CFG_L2_BTB_ENTRIES
typedef `NOVA_CFG_L2_BTB_ENTRIES NOVA_CFG_L2_BTB_ENTRIES;
`else
typedef 2048  NOVA_CFG_L2_BTB_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L2_BTB_ENTRIES)     NOVA_CFG_L2_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L2_BTB_ID_W)       L2_BTB_ID_t;

`ifdef NOVA_CFG_L0_BPP_ENTRIES
typedef `NOVA_CFG_L0_BPP_ENTRIES NOVA_CFG_L0_BPP_ENTRIES;
`else
typedef 64  NOVA_CFG_L0_BPP_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L0_BPP_ENTRIES) NOVA_CFG_L0_BPP_IDX;
typedef  NOVA_CFG_L0_BPP_IDX NOVA_CFG_L0_BPP_SIG_W;
typedef  Bit #(NOVA_CFG_L0_BPP_SIG_W)     L0_BPP_SIG_t;
typedef  L0_BPP_SIG_t  ITB_BP_SIG_t;


`ifdef NOVA_CFG_L1_BPP_ENTRIES
typedef `NOVA_CFG_L1_BPP_ENTRIES NOVA_CFG_L1_BPP_ENTRIES;
`else
typedef 2048  NOVA_CFG_L1_BPP_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L1_BPP_ENTRIES) NOVA_CFG_L1_BPP_IDX;
typedef NOVA_CFG_L1_BPP_IDX NOVA_CFG_L1_BPP_SIG_W;
typedef  Bit #(NOVA_CFG_L1_BPP_SIG_W)     L1_BPP_SIG_t;

`ifdef NOVA_CFG_L2_BPP_ENTRIES
typedef `NOVA_CFG_L2_BPP_ENTRIES NOVA_CFG_L2_BPP_ENTRIES;
`else
typedef 16384  NOVA_CFG_L2_BPP_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L2_BPP_ENTRIES) NOVA_CFG_L2_BPP_IDX;
typedef NOVA_CFG_L2_BPP_IDX NOVA_CFG_L2_BPP_SIG_W;
typedef  Bit #(NOVA_CFG_L2_BPP_SIG_W)     L2_BPP_SIG_t;

`ifdef NOVA_CFG_RAS_CMT_ENTRIES
typedef `NOVA_CFG_RAS_CMT_ENTRIES NOVA_CFG_RAS_CMT_ENTRIES;
`else
typedef 16  NOVA_CFG_RAS_CMT_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_RAS_CMT_ENTRIES)  NOVA_CFG_RAS_CMT_IDW;


`ifdef NOVA_CFG_RAS_OSQ_ENTRIES
typedef `NOVA_CFG_RAS_OSQ_ENTRIES NOVA_CFG_RAS_OSQ_ENTRIES;
`else
typedef 64  NOVA_CFG_RAS_OSQ_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_RAS_OSQ_ENTRIES)  NOVA_CFG_RAS_OSQ_IDW;
typedef  Bit #(NOVA_CFG_RAS_OSQ_IDW)     RAS_OSQ_ID_t;

`ifdef NOVA_CFG_RAS_P_ENTRIES
typedef `NOVA_CFG_RAS_P_ENTRIES NOVA_CFG_RAS_P_ENTRIES;
`else
typedef 32  NOVA_CFG_RAS_P_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_RAS_P_ENTRIES)  NOVA_CFG_RAS_P_IDW;
typedef  Bit #(NOVA_CFG_RAS_P_IDW)     RAS_PID_t;


`ifdef NOVA_CFG_LOOP_OSQ_ENTRIES
typedef `NOVA_CFG_LOOP_OSQ_ENTRIES NOVA_CFG_LOOP_OSQ_ENTRIES;
`else
typedef 64  NOVA_CFG_LOOP_OSQ_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_LOOP_OSQ_ENTRIES)  NOVA_CFG_LOOP_OSQ_IDW;
typedef  Bit #(NOVA_CFG_LOOP_OSQ_IDW)     LOOP_OSQ_ID_t;

`ifdef NOVA_CFG_LOOP_MAP_ENTRIES
typedef `NOVA_CFG_LOOP_MAP_ENTRIES NOVA_CFG_LOOP_MAP_ENTRIES;
`else
typedef 2  NOVA_CFG_LOOP_MAP_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_LOOP_MAP_ENTRIES)  NOVA_CFG_LOOP_MAP_IDW;
typedef  Bit #(NOVA_CFG_LOOP_MAP_IDW)     LOOP_MAP_ID_t;

typedef TAdd#(NOVA_CFG_LOOP_MAP_IDW, NOVA_CFG_LOOP_OSQ_IDW)  NOVA_CFG_LOOP_IDW;
typedef  Tuple2#(LOOP_MAP_ID_t, LOOP_OSQ_ID_t)         LOOP_ID_t;

`ifdef NOVA_CFG_LOOP_ENTRIES
typedef `NOVA_CFG_LOOP_ENTRIES NOVA_CFG_LOOP_ENTRIES;
`else
typedef 128  NOVA_CFG_LOOP_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_LOOP_ENTRIES)  NOVA_CFG_LOOP_CACHE_IDW;
typedef  Bit #(NOVA_CFG_LOOP_CACHE_IDW)     LOOP_CACHE_ID_t;

`ifdef NOVA_CFG_LOOP_ASSO
typedef `NOVA_CFG_LOOP_ASSO NOVA_CFG_LOOP_ASSO;
`else
typedef 2  NOVA_CFG_LOOP_ASSO;
`endif
typedef TLog#(NOVA_CFG_LOOP_ASSO)  NOVA_CFG_LOOP_ASSO_IDW;
typedef  Bit #(NOVA_CFG_LOOP_ASSO_IDW)     LOOP_ASSO_ID_t;

typedef TSub#(NOVA_CFG_LOOP_CACHE_IDW, NOVA_CFG_LOOP_ASSO_IDW)  NOVA_CFG_LOOP_SET_IDW;
typedef  Bit #(NOVA_CFG_LOOP_SET_IDW)     LOOP_SET_ID_t;

typedef  1   NOVA_CFG_IAT_OSQ_IDW;
typedef  Bit #(NOVA_CFG_IAT_OSQ_IDW)     ITA_OSQ_ID_t;

`ifdef NOVA_CFG_ITA_ENTRIES
typedef `NOVA_CFG_ITA_ENTRIES NOVA_CFG_ITA_ENTRIES;
`else
typedef 128  NOVA_CFG_ITA_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_ITA_ENTRIES)  NOVA_CFG_ITA_CACHE_IDW;
typedef  Bit #(NOVA_CFG_ITA_CACHE_IDW)     ITA_CACHE_ID_t;

`ifdef NOVA_CFG_ITA_ASSO
typedef `NOVA_CFG_ITA_ASSO NOVA_CFG_ITA_ASSO;
`else
typedef 2  NOVA_CFG_ITA_ASSO;
`endif
typedef TLog#(NOVA_CFG_ITA_ASSO)  NOVA_CFG_ITA_ASSO_IDW;
typedef  Bit #(NOVA_CFG_ITA_ASSO_IDW)     ITA_ASSO_ID_t;

`ifdef NOVA_CFG_ITA_IDX
typedef `NOVA_CFG_ITA_IDX NOVA_CFG_ITA_IDX;
`else
typedef 12  NOVA_CFG_ITA_IDX;
`endif
typedef  Bit #(NOVA_CFG_ITA_IDX)     ITA_IDX_t;

typedef TSub#(NOVA_CFG_ITA_CACHE_IDW, NOVA_CFG_ITA_ASSO_IDW)  NOVA_CFG_ITA_SET_IDW;
typedef  Bit #(NOVA_CFG_ITA_SET_IDW)     ITA_SET_ID_t;

typedef enum { 
    BC_NO,      // not a branch or jump
    BC_BRCC,    // conditional branch, brcc can be currently not predicated if not mapped in BTB
    BC_BRUC,    // un-conditional branch
    //BC_BRNT,    // conditional branch mostly not taken, not currently predicted
    BC_LOOP,    // Special BRCC: loop
    BC_JMP,     // unconditional jump
    BC_CALL,    // Special JMP: func call
    BC_RET,     // Special JMP: func ret
    BC_CONT,    // Special BRCC: countinue in a loop
    BC_IND      // Special JMP: indirect target other than func ret
   } Br_Class_t deriving (Bits, Eq, FShow);


endpackage
