
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
typedef TDiv#(NOVA_CFG_BPC_FETCH_W,2)              NOVA_CFG_BPC_FETCH_HW;
typedef TMul#(NOVA_CFG_BPC_FETCH_W,4)              NOVA_CFG_BPC_FETCH_BYTES;
typedef TSub#(TLog#(NOVA_CFG_BPC_FETCH_BYTES), 1)  NOVA_CFG_BPC_FETCH_AW;
typedef TSub#(NOVA_CFG_BPC_FETCH_AW,1)             NOVA_CFG_BPC_FETCH_HID;
typedef TSub#(PC_W, NOVA_CFG_BPC_FETCH_AW)         IFetch_HAddr_w;
typedef  Bit #(IFetch_HAddr_w)                     IFetch_HAddr_t;
typedef  Bit #(NOVA_CFG_BPC_FETCH_AW)              IFetch_LAddr_t;
typedef  Bit #(NOVA_CFG_BPC_FETCH_HID)             IFetch_HF_POS_t;

`ifdef NOVA_CFG_BPC_PRED_W
typedef `NOVA_CFG_BPC_PRED_W  NOVA_CFG_BPC_PRED_W;
`else
typedef 4  NOVA_CFG_BPC_PRED_W;
`endif
typedef TDiv#(NOVA_CFG_BPC_PRED_W,2)     NOVA_CFG_BPC_PRED_HW;
typedef TLog#(NOVA_CFG_BPC_PRED_W)       NOVA_CFG_BPC_PRED_ID_W;
typedef TDiv#(NOVA_CFG_BPC_PRED_W,2)     NOVA_CFG_BPC_PRED_ID_HW;
typedef  Bit #(NOVA_CFG_BPC_PRED_ID_W)   BPQ_PRED_POS_t;
typedef  Bit #(NOVA_CFG_BPC_PRED_ID_HW)  BPQ_PRED_HF_POS_t;

`ifdef NOVA_CFG_BPC_BHT_W
typedef `NOVA_CFG_BPC_BHT_W  NOVA_CFG_BPC_BHT_W;
`else
typedef 128  NOVA_CFG_BPC_BHT_W;
`endif
typedef  Bit #(NOVA_CFG_BPC_BHT_W)              BPC_BHT_t;

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
typedef TLog#(NOVA_CFG_BPC_BP_ID_NUM)  NOVA_CFG_BPC_BP_ID_W;
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
typedef TDiv#(NOVA_CFG_L0_BTB_ENTRIES,2)   NOVA_CFG_L0_BTB_HF_ENTRIES;
typedef TLog#(NOVA_CFG_L0_BTB_ENTRIES)     NOVA_CFG_L0_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L0_BTB_ID_W)       L0_BTB_ID_t;
typedef TLog#(NOVA_CFG_L0_BTB_HF_ENTRIES)  NOVA_CFG_L0_BTB_ID_HW;
typedef  Bit #(NOVA_CFG_L0_BTB_ID_HW)      L0_BTB_HF_ID_t;

`ifdef NOVA_CFG_L1_BTB_ENTRIES
typedef `NOVA_CFG_L1_BTB_ENTRIES NOVA_CFG_L1_BTB_ENTRIES;
`else
typedef 256  NOVA_CFG_L1_BTB_ENTRIES;
`endif
typedef TDiv#(NOVA_CFG_L1_BTB_ENTRIES,2)   NOVA_CFG_L1_BTB_HF_ENTRIES;
typedef TLog#(NOVA_CFG_L1_BTB_ENTRIES)     NOVA_CFG_L1_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L1_BTB_ID_W)       L1_BTB_ID_t;
typedef TLog#(NOVA_CFG_L1_BTB_HF_ENTRIES)  NOVA_CFG_L1_BTB_ID_HW;
typedef  Bit #(NOVA_CFG_L1_BTB_ID_HW)      L1_BTB_HF_ID_t;

`ifdef NOVA_CFG_L2_BTB_ENTRIES
typedef `NOVA_CFG_L2_BTB_ENTRIES NOVA_CFG_L2_BTB_ENTRIES;
`else
typedef 2048  NOVA_CFG_L2_BTB_ENTRIES;
`endif
typedef TDiv#(NOVA_CFG_L2_BTB_ENTRIES,2)   NOVA_CFG_L2_BTB_HF_ENTRIES;
typedef TLog#(NOVA_CFG_L2_BTB_ENTRIES)     NOVA_CFG_L2_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L2_BTB_ID_W)       L2_BTB_ID_t;
typedef TLog#(NOVA_CFG_L2_BTB_HF_ENTRIES)  NOVA_CFG_L2_BTB_ID_HW;
typedef  Bit #(NOVA_CFG_L2_BTB_ID_HW)      L2_BTB_HF_ID_t;

`ifdef NOVA_CFG_L0_BPP_ENTRIES
typedef `NOVA_CFG_L0_BPP_ENTRIES NOVA_CFG_L0_BPP_ENTRIES;
`else
typedef 64  NOVA_CFG_L0_BPP_ENTRIES;
`endif
typedef TDiv#(NOVA_CFG_L0_BPP_ENTRIES,2)  NOVA_CFG_L0_BPP_HF_ENTRIES;
typedef TLog#(NOVA_CFG_L0_BPP_ENTRIES) NOVA_CFG_L0_BPP_IDX;
typedef TSub#(NOVA_CFG_L0_BPP_IDX,1)   NOVA_CFG_L0_BPP_HF_IDX;
typedef  Bit #(NOVA_CFG_L0_BPP_HF_IDX) L0_BPP_HF_ID_t;
typedef  NOVA_CFG_L0_BPP_HF_IDX NOVA_CFG_L0_BPP_SIG_W;
typedef  Bit #(NOVA_CFG_L0_BPP_SIG_W)     L0_BPP_SIG_t;
typedef  L0_BPP_SIG_t  ITB_BP_SIG_t;


`ifdef NOVA_CFG_L1_BPP_ENTRIES
typedef `NOVA_CFG_L1_BPP_ENTRIES NOVA_CFG_L1_BPP_ENTRIES;
`else
typedef 2048  NOVA_CFG_L1_BPP_ENTRIES;
`endif
typedef TDiv#(NOVA_CFG_L1_BPP_ENTRIES,2)  NOVA_CFG_L1_BPP_HF_ENTRIES;
typedef TLog#(NOVA_CFG_L1_BPP_ENTRIES) NOVA_CFG_L1_BPP_IDX;
typedef TSub#(NOVA_CFG_L1_BPP_IDX,1)   NOVA_CFG_L1_BPP_HF_IDX;
typedef  Bit #(NOVA_CFG_L1_BPP_HF_IDX) L1_BPP_HF_ID_t;
typedef NOVA_CFG_L1_BPP_HF_IDX NOVA_CFG_L1_BPP_SIG_W;
typedef  Bit #(NOVA_CFG_L1_BPP_SIG_W)     L1_BPP_SIG_t;

`ifdef NOVA_CFG_L2_BPP_ENTRIES
typedef `NOVA_CFG_L2_BPP_ENTRIES NOVA_CFG_L2_BPP_ENTRIES;
`else
typedef 16384  NOVA_CFG_L2_BPP_ENTRIES;
`endif
typedef TDiv#(NOVA_CFG_L2_BPP_ENTRIES,2)  NOVA_CFG_L2_BPP_HF_ENTRIES;
typedef TLog#(NOVA_CFG_L2_BPP_ENTRIES) NOVA_CFG_L2_BPP_IDX;
typedef TSub#(NOVA_CFG_L2_BPP_IDX,1)   NOVA_CFG_L2_BPP_HF_IDX;
typedef  Bit #(NOVA_CFG_L2_BPP_HF_IDX) L2_BPP_HF_ID_t;
typedef NOVA_CFG_L2_BPP_HF_IDX NOVA_CFG_L2_BPP_SIG_W;
typedef  Bit #(NOVA_CFG_L2_BPP_SIG_W)     L2_BPP_SIG_t;


typedef enum { 
    BC_NO,      // not a branch or jump
    BC_BRCC,    // conditional branch, brcc can be currently not predicated if not mapped in BTB
    BC_BRUC,    // un-conditional branch
    //BC_BRNT,    // conditional branch mostly not taken, not currently predicted
    //BC_LOOP     // Special BRCC: Small loop
    BC_JMP,     // unconditional jump
    BC_CALL,    // Special JMP: func call
    BC_RET,     // Special JMP: func ret
    BC_CONT,    // Special BRCC: countinue in a loop
    BC_IND      // Special JMP: indirect target other than func ret
   } Br_Class_t deriving (Bits, Eq, FShow);


endpackage
