
package NOVA_Decls;


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
typedef  Bit #(TSub#(PC_W, NOVA_CFG_BPC_FETCH_AW)) IFetch_HAddr_t;
typedef  Bit #(NOVA_CFG_BPC_FETCH_AW)              IFetch_LAddr_t;

`ifdef NOVA_CFG_BPC_PRED_W
typedef `NOVA_CFG_BPC_PRED_W  NOVA_CFG_BPC_PRED_W;
`else
typedef 4  NOVA_CFG_BPC_PRED_W;
`endif

`ifdef NOVA_CFG_BPC_BR_ID_NUM
typedef `NOVA_CFG_BPC_BR_ID_NUM  NOVA_CFG_BPC_BR_ID_NUM;
`else
typedef 64  NOVA_CFG_BPC_BR_ID_NUM;
`endif
typedef TLog#(NOVA_CFG_BPC_BR_ID_NUM)  NOVA_CFG_BPC_BR_ID_W;
typedef  Bit #(NOVA_CFG_BPC_BR_ID_W)   BR_ID_t;

endpackage
