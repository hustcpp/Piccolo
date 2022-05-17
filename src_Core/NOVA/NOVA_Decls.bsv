
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
typedef  Bit #(TSub#(PC_W, NOVA_CFG_BPC_FETCH_AW)) IFetch_HAddr_t;
typedef  Bit #(NOVA_CFG_BPC_FETCH_AW)              IFetch_LAddr_t;

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
typedef 32  NOVA_CFG_BPC_BHT_W;
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
typedef TLog#(NOVA_CFG_L0_BTB_ENTRIES) NOVA_CFG_L0_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L0_BTB_ID_W)   L0_BTB_ID_t;

`ifdef NOVA_CFG_L1_BTB_ENTRIES
typedef `NOVA_CFG_L1_BTB_ENTRIES NOVA_CFG_L1_BTB_ENTRIES;
`else
typedef 1  NOVA_CFG_L1_BTB_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L1_BTB_ENTRIES) NOVA_CFG_L1_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L1_BTB_ID_W)   L1_BTB_ID_t;

`ifdef NOVA_CFG_L2_BTB_ENTRIES
typedef `NOVA_CFG_L2_BTB_ENTRIES NOVA_CFG_L2_BTB_ENTRIES;
`else
typedef 1  NOVA_CFG_L2_BTB_ENTRIES;
`endif
typedef TLog#(NOVA_CFG_L2_BTB_ENTRIES) NOVA_CFG_L2_BTB_ID_W;
typedef  Bit #(NOVA_CFG_L2_BTB_ID_W)   L2_BTB_ID_t;

`ifdef NOVA_CFG_L0_BPP_SIG_W
typedef `NOVA_CFG_L0_BPP_SIG_W NOVA_CFG_L0_BPP_SIG_W;
`else
typedef 8  NOVA_CFG_L0_BPP_SIG_W;
`endif
typedef  Bit #(NOVA_CFG_L0_BPP_SIG_W)     L0_BPP_SIG_t;

`ifdef NOVA_CFG_L1_BPP_SIG_W
typedef `NOVA_CFG_L1_BPP_SIG_W NOVA_CFG_L1_BPP_SIG_W;
`else
typedef 8  NOVA_CFG_L1_BPP_SIG_W;
`endif
typedef  Bit #(NOVA_CFG_L1_BPP_SIG_W)     L1_BPP_SIG_t;

`ifdef NOVA_CFG_L2_BPP_SIG_W
typedef `NOVA_CFG_L2_BPP_SIG_W NOVA_CFG_L2_BPP_SIG_W;
`else
typedef 8  NOVA_CFG_L2_BPP_SIG_W;
`endif
typedef  Bit #(NOVA_CFG_L2_BPP_SIG_W)     L2_BPP_SIG_t;
typedef  L0_BPP_SIG_t                     ITB_BP_SIG_t;

typedef enum { 
    BC_NO,      // not a branch or jump
    BC_BRCC,    // conditional branch
    BC_JMP,     // unconditional jump
    BC_CALL,    // func call
    BC_RET,     // func ret
    BC_CONT,    // countinue in a loop
    BC_IND,     // indirect target other than func ret
    BC_LOOP     // Small loop
   } Br_Class_t deriving (Bits, Eq, FShow);

// utility module to convert a get put interface to fifo like with no delay
interface GPCvt#(type a_type);
  method Action enq( a_type x);
  method Action deq();
  method a_type first();
  method Bool hsked();
endinterface

module mkGPCvt (GPCvt#(a_type))
  provisos( Bits#(a_type, sa));

  Wire#(a_type) data <- mkDWire(unpack(fromInteger(valueOf(0))));
  PulseWire     deq_evt <- mkPulseWire;
  PulseWire     hsk_evt <- mkPulseWire;

  method Action enq( a_type x) if (deq_evt);
    data <= x;
    hsk_evt.send();
  endmethod

  method Action deq();
    deq_evt.send();
  endmethod

  method a_type first();
    return data;
  endmethod

  method Bool hsked();
    return hsk_evt;
  endmethod
endmodule

instance ToPut #(GPCvt#(a), a);
  function Put#(a) toPut (GPCvt#(a) i);
    return (interface Put;
              method Action put(a x);
                i.enq(x);
              endmethod
            endinterface);
  endfunction
endinstance

instance ToGet #(GPCvt#(a), a);
  function Get#(a) toGet (GPCvt#(a) i);
    return (interface Get;
              method ActionValue#(a) get();
                let ret = i.first();
                i.deq;
                return ret;
              endmethod
            endinterface);
  endfunction
endinstance

endpackage
