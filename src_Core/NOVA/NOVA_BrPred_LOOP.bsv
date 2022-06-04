
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_LOOP;
// ================================================================
// Exports

export mkNOVA_BPC_LOOP;

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

typedef  Bit #(14)      Loop_Cnt_t;
typedef struct {
  Loop_Cnt_t            max_cnt;
  Loop_Cnt_t            spec_cnt;
} BPC_LOOP_Pack_t
deriving (FShow, Bits);

typedef struct {
  LOOP_SET_ID_t         set;
  LOOP_ASSO_ID_t        asso;
  Bool                  flushed;
} BPC_LOOP_OSQ_Pack_t
deriving (FShow, Bits);

typedef TSub#(NOVA_CFG_LOOP_OSQ_ENTRIES, 1)  NOVA_CFG_LOOP_OSQ_ENTRIES_MAX;

(* synthesize *)
module mkNOVA_BPC_LOOP (NOVA_BPC_LOOP_IFC);
  // ----------------
  // Instances
  SpCache#(NOVA_CFG_LOOP_ENTRIES, NOVA_CFG_LOOP_ASSO, BPC_LOOP_Pack_t, PC_t, LOOP_SET_ID_t, LOOP_ASSO_ID_t) 
                            cnt_cache <- mkSpCache;
  RegFile#(LOOP_OSQ_ID_t, BPC_LOOP_OSQ_Pack_t) 
                            osq <- mkRegFile(0, fromInteger(valueOf(NOVA_CFG_LOOP_OSQ_ENTRIES_MAX)));


  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
endmodule: mkNOVA_BPC_LOOP

endpackage
