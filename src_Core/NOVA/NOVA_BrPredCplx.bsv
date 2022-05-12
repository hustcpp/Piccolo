
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

// ----------------
// BSV additional libs

import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import ISA_Decls :: *;
import CPU_Globals :: *;

import NOVA_Decls :: *;
import NOVA_BrPredCplx_IFC     :: *;

// ================================================================
// Major States of BPC

// ================================================================

(* synthesize *)
module mkNOVA_BrPredCplx (NOVA_BrPredCplx_IFC);

   // ----------------
   // FIFOs

   FIFOF #(BPC_BPQ_Data_t)  ifc_bpq_fifo <- mkFIFOF;
   interface bpq_intf = toGet(ifc_bpq_fifo);

endmodule: mkNOVA_BrPredCplx
endpackage
