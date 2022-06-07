
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_ITA;
// ================================================================
// Exports

export mkNOVA_BPC_ITA;

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

(* synthesize *)
module mkNOVA_BPC_ITA (NOVA_BPC_ITA_IFC);
  // ----------------
  // Instances

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
endmodule: mkNOVA_BPC_ITA

endpackage
