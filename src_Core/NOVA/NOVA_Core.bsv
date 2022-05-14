
package NOVA_Core;
// ================================================================
// Exports

export mkNOVA_Core;

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

import AXI4_Types :: *;

import ISA_Decls :: *;

import TV_Info   :: *;

import CPU_Globals :: *;
import Fabric_Defs  :: *;

import NOVA_Decls              :: *;
import NOVA_Core_IFC           :: *;
import NOVA_BrPredCplx_IFC     :: *;
import NOVA_BrPredCplx         :: *;

// ================================================================
// Major States of CPU

// ================================================================

(* synthesize *)
module mkNOVA_Core (NOVA_Core_IFC);

// ================================================================
// mk Modules

  NOVA_BrPredCplx_IFC bp_cplx <- mkNOVA_BrPredCplx;

  AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;


// ================================================================
// Connect interfaces
  interface mem_master = master_xactor.axi_side;


endmodule: mkNOVA_Core
endpackage
