
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

import NOVA_Core_IFC     :: *;

// ================================================================
// Major States of CPU

// ================================================================

(* synthesize *)
module mkNOVA_Core (NOVA_Core_IFC);

// ================================================================
  AXI4_Master_Xactor_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master_xactor <- mkAXI4_Master_Xactor;
  interface mem_master = master_xactor.axi_side;

endmodule: mkNOVA_Core
endpackage
