
package NOVA_Core_IFC;

// ================================================================
// BSV library imports

import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls       :: *;

import AXI4_Types  :: *;
import Fabric_Defs :: *;

`ifdef INCLUDE_GDB_CONTROL
import DM_CPU_Req_Rsp :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info         :: *;
`endif

// ================================================================
// CPU interface

interface NOVA_Core_IFC;
   // ----------------
   // SoC fabric connections

   // Mem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User)  mem_master;

endinterface

// ================================================================

endpackage
