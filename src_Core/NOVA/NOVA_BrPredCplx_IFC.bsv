
package NOVA_BrPredCplx_IFC;

// ================================================================
// BSV library imports

import GetPut       :: *;
import ClientServer :: *;

// ================================================================
// Project imports

import ISA_Decls       :: *;
import NOVA_Decls      :: *;

typedef struct {
  IFetch_HAddr_t        pc_h;
  IFetch_LAddr_t        pc_os_start;
  IFetch_LAddr_t        pc_os_end;
  BR_ID_t               br_id;
} BPC_BPQ_Data_t
deriving (FShow, Bits);

// ================================================================
// Branch Prediction Complex interface

interface NOVA_BrPredCplx_IFC;
   // branch prediction queue to IFC 
   interface Get #(BPC_BPQ_Data_t)  bpq_intf;

endinterface

// ================================================================

endpackage
