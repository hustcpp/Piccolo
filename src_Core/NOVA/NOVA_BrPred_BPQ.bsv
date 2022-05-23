
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_BPQ;
// ================================================================
// Exports

export mkNOVA_BPC_BPQ;

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
module mkNOVA_BPC_BPQ (NOVA_BPC_BPQ_IFC);
  // ----------------
  // Instances
  FIFOF #(IFC_BPC_BRF_Pack_t)  ifc_brf_fifo <- mkFIFOF;
  RWire #(IFC_BPC_BRF_Pack_t)  flush        <- mkRWireSBR;
  GPCvt #(IFC_BPC_BPQ_Pack_t)  deq_val      <- mkGPCvt;
  GPCvt #(IFC_BPC_BPQ_Pack_t)  enq_val      <- mkGPCvt;
  PulseWire                    set_full     <- mkPulseWire;

  // ----------------
  // States
  Vector#(NOVA_CFG_BPC_BPQ_ENTRIES, Reg#(IFC_BPC_BPQ_Pack_t)) q <- replicateM(mkRegA(unpack(fromInteger(valueOf(0)))));
  Reg#(BPQ_PTR_t)   rd_ptr <- mkRegA(0);
  Reg#(BPQ_PTR_t)   wr_ptr <- mkRegA(0);
  Reg#(Bool)        full   <- mkRegA(False);
  
  Bool notEmpty = rd_ptr != wr_ptr || full;
  IFC_BPC_BPQ_Pack_t top = q[rd_ptr];

  // ----------------
  // Rules 
  rule rl_read_q if (notEmpty);
    if (flush.wget() matches tagged Valid .vl &&& vl.bp_id == top.bp_id)
      ifc_brf_fifo.enq(vl);
    else
      deq_val.enq(top);
  endrule

  rule rl_write_q;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) m;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) res = 0;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) rd_mask = (1<<(rd_ptr+1))-1;
    Bit#(NOVA_CFG_BPC_BPQ_ENTRIES) wr_mask = (1<<(wr_ptr+1))-1;
    let vl = fromMaybe(?, flush.wget());
    for (Integer i = 0; i < valueOf(NOVA_CFG_BPC_BPQ_ENTRIES); i=i+1)
      m[i] = pack(vl.bp_id == q[i].bp_id);

    if (wr_ptr <= rd_ptr)
    begin
      if ((~rd_mask & m) != 0)
          res = ~rd_mask & m;
      else
          res = wr_mask & m;
    end else begin
      res = ~rd_mask & wr_mask & m;
    end

    BPQ_PTR_t wr_ptr_w   = 0;
    for (Integer i = valueOf(NOVA_CFG_BPC_BPQ_ENTRIES)-1; i >= 0 ; i=i-1)
      if (res[i] == 1'b1)
        wr_ptr_w = fromInteger(i);

    if (isValid(flush.wget()))
    begin
        if (full)
            wr_ptr_w = rd_ptr;
    end else
        wr_ptr_w = wr_ptr;
    
    if (!full)
    begin
      if (enq_val.hsked())
        q[wr_ptr_w] <= enq_val.first();
    end

    if ((!full && enq_val.hsked()) || isValid(flush.wget()))
    begin
        wr_ptr <= wr_ptr_w+1;
        if (wr_ptr_w+1 == rd_ptr)
          set_full.send();
    end
  endrule

  rule rl_handle_wr;
    if (!full)
      enq_val.deq();
  endrule

  rule rl_set_full;
    if (deq_val.hsked)
    begin
      full <= False;
      rd_ptr <= rd_ptr+1;
    end else if (set_full)
      full <= True;
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface ifc_brf_intf = toGet(ifc_brf_fifo);
  interface flush_intf   = toPut(flush);
  interface enq_intf     = toPut(enq_val);
  interface ifc_deq_intf = toGet(deq_val);
endmodule: mkNOVA_BPC_BPQ

endpackage
