
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
  PulseWire                    set_full     <- mkPulseWire;
  PulseWire                    deq_evt      <- mkPulseWire;
  PulseWire                    enq_evt      <- mkPulseWire;
  PulseWire                    top_not_flush<- mkPulseWire;
  Wire#(BPQ_PTR_t)             wr_ptr_q     <- mkBypassWire;

  // ----------------
  // States
  Vector#(NOVA_CFG_BPC_BPQ_ENTRIES, Reg#(IFC_BPC_BPQ_Pack_t)) q <- replicateM(mkSRegA(unpack(fromInteger(valueOf(0)))));
  Reg#(BPQ_PTR_t)   rd_ptr <- mkSRegA(0);
  Reg#(BPQ_PTR_t)   wr_ptr <- mkSRegA(0);
  Reg#(Bool)        full   <- mkSRegA(False);
  
  Bool notEmpty = rd_ptr != wr_ptr || full;
  IFC_BPC_BPQ_Pack_t top = q[rd_ptr];

  // ----------------
  // Rules 
  rule rl_read_q if (notEmpty);
    if (flush.wget() matches tagged Valid .vl &&& vl.bp_id == top.bp_id)
      ifc_brf_fifo.enq(vl);
    else
      top_not_flush.send();
  endrule

  rule rl_write_ptr;
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

    BPQ_PTR_t wr_ptr_w   = wr_ptr;
    for (Integer i = valueOf(NOVA_CFG_BPC_BPQ_ENTRIES)-1; i >= 0 ; i=i-1)
      if (res[i] == 1'b1)
        wr_ptr_w = fromInteger(i);

    if (isValid(flush.wget()))
    begin
        if (full)
            wr_ptr_w = rd_ptr;
    end else
        wr_ptr_w = wr_ptr;
    wr_ptr_q <= wr_ptr_w;
  endrule
    
  rule rl_write_q;
    if ((!full && enq_evt) || isValid(flush.wget()))
    begin
        wr_ptr <= wr_ptr_q+1;
        if (wr_ptr_q+1 == rd_ptr)
          set_full.send();
    end
  endrule

  rule rl_set_full;
    if (deq_evt)
    begin
      full <= False;
      rd_ptr <= rd_ptr+1;
    end else if (set_full)
      full <= True;
  endrule

  // ----------------
  // method
  let deq_get = 
  (interface Get#(IFC_BPC_BPQ_Pack_t);
    method ActionValue#(IFC_BPC_BPQ_Pack_t) get() if (notEmpty && top_not_flush);
      deq_evt.send();
      return top;
    endmethod
  endinterface);

  let enq_put =
  (interface Put#(IFC_BPC_BPQ_Pack_t);
    method Action put(IFC_BPC_BPQ_Pack_t val) if (!full);
      enq_evt.send();
      q[wr_ptr_q] <= val;
    endmethod
  endinterface);


  // ----------------
  // Interfaces
  interface ifc_brf_intf = toGet(ifc_brf_fifo);
  interface flush_intf   = toPut(flush);
  interface enq_intf     = enq_put;
  interface ifc_deq_intf = deq_get;
endmodule: mkNOVA_BPC_BPQ

endpackage
