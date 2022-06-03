
package NOVA_Utils;

import GetPut       :: *;
import Vector       :: *;

import ISA_Decls       :: *;
import NOVA_Decls      :: *;

// utility module to convert a get put interface to fifo like with no delay
interface GPCvt#(type a_type);
  method Action enq( a_type x);
  method Action deq();
  method a_type first();
  method Bool hsked();
  method Bool deq_ready();
  method Bool enq_valid();
endinterface

module mkGPCvt (GPCvt#(a_type))
  provisos( Bits#(a_type, sa));

  Wire#(a_type) data <- mkDWire(unpack(fromInteger(valueOf(0))));
  PulseWire     deq_evt <- mkPulseWire;
  PulseWire     enq_evt <- mkPulseWire;

  method Action enq( a_type x);
    data <= x;
    enq_evt.send();
  endmethod

  method Action deq();
    deq_evt.send();
  endmethod

  method a_type first();
    return data;
  endmethod

  method Bool hsked();
    return enq_evt && deq_evt;
  endmethod

  method Bool enq_valid();
    return enq_evt;
  endmethod

  method Bool deq_ready();
    return deq_evt;
  endmethod
endmodule

//module mkGPSizedCvt#(Integer x) (GPCvt#(a_type))
//  provisos( Bits#(a_type, sa));
//
//  FIFO#(a_type) fifo <- mkBypassSizedFIFO(x); 
//  PulseWire     deq_evt <- mkPulseWire;
//  PulseWire     enq_evt <- mkPulseWire;
//
//  method Action enq( a_type x);
//    fifo.enq(x);
//    enq_evt.send();
//  endmethod
//
//  method Action deq();
//    fifo.deq();
//    deq_evt.send();
//  endmethod
//
//  method a_type first();
//    return fifo.first();
//  endmethod
//
//  method Bool enq_hsked();
//    return enq_evt;
//  endmethod
//
//  method Bool deq_hsked();
//    return deq_evt;
//  endmethod
//endmodule

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

interface LRU#(numeric type len);
  method Action access( Bit#(len) val);
  method Bit#(TLog#(len)) lru(Bit#(len) entry_sel_valid);
endinterface

module mkLRU (LRU#(len));
  Reg#(Bit#(TMul#(len,len))) age_matrix <- mkRegA(0);

  method Action access(Bit#(len) val);
  endmethod

  method Bit#(TLog#(len)) lru(Bit#(len) entry_sel_valid);
    Bit#(TLog#(len)) entry_sel_out = 'b0;
    Bit#(TMul#(len,len)) m = age_matrix;
    Integer ilen = valueOf(len);
    for (Integer i=0;i<ilen;i=i+1) begin :gen_sel_out
      Bit#(len) entry_sel = m[ilen*i+ilen-1:ilen*i];
      if (((entry_sel & entry_sel_valid) == 'b0) && (entry_sel_valid[i] == 1'b1))
        entry_sel_out = fromInteger(i);
    end
    return entry_sel_out;
  endmethod
endmodule

interface Que#(numeric type entries, type data_t);
  method data_t front();
  method data_t last();
  method Bool empty();
  method Bool not_empty();
  method Bool full();
  method Action deq_front();
  method Action deq_last();
  method Action enq(data_t val);
endinterface

module mkSizedQue (Que#(entries, data_t))
  provisos (Bits#(data_t, data_width),
            Log#(entries, idw));
  Vector#(entries, Reg#(data_t)) q <- replicateM(mkRegA(unpack(0)));
  Reg#(Bit#(idw)) rd_ptr <- mkRegA(0);
  Reg#(Bit#(idw)) ft_ptr <- mkRegA(0);
  Reg#(Bit#(idw)) wr_ptr <- mkRegA(0);
  Reg#(Bool)      full_r <- mkRegA(False);
  PulseWire       deq_front_evt <- mkPulseWire();
  PulseWire       deq_last_evt  <- mkPulseWire();
  RWire#(data_t)  enq_evt       <- mkRWireSBR();
  
  Bool is_empty     = rd_ptr != wr_ptr || full_r;
  Bool is_not_empty = !is_empty;

  rule rl_handle_enq_deq;
    Bool has_deq = deq_front_evt || deq_last_evt;
    Bool has_enq = isValid(enq_evt.wget());
    Bit#(idw) ft_ptr_nxt = ft_ptr;
    Bit#(idw) rd_ptr_nxt = rd_ptr;
    Bit#(idw) wr_ptr_nxt = wr_ptr;
    Bool can_enq = is_not_empty || has_deq;
    
    if (deq_front_evt && can_enq)
    begin
      ft_ptr_nxt = ft_ptr - 1;
      wr_ptr_nxt = ft_ptr;
    end
    if (deq_last_evt && can_enq)
    begin
      rd_ptr_nxt = rd_ptr + 1;
    end
    if (enq_evt.wget() matches tagged Valid .enq_data &&& !full_r)
    begin
      q[wr_ptr] <= enq_data;
      wr_ptr_nxt = wr_ptr + 1;
      if (!has_deq && rd_ptr_nxt == wr_ptr_nxt)
        full_r <= True;
    end else if (has_deq && !has_enq) begin
      full_r <= False;
    end
  endrule

  method Action deq_front() if (is_not_empty);
    deq_front_evt.send();
  endmethod

  method Action deq_last() if (is_not_empty);
    deq_last_evt.send();
  endmethod
  
  method Action enq(data_t val) if (!full_r);
    enq_evt.wset(val);
  endmethod
  
  method front     = q[ft_ptr];
  method last      = q[rd_ptr];
  method full      = full_r;
  method empty     = is_empty;
  method not_empty = is_not_empty;
endmodule

endpackage
