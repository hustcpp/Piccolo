
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
endinterface

module mkGPCvt (GPCvt#(a_type))
  provisos( Bits#(a_type, sa));

  Wire#(a_type) data <- mkDWire(unpack(fromInteger(valueOf(0))));
  PulseWire     deq_evt <- mkPulseWire;
  PulseWire     hsk_evt <- mkPulseWire;

  method Action enq( a_type x) if (deq_evt);
    data <= x;
    hsk_evt.send();
  endmethod

  method Action deq();
    deq_evt.send();
  endmethod

  method a_type first();
    return data;
  endmethod

  method Bool hsked();
    return hsk_evt;
  endmethod
endmodule

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
  Vector#(len, Vector#(len, Reg#(Bit#(1)))) age_matrix <- replicateM(replicateM(mkRegA(0)));

  method Action access(Bit#(len) val);
    Vector#(len, Bool) x = unpack(val);
    for (Integer i=0;i<valueOf(len);i=i+1) begin :older_1d
      for (Integer j=0;j<valueOf(len);j=j+1) begin :older_2d
        if (i < j) begin:gen_i_lt_j
          // age_matrix[i][j]==1 means j is older than i
          if ( x[i] || x[j])
            age_matrix[i][j] <= x[i] ? 1'b1 :
                                  x[j] ? 1'b0 :
                                  1'b0 ;
        end 
        else begin:gen_i_nlt_j
            age_matrix[i][j] <= 1'b0;
        end
      end
    end
  endmethod

  method Bit#(TLog#(len)) lru(Bit#(len) entry_sel_valid);
    Bit#(TLog#(len)) entry_sel_out = 'b0;
    for (Integer i=0;i<valueOf(len);i=i+1) begin :gen_sel_out
      let age_matrix_rd = readVReg(age_matrix[i]);
      if (((pack(age_matrix_rd) & entry_sel_valid) == 'b0) && (entry_sel_valid[i] == 1'b1))
        entry_sel_out = fromInteger(i);
    end
    return entry_sel_out;
  endmethod
endmodule

endpackage
