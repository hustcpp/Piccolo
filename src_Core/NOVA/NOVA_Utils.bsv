
package NOVA_Utils;

import GetPut       :: *;
import Vector       :: *;
import RegFile      :: *;
import LFSR         :: *;
import FIFOF        :: *;

import ISA_Decls       :: *;
import NOVA_Decls      :: *;

module mkSRegA#(parameter a_type resetval) (Reg#(a_type))
  provisos (Bits#(a_type, sizea));
  Reg#(a_type) i_reg <- mkRegA(resetval);
  Wire#(a_type) i_wire <- mkBypassWire;
  
  rule rl_rd_reg;
    i_wire <= i_reg;
  endrule

  method Action _write(a_type x1);
    i_reg <= x1;
  endmethod

  method a_type _read();
    return i_wire;
  endmethod
endmodule

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

interface SpCache#(numeric type entries, numeric type asso, type data_t, type addr_t, type idx_t, type asso_t);
  method Maybe#(data_t) rd_data(addr_t addr);
  method data_t rd_data_direct(idx_t idx, asso_t asso_v);
  method Action wr_data(addr_t addr, data_t data);
  method Maybe#(asso_t) find_hit(addr_t addr);
endinterface

typedef struct {
  tag_t                 tag;
  Bool                  vld;
} SpC_tag_pack_t#(type tag_t)
deriving (FShow, Bits);

module mkSpCache (SpCache#(entries, asso, data_t, addr_t, idx_t, asso_t))
  provisos (Bits#(data_t, data_width),
            Alias#(addr_t, Bit#(addr_width)),
            Alias#(asso_t, Bit#(asso_width)),
            Alias#(idx_t,  Bit#(idx_width)),
            Div#(entries, asso, sets),
            Log#(sets, idx_min_width),
            Log#(asso, asso_min_width),
            Add#(1, asso_max, asso),
            Add#(1, sets_max, sets),
            Add#(idx_width, tag_width, addr_width),
            Alias#(tag_t, Bit#(tag_width)),
            Add#(idx_diff_width, idx_min_width, idx_width),
            Add#(asso_diff_width, asso_min_width, asso_width),
            Add#(asso_lsb_diff, asso_width, 16)
            );


  RegFile#(idx_t, Vector#(asso, data_t))                 data_ram <- mkRegFile(0, fromInteger(valueOf(sets_max)));
  RegFile#(idx_t, Vector#(asso, SpC_tag_pack_t#(tag_t))) tag_ram  <- mkRegFile(0, fromInteger(valueOf(sets_max)));
  LFSR#(Bit#(16)) asso_sel <- mkLFSR_16;

  function Maybe#(asso_t) i_find_hit(idx_t idx, tag_t tag, Vector#(asso, SpC_tag_pack_t#(tag_t)) tag_rd);
    Maybe#(asso_t) asso_res = Invalid;
    for (Integer i = 0; i < valueOf(asso); i=i+1)
    begin
      if (tag_rd[i].vld && tag_rd[i].tag == tag)
      begin
        asso_t asso_i = fromInteger(i);
        asso_res = tagged Valid asso_i;
      end
    end
    return asso_res;
  endfunction

  function Maybe#(asso_t) i_find_idle(idx_t idx, tag_t tag, Vector#(asso, SpC_tag_pack_t#(tag_t)) tag_rd);
    Maybe#(asso_t) asso_res = Invalid;
    for (Integer i = 0; i < valueOf(asso); i=i+1)
    begin
      if (!tag_rd[i].vld)
      begin
        asso_t asso_i = fromInteger(i);
        asso_res = tagged Valid asso_i;
      end
    end
    return asso_res;
  endfunction

  function Action i_write_cache(idx_t idx, asso_t asso_in, data_t data, Vector#(asso, data_t) data_rd);
  action
    Vector#(asso, data_t) data_nxt = data_rd;
    asso_t asso_sel = {1'b0, asso_in} >= fromInteger(valueOf(asso)) ? 0 : asso_in;
    data_nxt[asso_sel] = data;
    data_ram.upd(idx, data_nxt);
  endaction
  endfunction

  method Maybe#(asso_t) find_hit(addr_t addr);
    idx_t idx = truncate(addr);
    tag_t tag = truncate((addr >> valueOf(idx_width)));
    Vector#(asso, SpC_tag_pack_t#(tag_t)) tag_rd = tag_ram.sub(idx);
    return i_find_hit(idx, tag, tag_rd); 
  endmethod

  method Action wr_data(addr_t addr, data_t data);
    idx_t idx = truncate(addr);
    tag_t tag = truncate((addr >> valueOf(idx_width)));
    Vector#(asso, SpC_tag_pack_t#(tag_t)) tag_rd = tag_ram.sub(idx);
    let asso_hit  = i_find_hit(idx, tag, tag_rd);
    let asso_idle = i_find_idle(idx, tag, tag_rd);

    Vector#(asso, data_t) data_rd = data_ram.sub(idx);
    if (asso_hit matches tagged Valid .asso_v)
      i_write_cache(idx, asso_v, data, data_rd);
    else if (asso_idle matches tagged Valid .asso_v)
      i_write_cache(idx, asso_v, data, data_rd);
    else
    begin
      Bit#(16) asso_rpl_t = asso_sel.value;
      asso_t asso_rpl = truncate(asso_rpl_t);
      asso_sel.next;
      i_write_cache(idx, asso_rpl, data, data_rd);
    end
  endmethod

  method Maybe#(data_t) rd_data(addr_t addr);
    idx_t idx = truncate(addr);
    tag_t tag = truncate((addr >> valueOf(idx_width)));
    Vector#(asso, SpC_tag_pack_t#(tag_t)) tag_rd = tag_ram.sub(idx);
    let asso_hit = i_find_hit(idx, tag, tag_rd); 
    Maybe#(data_t) res = Invalid;
    Vector#(asso, data_t) data_rd = data_ram.sub(idx);
    if (asso_hit matches tagged Valid .asso_v)
    begin
      data_t data_sel = data_rd[asso_v];
      res = tagged Valid data_sel;
    end
    return res;
  endmethod

  method data_t rd_data_direct(idx_t idx, asso_t asso_v);
    Vector#(asso, data_t) data_rd = data_ram.sub(idx);
    data_t res = data_rd[asso_v];
    return res;
  endmethod

endmodule

interface FIFOMgr#(numeric type entries, type addr_t);
  method Bool   is_valid(addr_t addr, addr_t limit);
  method Bool   is_full();
  method Bool   is_empty();
  method Bool   is_not_empty();
  method addr_t get_rd();
  method addr_t get_wr();
  method Action inc_rd();
  method Action inc_rd_nb();
  method Action set_wr(addr_t addr);
  method Action inc_wr();
  method Action inc_wr_nb();
endinterface

module mkFIFOMgr (FIFOMgr#(entries, addr_t))
  provisos (Alias#(addr_t, Bit#(addr_width)),
            Log#(entries, addr_min_width),
            Add#(addr_diff_width, addr_min_width, addr_width)
            );
  Reg#(addr_t)                   rd_r    <- mkSRegA(0);
  Reg#(addr_t)                   wr_r    <- mkSRegA(0);
  Reg#(Bool)                     full_r  <- mkSRegA(False);
  
  PulseWire                      alloc_evt  <- mkPulseWire;
  PulseWire                      free_evt   <- mkPulseWire;

  addr_t rd_p1  = rd_r + 1;
  addr_t wr_p1  = wr_r + 1;
  Bool valid = rd_r != wr_r || full_r;
  Bool not_valid = !valid;

  rule rl_handle_full;
    if (free_evt && !alloc_evt)
      full_r <= False;
    else if (!free_evt && alloc_evt && wr_p1 == rd_r)
      full_r <= True;
  endrule

  method Bool   is_valid(addr_t addr, addr_t limit);
    Bool wrapped = ((rd_r > wr_r) || full_r) && (limit >= wr_r);
    Bool res = ((addr >= limit) && (addr < wr_r))
            || ( wrapped && (addr >= limit));
    return res;
  endmethod

  method Bool   is_full();
    return full_r;
  endmethod

  method Bool   is_empty();
    return not_valid;
  endmethod

  method Bool   is_not_empty();
    return valid;
  endmethod

  method Action inc_rd() if (valid);
    rd_r <= rd_p1;
    free_evt.send();
  endmethod

  method Action inc_rd_nb();
    if (valid)
    begin
      rd_r <= rd_p1;
      free_evt.send();
    end
  endmethod

  method Action set_wr(addr_t addr);
    wr_r <= addr;
    alloc_evt.send();
  endmethod

  method Action inc_wr() if (!full_r);
    wr_r <= wr_p1;
    alloc_evt.send();
  endmethod

  method Action inc_wr_nb();
    if (!full_r)
    begin
      wr_r <= wr_p1;
      alloc_evt.send();
    end
  endmethod

  method addr_t get_rd();
    return rd_r;
  endmethod

  method addr_t get_wr();
    return wr_r;
  endmethod

endmodule

interface FreeQueMgr#(numeric type entries, type addr_t);
  method Bool   is_valid(addr_t addr);
  method Bool   is_full();
  method Bool   is_empty();
  method Bool   is_not_empty();
  method ActionValue#(addr_t) get_entry();
  method Action free_entry(addr_t addr);
  method Action free_multi_entry(Bit#(entries) free_entries);
endinterface

module mkFreeQueMgr (FreeQueMgr#(entries, addr_t))
  provisos (Alias#(addr_t, Bit#(addr_width)),
            Log#(entries, addr_min_width),
            Add#(addr_diff_width, addr_min_width, addr_width)
            );
  
  Reg#(Bit#(entries))    vld_r  <- mkRegA(0);
  Wire#(Bit#(entries))   vld    <- mkWire;

  Bool      full    = vld == fromInteger(-1);
  Bool      empty   = vld == fromInteger(0);

  rule rl_reg_read;
    vld   <= vld_r;
  endrule

  method Bool   is_full();
    return full;
  endmethod

  method Bool   is_empty();
    return empty;
  endmethod

  method Bool   is_not_empty();
    return !empty;
  endmethod


  method ActionValue#(addr_t) get_entry() if (!full);
    addr_t res = 'b0;
    for (Integer i = 1; i < valueOf(entries); i=i+1)
    begin
      if (vld[i] == 1'b0)
        res = fromInteger(i);
    end
    vld_r[res] <= 1'b1;
    return res;
  endmethod

  method Action free_entry(addr_t addr);
    Bit#(entries) vld_nxt = vld;
    vld_nxt[addr] = 1'b0;
    vld_r <= vld_nxt;
  endmethod

  method Action free_multi_entry(Bit#(entries) free_entries);
    Bit#(entries) vld_nxt = vld ^ free_entries;
    vld_r <= vld_nxt;
  endmethod

  method Bool   is_valid(addr_t addr);
    return vld[addr] == 1'b1;
  endmethod

endmodule

function data_t readWire(Wire#(data_t) val);
  return val;
endfunction

function Vector#(n, data_t) readVWire(Vector#(n, Wire#(data_t)) vval);
  return map(readWire, vval);
endfunction

function Action writeWire(Tuple2#(Wire#(data_t), data_t) val);
action
  match {.dst, .wval} = val;
  dst <= wval;
endaction
endfunction

function Action writeVWire(Vector#(n, Wire#(data_t)) vdst, Vector#(n, data_t) val);
action
  let a = map(writeWire, zip(vdst, val));
endaction
endfunction

function Tuple2#(IFetch_HAddr_t, IFetch_LAddr_t) split_pc(PC_t addr);
  IFetch_HAddr_t pch = truncate(addr >> valueOf(NOVA_CFG_BPC_FETCH_AW));
  IFetch_LAddr_t pcl = truncate(addr);
  return tuple2(pch, pcl);
endfunction

function Tuple2#(Vector#(2, IFetch_HAddr_t), IFetch_LAddr_t) split_banked_pc(PC_t addr);
  Vector#(2, IFetch_HAddr_t) pchs;
  match {.pch, .pcl} = split_pc(addr);
  pchs[1] = pch;        
  pchs[0] = pch + zeroExtend(msb(pcl));
  return tuple2(pchs, pcl);
endfunction

interface FIFOR#(type dt);
  method Action enable(Bool ena);
  method Action deq();
  method dt first();
endinterface

module mkFIFOR#(parameter FIFOF#(a_type) ff) (FIFOR#(a_type))
  provisos (Bits#(a_type, sizea));
  RWire#(a_type) i_wire <- mkRWire;
  Wire#(Bool)   i_enable <- mkWire;
  Wire#(a_type) i_res <- mkWire;

  rule rl_rd_fifo if (i_enable);
    let res = ff.first();
    i_wire.wset(res);
  endrule

  rule rl_get_res if (!i_enable || isValid(i_wire.wget));
    a_type res = unpack(fromInteger(0));
    if (i_enable)
      i_res <= i_wire.wget().Valid;
    else
      i_res <= res;
  endrule

  method a_type first();
    return i_res;
  endmethod

  method Action deq();
    if (i_enable)
      ff.deq();
  endmethod

  method Action enable(Bool ena);
    i_enable <= ena;
  endmethod
endmodule

endpackage
