
// ================================================================
// Branch Prediction Complex

package NOVA_BrPredCplx;
// ================================================================
// Exports

export mkNOVA_BrPredCplx;

// ================================================================
// BSV library imports

import FIFOF        :: *;
import SpecialFIFOs :: *;
import GetPut       :: *;
import ClientServer :: *;
import Connectable  :: *;
import ConfigReg    :: *;

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

(* synthesize *)
module mkNOVA_BPC_L0_BTB (NOVA_BPC_L0_BTB_IFC);
  // ----------------
  // Instances
  Vector#(2, Vector#(TDiv#(NOVA_CFG_L0_BTB_ENTRIES, 2), Reg#(Maybe#(BPC_L0_BTB_ENTRY_t)))) 
    btb <- replicateM(replicateM(mkRegA(Invalid)));
  GPCvt #(BPC_BTB_REQ_t)            req         <- mkGPCvt;
  GPCvt #(BPC_L0_BTB_RSP_t)         rsp         <- mkGPCvt;
  GPCvt #(BPC_BTB_UPDT_REQ_t)       updt_req    <- mkGPCvt;
  GPCvt #(BPC_L0_BTB_UPDT_RSP_t)    updt_rsp    <- mkGPCvt;

  // ----------------
  // States

  // ----------------
  // Rules 
  rule handle_lkup if (req.hsked());
    Vector#(2, IFetch_HAddr_t) ra;
    let val = req.first();
    ra[1] = val.pc_odd_h;
    ra[0] = val.pc_evn_h;

  endrule

  rule accept_lkup;
    req.deq();
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface lkup_server = toGPServer(req, rsp);
  interface updt_server = toGPServer(updt_req, updt_rsp);
endmodule: mkNOVA_BPC_L0_BTB

(* synthesize *)
module mkNOVA_BPC_L0_BPP (NOVA_BPC_L0_BPP_IFC);
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
endmodule: mkNOVA_BPC_L0_BPP

(* synthesize *)
module mkNOVA_BPC_L1_BTB (NOVA_BPC_L1_BTB_IFC);
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
endmodule: mkNOVA_BPC_L1_BTB

(* synthesize *)
module mkNOVA_BPC_L1_BPP (NOVA_BPC_L1_BPP_IFC);
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
endmodule: mkNOVA_BPC_L1_BPP

(* synthesize *)
module mkNOVA_BPC_L2_BTB (NOVA_BPC_L2_BTB_IFC);
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
endmodule: mkNOVA_BPC_L2_BTB

(* synthesize *)
module mkNOVA_BPC_L2_BPP (NOVA_BPC_L2_BPP_IFC);
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
endmodule: mkNOVA_BPC_L2_BPP

(* synthesize *)
module mkNOVA_BPC_RAS (NOVA_BPC_RAS_IFC);
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
endmodule: mkNOVA_BPC_RAS

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

(* synthesize *)
module mkNOVA_BPC_LOOP (NOVA_BPC_LOOP_IFC);
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
endmodule: mkNOVA_BPC_LOOP

(* synthesize *)
module mkNOVA_BPC_CTRL (NOVA_BPC_CTRL_IFC);
  // ----------------
  // Instances
  FIFOF #(BPC_IFC_FBU_Pack_t)  ifc_fbu_fifo <- mkFIFOF;
  Vector#(NOVA_CFG_BRU_N, FIFOF #(EXU_BPC_BCU_Pack_t))  exu_bcu_fifos <- replicateM(mkFIFOF);
  FIFOF #(ROB_BPC_CMT_Pack_t)  rob_cmt_fifo <- mkFIFOF;
  FIFOF #(ROB_BPC_EXCP_Pack_t) rob_excp_fifo <- mkFIFOF;
  FIFOF #(BPC_IFC_ITBF_Pack_t) itb_flush_fifo <- mkFIFOF;

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface ifc_fbu_intf = toPut(ifc_fbu_fifo);
  interface exu_bcu_intfs = map(toGet, exu_bcu_fifos);
  interface rob_cmt_intf  = toPut(rob_cmt_fifo);
  interface rob_excp_intf = toPut(rob_excp_fifo);
  interface itb_flush_intf = toPut(itb_flush_fifo);
endmodule: mkNOVA_BPC_CTRL

(* synthesize *)
module mkNOVA_BrPredCplx (NOVA_BrPredCplx_IFC);
  // ----------------
  // Instances
  NOVA_BPC_BPQ_IFC     bpq    <- mkNOVA_BPC_BPQ;
  NOVA_BPC_L0_BTB_IFC  l0_btb <- mkNOVA_BPC_L0_BTB;
  NOVA_BPC_L0_BPP_IFC  l0_bpp <- mkNOVA_BPC_L0_BPP;
  NOVA_BPC_L1_BTB_IFC  l1_btb <- mkNOVA_BPC_L1_BTB;
  NOVA_BPC_L1_BPP_IFC  l1_bpp <- mkNOVA_BPC_L1_BPP;
  NOVA_BPC_L2_BTB_IFC  l2_btb <- mkNOVA_BPC_L2_BTB;
  NOVA_BPC_L2_BPP_IFC  l2_bpp <- mkNOVA_BPC_L2_BPP;
  NOVA_BPC_RAS_IFC     ras    <- mkNOVA_BPC_RAS;
  NOVA_BPC_ITA_IFC     ita    <- mkNOVA_BPC_ITA;
  NOVA_BPC_LOOP_IFC    loop   <- mkNOVA_BPC_LOOP;
  NOVA_BPC_CTRL_IFC    ctrl   <- mkNOVA_BPC_CTRL;

  mkConnection(ctrl.bpq_flush_intf, bpq.flush_intf);
  mkConnection(ctrl.bpq_enq_intf,   bpq.enq_intf);

  mkConnection(ctrl.l0_btb_client,  l0_btb.lkup_server);
  mkConnection(ctrl.l1_btb_client,  l1_btb.lkup_server);
  mkConnection(ctrl.l2_btb_client,  l2_btb.lkup_server);

  mkConnection(ctrl.l0_bpp_client,  l0_bpp.lkup_server);
  mkConnection(ctrl.l1_bpp_client,  l1_bpp.lkup_server);
  mkConnection(ctrl.l2_bpp_client,  l2_bpp.lkup_server);

  mkConnection(ctrl.l0_btb_updt_client,  l0_btb.updt_server);
  mkConnection(ctrl.l1_btb_updt_client,  l1_btb.updt_server);
  mkConnection(ctrl.l2_btb_updt_client,  l2_btb.updt_server);

  mkConnection(ctrl.l0_bpp_updt_client,  l0_bpp.updt_server);
  mkConnection(ctrl.l1_bpp_updt_client,  l1_bpp.updt_server);
  mkConnection(ctrl.l2_bpp_updt_client,  l2_bpp.updt_server);

  mkConnection(ctrl.ita_lkup_client,  ita.lkup_server);
  mkConnection(ctrl.ita_alloc,  ita.alloc);
  mkConnection(ctrl.ita_cmt,  ita.cmt);

  mkConnection(ctrl.ita_lkup_client,  ita.lkup_server);
  mkConnection(ctrl.ita_alloc,  ita.alloc);
  mkConnection(ctrl.ita_cmt,  ita.cmt);

  mkConnection(ctrl.loop_lkup_client,  loop.lkup_server);
  mkConnection(ctrl.loop_alloc,  loop.alloc);
  mkConnection(ctrl.loop_cmt,  loop.cmt);

  // ----------------
  // States

  // ----------------
  // Rules 

  // ----------------
  // method

  // ----------------
  // Interfaces

  interface ifc_bpq_intf   = bpq.ifc_deq_intf;
  interface ifc_brf_intf   = bpq.ifc_brf_intf;
  interface itb_flush_intf = ctrl.itb_flush_intf;
  interface ifc_fbu_intf   = ctrl.ifc_fbu_intf;
  interface exu_bcu_intfs  = ctrl.exu_bcu_intfs;
  interface rob_cmt_intf   = ctrl.rob_cmt_intf;
  interface rob_excp_intf  = ctrl.rob_excp_intf;

endmodule: mkNOVA_BrPredCplx

endpackage
