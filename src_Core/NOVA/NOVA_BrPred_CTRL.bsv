
// ================================================================
// Branch Prediction Complex

package NOVA_BrPred_CTRL;
// ================================================================
// Exports

export mkNOVA_BPC_CTRL;

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

typedef struct {
  PC_t                  pc;
  Br_Class_t            br_class;
  IFetch_LAddr_t        pc_os_end;
  Bit#(2)               bpp_lvl;
  RAS_OSQ_ID_t          ras_id;
  LOOP_ID_t             loop_id;
  BPC_BHT_SV_ID_t       bht_ptr;
} BPC_CTRL_Pack_t
deriving (FShow, Bits);

//(* synthesize *)
module mkNOVA_BPC_CTRL #(NOVA_BPC_CTRL_Int_IFC ifc) (NOVA_BPC_CTRL_IFC);
  // ----------------
  // Instances
  FIFOF #(BPC_IFC_FBU_Pack_t)   ifc_fbu_fifo   <- mkFIFOF;
  FIFOF #(ROB_BPC_CMT_Pack_t)   rob_cmt_fifo   <- mkFIFOF;
  FIFOF #(ROB_BPC_FLUSH_Pack_t) rob_flush_fifo <- mkFIFOF;
  FIFOF #(BPC_IFC_ITBF_Pack_t)  itb_flush_fifo <- mkFIFOF;

  FIFOMgr#(NOVA_CFG_BPC_BP_ID_NUM, BP_ID_t)
                               osq_mgr     <- mkFIFOMgr;
  BP_ID_t osq_wr_ptr =         osq_mgr.get_wr();
  BP_ID_t osq_rd_ptr =         osq_mgr.get_rd();
  RegFile#(BP_ID_t, BPC_CTRL_Pack_t)
                               osq_r <- mkRegFile(0, fromInteger(valueOf(NOVA_CFG_BPC_BP_ID_MAX)));
  Reg#(Bit#(NOVA_CFG_BPC_BP_ID_NUM))
                               osq_flushed_r <- mkSRegA(0);

  Reg#(Vector#(2, IFetch_HAddr_t))
                               s0_pch_r <- mkSRegA(replicate(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s0_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s0_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(BP_ID_t)                s0_bid_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(Bool)        s0_wait_target_pc_r <- mkSRegA(False);
  Wire#(PC_t)           s0_ras_ita_pc_w <- mkWire;
  PulseWire              s0_ras_ita_rdy <- mkPulseWire;
  PulseWire                s0_alloc_osq <- mkPulseWire;

  Reg#(Vector#(2, IFetch_HAddr_t))
                               s1_pch_r <- mkSRegA(replicate(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s1_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s1_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(BP_ID_t)                s1_bid_r <- mkSRegA(fromInteger(valueOf(0)));
  PulseWire                    s1_acpt  <- mkPulseWire;

  Reg#(Vector#(2, IFetch_HAddr_t))
                               s2_pch_r <- mkSRegA(replicate(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s2_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s2_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(BP_ID_t)                s2_bid_r <- mkSRegA(fromInteger(valueOf(0)));
  PulseWire                    s2_acpt  <- mkPulseWire;

  PulseWire                    s0_bpp_mispred <- mkPulseWire;
  PulseWire                    s0_btb_mispred <- mkPulseWire;
  PulseWire                    s1_bpp_mispred <- mkPulseWire;
  PulseWire                    s1_btb_mispred <- mkPulseWire;

  Reg#(BPC_BHT_SV_t)           ght_sv_r <- mkRegA(0); // saved bht

  RWire#(BPC_L0_BTB_RSP_t)     l0_btb_rsp_w <- mkRWireSBR;
  Reg#(BPC_L0_BTB_RSP_t)       l0_btb_rsp_r <- mkSRegA(unpack(fromInteger(0)));
  Reg#(BPC_L0_BPP_RSP_t)       l0_bpp_rsp_r <- mkSRegA(unpack(fromInteger(0)));

  Reg#(BPC_L1_BTB_RSP_t)       l1_btb_rsp_r <- mkRegA(unpack(fromInteger(0)));
  Reg#(BPC_L1_BPP_RSP_t)       l1_bpp_rsp_r <- mkRegA(unpack(fromInteger(0)));

  Reg#(BPC_L2_BTB_RSP_t)       l2_btb_rsp_r <- mkRegA(unpack(fromInteger(0)));
  Reg#(BPC_L2_BPP_RSP_t)       l2_bpp_rsp_r <- mkRegA(unpack(fromInteger(0)));

  // ----------------
  // States

  // ----------------
  // Rules 
  rule rl_req_lvl0_btb;
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_BPP_LKUP_REQ_t{btb_req: btb_req, ght: s0_ght_r};
    ifc.l0_btb.request.put(btb_req);
    ifc.l0_bpp_pre_lkup.put(bpp_req);
  endrule

  rule rl_rsp_lvl0_btb;
    let btb_rsp <- ifc.l0_btb.response.get();
    l0_btb_rsp_w.wset(btb_rsp);
  endrule

  rule rl_req_lvl0_bpp if (l0_btb_rsp_w.wget() matches tagged Valid .l0_btb_rsp);
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_L0_BPP_REQ_t{
        pc_os   : s0_pcl_r,
        btb_rsp : l0_btb_rsp
        };
    ifc.l0_bpp.request.put(bpp_req);  
  endrule

  rule rl_handle_stage0;
    let btb_rsp = l0_btb_rsp_w.wget().Valid;
    let bpp_rsp <- ifc.l0_bpp.response.get();
    Vector#(2, IFetch_HAddr_t) s0_pch_nxt = s0_pch_r;
    IFetch_LAddr_t             s0_pcl_nxt = s0_pcl_r;
    BPC_BHT_t                  s0_ght_nxt = s0_ght_r;
    
    // if path end is taken, check btb for target address
    // if btb does not have the target address, address is increase even the prediction is taken
    s0_pch_nxt[0] = s0_pch_nxt[0] + 1;
    s0_pch_nxt[1] = s0_pch_nxt[1] + 1;

    if (bpp_rsp.taken)
    begin
      if (bpp_rsp.target_pc matches tagged Valid .addr)
      begin
        match {.pch, .pcl} = split_banked_pc(addr);
        s0_pch_nxt = pch;        
        s0_pcl_nxt = pcl;
      end

      // get next ght
      s0_ght_nxt = s0_ght_nxt << bpp_rsp.brcc_cnt;
      s0_ght_nxt[0] = pack(bpp_rsp.brcc_taken);

    end

    // update s0 regs
    if (s1_acpt)
    begin
      s0_ght_r      <= s0_ght_nxt;
      s0_pcl_r      <= s0_pcl_nxt;
      s0_pch_r      <= s0_pch_nxt;
      s0_bid_r      <= osq_wr_ptr;
      l0_btb_rsp_r  <= l0_btb_rsp_w.wget.Valid;
      l0_bpp_rsp_r  <= bpp_rsp;

      let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_nxt};
      let bpp_req = BPC_BPP_LKUP_REQ_t{btb_req: btb_req, ght: s0_ght_nxt};
      ifc.l1_btb.request.put(btb_req);
      ifc.l2_btb.request.put(btb_req);
      ifc.l1_bpp_pre_lkup.put(bpp_req);
      ifc.l2_bpp_pre_lkup.put(bpp_req);

      // ras ita loop using l0 info
      IFetch_HAddr_t pch = s0_pch_r[msb(s0_pcl_r)];
      let rli_req = BPC_SPLBP_REQ_t{
          pc_h    : pch,
          pc_os   : s0_pcl_r,
          ght     : s0_ght_r
          };
      if (bpp_rsp.br_class == BC_RET) 
        ifc.ras_lkup.request.put(rli_req);
      else if (bpp_rsp.br_class == BC_IND) 
        ifc.ita_lkup.request.put(rli_req);
      else if (bpp_rsp.br_class == BC_LOOP) 
        ifc.loop_lkup.request.put(rli_req);

      // allocate a new osq
      if (bpp_rsp.taken)
        s0_alloc_osq.send();

      if (  bpp_rsp.br_class != BC_RET
         || bpp_rsp.br_class != BC_IND
          )
        s0_wait_target_pc_r <= True;
      else if (s0_ras_ita_rdy)
      begin
        s0_wait_target_pc_r <= False;
        match {.pch2, .pcl2} = split_banked_pc(s0_ras_ita_pc_w);
        s0_pch_nxt = pch2;        
        s0_pcl_nxt = pcl2;
      end

      // send to BPQ if not return or indirect
      if (!s0_wait_target_pc_r)
      begin
        let bpq_req = IFC_BPC_BPQ_Pack_t {
          pc_h           : pch, 
          pc_os_start    : s0_pcl_r,     
          pc_os_end      : bpp_rsp.pc_os_end, 
          bp_id          : s0_bid_r, 
          has_taken      : bpp_rsp.taken,     
          has_taken_brcc : bpp_rsp.brcc_taken,     
          loop_start     : bpp_rsp.br_class == BC_LOOP,
          cross_boundry  : False,     // TBD
          itb_l0_btb_id  : Invalid, // TBD   
          itb_l0_bp_sig  : bpp_rsp.bp_sig     
          };
      end
    end
  endrule

  rule rl_l1_btb_rsp;
    let btb1_rsp <- ifc.l1_btb.response.get();
    l1_btb_rsp_r <= btb1_rsp;

    // l1 bpp req is using l1 btb rsp, one cycle before bpp rsp
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_L1_BPP_REQ_t{
        pc_os   : s0_pcl_r,
        btb_rsp : btb1_rsp
        };
    ifc.l1_bpp.request.put(bpp_req);
  endrule

  rule rl_handle_stage1;
    let bpp1_rsp <- ifc.l1_bpp.response.get();
    let loop_rsp <- ifc.loop_lkup.response.get();
    let ras_rsp  <- ifc.ras_lkup.response.get();
    let ita_rsp  <- ifc.ita_lkup.response.get();

    Bool l0_bpp_mispred = bpp1_rsp.taken      != l0_bpp_rsp_r.taken      
                       || bpp1_rsp.brcc_taken != l0_bpp_rsp_r.brcc_taken 
                       || bpp1_rsp.pc_os_end  != l0_bpp_rsp_r.pc_os_end  
                       || bpp1_rsp.brcc_cnt   != l0_bpp_rsp_r.brcc_cnt   
                       || bpp1_rsp.br_class   != l0_bpp_rsp_r.br_class   
                       ;

    PC_t s0_ras_ita_pc_nxt = 0;
    if (bpp1_rsp.br_class == BC_RET && ras_rsp.taken)
      s0_ras_ita_pc_nxt = ras_rsp.target_pc;
    if (bpp1_rsp.br_class == BC_IND && ita_rsp.taken) 
      s0_ras_ita_pc_nxt = ita_rsp.target_pc;
    s0_ras_ita_pc_w <= s0_ras_ita_pc_nxt;

    if ((bpp1_rsp.br_class == BC_RET || bpp1_rsp.br_class == BC_IND) && s0_bid_r == s1_bid_r)
      s0_ras_ita_rdy.send();

    // loop prediction not aligned with l0 bpp
    if (bpp1_rsp.br_class == BC_LOOP) 
    begin
      if (loop_rsp.taken == l0_bpp_rsp_r.taken)
        l0_bpp_mispred = True;
    end
    if (l0_bpp_mispred)
      s0_bpp_mispred.send();

    //btb target is different
    if (   bpp1_rsp.br_class != BC_RET
        && bpp1_rsp.br_class != BC_IND
        && bpp1_rsp.target_pc != l0_bpp_rsp_r.target_pc
        && bpp1_rsp.taken
       )
    begin
      s0_btb_mispred.send();
    end
  endrule

  rule rl_l2_btb_rsp;
    let btb2_rsp <- ifc.l2_btb.response.get();
    l2_btb_rsp_r <= btb2_rsp;

    // l2 bpp req is using l2 btb rsp, one cycle before bpp rsp
    let btb_req = BPC_BTB_REQ_t{pc_h: s1_pch_r};
    let bpp_req = BPC_L2_BPP_REQ_t{
        pc_os   : s1_pcl_r,
        btb_rsp : btb2_rsp
        };
    ifc.l2_bpp.request.put(bpp_req);
  endrule

  rule rl_handle_stage2;
    let bpp2_rsp <- ifc.l2_bpp.response.get();

    Bool l1_bpp_mispred = bpp2_rsp.pc_os_end  != l1_bpp_rsp_r.pc_os_end  
                       || bpp2_rsp.brcc_cnt   != l1_bpp_rsp_r.brcc_cnt   
                       || bpp2_rsp.br_class   != l1_bpp_rsp_r.br_class   
                       ;

    // loop prediction can be different than bpp
    if (bpp2_rsp.br_class != BC_LOOP) 
      l1_bpp_mispred = l1_bpp_mispred
                     || bpp2_rsp.taken      != l1_bpp_rsp_r.taken      
                     || bpp2_rsp.brcc_taken != l1_bpp_rsp_r.brcc_taken 
                     ;
    if (l1_bpp_mispred)
      s1_bpp_mispred.send();

    //btb target is different
    if (   bpp2_rsp.br_class != BC_RET
        && bpp2_rsp.br_class != BC_IND
        && bpp2_rsp.target_pc != l1_bpp_rsp_r.target_pc
        && bpp2_rsp.taken
       )
    begin
      s1_btb_mispred.send();
    end
  endrule

  rule rl_handle_osq;
    
    if (s1_btb_mispred || s1_bpp_mispred)
    begin
      // select s2 to reset osq  
      osq_mgr.set_wr(s2_bid_r);
    end
    else if (s0_btb_mispred || s0_bpp_mispred)
    begin
      // select s1 to reset osq  
      osq_mgr.set_wr(s1_bid_r);
    end
    else if (s0_alloc_osq)
    begin
      // allocate new entry when  
      osq_mgr.inc_wr();
    end
  endrule

  // ----------------
  // method

  // ----------------
  // Interfaces
  interface ifc_fbu_intf   = toPut(ifc_fbu_fifo);
  interface rob_cmt_intf   = toPut(rob_cmt_fifo);
  interface rob_flush_intf = toPut(rob_flush_fifo);
  interface itb_flush_intf = toPut(itb_flush_fifo);

endmodule: mkNOVA_BPC_CTRL

endpackage
