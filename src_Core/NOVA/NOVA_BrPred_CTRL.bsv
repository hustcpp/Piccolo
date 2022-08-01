
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
} BPC_CTRL0_Pack_t
deriving (FShow, Bits);

typedef struct {
  RAS_OSQ_ID_t          ras_id;
  LOOP_ID_t             loop_id;
} BPC_CTRL1_Pack_t
deriving (FShow, Bits);

typedef struct {
  BPC_BHT_SV_ID_t ght_ptr;
} BPC_CTRL2_Pack_t
deriving (FShow, Bits);

//(* synthesize *)
module mkNOVA_BPC_CTRL #(NOVA_BPC_CTRL_Int_IFC ifc) (NOVA_BPC_CTRL_IFC);
  // ----------------
  // Instances
  FIFOF #(BPC_IFC_FBU_Pack_t)   ifc_fbu_fifo   <- mkBypassFIFOF;
  FIFOF #(ROB_BPC_CMT_Pack_t)   rob_cmt_fifo   <- mkBypassFIFOF;
  FIFOF #(ROB_BPC_FLUSH_Pack_t) rob_flush_fifo <- mkBypassFIFOF;
  FIFOF #(BPC_IFC_ITBF_Pack_t)  itb_flush_fifo <- mkBypassFIFOF;
  RWire #(BPC_IFC_FBU_Pack_t)   fbu_flush      <- mkRWireSBR;
  RWire #(ROB_BPC_FLUSH_Pack_t) rob_flush      <- mkRWireSBR;
  RWire #(ROB_BPC_CMT_Pack_t)   rob_cmt        <- mkRWireSBR;

  FIFOMgr#(NOVA_CFG_BPC_BP_ID_NUM, BP_ID_t)
                               osq_mgr     <- mkFIFOMgr;
  BP_ID_t osq_wr_ptr =         osq_mgr.get_wr();
  BP_ID_t osq_rd_ptr =         osq_mgr.get_rd();
  Bool    osq_full   =         osq_mgr.is_full();
  RegFile#(BP_ID_t, BPC_CTRL0_Pack_t)
                               osq_p0_r <- mkRegFile(0, fromInteger(valueOf(NOVA_CFG_BPC_BP_ID_MAX)));
  RegFile#(BP_ID_t, BPC_CTRL1_Pack_t)
                               osq_p1_r <- mkRegFile(0, fromInteger(valueOf(NOVA_CFG_BPC_BP_ID_MAX)));
  RegFile#(BP_ID_t, BPC_CTRL2_Pack_t)
                               osq_p2_r <- mkRegFile(0, fromInteger(valueOf(NOVA_CFG_BPC_BP_ID_MAX)));
  Reg#(Bit#(NOVA_CFG_BPC_BP_ID_NUM))
                               osq_flushed_r <- mkSRegA(0);

  Reg#(IFetch_HAddr_t)
                               s0_pch_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCH)));
  Reg#(IFetch_LAddr_t)         s0_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(PC_t)                   s0_pc_r  <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PC)));
  Reg#(BPC_BHT_t)              s0_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(BP_ID_t)                s0_bid_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(Bool)        s0_wait_target_pc_r <- mkSRegA(False);
  Wire#(PC_t)           s0_ras_ita_pc_w <- mkWire;
  PulseWire              s0_ras_ita_rdy <- mkPulseWire;
  PulseWire                    s0_pass_w <- mkPulseWire;

  Reg#(IFetch_HAddr_t)
                               s1_pch_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCH)));
  Reg#(IFetch_LAddr_t)         s1_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s1_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(BP_ID_t)                s1_bid_r <- mkSRegA(fromInteger(valueOf(0)));
  FIFOF#(Bool)                 s1_vld   <- mkPipelineFIFOF;
  PulseWire                    s1_pass_w <- mkPulseWire;
  Wire #(BPC_RAS_RSP_t)        ras_rsp_w  <- mkDWire(unpack(fromInteger(valueOf(0))));
  Wire #(BPC_ITA_RSP_t)        ita_rsp_w  <- mkDWire(unpack(fromInteger(valueOf(0))));
  Wire #(BPC_LOOP_RSP_t)       loop_rsp_w <- mkDWire(unpack(fromInteger(valueOf(0))));

  Reg#(IFetch_HAddr_t)
                               s2_pch_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCH)));
  Reg#(IFetch_LAddr_t)         s2_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s2_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(BP_ID_t)                s2_bid_r <- mkSRegA(fromInteger(valueOf(0)));
  FIFOF#(Bool)                 s2_vld   <- mkPipelineFIFOF;
  PulseWire                    s2_pass_w <- mkPulseWire;
  Reg#(BPC_BHT_SV_ID_t)        s2_ght_wptr_r <- mkSRegA(fromInteger(valueOf(0)));
  Reg#(BPC_BHT_SV_ID_t)        s2_ght_rptr_r <- mkSRegA(fromInteger(valueOf(0)));
  Wire#(BPC_BHT_SV_ID_t)       s2_ght_wptr_nxt <- mkDWire(fromInteger(valueOf(0)));
  Reg#(BPC_BHT_SV_t)           ght_sv_r <- mkRegA(0); // saved bht

  PulseWire                    s0_bpp_mispred <- mkPulseWire;
  PulseWire                    s0_btb_mispred <- mkPulseWire;
  PulseWire                    s1_bpp_mispred <- mkPulseWire;
  PulseWire                    s1_btb_mispred <- mkPulseWire;

  Wire#(BPC_L0_BTB_RSP_t)      l0_btb_rsp_w <- mkWire;
  Wire#(BPC_L0_BPP_RSP_t)      l0_bpp_rsp_w <- mkDWire(unpack(fromInteger(valueOf(0))));
  Reg#(BPC_L0_BTB_RSP_t)       l0_btb_rsp_r <- mkSRegA(unpack(fromInteger(0)));
  Reg#(BPC_L0_BPP_RSP_t)       l0_bpp_rsp_r <- mkSRegA(unpack(fromInteger(0)));

  Wire#(BPC_L1_BTB_RSP_t)      l1_btb_rsp_w <- mkWire;
  Wire #(BPC_L1_BPP_RSP_t)     l1_bpp_rsp_w <- mkDWire(unpack(fromInteger(valueOf(0))));
  Reg#(BPC_L1_BTB_RSP_t)       l1_btb_rsp_r <- mkRegA(unpack(fromInteger(0)));
  Reg#(BPC_L1_BPP_RSP_t)       l1_bpp_rsp_r <- mkRegA(unpack(fromInteger(0)));

  Reg#(BPC_L2_BTB_RSP_t)       l2_btb_rsp_r <- mkRegA(unpack(fromInteger(0)));
  Reg#(BPC_L2_BPP_RSP_t)       l2_bpp_rsp_r <- mkRegA(unpack(fromInteger(0)));

  BPC_BHT_SV_ID_t s2_ght_wptr_p = s2_ght_wptr_r + zeroExtend(l1_bpp_rsp_r.brcc_cnt);
  Bool  ght_sv_full = (pack(s2_ght_wptr_p > s2_ght_wptr_r)  // wptr wrapped?
                     ^ pack(s2_ght_wptr_p > s2_ght_rptr_r)  // wptr change relative position to rdptr?
                     ^ pack(s2_ght_wptr_r > s2_ght_rptr_r)
                     ) == 1'b1;

  RWire #(BP_ID_t)             flush_id_w   <- mkRWireSBR;
  Wire #(BPC_CTRL0_Pack_t)     flush_p0_w   <- mkDWire(unpack(fromInteger(0)));
  Wire #(BPC_CTRL1_Pack_t)     flush_p1_w   <- mkDWire(unpack(fromInteger(0)));
  Wire #(BPC_CTRL2_Pack_t)     flush_p2_w   <- mkDWire(unpack(fromInteger(0)));
  BPC_BHT_t                    flush_ght = truncate(ght_sv_r >> flush_p2_w.ght_ptr);
  // ----------------
  // States

  // ----------------
  // Rules 
  rule rl_req_l0_btb;
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_BPP_LKUP_REQ_t{btb_req: btb_req, ght: s0_ght_r};
    ifc.l0_btb.request.put(btb_req);
    ifc.l0_bpp_pre_lkup.put(bpp_req);
  endrule

  rule rl_rsp_l0_btb;
    let btb_rsp <- ifc.l0_btb.response.get();
    l0_btb_rsp_w <= btb_rsp;
  endrule

  rule rl_req_l0_bpp;
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_L0_BPP_REQ_t{
        pc_os   : s0_pcl_r,
        btb_rsp : l0_btb_rsp_w
        };
    ifc.l0_bpp.request.put(bpp_req);  
  endrule
  
  rule rl_handle_stage0;
    let btb_rsp = l0_btb_rsp_w;
    let bpp_rsp <- ifc.l0_bpp.response.get();
    l0_bpp_rsp_w <= bpp_rsp;
    IFetch_HAddr_t             s0_pch_nxt = s0_pch_r;
    IFetch_LAddr_t             s0_pcl_nxt = s0_pcl_r;
    BPC_BHT_t                  s0_ght_nxt = s0_ght_r;
    PC_t                       s0_pc_nxt  = s0_pc_r;
    Bool s0_pass = !s0_wait_target_pc_r && !osq_full;

    // if path end is taken, check btb for target address
    // if btb does not have the target address, address is increase even the prediction is taken
    s0_pch_nxt[0] = s0_pch_nxt[0] + 1;
    s0_pch_nxt[1] = s0_pch_nxt[1] + 1;
    s0_pc_nxt     = s0_pc_r + 1;

    if (bpp_rsp.taken)
    begin
      if (bpp_rsp.target_pc matches tagged Valid .addr)
        s0_pc_nxt  = addr;

      // get next ght
      s0_ght_nxt = s0_ght_nxt << bpp_rsp.brcc_cnt;
      s0_ght_nxt[0] = pack(bpp_rsp.brcc_taken);
    end

    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_nxt};
    let bpp_req = BPC_BPP_LKUP_REQ_t{btb_req: btb_req, ght: s0_ght_nxt};
    ifc.l1_btb.request.put(btb_req);
    ifc.l2_btb.request.put(btb_req);
    ifc.l1_bpp_pre_lkup.put(bpp_req);
    ifc.l2_bpp_pre_lkup.put(bpp_req);

    // ras ita loop using l0 info
    IFetch_HAddr_t             pch_end = s0_pch_r;
    let rli_req = BPC_SPLBP_REQ_t{
        pc_h    : pch_end,
        pc_os   : bpp_rsp.pc_os_end,
        ght     : s0_ght_r
        };
    let rli_alloc_req = BPC_SPLBP_ALLOC_t{
        pc_h        : pch_end,
        pc_os       : bpp_rsp.pc_os_end,
        ght         : s0_ght_r,
        target_pc   : bpp_rsp.target_pc.Valid
        };
    if (bpp_rsp.br_class == BC_RET) 
      ifc.ras_lkup.request.put(rli_req);
    else if (bpp_rsp.br_class == BC_CALL) 
    begin
      // what if call target pc is not yet in BTB? replay later?
      if (isValid(bpp_rsp.target_pc))
        ifc.ras_alloc.put(rli_alloc_req);
    end
    else if (bpp_rsp.br_class == BC_IND) 
      ifc.ita_lkup.request.put(rli_req);
    else if (bpp_rsp.br_class == BC_LOOP) 
      ifc.loop_lkup.request.put(rli_req);

    if (  bpp_rsp.br_class != BC_RET
       || bpp_rsp.br_class != BC_IND
        )
      s0_wait_target_pc_r <= True;
    else if (s0_ras_ita_rdy || isValid(flush_id_w.wget))
    begin
      s0_wait_target_pc_r <= False;
      s0_pc_nxt = s0_ras_ita_pc_w;
    end

    if (isValid(flush_id_w.wget))
    begin
      s0_pc_nxt = flush_p0_w.pc;
    end

    if (s0_pass)
    begin
      s0_pass_w.send();
      s1_vld.enq(True);

      match {.pch, .pcl} = split_pc(s0_pc_nxt);
      s0_pch_nxt = pch;        
      s0_pcl_nxt = pcl;

      // update s0 regs
      s0_ght_r      <= s0_ght_nxt;
      s0_pcl_r      <= s0_pcl_nxt;
      s0_pch_r      <= s0_pch_nxt;
      s0_pc_r       <= s0_pc_nxt;
      s0_bid_r      <= osq_wr_ptr;
      l0_btb_rsp_r  <= l0_btb_rsp_w;
      l0_bpp_rsp_r  <= bpp_rsp;
    end
  endrule
  
  rule rl_l0_osq_upd if (s0_pass_w);
    match {.pch1, .pcl1} = split_pc(s0_pc_r);
    let bpq_req = IFC_BPC_BPQ_Pack_t {
      pc_h           : pch1, 
      pc_os_start    : s0_pcl_r,     
      pc_os_end      : l0_bpp_rsp_w.pc_os_end, 
      bp_id          : osq_wr_ptr,
      has_taken      : l0_bpp_rsp_w.taken,     
      has_taken_brcc : l0_bpp_rsp_w.brcc_taken,     
      loop_start     : l0_bpp_rsp_w.br_class == BC_LOOP,
      itb_l0_btb_id  : Invalid, // TBD   
      itb_l0_bp_sig  : l0_bpp_rsp_w.bp_sig     
      };
    ifc.bpq_enq_intf.put(bpq_req);

    let osq_new = BPC_CTRL0_Pack_t{
      pc        : s0_pc_r,
      br_class  : l0_bpp_rsp_w.br_class
      };
    osq_p0_r.upd(osq_wr_ptr, osq_new);
  endrule

  rule rl_l1_btb_rsp;
    let btb1_rsp <- ifc.l1_btb.response.get();
    l1_btb_rsp_r <= btb1_rsp;
    l1_btb_rsp_w <= btb1_rsp;

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

    loop_rsp_w <= loop_rsp;
    ras_rsp_w  <= ras_rsp;
    ita_rsp_w  <= ita_rsp;
    l1_bpp_rsp_r  <= bpp1_rsp;

    Bool s1_pass = True;

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
    begin
      s0_bpp_mispred.send();
      s1_pass = False;
    end

    //btb target is different
    if (   bpp1_rsp.br_class != BC_RET
        && bpp1_rsp.br_class != BC_IND
        && bpp1_rsp.target_pc != l0_bpp_rsp_r.target_pc
        && bpp1_rsp.taken
       )
    begin
      s0_btb_mispred.send();
      s1_pass = False;
    end

    s1_vld.deq();
    if (s1_pass) 
    begin
      s2_vld.enq(True);
      s1_pass_w.send();
    end
  endrule

  rule rl_l1_osq_upd if (s1_pass_w);
    let osq_new = BPC_CTRL1_Pack_t{
      ras_id    : ras_rsp_w.id,
      loop_id   : loop_rsp_w.id
      }; 
    osq_p1_r.upd(s1_bid_r, osq_new);
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

  rule rl_handle_stage2 if (!ght_sv_full);
    let bpp2_rsp <- ifc.l2_bpp.response.get();
    Bool s2_pass = True;

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
    begin
      s1_bpp_mispred.send();
      s2_pass = False;
    end

    //btb target is different
    if (   bpp2_rsp.br_class != BC_RET
        && bpp2_rsp.br_class != BC_IND
        && bpp2_rsp.target_pc != l1_bpp_rsp_r.target_pc
        && bpp2_rsp.taken
       )
    begin
      s1_btb_mispred.send();
      s2_pass = False;
    end
    s2_vld.deq();

    BPC_BHT_SV_ID_t    s2_ght_wptr_p = s2_ght_wptr_r;
    BPC_BHT_SV_t       ght_sv_nxt = ght_sv_r;

    if (l1_bpp_rsp_r.taken)
    begin
      // get next ght
      ght_sv_nxt = ght_sv_r << l1_bpp_rsp_r.brcc_cnt;
      ght_sv_nxt[0] = pack(l1_bpp_rsp_r.brcc_taken);
      s2_ght_wptr_p = s2_ght_wptr_p;
    end

    if (s2_pass)
    begin
      // push the ght to globle ght queue
      s2_pass_w.send();
      ght_sv_r <= ght_sv_nxt;
      s2_ght_wptr_r <= s2_ght_wptr_p;
      s2_ght_wptr_nxt <= s2_ght_wptr_p;
    end
  endrule
  
  rule rl_rd_fbu_flush_fifo;
    let flush = ifc_fbu_fifo.first();
    ifc_fbu_fifo.deq();
    fbu_flush.wset(flush);
    // 
  endrule

  rule r2_l1_osq_upd if (s2_pass_w);
    let osq_new = BPC_CTRL2_Pack_t{
      ght_ptr   : s2_ght_wptr_nxt
      }; 
    osq_p2_r.upd(s2_bid_r, osq_new);
  endrule

  rule rl_rd_rob_flush_fifo;
    let flush = rob_flush_fifo.first();
    rob_flush_fifo.deq();
    rob_flush.wset(flush);
  endrule

  rule rl_rd_rob_cmd_fifo;
    let cmt = rob_cmt_fifo.first();
    rob_cmt_fifo.deq();
    rob_cmt.wset(cmt);
  endrule

  //(* descending_urgency = "rl_handle_osq rl_handle_stage1" *)
  rule rl_handle_osq;
    Maybe#(BP_ID_t) flush_id = Invalid;
    if (rob_flush.wget matches tagged Valid .flush)
    begin
      flush_id = tagged Valid flush.bp_id;
    end
    else if (fbu_flush.wget matches tagged Valid .flush)
    begin
      flush_id = tagged Valid flush.bp_id;
    end
    else if (s1_btb_mispred || s1_bpp_mispred)
    begin
      // select s2 to reset osq  
      flush_id = tagged Valid s2_bid_r;
    end
    else if (s0_btb_mispred || s0_bpp_mispred)
    begin
      // select s1 to reset osq  
      flush_id = tagged Valid s1_bid_r;
    end
    if (flush_id matches tagged Valid .flush_id_v)
      flush_id_w.wset(flush_id_v);
  endrule

  rule rl_handle_osq_inc;
    if (flush_id_w.wget matches tagged Valid .flush_id_v)
      osq_mgr.set_wr(flush_id_v);
    // allocate a new osq
    else if (s0_pass_w && l0_bpp_rsp_w.taken)
      osq_mgr.inc_wr();
  endrule

  rule rl_rd_flush_osq;
    if (flush_id_w.wget matches tagged Valid .flush_id_v)
    begin
      flush_p0_w <= osq_p0_r.sub(flush_id_v);
      flush_p1_w <= osq_p1_r.sub(flush_id_v);
      flush_p2_w <= osq_p2_r.sub(flush_id_v);
    end
  endrule

  rule rl_ras_cmt;
    Bool make_req = False;
    let cmt_req = BPC_RAS_CMT_t {
      excp             :  False,
      commit           :  False,
      flush_mispred    :  False,
      flush            :  Invalid 
    };
    if (rob_flush.wget matches tagged Valid .flush)
      if (flush.misspred_pc)
      begin
        make_req = True;
        cmt_req.flush_mispred = True;
      end
    if (rob_cmt.wget matches tagged Valid .cmt)
    begin
      make_req = True;
      cmt_req.commit = !cmt.excp;
      cmt_req.excp   = cmt.excp;
    end
    if (flush_id_w.wget matches tagged Valid .flush_id_v)
    begin
      make_req = True;
      cmt_req.flush = tagged Valid flush_p1_w.ras_id;
    end
    if (make_req)
      ifc.ras_cmt.put(cmt_req);
  endrule

  rule rl_loop_cmt;
    Bool make_req = False;
    let cmt_req = BPC_LOOP_CMT_t {
      excp             :  False,
      commit           :  False,
      flush_mispred    :  False,
      flush            :  Invalid 
    };
    if (rob_flush.wget matches tagged Valid .flush)
      if (flush.misspred_dir)
      begin
        make_req = True;
        cmt_req.flush_mispred = True;
      end
    if (rob_cmt.wget matches tagged Valid .cmt)
    begin
      make_req = True;
      cmt_req.commit = !cmt.excp;
      cmt_req.excp   = cmt.excp;
    end
    if (flush_id_w.wget matches tagged Valid .flush_id_v)
    begin
      make_req = True;
      cmt_req.flush = tagged Valid flush_p1_w.loop_id;
    end
    if (make_req)
      ifc.loop_cmt.put(cmt_req);
  endrule

  rule rl_bpp_updt;
    Bool make_req = False;
    match {.pch, .pcl} = split_pc(flush_p0_w.pc);
    let req = BPC_BPP_UPDT_REQ_t{
            pc_h        : pch,
            pc_os       : pcl,
            ght         : flush_ght,
            taken       : False,
            br_class    : unpack(fromInteger(0))
        }; 
  endrule

  rule rl_btb_updt;
    Bool make_req = False;
    match {.pch, .pcl} = split_pc(flush_p0_w.pc);
    //let req = BPC_BTB_UPDT_REQ_t{
    //        pc_h        : pch,
    //        pc_os       : pcl,
    //        ght         : flush_ght,
    //        taken       : False,
    //        br_class    : unpack(fromInteger(0))
    //    }; 

    //if (rob_flush.wget matches tagged Valid .flush)
    //begin
    //  req.taken = flush.taken;
    //  if (flush.misspred_pc)
    //    make_req = True;
    //end
    //else if (fbu_flush.wget matches tagged Valid .flush)
    //begin
    //  flush_id = tagged Valid flush.bp_id;
    //end
    //else if (s1_btb_mispred || s1_bpp_mispred)
    //begin
    //  // select s2 to reset osq  
    //  flush_id = tagged Valid s2_bid_r;
    //end
    //else if (s0_btb_mispred || s0_bpp_mispred)
    //begin
    //  // select s1 to reset osq  
    //  flush_id = tagged Valid s1_bid_r;
    //end

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
