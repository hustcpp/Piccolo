
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

(* synthesize *)
module mkNOVA_BPC_CTRL (NOVA_BPC_CTRL_IFC);
  // ----------------
  // Instances
  FIFOF #(BPC_IFC_FBU_Pack_t)   ifc_fbu_fifo   <- mkFIFOF;
  FIFOF #(ROB_BPC_CMT_Pack_t)   rob_cmt_fifo   <- mkFIFOF;
  FIFOF #(ROB_BPC_FLUSH_Pack_t) rob_flush_fifo  <- mkFIFOF;
  FIFOF #(BPC_IFC_ITBF_Pack_t)  itb_flush_fifo <- mkFIFOF;

  FIFOMgr#(NOVA_CFG_BPC_BP_ID_NUM, BP_ID_t)
                               osq_mgr     <- mkFIFOMgr;
  RegFile#(BP_ID_t, BPC_CTRL_Pack_t)
                               osq_r <- mkRegFile(0, fromInteger(valueOf(NOVA_CFG_BPC_BP_ID_MAX)));
  Reg#(Bit#(NOVA_CFG_BPC_BP_ID_NUM))
                               osq_flushed_r <- mkRegA(0);

  Reg#(Vector#(2, IFetch_HAddr_t))
                               s0_pch_r <- mkSRegA(replicate(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s0_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s0_ght_r <- mkSRegA(fromInteger(valueOf(0)));

  Reg#(Vector#(2, IFetch_HAddr_t))
                               s1_pch_r <- mkSRegA(replicate(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s1_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s1_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  PulseWire                    s1_acpt  <- mkPulseWire;

  Reg#(Vector#(2, IFetch_HAddr_t))
                               s2_pch_r <- mkSRegA(replicate(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s2_pcl_r <- mkSRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s2_ght_r <- mkSRegA(fromInteger(valueOf(0)));
  PulseWire                    s2_acpt  <- mkPulseWire;

  Reg#(BPC_BHT_SV_t)           ght_sv_r <- mkRegA(0); // saved bht

  RWire#(BPC_L0_BTB_RSP_t)     l0_btb_rsp_w <- mkRWireSBR;
  RWire#(BPC_L0_BPP_RSP_t)     l0_bpp_rsp_w <- mkRWireSBR;
  Reg#(BPC_L0_BTB_RSP_t)       l0_btb_rsp_r[2] <- mkCRegA(2, unpack(fromInteger(0)));
  Reg#(BPC_L0_BPP_RSP_t)       l0_bpp_rsp_r[2] <- mkCRegA(2, unpack(fromInteger(0)));

  RWire#(BPC_L1_BTB_RSP_t)     l1_btb_rsp_w <- mkRWireSBR;
  RWire#(BPC_L1_BPP_RSP_t)     l1_bpp_rsp_w <- mkRWireSBR;
  Reg#(BPC_L1_BTB_RSP_t)       l1_btb_rsp_r <- mkRegA(unpack(fromInteger(0)));
  Reg#(BPC_L1_BPP_RSP_t)       l1_bpp_rsp_r <- mkRegA(unpack(fromInteger(0)));

  GPCvt #(BPC_BTB_REQ_t)       l0_btb_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L0_BTB_RSP_t)    l0_btb_rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_L0_BPP_REQ_t)    l0_bpp_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L0_BPP_RSP_t)    l0_bpp_rsp_agent   <- mkGPCvt;
  GPCvt#(BPC_BPP_LKUP_REQ_t)   l0_bpp_pre_agent   <- mkGPCvt;

  GPCvt #(BPC_BTB_REQ_t)       l1_btb_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L1_BTB_RSP_t)    l1_btb_rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_L1_BPP_REQ_t)    l1_bpp_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L1_BPP_RSP_t)    l1_bpp_rsp_agent   <- mkGPCvt;
  GPCvt#(BPC_BPP_LKUP_REQ_t)   l1_bpp_pre_agent   <- mkGPCvt;

  GPCvt #(BPC_BTB_REQ_t)       l2_btb_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L2_BTB_RSP_t)    l2_btb_rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_L2_BPP_REQ_t)    l2_bpp_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L2_BPP_RSP_t)    l2_bpp_rsp_agent   <- mkGPCvt;
  GPCvt#(BPC_BPP_LKUP_REQ_t)   l2_bpp_pre_agent   <- mkGPCvt;

  GPCvt#(BPC_SPLBP_REQ_t)      ras_lkup_req_agent <- mkGPCvt;
  GPCvt#(BPC_RAS_RSP_t)        ras_lkup_rsp_agent <- mkGPCvt;
  GPCvt#(BPC_SPLBP_ALLOC_t)    ras_alloc_agent    <- mkGPCvt;
  GPCvt#(BPC_RAS_CMT_t)        ras_cmt_agent      <- mkGPCvt;

  GPCvt#(BPC_SPLBP_REQ_t)      ita_lkup_req_agent <- mkGPCvt;
  GPCvt#(BPC_ITA_RSP_t)        ita_lkup_rsp_agent <- mkGPCvt;
  GPCvt#(BPC_SPLBP_ALLOC_t)    ita_alloc_agent    <- mkGPCvt;
  GPCvt#(BPC_ITA_CMT_t)        ita_cmt_agent      <- mkGPCvt;

  GPCvt#(BPC_SPLBP_REQ_t)      loop_lkup_req_agent <- mkGPCvt;
  GPCvt#(BPC_LOOP_RSP_t)       loop_lkup_rsp_agent <- mkGPCvt;
  GPCvt#(BPC_SPLBP_ALLOC_t)    loop_alloc_agent    <- mkGPCvt;
  GPCvt#(BPC_LOOP_CMT_t)       loop_cmt_agent      <- mkGPCvt;

  // ----------------
  // States

  // ----------------
  // Rules 
  rule rl_req_lvl0_btb;
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_BPP_LKUP_REQ_t{btb_req: btb_req, ght: s0_ght_r};
    l0_btb_req_agent.enq(btb_req);
    l0_bpp_pre_agent.enq(bpp_req);
  endrule

  rule rl_rsp_lvl0_btb;
    let btb_rsp = l0_btb_rsp_agent.first();
    l0_btb_rsp_agent.deq();
    l0_btb_rsp_w.wset(btb_rsp);
  endrule

  rule rl_req_lvl0_bpp if (l0_btb_rsp_w.wget() matches tagged Valid .l0_btb_rsp);
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_L0_BPP_REQ_t{
        pc_os   : s0_pcl_r,
        btb_rsp : l0_btb_rsp
        };
    l0_bpp_req_agent.enq(bpp_req);  
  endrule

  rule rl_rsp_lvl0_bpp;
    let bpp_rsp = l0_bpp_rsp_agent.first();
    l0_bpp_rsp_agent.deq();
    l0_bpp_rsp_w.wset(bpp_rsp);
  endrule

  rule rl_handle_stage0;
    let btb_rsp = l0_btb_rsp_w.wget().Valid;
    Vector#(2, IFetch_HAddr_t) s0_pch_nxt = s0_pch_r;
    IFetch_LAddr_t             s0_pcl_nxt = s0_pcl_r;
    BPC_BHT_t                  s0_ght_nxt = s0_ght_r;
    
    if (l0_bpp_rsp_w.wget matches tagged Valid .l0_bpp_rsp)
    begin
      // if path end is taken, check btb for target address
      // if btb does not have the target address, address is increase even the prediction is taken
      s0_pch_nxt[0] = s0_pch_nxt[0] + 1;
      s0_pch_nxt[1] = s0_pch_nxt[1] + 1;

      if (l0_bpp_rsp.taken)
      begin
        bit os_end_msb = msb(l0_bpp_rsp.pc_os_end);
        IFetch_HF_POS_t sel = truncate(l0_bpp_rsp.pc_os_end);
        let target_pos = btb_rsp.btb_map[os_end_msb].target_pos;
        let target_pcs = btb_rsp.btb_addr[os_end_msb].target_pc;
        Maybe#(PC_t) target_pc = Invalid;
        if (target_pos[sel] matches tagged Valid .pos)
          target_pc = target_pcs[pos];
        if (target_pc matches tagged Valid .addr)
        begin
          IFetch_HAddr_t pch = truncate(addr >> valueOf(NOVA_CFG_BPC_FETCH_AW));
          IFetch_LAddr_t pcl = truncate(addr);
          s0_pch_nxt[1] = pch;        
          s0_pch_nxt[0] = pch + zeroExtend(msb(pcl));
          s0_pcl_nxt    = pcl;
        end

        // get next ght
        s0_ght_nxt = s0_ght_nxt << l0_bpp_rsp.brcc_cnt;
        s0_ght_nxt[0] = pack(l0_bpp_rsp.brcc_taken);
      end
    end
    

    // update s0 regs
    if (l0_bpp_rsp_w.wget matches tagged Valid .l0_bpp_rsp &&& s1_acpt)
    begin
      s0_ght_r      <= s0_ght_nxt;
      s0_pcl_r      <= s0_pcl_nxt;
      s0_pch_r      <= s0_pch_nxt;
      l0_btb_rsp_r[0]  <= l0_btb_rsp_w.wget.Valid;
      l0_bpp_rsp_r[0]  <= l0_bpp_rsp_w.wget.Valid;

      let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_nxt};
      let bpp_req = BPC_BPP_LKUP_REQ_t{btb_req: btb_req, ght: s0_ght_nxt};
      l1_btb_req_agent.enq(btb_req);
      l1_bpp_pre_agent.enq(bpp_req);

      // ras ita loop using l0 info
      IFetch_HAddr_t pch = s0_pch_r[msb(s0_pcl_r)];
      let rli_req = BPC_SPLBP_REQ_t{
          pc_h    : pch,
          pc_os   : s0_pcl_r,
          ght     : s0_ght_r
          };
      if (l0_bpp_rsp.br_class == BC_RET) 
        ras_lkup_req_agent.enq(rli_req);
      if (l0_bpp_rsp.br_class == BC_IND) 
        ita_lkup_req_agent.enq(rli_req);
      if (l0_bpp_rsp.br_class == BC_LOOP) 
        loop_lkup_req_agent.enq(rli_req);
    end
  endrule

  rule rl_lvl1_btb_rsp;
    let btb1_rsp = l1_btb_rsp_agent.first();
    l1_btb_rsp_agent.deq();
    l1_btb_rsp_w.wset(btb1_rsp);
  endrule

  rule rl_lvl1_bpp_rsp;
    let bpp1_rsp = l1_bpp_rsp_agent.first();
    l1_bpp_rsp_agent.deq();
    l1_bpp_rsp_w.wset(bpp1_rsp);
  endrule

  rule rl_req_lvl1_bpp if (l1_btb_rsp_w.wget() matches tagged Valid .l1_btb_rsp);
    // l1 bpp req is using l1 btb rsp, one cycle before bpp rsp
    let btb_req = BPC_BTB_REQ_t{pc_h: s0_pch_r};
    let bpp_req = BPC_L1_BPP_REQ_t{
        pc_os   : s0_pcl_r,
        btb_rsp : l1_btb_rsp
        };
    l1_bpp_req_agent.enq(bpp_req);
  endrule

  rule rl_handle_stage1 if (l1_bpp_rsp_w.wget() matches tagged Valid .l1_bpp_rsp);
    let bpp1_rsp = l1_bpp_rsp_agent.first();
    let loop_rsp = loop_lkup_rsp_agent.first();
    let ras_rsp  = ras_lkup_rsp_agent.first();
    let ita_rsp  = ita_lkup_rsp_agent.first();

    if (l1_bpp_rsp.br_class == BC_RET) 
    begin
    end
    if (l1_bpp_rsp.br_class == BC_IND) 
    begin
    end
    if (l1_bpp_rsp.br_class == BC_LOOP) 
    begin
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

  interface l0_btb_client  = toGPClient(l0_btb_req_agent, l0_btb_rsp_agent);
  interface l0_bpp_client  = toGPClient(l0_bpp_req_agent, l0_bpp_rsp_agent);
  interface l0_bpp_pre_lkup = toGet(l0_bpp_pre_agent);

  interface l1_btb_client  = toGPClient(l1_btb_req_agent, l1_btb_rsp_agent);
  interface l1_bpp_client  = toGPClient(l1_bpp_req_agent, l1_bpp_rsp_agent);
  interface l1_bpp_pre_lkup = toGet(l1_bpp_pre_agent);

  interface l2_btb_client  = toGPClient(l2_btb_req_agent, l2_btb_rsp_agent);
  interface l2_bpp_client  = toGPClient(l2_bpp_req_agent, l2_bpp_rsp_agent);
  interface l2_bpp_pre_lkup = toGet(l2_bpp_pre_agent);

  interface ras_lkup_client  = toGPClient(ras_lkup_req_agent, ras_lkup_rsp_agent);
  interface ras_alloc        = toGet(ras_alloc_agent);
  interface ras_cmt          = toGet(ras_cmt_agent);

  interface loop_lkup_client = toGPClient(loop_lkup_req_agent, loop_lkup_rsp_agent);
  interface loop_alloc       = toGet(loop_alloc_agent);
  interface loop_cmt         = toGet(loop_cmt_agent);

  interface ita_lkup_client  = toGPClient(ita_lkup_req_agent, ita_lkup_rsp_agent);
  interface ita_alloc        = toGet(ita_alloc_agent);
  interface ita_cmt          = toGet(ita_cmt_agent);

endmodule: mkNOVA_BPC_CTRL

endpackage
