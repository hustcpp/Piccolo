
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

  Vector#(2, Reg#(IFetch_HAddr_t))
                               s0_pch_r <- replicateM(mkRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s0_pcl_r <- mkRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s0_ght_r <- mkRegA(fromInteger(valueOf(0)));

  Vector#(2, Wire#(IFetch_HAddr_t))
                               s0_pch <- replicateM(mkWire);
  Wire#(IFetch_LAddr_t)        s0_pcl <- mkWire;
  Wire#(BPC_BHT_t)             s0_ght <- mkWire;

  Vector#(2, Reg#(IFetch_HAddr_t))
                               s1_pch_r <- replicateM(mkRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s1_pcl_r <- mkRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s1_ght_r <- mkRegA(fromInteger(valueOf(0)));
  PulseWire                    s1_acpt  <- mkPulseWire;

  Vector#(2, Reg#(IFetch_HAddr_t))
                               s2_pch_r <- replicateM(mkRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCH))));
  Reg#(IFetch_LAddr_t)         s2_pcl_r <- mkRegA(fromInteger(valueOf(NOVA_CFG_RESET_PCL)));
  Reg#(BPC_BHT_t)              s2_ght_r <- mkRegA(fromInteger(valueOf(0)));
  PulseWire                    s2_acpt  <- mkPulseWire;

  Reg#(BPC_BHT_SV_t)           ght_sv_r <- mkRegA(0); // saved bht

  RWire#(BPC_L0_BTB_RSP_t)     l0_btb_rsp_w <- mkRWireSBR;
  RWire#(BPC_L0_BPP_RSP_t)     l0_bpp_rsp_w <- mkRWireSBR;
  Reg#(BPC_L0_BTB_RSP_t)       l0_btb_rsp_r <- mkRegA(unpack(fromInteger(0)));
  Reg#(BPC_L0_BPP_RSP_t)       l0_bpp_rsp_r <- mkRegA(unpack(fromInteger(0)));

  GPCvt #(BPC_BTB_REQ_t)       l0_btb_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L0_BTB_RSP_t)    l0_btb_rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_L0_BPP_REQ_t)    l0_bpp_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L0_BPP_RSP_t)    l0_bpp_rsp_agent   <- mkGPCvt;

  GPCvt #(BPC_BTB_REQ_t)       l1_btb_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L1_BTB_RSP_t)    l1_btb_rsp_agent   <- mkGPCvt;
  GPCvt #(BPC_L1_BPP_REQ_t)    l1_bpp_req_agent   <- mkGPCvt;
  GPCvt #(BPC_L1_BPP_RSP_t)    l1_bpp_rsp_agent   <- mkGPCvt;

  // ----------------
  // States

  // ----------------
  // Rules 
  rule rl_rd_regs;
    writeVWire(s0_pch, readVReg(s0_pch_r));
    s0_pcl <= s0_pcl_r;
    s0_ght <= s0_ght_r;
  endrule

  rule rl_req_lvl0_btb;
    let btb_req = BPC_BTB_REQ_t{pc_h: readVWire(s0_pch)};
    l0_btb_req_agent.enq(btb_req);
  endrule

  rule rl_rsp_lvl0_btb;
    let btb_rsp = l0_btb_rsp_agent.first();
    l0_btb_rsp_agent.deq();
    l0_btb_rsp_w.wset(btb_rsp);
  endrule

  rule rl_req_lvl0_bpp if (l0_btb_rsp_w.wget() matches tagged Valid .l0_btb_rsp);
    let btb_req = BPC_BTB_REQ_t{pc_h: readVWire(s0_pch)};
    let bpp_req = BPC_L0_BPP_REQ_t{
        ght     : s0_ght_r,
        pc_os   : s0_pcl_r,
        btb_req : btb_req,
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
    Vector#(2, IFetch_HAddr_t) s0_pch_nxt = readVWire(s0_pch);
    IFetch_LAddr_t             s0_pcl_nxt = s0_pcl;
    BPC_BHT_t                  s0_ght_nxt = s0_ght;
    
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
    if (isValid(l0_bpp_rsp_w.wget) && s1_acpt)
    begin
      s0_ght_r      <= s0_ght_nxt;
      s0_pcl_r      <= s0_pcl_nxt;
      writeVReg(s0_pch_r, s0_pch_nxt);
      l0_btb_rsp_r  <= l0_btb_rsp_w.wget.Valid;
      l0_bpp_rsp_r  <= l0_bpp_rsp_w.wget.Valid;
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
  interface l1_btb_client  = toGPClient(l1_btb_req_agent, l1_btb_rsp_agent);
  interface l1_bpp_client  = toGPClient(l1_bpp_req_agent, l1_bpp_rsp_agent);
endmodule: mkNOVA_BPC_CTRL

endpackage
