// Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved.

package BSCore;

// ================================================================
// This package defines the interface and implementation of the 'P1 Core'
// for the DARPA SSITH project.
// This P1 core contains:
//    - Piccolo CPU, including
//        - Near_Mem (ICache and DCache)
//        - Near_Mem_IO (Timer, Software-interrupt, and other mem-mapped-locations)
//        - External interrupt request lines
//        - 2 x AXI4 Master interfaces (from DM and ICache, and from DCache)
//    - RISC-V Debug Module (DM)
//    - JTAG TAP interface for DM
//    - Optional Tandem Verification trace stream output interface

// ================================================================
// BSV library imports

import Vector        :: *;
import FIFO          :: *;
import GetPut        :: *;
import ClientServer  :: *;
import Connectable   :: *;
import Bus           :: *;
import Clocks        :: *;
import RegUInit      :: *;

// ----------------
// BSV additional libs

import GetPut_Aux :: *;
import Semi_FIFOF :: *;

// ================================================================
// Project imports

import SoC_Map  :: *;

// The basic core
import Core_IFC :: *;
import Core     :: *;

// External interrupt request interface
import PLIC :: *;    // for PLIC_Source_IFC type which is exposed at P2_Core interface

// Main Fabric
import AXI4_Types   :: *;
import AXI4_Fabric  :: *;
import Fabric_Defs  :: *;

`ifdef FABRIC_AHBL
import AHBL_Types   :: *;
import AHBL_Defs    :: *;
`endif

`ifdef INCLUDE_DMEM_SLAVE
import AXI4_Lite_Types :: *;
`endif

`ifdef INCLUDE_TANDEM_VERIF
import TV_Info :: *;
import AXI4_Stream ::*;
`endif

`ifdef INCLUDE_GDB_CONTROL
import Debug_Module :: *;
import Jtag         :: *;
import JtagTap      :: *;
import Giraffe_IFC  :: *;
`endif

// ================================================================
// Constant: cycles to hold SoC in reset for ndm reset:

UInt#(6) ndm_interval = 20;
UInt#(6) por_interval = 20;

// ================================================================
// The BSCore interface

interface BSCore_IFC;

   // ----------------------------------------------------------------
   // Core CPU interfaces
`ifndef Near_Mem_TCM
   // CPU IMem to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master0;
`endif

`ifdef FABRIC_AHBL
   // CPU DMem (incl. I/O) to Fabric master interface
   interface AHBL_Master_IFC#(AHBL_Defs::AHB_Wd_Data) master1;
`else
   // CPU DMem (incl. I/O) to Fabric master interface
   interface AXI4_Master_IFC #(Wd_Id, Wd_Addr, Wd_Data, Wd_User) master1;
`endif

   // External interrupt sources
   (* always_ready, always_enabled, prefix="" *)
   method  Action interrupt_reqs ((* port="cpu_external_interrupt_req" *) Bit #(N_External_Interrupt_Sources)  reqs);

`ifdef INCLUDE_DMEM_SLAVE
   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

   interface AXI4_Lite_Slave_IFC #(Wd_Addr, Wd_Data, Wd_User) slave0;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface.  The data signal is
   // packed output tuples (n,vb),/ where 'vb' is a vector of
   // bytes with relevant bytes in locations [0]..[n-1]

   interface AXI4_Stream_Master_IFC #(Wd_SId, Wd_SDest, Wd_SData, Wd_SUser)  tv_verifier_info_tx;
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------
   // JTAG interface

`ifdef JTAG_TAP
   interface JTAG_IFC jtag;
`endif
   // This reset output only if there's a debug module:
   interface Reset ndm_reset;
`endif

   // ----------------
   // For ISA tests: watch memory writes to <tohost> addr
`ifdef Near_Mem_TCM
`ifdef WATCH_TOHOST
   method Action set_watch_tohost (Bool  watch_tohost, Bit #(64)  tohost_addr);
   method Bit #(64) mv_tohost_value;
`endif
`endif

endinterface

// ================================================================

(* synthesize *)
module mkBSCore ((*reset="dmi_reset"*)Reset dmi_reset, BSCore_IFC _ifc);
   // A "power-on reset" generator:
   Reg #(UInt #(6)) initCnt <- mkRegUInit(por_interval);
   let clk <- exposeCurrentClock;
   let rstIfc <- mkReset(2, True, clk, reset_by noReset);
   rule rl_decInitCnt (initCnt != 0);
      initCnt <= initCnt - 1;
      rstIfc.assertReset();
   endrule
   let por_reset = rstIfc.new_rst;

   // -----------------
   let dmi_resetN <- mkResetInverter(dmi_reset); // dmi_reset is active-high

   // -----------------

   // Reset this by default reset, so core is reset by both default and ndm
   let ndmIfc <- mkReset(2, True, clk); //, reset_by por_reset); -- JES 12/9
   let coreRSTN = ndmIfc.new_rst;

   // Core: CPU + Near_Mem_IO (CLINT) + PLIC + Debug module (optional) + TV (optional)
   Core_IFC::Core_IFC #(N_External_Interrupt_Sources)  core <- mkCore(por_reset, reset_by coreRSTN);

   // ================================================================
   // Tie-offs (not used in SSITH GFE)

   // Set core's verbosity
   rule rl_never (False);
      core.set_verbosity (?, ?);
   endrule

   // Tie-offs
   rule rl_always (True);
      // Non-maskable interrupt request.
      core.nmi_req (False);
   endrule

   // ================================================================
   // Reset on startup, and also on NDM reset from Debug Module
   // (NDM reset from Debug Module = "non-debug-module-reset" = reset all except Debug Module)

   Reg #(Bool)          rg_once      <- mkReg (False, reset_by por_reset); // also set False by ndmreset
   Reg #(Maybe #(Bool)) rg_ndm_reset <- mkReg (tagged Invalid, reset_by por_reset);
`ifdef INCLUDE_GDB_CONTROL
   Reg #(UInt #(6))     rg_ndm_count <- mkReg (0, reset_by por_reset);

   rule decNdmCountRl (rg_ndm_count != 0);
      rg_ndm_count <= rg_ndm_count -1;
      ndmIfc.assertReset();
   endrule
`endif

   let coreInReset <- isResetAsserted(reset_by coreRSTN);

   rule rl_once (! rg_once && ! coreInReset);
      Bool running = True;
      if (rg_ndm_reset matches tagged Valid False)
	 running = False;
      core.cpu_reset_server.request.put (running);
      // TODO: maybe set rg_ndm_count if debug_module present?
      rg_once <= True;
   endrule

   rule rl_reset_response;
      let running <- core.cpu_reset_server.response.get;
`ifdef INCLUDE_GDB_CONTROL
      // wait for end of ndm_interval:
      when (rg_ndm_count == 0, noAction);
      // Respond to Debug module if this is an ndm-reset:
      if (rg_ndm_reset matches tagged Valid .x)
	 core.ndm_reset_client.response.put (running);
      rg_ndm_reset <= tagged Invalid;
`endif
   endrule

   // ----------------
   // Also do a reset if requested from Debug Module (NDM = Non-Debug-Module reset)

   rule rl_ndmreset (rg_once); // inhibited until after initialization
`ifdef INCLUDE_GDB_CONTROL
      let running <- core.ndm_reset_client.request.get;
      rg_ndm_reset <= tagged Valid running;
      rg_ndm_count <= ndm_interval;
`endif
      rg_once <= False;
   endrule

   // ================================================================
`ifdef INCLUDE_GDB_CONTROL

   // Instantiate JTAG TAP controller,
   // connect to core.dm_dmi;
   // and export its JTAG interface

   Wire#(Bit#(7)) w_dmi_req_addr <- mkDWire(0);
   Wire#(Bit#(32)) w_dmi_req_data <- mkDWire(0);
   Wire#(Bit#(2)) w_dmi_req_op <- mkDWire(0);

   Wire#(Bit#(32)) w_dmi_rsp_data <- mkDWire(0);
   Wire#(Bit#(2)) w_dmi_rsp_response <- mkDWire(0);

   BusReceiver#(Tuple3#(Bit#(7),Bit#(32),Bit#(2))) bus_dmi_req <- mkBusReceiver(reset_by dmi_resetN);
   BusSender#(Tuple2#(Bit#(32),Bit#(2))) bus_dmi_rsp <- mkBusSender(unpack(0), reset_by dmi_resetN);

`ifdef JTAG_TAP
   let jtagtap <- mkJtagTap(reset_by dmi_resetN);

   mkConnection(jtagtap.dmi.req_ready, pack(bus_dmi_req.in.ready), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_valid, compose(bus_dmi_req.in.valid, unpack), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_addr, w_dmi_req_addr._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_data, w_dmi_req_data._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.req_op, w_dmi_req_op._write, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_valid, pack(bus_dmi_rsp.out.valid), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_ready, compose(bus_dmi_rsp.out.ready, unpack), reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_data, w_dmi_rsp_data, reset_by dmi_resetN);
   mkConnection(jtagtap.dmi.rsp_response, w_dmi_rsp_response, reset_by dmi_resetN);
`endif

   rule rl_dmi_req;
      bus_dmi_req.in.data(tuple3(w_dmi_req_addr, w_dmi_req_data, w_dmi_req_op));
   endrule

   rule rl_dmi_rsp;
      match {.data, .response} = bus_dmi_rsp.out.data;
      w_dmi_rsp_data <= data;
      w_dmi_rsp_response <= response;
   endrule

   (* preempts = "rl_dmi_req_cpu, rl_dmi_rsp_cpu" *)
   rule rl_dmi_req_cpu;
      match {.addr, .data, .op} = bus_dmi_req.out.first;
      bus_dmi_req.out.deq;
      case (op)
	 1: core.dm_dmi.read_addr(addr);
	 2: begin
	       core.dm_dmi.write(addr, data);
	       bus_dmi_rsp.in.enq(tuple2(?, 0));
	    end
	 default: bus_dmi_rsp.in.enq(tuple2(?, 2));
      endcase
   endrule

   rule rl_dmi_rsp_cpu;
      let data <- core.dm_dmi.read_data;
      bus_dmi_rsp.in.enq(tuple2(data, 0));
   endrule

`endif

`ifdef INCLUDE_TANDEM_VERIF
   let tv_xactor <- mkTV_Xactor;
   mkConnection (core.tv_verifier_info_get, tv_xactor.tv_in);
`endif

   // ================================================================
   // INTERFACE
`ifndef Near_Mem_TCM
   // CPU IMem to Fabric master interface
   interface AXI4_Master_IFC master0 = core.cpu_imem_master;
`endif

`ifdef FABRIC_AHBL
   // CPU DMem to Fabric master interface
   interface AHBL_Master_IFC master1 = core.cpu_dmem_master;
`else
   // CPU DMem to Fabric master interface
   interface AXI4_Master_IFC master1 = core.cpu_dmem_master;
`endif

   // External interrupts
   method  Action interrupt_reqs (Bit #(N_External_Interrupt_Sources) reqs);
      for (Integer j = 0; j < valueOf (N_External_Interrupt_Sources); j = j + 1) begin
	 Bool req_j = unpack (reqs [j]);
	 core.core_external_interrupt_sources [j].m_interrupt_req (req_j);
      end
   endmethod

`ifdef INCLUDE_DMEM_SLAVE
   // ----------------------------------------------------------------
   // Optional AXI4-Lite D-cache slave interface

   interface AXI4_Lite_Slave_IFC slave0 = core.cpu_dmem_slave;
`endif

`ifdef INCLUDE_TANDEM_VERIF
   // ----------------------------------------------------------------
   // Optional Tandem Verifier interface.  The data signal is
   // packed output tuples (n,vb),/ where 'vb' is a vector of
   // bytes with relevant bytes in locations [0]..[n-1]

   interface tv_verifier_info_tx = tv_xactor.axi_out;
`endif

`ifdef INCLUDE_GDB_CONTROL
   // ----------------------------------------------------------------
   // Optional Debug Module interfaces

`ifdef JTAG_TAP
   interface JTAG_IFC jtag = jtagtap.jtag;
`endif

   interface ndm_reset = coreRSTN;
`endif

`ifdef Near_Mem_TCM
`ifdef WATCH_TOHOST
   // For ISA tests: watch memory writes to <tohost> addr
   method Action set_watch_tohost (Bool  watch_tohost, Bit #(64)  tohost_addr);
      core.set_watch_tohost (watch_tohost, tohost_addr);
   endmethod

   method Bit #(64) mv_tohost_value = core.mv_tohost_value;
`endif
`endif

endmodule

// ================================================================

endpackage