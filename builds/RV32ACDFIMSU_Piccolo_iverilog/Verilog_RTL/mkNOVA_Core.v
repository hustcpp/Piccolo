//
// Generated by Bluespec Compiler, version 2021.12.1 (build fd50140)
//
//
// Ports:
// Name                         I/O  size props
// mem_master_awvalid             O     1 reg
// mem_master_awid                O     4 reg
// mem_master_awaddr              O    64 reg
// mem_master_awlen               O     8 reg
// mem_master_awsize              O     3 reg
// mem_master_awburst             O     2 reg
// mem_master_awlock              O     1 reg
// mem_master_awcache             O     4 reg
// mem_master_awprot              O     3 reg
// mem_master_awqos               O     4 reg
// mem_master_awregion            O     4 reg
// mem_master_wvalid              O     1 reg
// mem_master_wdata               O    64 reg
// mem_master_wstrb               O     8 reg
// mem_master_wlast               O     1 reg
// mem_master_bready              O     1 reg
// mem_master_arvalid             O     1 reg
// mem_master_arid                O     4 reg
// mem_master_araddr              O    64 reg
// mem_master_arlen               O     8 reg
// mem_master_arsize              O     3 reg
// mem_master_arburst             O     2 reg
// mem_master_arlock              O     1 reg
// mem_master_arcache             O     4 reg
// mem_master_arprot              O     3 reg
// mem_master_arqos               O     4 reg
// mem_master_arregion            O     4 reg
// mem_master_rready              O     1 reg
// CLK                            I     1 clock
// RST_N                          I     1 reset
// mem_master_awready             I     1
// mem_master_wready              I     1
// mem_master_bvalid              I     1
// mem_master_bid                 I     4 reg
// mem_master_bresp               I     2 reg
// mem_master_arready             I     1
// mem_master_rvalid              I     1
// mem_master_rid                 I     4 reg
// mem_master_rdata               I    64 reg
// mem_master_rresp               I     2 reg
// mem_master_rlast               I     1 reg
//
// No combinational paths from inputs to outputs
//
//

`ifdef BSV_ASSIGNMENT_DELAY
`else
  `define BSV_ASSIGNMENT_DELAY
`endif

`ifdef BSV_POSITIVE_RESET
  `define BSV_RESET_VALUE 1'b1
  `define BSV_RESET_EDGE posedge
`else
  `define BSV_RESET_VALUE 1'b0
  `define BSV_RESET_EDGE negedge
`endif

module mkNOVA_Core(CLK,
		   RST_N,

		   mem_master_awvalid,

		   mem_master_awid,

		   mem_master_awaddr,

		   mem_master_awlen,

		   mem_master_awsize,

		   mem_master_awburst,

		   mem_master_awlock,

		   mem_master_awcache,

		   mem_master_awprot,

		   mem_master_awqos,

		   mem_master_awregion,

		   mem_master_awready,

		   mem_master_wvalid,

		   mem_master_wdata,

		   mem_master_wstrb,

		   mem_master_wlast,

		   mem_master_wready,

		   mem_master_bvalid,
		   mem_master_bid,
		   mem_master_bresp,

		   mem_master_bready,

		   mem_master_arvalid,

		   mem_master_arid,

		   mem_master_araddr,

		   mem_master_arlen,

		   mem_master_arsize,

		   mem_master_arburst,

		   mem_master_arlock,

		   mem_master_arcache,

		   mem_master_arprot,

		   mem_master_arqos,

		   mem_master_arregion,

		   mem_master_arready,

		   mem_master_rvalid,
		   mem_master_rid,
		   mem_master_rdata,
		   mem_master_rresp,
		   mem_master_rlast,

		   mem_master_rready);
  input  CLK;
  input  RST_N;

  // value method mem_master_m_awvalid
  output mem_master_awvalid;

  // value method mem_master_m_awid
  output [3 : 0] mem_master_awid;

  // value method mem_master_m_awaddr
  output [63 : 0] mem_master_awaddr;

  // value method mem_master_m_awlen
  output [7 : 0] mem_master_awlen;

  // value method mem_master_m_awsize
  output [2 : 0] mem_master_awsize;

  // value method mem_master_m_awburst
  output [1 : 0] mem_master_awburst;

  // value method mem_master_m_awlock
  output mem_master_awlock;

  // value method mem_master_m_awcache
  output [3 : 0] mem_master_awcache;

  // value method mem_master_m_awprot
  output [2 : 0] mem_master_awprot;

  // value method mem_master_m_awqos
  output [3 : 0] mem_master_awqos;

  // value method mem_master_m_awregion
  output [3 : 0] mem_master_awregion;

  // value method mem_master_m_awuser

  // action method mem_master_m_awready
  input  mem_master_awready;

  // value method mem_master_m_wvalid
  output mem_master_wvalid;

  // value method mem_master_m_wdata
  output [63 : 0] mem_master_wdata;

  // value method mem_master_m_wstrb
  output [7 : 0] mem_master_wstrb;

  // value method mem_master_m_wlast
  output mem_master_wlast;

  // value method mem_master_m_wuser

  // action method mem_master_m_wready
  input  mem_master_wready;

  // action method mem_master_m_bvalid
  input  mem_master_bvalid;
  input  [3 : 0] mem_master_bid;
  input  [1 : 0] mem_master_bresp;

  // value method mem_master_m_bready
  output mem_master_bready;

  // value method mem_master_m_arvalid
  output mem_master_arvalid;

  // value method mem_master_m_arid
  output [3 : 0] mem_master_arid;

  // value method mem_master_m_araddr
  output [63 : 0] mem_master_araddr;

  // value method mem_master_m_arlen
  output [7 : 0] mem_master_arlen;

  // value method mem_master_m_arsize
  output [2 : 0] mem_master_arsize;

  // value method mem_master_m_arburst
  output [1 : 0] mem_master_arburst;

  // value method mem_master_m_arlock
  output mem_master_arlock;

  // value method mem_master_m_arcache
  output [3 : 0] mem_master_arcache;

  // value method mem_master_m_arprot
  output [2 : 0] mem_master_arprot;

  // value method mem_master_m_arqos
  output [3 : 0] mem_master_arqos;

  // value method mem_master_m_arregion
  output [3 : 0] mem_master_arregion;

  // value method mem_master_m_aruser

  // action method mem_master_m_arready
  input  mem_master_arready;

  // action method mem_master_m_rvalid
  input  mem_master_rvalid;
  input  [3 : 0] mem_master_rid;
  input  [63 : 0] mem_master_rdata;
  input  [1 : 0] mem_master_rresp;
  input  mem_master_rlast;

  // value method mem_master_m_rready
  output mem_master_rready;

  // signals for module outputs
  wire [63 : 0] mem_master_araddr, mem_master_awaddr, mem_master_wdata;
  wire [7 : 0] mem_master_arlen, mem_master_awlen, mem_master_wstrb;
  wire [3 : 0] mem_master_arcache,
	       mem_master_arid,
	       mem_master_arqos,
	       mem_master_arregion,
	       mem_master_awcache,
	       mem_master_awid,
	       mem_master_awqos,
	       mem_master_awregion;
  wire [2 : 0] mem_master_arprot,
	       mem_master_arsize,
	       mem_master_awprot,
	       mem_master_awsize;
  wire [1 : 0] mem_master_arburst, mem_master_awburst;
  wire mem_master_arlock,
       mem_master_arvalid,
       mem_master_awlock,
       mem_master_awvalid,
       mem_master_bready,
       mem_master_rready,
       mem_master_wlast,
       mem_master_wvalid;

  // ports of submodule bp_cplx
  wire [92 : 0] bp_cplx$ifc_fbu_intf_put;
  wire [13 : 0] bp_cplx$rob_excp_intf_put;
  wire [5 : 0] bp_cplx$rob_cmt_intf_put;
  wire bp_cplx$EN_exu_bcu_intfs_0_get,
       bp_cplx$EN_ifc_bpq_intf_get,
       bp_cplx$EN_ifc_brf_intf_get,
       bp_cplx$EN_ifc_fbu_intf_put,
       bp_cplx$EN_itb_flush_intf_put,
       bp_cplx$EN_rob_cmt_intf_put,
       bp_cplx$EN_rob_excp_intf_put;

  // ports of submodule master_xactor_f_rd_addr
  wire [96 : 0] master_xactor_f_rd_addr$D_IN, master_xactor_f_rd_addr$D_OUT;
  wire master_xactor_f_rd_addr$CLR,
       master_xactor_f_rd_addr$DEQ,
       master_xactor_f_rd_addr$EMPTY_N,
       master_xactor_f_rd_addr$ENQ;

  // ports of submodule master_xactor_f_rd_data
  wire [70 : 0] master_xactor_f_rd_data$D_IN;
  wire master_xactor_f_rd_data$CLR,
       master_xactor_f_rd_data$DEQ,
       master_xactor_f_rd_data$ENQ,
       master_xactor_f_rd_data$FULL_N;

  // ports of submodule master_xactor_f_wr_addr
  wire [96 : 0] master_xactor_f_wr_addr$D_IN, master_xactor_f_wr_addr$D_OUT;
  wire master_xactor_f_wr_addr$CLR,
       master_xactor_f_wr_addr$DEQ,
       master_xactor_f_wr_addr$EMPTY_N,
       master_xactor_f_wr_addr$ENQ;

  // ports of submodule master_xactor_f_wr_data
  wire [72 : 0] master_xactor_f_wr_data$D_IN, master_xactor_f_wr_data$D_OUT;
  wire master_xactor_f_wr_data$CLR,
       master_xactor_f_wr_data$DEQ,
       master_xactor_f_wr_data$EMPTY_N,
       master_xactor_f_wr_data$ENQ;

  // ports of submodule master_xactor_f_wr_resp
  wire [5 : 0] master_xactor_f_wr_resp$D_IN;
  wire master_xactor_f_wr_resp$CLR,
       master_xactor_f_wr_resp$DEQ,
       master_xactor_f_wr_resp$ENQ,
       master_xactor_f_wr_resp$FULL_N;

  // rule scheduling signals
  wire CAN_FIRE_mem_master_m_arready,
       CAN_FIRE_mem_master_m_awready,
       CAN_FIRE_mem_master_m_bvalid,
       CAN_FIRE_mem_master_m_rvalid,
       CAN_FIRE_mem_master_m_wready,
       WILL_FIRE_mem_master_m_arready,
       WILL_FIRE_mem_master_m_awready,
       WILL_FIRE_mem_master_m_bvalid,
       WILL_FIRE_mem_master_m_rvalid,
       WILL_FIRE_mem_master_m_wready;

  // value method mem_master_m_awvalid
  assign mem_master_awvalid = master_xactor_f_wr_addr$EMPTY_N ;

  // value method mem_master_m_awid
  assign mem_master_awid = master_xactor_f_wr_addr$D_OUT[96:93] ;

  // value method mem_master_m_awaddr
  assign mem_master_awaddr = master_xactor_f_wr_addr$D_OUT[92:29] ;

  // value method mem_master_m_awlen
  assign mem_master_awlen = master_xactor_f_wr_addr$D_OUT[28:21] ;

  // value method mem_master_m_awsize
  assign mem_master_awsize = master_xactor_f_wr_addr$D_OUT[20:18] ;

  // value method mem_master_m_awburst
  assign mem_master_awburst = master_xactor_f_wr_addr$D_OUT[17:16] ;

  // value method mem_master_m_awlock
  assign mem_master_awlock = master_xactor_f_wr_addr$D_OUT[15] ;

  // value method mem_master_m_awcache
  assign mem_master_awcache = master_xactor_f_wr_addr$D_OUT[14:11] ;

  // value method mem_master_m_awprot
  assign mem_master_awprot = master_xactor_f_wr_addr$D_OUT[10:8] ;

  // value method mem_master_m_awqos
  assign mem_master_awqos = master_xactor_f_wr_addr$D_OUT[7:4] ;

  // value method mem_master_m_awregion
  assign mem_master_awregion = master_xactor_f_wr_addr$D_OUT[3:0] ;

  // action method mem_master_m_awready
  assign CAN_FIRE_mem_master_m_awready = 1'd1 ;
  assign WILL_FIRE_mem_master_m_awready = 1'd1 ;

  // value method mem_master_m_wvalid
  assign mem_master_wvalid = master_xactor_f_wr_data$EMPTY_N ;

  // value method mem_master_m_wdata
  assign mem_master_wdata = master_xactor_f_wr_data$D_OUT[72:9] ;

  // value method mem_master_m_wstrb
  assign mem_master_wstrb = master_xactor_f_wr_data$D_OUT[8:1] ;

  // value method mem_master_m_wlast
  assign mem_master_wlast = master_xactor_f_wr_data$D_OUT[0] ;

  // action method mem_master_m_wready
  assign CAN_FIRE_mem_master_m_wready = 1'd1 ;
  assign WILL_FIRE_mem_master_m_wready = 1'd1 ;

  // action method mem_master_m_bvalid
  assign CAN_FIRE_mem_master_m_bvalid = 1'd1 ;
  assign WILL_FIRE_mem_master_m_bvalid = 1'd1 ;

  // value method mem_master_m_bready
  assign mem_master_bready = master_xactor_f_wr_resp$FULL_N ;

  // value method mem_master_m_arvalid
  assign mem_master_arvalid = master_xactor_f_rd_addr$EMPTY_N ;

  // value method mem_master_m_arid
  assign mem_master_arid = master_xactor_f_rd_addr$D_OUT[96:93] ;

  // value method mem_master_m_araddr
  assign mem_master_araddr = master_xactor_f_rd_addr$D_OUT[92:29] ;

  // value method mem_master_m_arlen
  assign mem_master_arlen = master_xactor_f_rd_addr$D_OUT[28:21] ;

  // value method mem_master_m_arsize
  assign mem_master_arsize = master_xactor_f_rd_addr$D_OUT[20:18] ;

  // value method mem_master_m_arburst
  assign mem_master_arburst = master_xactor_f_rd_addr$D_OUT[17:16] ;

  // value method mem_master_m_arlock
  assign mem_master_arlock = master_xactor_f_rd_addr$D_OUT[15] ;

  // value method mem_master_m_arcache
  assign mem_master_arcache = master_xactor_f_rd_addr$D_OUT[14:11] ;

  // value method mem_master_m_arprot
  assign mem_master_arprot = master_xactor_f_rd_addr$D_OUT[10:8] ;

  // value method mem_master_m_arqos
  assign mem_master_arqos = master_xactor_f_rd_addr$D_OUT[7:4] ;

  // value method mem_master_m_arregion
  assign mem_master_arregion = master_xactor_f_rd_addr$D_OUT[3:0] ;

  // action method mem_master_m_arready
  assign CAN_FIRE_mem_master_m_arready = 1'd1 ;
  assign WILL_FIRE_mem_master_m_arready = 1'd1 ;

  // action method mem_master_m_rvalid
  assign CAN_FIRE_mem_master_m_rvalid = 1'd1 ;
  assign WILL_FIRE_mem_master_m_rvalid = 1'd1 ;

  // value method mem_master_m_rready
  assign mem_master_rready = master_xactor_f_rd_data$FULL_N ;

  // submodule bp_cplx
  mkNOVA_BrPredCplx bp_cplx(.CLK(CLK),
			    .RST_N(RST_N),
			    .ifc_fbu_intf_put(bp_cplx$ifc_fbu_intf_put),
			    .rob_cmt_intf_put(bp_cplx$rob_cmt_intf_put),
			    .rob_excp_intf_put(bp_cplx$rob_excp_intf_put),
			    .EN_ifc_bpq_intf_get(bp_cplx$EN_ifc_bpq_intf_get),
			    .EN_ifc_brf_intf_get(bp_cplx$EN_ifc_brf_intf_get),
			    .EN_ifc_fbu_intf_put(bp_cplx$EN_ifc_fbu_intf_put),
			    .EN_exu_bcu_intfs_0_get(bp_cplx$EN_exu_bcu_intfs_0_get),
			    .EN_rob_cmt_intf_put(bp_cplx$EN_rob_cmt_intf_put),
			    .EN_rob_excp_intf_put(bp_cplx$EN_rob_excp_intf_put),
			    .EN_itb_flush_intf_put(bp_cplx$EN_itb_flush_intf_put),
			    .ifc_bpq_intf_get(),
			    .RDY_ifc_bpq_intf_get(),
			    .ifc_brf_intf_get(),
			    .RDY_ifc_brf_intf_get(),
			    .RDY_ifc_fbu_intf_put(),
			    .exu_bcu_intfs_0_get(),
			    .RDY_exu_bcu_intfs_0_get(),
			    .RDY_rob_cmt_intf_put(),
			    .RDY_rob_excp_intf_put(),
			    .RDY_itb_flush_intf_put());

  // submodule master_xactor_f_rd_addr
  FIFO2 #(.width(32'd97), .guarded(1'd1)) master_xactor_f_rd_addr(.RST(RST_N),
								  .CLK(CLK),
								  .D_IN(master_xactor_f_rd_addr$D_IN),
								  .ENQ(master_xactor_f_rd_addr$ENQ),
								  .DEQ(master_xactor_f_rd_addr$DEQ),
								  .CLR(master_xactor_f_rd_addr$CLR),
								  .D_OUT(master_xactor_f_rd_addr$D_OUT),
								  .FULL_N(),
								  .EMPTY_N(master_xactor_f_rd_addr$EMPTY_N));

  // submodule master_xactor_f_rd_data
  FIFO2 #(.width(32'd71), .guarded(1'd1)) master_xactor_f_rd_data(.RST(RST_N),
								  .CLK(CLK),
								  .D_IN(master_xactor_f_rd_data$D_IN),
								  .ENQ(master_xactor_f_rd_data$ENQ),
								  .DEQ(master_xactor_f_rd_data$DEQ),
								  .CLR(master_xactor_f_rd_data$CLR),
								  .D_OUT(),
								  .FULL_N(master_xactor_f_rd_data$FULL_N),
								  .EMPTY_N());

  // submodule master_xactor_f_wr_addr
  FIFO2 #(.width(32'd97), .guarded(1'd1)) master_xactor_f_wr_addr(.RST(RST_N),
								  .CLK(CLK),
								  .D_IN(master_xactor_f_wr_addr$D_IN),
								  .ENQ(master_xactor_f_wr_addr$ENQ),
								  .DEQ(master_xactor_f_wr_addr$DEQ),
								  .CLR(master_xactor_f_wr_addr$CLR),
								  .D_OUT(master_xactor_f_wr_addr$D_OUT),
								  .FULL_N(),
								  .EMPTY_N(master_xactor_f_wr_addr$EMPTY_N));

  // submodule master_xactor_f_wr_data
  FIFO2 #(.width(32'd73), .guarded(1'd1)) master_xactor_f_wr_data(.RST(RST_N),
								  .CLK(CLK),
								  .D_IN(master_xactor_f_wr_data$D_IN),
								  .ENQ(master_xactor_f_wr_data$ENQ),
								  .DEQ(master_xactor_f_wr_data$DEQ),
								  .CLR(master_xactor_f_wr_data$CLR),
								  .D_OUT(master_xactor_f_wr_data$D_OUT),
								  .FULL_N(),
								  .EMPTY_N(master_xactor_f_wr_data$EMPTY_N));

  // submodule master_xactor_f_wr_resp
  FIFO2 #(.width(32'd6), .guarded(1'd1)) master_xactor_f_wr_resp(.RST(RST_N),
								 .CLK(CLK),
								 .D_IN(master_xactor_f_wr_resp$D_IN),
								 .ENQ(master_xactor_f_wr_resp$ENQ),
								 .DEQ(master_xactor_f_wr_resp$DEQ),
								 .CLR(master_xactor_f_wr_resp$CLR),
								 .D_OUT(),
								 .FULL_N(master_xactor_f_wr_resp$FULL_N),
								 .EMPTY_N());

  // submodule bp_cplx
  assign bp_cplx$ifc_fbu_intf_put = 93'h0 ;
  assign bp_cplx$rob_cmt_intf_put = 6'h0 ;
  assign bp_cplx$rob_excp_intf_put = 14'h0 ;
  assign bp_cplx$EN_ifc_bpq_intf_get = 1'b0 ;
  assign bp_cplx$EN_ifc_brf_intf_get = 1'b0 ;
  assign bp_cplx$EN_ifc_fbu_intf_put = 1'b0 ;
  assign bp_cplx$EN_exu_bcu_intfs_0_get = 1'b0 ;
  assign bp_cplx$EN_rob_cmt_intf_put = 1'b0 ;
  assign bp_cplx$EN_rob_excp_intf_put = 1'b0 ;
  assign bp_cplx$EN_itb_flush_intf_put = 1'b0 ;

  // submodule master_xactor_f_rd_addr
  assign master_xactor_f_rd_addr$D_IN = 97'h0 ;
  assign master_xactor_f_rd_addr$ENQ = 1'b0 ;
  assign master_xactor_f_rd_addr$DEQ =
	     master_xactor_f_rd_addr$EMPTY_N && mem_master_arready ;
  assign master_xactor_f_rd_addr$CLR = 1'b0 ;

  // submodule master_xactor_f_rd_data
  assign master_xactor_f_rd_data$D_IN =
	     { mem_master_rid,
	       mem_master_rdata,
	       mem_master_rresp,
	       mem_master_rlast } ;
  assign master_xactor_f_rd_data$ENQ =
	     mem_master_rvalid && master_xactor_f_rd_data$FULL_N ;
  assign master_xactor_f_rd_data$DEQ = 1'b0 ;
  assign master_xactor_f_rd_data$CLR = 1'b0 ;

  // submodule master_xactor_f_wr_addr
  assign master_xactor_f_wr_addr$D_IN = 97'h0 ;
  assign master_xactor_f_wr_addr$ENQ = 1'b0 ;
  assign master_xactor_f_wr_addr$DEQ =
	     master_xactor_f_wr_addr$EMPTY_N && mem_master_awready ;
  assign master_xactor_f_wr_addr$CLR = 1'b0 ;

  // submodule master_xactor_f_wr_data
  assign master_xactor_f_wr_data$D_IN = 73'h0 ;
  assign master_xactor_f_wr_data$ENQ = 1'b0 ;
  assign master_xactor_f_wr_data$DEQ =
	     master_xactor_f_wr_data$EMPTY_N && mem_master_wready ;
  assign master_xactor_f_wr_data$CLR = 1'b0 ;

  // submodule master_xactor_f_wr_resp
  assign master_xactor_f_wr_resp$D_IN = { mem_master_bid, mem_master_bresp } ;
  assign master_xactor_f_wr_resp$ENQ =
	     mem_master_bvalid && master_xactor_f_wr_resp$FULL_N ;
  assign master_xactor_f_wr_resp$DEQ = 1'b0 ;
  assign master_xactor_f_wr_resp$CLR = 1'b0 ;
endmodule  // mkNOVA_Core

