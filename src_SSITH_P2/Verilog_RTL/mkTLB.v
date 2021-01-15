//
// Generated by Bluespec Compiler, version 2019.05.beta2 (build a88bf40db, 2019-05-24)
//
//
//
//
// Ports:
// Name                         I/O  size props
// mv_vm_xlate                    O   201
// RDY_mv_vm_xlate                O     1 const
// RDY_ma_insert                  O     1 const
// RDY_ma_flush                   O     1 const
// CLK                            I     1 clock
// RST_N                          I     1 unused
// mv_vm_xlate_va                 I    64
// mv_vm_xlate_satp               I    64
// mv_vm_xlate_dmem_not_imem      I     1
// mv_vm_xlate_read_not_write     I     1
// mv_vm_xlate_priv               I     2
// mv_vm_xlate_sstatus_SUM        I     1
// mv_vm_xlate_mstatus_MXR        I     1
// ma_insert_asid                 I    16 reg
// ma_insert_vpn                  I    27
// ma_insert_pte                  I    64 reg
// ma_insert_level                I     2
// ma_insert_pte_pa               I    64 reg
// EN_ma_insert                   I     1
// EN_ma_flush                    I     1
//
// Combinational paths from inputs to outputs:
//   (mv_vm_xlate_va,
//    mv_vm_xlate_satp,
//    mv_vm_xlate_dmem_not_imem,
//    mv_vm_xlate_read_not_write,
//    mv_vm_xlate_priv,
//    mv_vm_xlate_sstatus_SUM,
//    mv_vm_xlate_mstatus_MXR) -> mv_vm_xlate
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

module mkTLB(CLK,
	     RST_N,

	     mv_vm_xlate_va,
	     mv_vm_xlate_satp,
	     mv_vm_xlate_dmem_not_imem,
	     mv_vm_xlate_read_not_write,
	     mv_vm_xlate_priv,
	     mv_vm_xlate_sstatus_SUM,
	     mv_vm_xlate_mstatus_MXR,
	     mv_vm_xlate,
	     RDY_mv_vm_xlate,

	     ma_insert_asid,
	     ma_insert_vpn,
	     ma_insert_pte,
	     ma_insert_level,
	     ma_insert_pte_pa,
	     EN_ma_insert,
	     RDY_ma_insert,

	     EN_ma_flush,
	     RDY_ma_flush);
  input  CLK;
  input  RST_N;

  // value method mv_vm_xlate
  input  [63 : 0] mv_vm_xlate_va;
  input  [63 : 0] mv_vm_xlate_satp;
  input  mv_vm_xlate_dmem_not_imem;
  input  mv_vm_xlate_read_not_write;
  input  [1 : 0] mv_vm_xlate_priv;
  input  mv_vm_xlate_sstatus_SUM;
  input  mv_vm_xlate_mstatus_MXR;
  output [200 : 0] mv_vm_xlate;
  output RDY_mv_vm_xlate;

  // action method ma_insert
  input  [15 : 0] ma_insert_asid;
  input  [26 : 0] ma_insert_vpn;
  input  [63 : 0] ma_insert_pte;
  input  [1 : 0] ma_insert_level;
  input  [63 : 0] ma_insert_pte_pa;
  input  EN_ma_insert;
  output RDY_ma_insert;

  // action method ma_flush
  input  EN_ma_flush;
  output RDY_ma_flush;

  // signals for module outputs
  wire [200 : 0] mv_vm_xlate;
  wire RDY_ma_flush, RDY_ma_insert, RDY_mv_vm_xlate;

  // register tlb0_valids_0
  reg tlb0_valids_0;
  wire tlb0_valids_0$D_IN, tlb0_valids_0$EN;

  // register tlb0_valids_1
  reg tlb0_valids_1;
  wire tlb0_valids_1$D_IN, tlb0_valids_1$EN;

  // register tlb0_valids_10
  reg tlb0_valids_10;
  wire tlb0_valids_10$D_IN, tlb0_valids_10$EN;

  // register tlb0_valids_11
  reg tlb0_valids_11;
  wire tlb0_valids_11$D_IN, tlb0_valids_11$EN;

  // register tlb0_valids_12
  reg tlb0_valids_12;
  wire tlb0_valids_12$D_IN, tlb0_valids_12$EN;

  // register tlb0_valids_13
  reg tlb0_valids_13;
  wire tlb0_valids_13$D_IN, tlb0_valids_13$EN;

  // register tlb0_valids_14
  reg tlb0_valids_14;
  wire tlb0_valids_14$D_IN, tlb0_valids_14$EN;

  // register tlb0_valids_15
  reg tlb0_valids_15;
  wire tlb0_valids_15$D_IN, tlb0_valids_15$EN;

  // register tlb0_valids_2
  reg tlb0_valids_2;
  wire tlb0_valids_2$D_IN, tlb0_valids_2$EN;

  // register tlb0_valids_3
  reg tlb0_valids_3;
  wire tlb0_valids_3$D_IN, tlb0_valids_3$EN;

  // register tlb0_valids_4
  reg tlb0_valids_4;
  wire tlb0_valids_4$D_IN, tlb0_valids_4$EN;

  // register tlb0_valids_5
  reg tlb0_valids_5;
  wire tlb0_valids_5$D_IN, tlb0_valids_5$EN;

  // register tlb0_valids_6
  reg tlb0_valids_6;
  wire tlb0_valids_6$D_IN, tlb0_valids_6$EN;

  // register tlb0_valids_7
  reg tlb0_valids_7;
  wire tlb0_valids_7$D_IN, tlb0_valids_7$EN;

  // register tlb0_valids_8
  reg tlb0_valids_8;
  wire tlb0_valids_8$D_IN, tlb0_valids_8$EN;

  // register tlb0_valids_9
  reg tlb0_valids_9;
  wire tlb0_valids_9$D_IN, tlb0_valids_9$EN;

  // register tlb1_valids_0
  reg tlb1_valids_0;
  wire tlb1_valids_0$D_IN, tlb1_valids_0$EN;

  // register tlb1_valids_1
  reg tlb1_valids_1;
  wire tlb1_valids_1$D_IN, tlb1_valids_1$EN;

  // register tlb1_valids_2
  reg tlb1_valids_2;
  wire tlb1_valids_2$D_IN, tlb1_valids_2$EN;

  // register tlb1_valids_3
  reg tlb1_valids_3;
  wire tlb1_valids_3$D_IN, tlb1_valids_3$EN;

  // register tlb1_valids_4
  reg tlb1_valids_4;
  wire tlb1_valids_4$D_IN, tlb1_valids_4$EN;

  // register tlb1_valids_5
  reg tlb1_valids_5;
  wire tlb1_valids_5$D_IN, tlb1_valids_5$EN;

  // register tlb1_valids_6
  reg tlb1_valids_6;
  wire tlb1_valids_6$D_IN, tlb1_valids_6$EN;

  // register tlb1_valids_7
  reg tlb1_valids_7;
  wire tlb1_valids_7$D_IN, tlb1_valids_7$EN;

  // register tlb2_valids_0
  reg tlb2_valids_0;
  wire tlb2_valids_0$D_IN, tlb2_valids_0$EN;

  // register tlb2_valids_1
  reg tlb2_valids_1;
  wire tlb2_valids_1$D_IN, tlb2_valids_1$EN;

  // register tlb2_valids_2
  reg tlb2_valids_2;
  wire tlb2_valids_2$D_IN, tlb2_valids_2$EN;

  // register tlb2_valids_3
  reg tlb2_valids_3;
  wire tlb2_valids_3$D_IN, tlb2_valids_3$EN;

  // ports of submodule tlb0_entries
  wire [166 : 0] tlb0_entries$D_IN, tlb0_entries$D_OUT_1;
  wire [3 : 0] tlb0_entries$ADDR_1,
	       tlb0_entries$ADDR_2,
	       tlb0_entries$ADDR_3,
	       tlb0_entries$ADDR_4,
	       tlb0_entries$ADDR_5,
	       tlb0_entries$ADDR_IN;
  wire tlb0_entries$WE;

  // ports of submodule tlb1_entries
  wire [158 : 0] tlb1_entries$D_IN, tlb1_entries$D_OUT_1;
  wire [2 : 0] tlb1_entries$ADDR_1,
	       tlb1_entries$ADDR_2,
	       tlb1_entries$ADDR_3,
	       tlb1_entries$ADDR_4,
	       tlb1_entries$ADDR_5,
	       tlb1_entries$ADDR_IN;
  wire tlb1_entries$WE;

  // ports of submodule tlb2_entries
  wire [150 : 0] tlb2_entries$D_IN, tlb2_entries$D_OUT_1;
  wire [1 : 0] tlb2_entries$ADDR_1,
	       tlb2_entries$ADDR_2,
	       tlb2_entries$ADDR_3,
	       tlb2_entries$ADDR_4,
	       tlb2_entries$ADDR_5,
	       tlb2_entries$ADDR_IN;
  wire tlb2_entries$WE;

  // rule scheduling signals
  wire CAN_FIRE_RL_rl_flush,
       CAN_FIRE_ma_flush,
       CAN_FIRE_ma_insert,
       WILL_FIRE_RL_rl_flush,
       WILL_FIRE_ma_flush,
       WILL_FIRE_ma_insert;

  // remaining internal signals
  reg [63 : 0] _theResult___fst__h7242;
  reg SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24,
      SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46,
      SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d64;
  wire [129 : 0] IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91,
		 IF_SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb_ETC___d85,
		 IF_SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb_ETC___d90;
  wire [63 : 0] _theResult___snd_fst__h6325,
		_theResult___snd_fst__h6963,
		_theResult___snd_fst__h7467,
		_theResult___snd_snd_fst__h6327,
		_theResult___snd_snd_fst__h6965,
		pa___1__h7247,
		pa___1__h7296,
		pa___1__h7364,
		pte___1__h7516,
		pte___1__h7545,
		pte___2__h7240,
		x__h6571,
		x__h6591,
		x__h7236,
		x__h7551;
  wire [55 : 0] x__h7250, x__h7299, x__h7367;
  wire [3 : 0] result_exc_code__h6274;
  wire [1 : 0] IF_mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_AR_ETC___d126,
	       IF_mv_vm_xlate_priv_ULE_0b1_AND_mv_vm_xlate_sa_ETC___d128;
  wire IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d109,
       NOT_mv_vm_xlate_dmem_not_imem_05_AND_mv_vm_xla_ETC___d169,
       NOT_mv_vm_xlate_priv_EQ_0b0_6_56_OR_IF_SEL_ARR_ETC___d183,
       SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d31,
       SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d57,
       SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d75,
       SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d52,
       SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d70,
       mv_vm_xlate_dmem_not_imem_OR_NOT_mv_vm_xlate_r_ETC___d117,
       mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_ARR_t_ETC___d118,
       y__h7080;

  // value method mv_vm_xlate
  assign mv_vm_xlate =
	     { IF_mv_vm_xlate_priv_ULE_0b1_AND_mv_vm_xlate_sa_ETC___d128,
	       x__h7236,
	       result_exc_code__h6274,
	       mv_vm_xlate_priv <= 2'b01 && mv_vm_xlate_satp[63:60] == 4'd8 &&
	       SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d75 &&
	       NOT_mv_vm_xlate_priv_EQ_0b0_6_56_OR_IF_SEL_ARR_ETC___d183,
	       x__h7551,
	       IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[65:0] } ;
  assign RDY_mv_vm_xlate = 1'd1 ;

  // action method ma_insert
  assign RDY_ma_insert = 1'd1 ;
  assign CAN_FIRE_ma_insert = 1'd1 ;
  assign WILL_FIRE_ma_insert = EN_ma_insert ;

  // action method ma_flush
  assign RDY_ma_flush = 1'd1 ;
  assign CAN_FIRE_ma_flush = 1'd1 ;
  assign WILL_FIRE_ma_flush = EN_ma_flush ;

  // submodule tlb0_entries
  RegFile #(.addr_width(32'd4),
	    .data_width(32'd167),
	    .lo(4'h0),
	    .hi(4'd15)) tlb0_entries(.CLK(CLK),
				     .ADDR_1(tlb0_entries$ADDR_1),
				     .ADDR_2(tlb0_entries$ADDR_2),
				     .ADDR_3(tlb0_entries$ADDR_3),
				     .ADDR_4(tlb0_entries$ADDR_4),
				     .ADDR_5(tlb0_entries$ADDR_5),
				     .ADDR_IN(tlb0_entries$ADDR_IN),
				     .D_IN(tlb0_entries$D_IN),
				     .WE(tlb0_entries$WE),
				     .D_OUT_1(tlb0_entries$D_OUT_1),
				     .D_OUT_2(),
				     .D_OUT_3(),
				     .D_OUT_4(),
				     .D_OUT_5());

  // submodule tlb1_entries
  RegFile #(.addr_width(32'd3),
	    .data_width(32'd159),
	    .lo(3'h0),
	    .hi(3'd7)) tlb1_entries(.CLK(CLK),
				    .ADDR_1(tlb1_entries$ADDR_1),
				    .ADDR_2(tlb1_entries$ADDR_2),
				    .ADDR_3(tlb1_entries$ADDR_3),
				    .ADDR_4(tlb1_entries$ADDR_4),
				    .ADDR_5(tlb1_entries$ADDR_5),
				    .ADDR_IN(tlb1_entries$ADDR_IN),
				    .D_IN(tlb1_entries$D_IN),
				    .WE(tlb1_entries$WE),
				    .D_OUT_1(tlb1_entries$D_OUT_1),
				    .D_OUT_2(),
				    .D_OUT_3(),
				    .D_OUT_4(),
				    .D_OUT_5());

  // submodule tlb2_entries
  RegFile #(.addr_width(32'd2),
	    .data_width(32'd151),
	    .lo(2'h0),
	    .hi(2'd3)) tlb2_entries(.CLK(CLK),
				    .ADDR_1(tlb2_entries$ADDR_1),
				    .ADDR_2(tlb2_entries$ADDR_2),
				    .ADDR_3(tlb2_entries$ADDR_3),
				    .ADDR_4(tlb2_entries$ADDR_4),
				    .ADDR_5(tlb2_entries$ADDR_5),
				    .ADDR_IN(tlb2_entries$ADDR_IN),
				    .D_IN(tlb2_entries$D_IN),
				    .WE(tlb2_entries$WE),
				    .D_OUT_1(tlb2_entries$D_OUT_1),
				    .D_OUT_2(),
				    .D_OUT_3(),
				    .D_OUT_4(),
				    .D_OUT_5());

  // rule RL_rl_flush
  assign CAN_FIRE_RL_rl_flush = EN_ma_flush ;
  assign WILL_FIRE_RL_rl_flush = EN_ma_flush ;

  // register tlb0_valids_0
  assign tlb0_valids_0$D_IN = !EN_ma_flush ;
  assign tlb0_valids_0$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd0 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_1
  assign tlb0_valids_1$D_IN = !EN_ma_flush ;
  assign tlb0_valids_1$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd1 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_10
  assign tlb0_valids_10$D_IN = !EN_ma_flush ;
  assign tlb0_valids_10$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd10 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_11
  assign tlb0_valids_11$D_IN = !EN_ma_flush ;
  assign tlb0_valids_11$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd11 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_12
  assign tlb0_valids_12$D_IN = !EN_ma_flush ;
  assign tlb0_valids_12$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd12 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_13
  assign tlb0_valids_13$D_IN = !EN_ma_flush ;
  assign tlb0_valids_13$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd13 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_14
  assign tlb0_valids_14$D_IN = !EN_ma_flush ;
  assign tlb0_valids_14$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd14 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_15
  assign tlb0_valids_15$D_IN = !EN_ma_flush ;
  assign tlb0_valids_15$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd15 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_2
  assign tlb0_valids_2$D_IN = !EN_ma_flush ;
  assign tlb0_valids_2$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd2 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_3
  assign tlb0_valids_3$D_IN = !EN_ma_flush ;
  assign tlb0_valids_3$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd3 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_4
  assign tlb0_valids_4$D_IN = !EN_ma_flush ;
  assign tlb0_valids_4$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd4 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_5
  assign tlb0_valids_5$D_IN = !EN_ma_flush ;
  assign tlb0_valids_5$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd5 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_6
  assign tlb0_valids_6$D_IN = !EN_ma_flush ;
  assign tlb0_valids_6$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd6 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_7
  assign tlb0_valids_7$D_IN = !EN_ma_flush ;
  assign tlb0_valids_7$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd7 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_8
  assign tlb0_valids_8$D_IN = !EN_ma_flush ;
  assign tlb0_valids_8$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd8 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb0_valids_9
  assign tlb0_valids_9$D_IN = !EN_ma_flush ;
  assign tlb0_valids_9$EN =
	     EN_ma_insert && ma_insert_vpn[3:0] == 4'd9 &&
	     ma_insert_level == 2'd0 ||
	     EN_ma_flush ;

  // register tlb1_valids_0
  assign tlb1_valids_0$D_IN = !EN_ma_flush ;
  assign tlb1_valids_0$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd0 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb1_valids_1
  assign tlb1_valids_1$D_IN = !EN_ma_flush ;
  assign tlb1_valids_1$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd1 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb1_valids_2
  assign tlb1_valids_2$D_IN = !EN_ma_flush ;
  assign tlb1_valids_2$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd2 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb1_valids_3
  assign tlb1_valids_3$D_IN = !EN_ma_flush ;
  assign tlb1_valids_3$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd3 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb1_valids_4
  assign tlb1_valids_4$D_IN = !EN_ma_flush ;
  assign tlb1_valids_4$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd4 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb1_valids_5
  assign tlb1_valids_5$D_IN = !EN_ma_flush ;
  assign tlb1_valids_5$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd5 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb1_valids_6
  assign tlb1_valids_6$D_IN = !EN_ma_flush ;
  assign tlb1_valids_6$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd6 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb1_valids_7
  assign tlb1_valids_7$D_IN = !EN_ma_flush ;
  assign tlb1_valids_7$EN =
	     EN_ma_insert && ma_insert_vpn[11:9] == 3'd7 &&
	     ma_insert_level == 2'd1 ||
	     EN_ma_flush ;

  // register tlb2_valids_0
  assign tlb2_valids_0$D_IN = !EN_ma_flush ;
  assign tlb2_valids_0$EN =
	     EN_ma_insert && ma_insert_vpn[19:18] == 2'd0 &&
	     ma_insert_level != 2'd0 &&
	     ma_insert_level != 2'd1 ||
	     EN_ma_flush ;

  // register tlb2_valids_1
  assign tlb2_valids_1$D_IN = !EN_ma_flush ;
  assign tlb2_valids_1$EN =
	     EN_ma_insert && ma_insert_vpn[19:18] == 2'd1 &&
	     ma_insert_level != 2'd0 &&
	     ma_insert_level != 2'd1 ||
	     EN_ma_flush ;

  // register tlb2_valids_2
  assign tlb2_valids_2$D_IN = !EN_ma_flush ;
  assign tlb2_valids_2$EN =
	     EN_ma_insert && ma_insert_vpn[19:18] == 2'd2 &&
	     ma_insert_level != 2'd0 &&
	     ma_insert_level != 2'd1 ||
	     EN_ma_flush ;

  // register tlb2_valids_3
  assign tlb2_valids_3$D_IN = !EN_ma_flush ;
  assign tlb2_valids_3$EN =
	     EN_ma_insert && ma_insert_vpn[19:18] == 2'd3 &&
	     ma_insert_level != 2'd0 &&
	     ma_insert_level != 2'd1 ||
	     EN_ma_flush ;

  // submodule tlb0_entries
  assign tlb0_entries$ADDR_1 = mv_vm_xlate_va[15:12] ;
  assign tlb0_entries$ADDR_2 = 4'h0 ;
  assign tlb0_entries$ADDR_3 = 4'h0 ;
  assign tlb0_entries$ADDR_4 = 4'h0 ;
  assign tlb0_entries$ADDR_5 = 4'h0 ;
  assign tlb0_entries$ADDR_IN = ma_insert_vpn[3:0] ;
  assign tlb0_entries$D_IN =
	     { ma_insert_asid,
	       ma_insert_vpn[26:4],
	       ma_insert_pte,
	       ma_insert_pte_pa } ;
  assign tlb0_entries$WE = EN_ma_insert && ma_insert_level == 2'd0 ;

  // submodule tlb1_entries
  assign tlb1_entries$ADDR_1 = mv_vm_xlate_va[23:21] ;
  assign tlb1_entries$ADDR_2 = 3'h0 ;
  assign tlb1_entries$ADDR_3 = 3'h0 ;
  assign tlb1_entries$ADDR_4 = 3'h0 ;
  assign tlb1_entries$ADDR_5 = 3'h0 ;
  assign tlb1_entries$ADDR_IN = ma_insert_vpn[11:9] ;
  assign tlb1_entries$D_IN =
	     { ma_insert_asid,
	       ma_insert_vpn[26:12],
	       ma_insert_pte,
	       ma_insert_pte_pa } ;
  assign tlb1_entries$WE = EN_ma_insert && ma_insert_level == 2'd1 ;

  // submodule tlb2_entries
  assign tlb2_entries$ADDR_1 = mv_vm_xlate_va[31:30] ;
  assign tlb2_entries$ADDR_2 = 2'h0 ;
  assign tlb2_entries$ADDR_3 = 2'h0 ;
  assign tlb2_entries$ADDR_4 = 2'h0 ;
  assign tlb2_entries$ADDR_5 = 2'h0 ;
  assign tlb2_entries$ADDR_IN = ma_insert_vpn[19:18] ;
  assign tlb2_entries$D_IN =
	     { ma_insert_asid,
	       ma_insert_vpn[26:20],
	       ma_insert_pte,
	       ma_insert_pte_pa } ;
  assign tlb2_entries$WE =
	     EN_ma_insert && ma_insert_level != 2'd0 &&
	     ma_insert_level != 2'd1 ;

  // remaining internal signals
  assign IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d109 =
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[67] |
	     y__h7080 ;
  assign IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91 =
	     { x__h6571, 2'd0, x__h6591 } |
	     IF_SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb_ETC___d85 |
	     IF_SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb_ETC___d90 ;
  assign IF_SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb_ETC___d85 =
	     (SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d52 &&
	      tlb1_entries$D_OUT_1[142:128] == mv_vm_xlate_va[38:24]) ?
	       { tlb1_entries$D_OUT_1[127:64],
		 2'd1,
		 tlb1_entries$D_OUT_1[63:0] } :
	       130'd0 ;
  assign IF_SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb_ETC___d90 =
	     (SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d70 &&
	      tlb2_entries$D_OUT_1[134:128] == mv_vm_xlate_va[38:32]) ?
	       { tlb2_entries$D_OUT_1[127:64],
		 2'd2,
		 tlb2_entries$D_OUT_1[63:0] } :
	       130'd0 ;
  assign IF_mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_AR_ETC___d126 =
	     (mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_ARR_t_ETC___d118 ||
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[72] ||
	      !mv_vm_xlate_read_not_write &&
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[73]) ?
	       2'd2 :
	       2'd0 ;
  assign IF_mv_vm_xlate_priv_ULE_0b1_AND_mv_vm_xlate_sa_ETC___d128 =
	     (mv_vm_xlate_priv <= 2'b01 && mv_vm_xlate_satp[63:60] == 4'd8) ?
	       (SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d75 ?
		  IF_mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_AR_ETC___d126 :
		  2'd1) :
	       2'd0 ;
  assign NOT_mv_vm_xlate_dmem_not_imem_05_AND_mv_vm_xla_ETC___d169 =
	     !mv_vm_xlate_dmem_not_imem && mv_vm_xlate_read_not_write &&
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[69] ||
	     mv_vm_xlate_dmem_not_imem && mv_vm_xlate_read_not_write &&
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d109 ||
	     mv_vm_xlate_dmem_not_imem && !mv_vm_xlate_read_not_write &&
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[68] ;
  assign NOT_mv_vm_xlate_priv_EQ_0b0_6_56_OR_IF_SEL_ARR_ETC___d183 =
	     (mv_vm_xlate_priv != 2'b0 ||
	      IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[70]) &&
	     (mv_vm_xlate_priv != 2'b01 ||
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[70] ||
	      mv_vm_xlate_sstatus_SUM) &&
	     NOT_mv_vm_xlate_dmem_not_imem_05_AND_mv_vm_xla_ETC___d169 &&
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[72] &&
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[73] &&
	     !pte___2__h7240[7] &&
	     !mv_vm_xlate_read_not_write ;
  assign SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d31 =
	     SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 &&
	     (tlb0_entries$D_OUT_1[166:151] == mv_vm_xlate_satp[59:44] ||
	      tlb0_entries$D_OUT_1[69]) ;
  assign SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d57 =
	     (SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d31 &&
	      tlb0_entries$D_OUT_1[150:128] == mv_vm_xlate_va[38:16]) |
	     (SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d52 &&
	      tlb1_entries$D_OUT_1[142:128] == mv_vm_xlate_va[38:24]) ;
  assign SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d75 =
	     SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d57 |
	     (SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d70 &&
	      tlb2_entries$D_OUT_1[134:128] == mv_vm_xlate_va[38:32]) ;
  assign SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d52 =
	     SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 &&
	     (tlb1_entries$D_OUT_1[158:143] == mv_vm_xlate_satp[59:44] ||
	      tlb1_entries$D_OUT_1[69]) ;
  assign SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d70 =
	     SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d64 &&
	     (tlb2_entries$D_OUT_1[150:135] == mv_vm_xlate_satp[59:44] ||
	      tlb2_entries$D_OUT_1[69]) ;
  assign _theResult___snd_fst__h6325 =
	     SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d75 ?
	       _theResult___snd_fst__h6963 :
	       mv_vm_xlate_va ;
  assign _theResult___snd_fst__h6963 =
	     (mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_ARR_t_ETC___d118 ||
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[72] ||
	      !mv_vm_xlate_read_not_write &&
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[73]) ?
	       mv_vm_xlate_va :
	       _theResult___fst__h7242 ;
  assign _theResult___snd_fst__h7467 =
	     (!pte___2__h7240[7] && !mv_vm_xlate_read_not_write) ?
	       pte___1__h7545 :
	       pte___2__h7240 ;
  assign _theResult___snd_snd_fst__h6327 =
	     SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d75 ?
	       _theResult___snd_snd_fst__h6965 :
	       IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[129:66] ;
  assign _theResult___snd_snd_fst__h6965 =
	     (mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_ARR_t_ETC___d118 ||
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[72] ||
	      !mv_vm_xlate_read_not_write &&
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[73]) ?
	       IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[129:66] :
	       _theResult___snd_fst__h7467 ;
  assign mv_vm_xlate_dmem_not_imem_OR_NOT_mv_vm_xlate_r_ETC___d117 =
	     (mv_vm_xlate_dmem_not_imem || !mv_vm_xlate_read_not_write ||
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[69]) &&
	     (!mv_vm_xlate_dmem_not_imem || !mv_vm_xlate_read_not_write ||
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d109) &&
	     (!mv_vm_xlate_dmem_not_imem || mv_vm_xlate_read_not_write ||
	      !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[68]) ;
  assign mv_vm_xlate_priv_EQ_0b0_6_AND_NOT_IF_SEL_ARR_t_ETC___d118 =
	     mv_vm_xlate_priv == 2'b0 &&
	     !IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[70] ||
	     mv_vm_xlate_priv == 2'b01 &&
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[70] &&
	     !mv_vm_xlate_sstatus_SUM ||
	     mv_vm_xlate_dmem_not_imem_OR_NOT_mv_vm_xlate_r_ETC___d117 ;
  assign pa___1__h7247 = { 8'd0, x__h7250 } ;
  assign pa___1__h7296 = { 8'd0, x__h7299 } ;
  assign pa___1__h7364 = { 8'd0, x__h7367 } ;
  assign pte___1__h7516 =
	     { IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[129:73],
	       1'd1,
	       IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[71:66] } ;
  assign pte___1__h7545 =
	     { pte___2__h7240[63:8], 1'd1, pte___2__h7240[6:0] } ;
  assign pte___2__h7240 =
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[72] ?
	       IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[129:66] :
	       pte___1__h7516 ;
  assign result_exc_code__h6274 =
	     mv_vm_xlate_dmem_not_imem ?
	       (mv_vm_xlate_read_not_write ? 4'd13 : 4'd15) :
	       4'd12 ;
  assign x__h6571 =
	     (SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d31 &&
	      tlb0_entries$D_OUT_1[150:128] == mv_vm_xlate_va[38:16]) ?
	       tlb0_entries$D_OUT_1[127:64] :
	       64'd0 ;
  assign x__h6591 =
	     (SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d31 &&
	      tlb0_entries$D_OUT_1[150:128] == mv_vm_xlate_va[38:16]) ?
	       tlb0_entries$D_OUT_1[63:0] :
	       64'd0 ;
  assign x__h7236 =
	     (mv_vm_xlate_priv <= 2'b01 && mv_vm_xlate_satp[63:60] == 4'd8) ?
	       _theResult___snd_fst__h6325 :
	       mv_vm_xlate_va ;
  assign x__h7250 =
	     { IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[119:76],
	       mv_vm_xlate_va[11:0] } ;
  assign x__h7299 =
	     { IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[119:85],
	       mv_vm_xlate_va[20:0] } ;
  assign x__h7367 =
	     { IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[119:94],
	       mv_vm_xlate_va[29:0] } ;
  assign x__h7551 =
	     (mv_vm_xlate_priv <= 2'b01 && mv_vm_xlate_satp[63:60] == 4'd8) ?
	       _theResult___snd_snd_fst__h6327 :
	       IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[129:66] ;
  assign y__h7080 =
	     mv_vm_xlate_mstatus_MXR &
	     IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[69] ;
  always@(mv_vm_xlate_va or
	  tlb0_valids_0 or
	  tlb0_valids_1 or
	  tlb0_valids_2 or
	  tlb0_valids_3 or
	  tlb0_valids_4 or
	  tlb0_valids_5 or
	  tlb0_valids_6 or
	  tlb0_valids_7 or
	  tlb0_valids_8 or
	  tlb0_valids_9 or
	  tlb0_valids_10 or
	  tlb0_valids_11 or
	  tlb0_valids_12 or
	  tlb0_valids_13 or tlb0_valids_14 or tlb0_valids_15)
  begin
    case (mv_vm_xlate_va[15:12])
      4'd0:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_0;
      4'd1:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_1;
      4'd2:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_2;
      4'd3:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_3;
      4'd4:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_4;
      4'd5:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_5;
      4'd6:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_6;
      4'd7:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_7;
      4'd8:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_8;
      4'd9:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_9;
      4'd10:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_10;
      4'd11:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_11;
      4'd12:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_12;
      4'd13:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_13;
      4'd14:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_14;
      4'd15:
	  SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_valid_ETC___d24 =
	      tlb0_valids_15;
    endcase
  end
  always@(mv_vm_xlate_va or
	  tlb1_valids_0 or
	  tlb1_valids_1 or
	  tlb1_valids_2 or
	  tlb1_valids_3 or
	  tlb1_valids_4 or tlb1_valids_5 or tlb1_valids_6 or tlb1_valids_7)
  begin
    case (mv_vm_xlate_va[23:21])
      3'd0:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_0;
      3'd1:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_1;
      3'd2:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_2;
      3'd3:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_3;
      3'd4:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_4;
      3'd5:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_5;
      3'd6:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_6;
      3'd7:
	  SEL_ARR_tlb1_valids_0_6_tlb1_valids_1_7_tlb1_v_ETC___d46 =
	      tlb1_valids_7;
    endcase
  end
  always@(mv_vm_xlate_va or
	  tlb2_valids_0 or tlb2_valids_1 or tlb2_valids_2 or tlb2_valids_3)
  begin
    case (mv_vm_xlate_va[31:30])
      2'd0:
	  SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d64 =
	      tlb2_valids_0;
      2'd1:
	  SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d64 =
	      tlb2_valids_1;
      2'd2:
	  SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d64 =
	      tlb2_valids_2;
      2'd3:
	  SEL_ARR_tlb2_valids_0_8_tlb2_valids_1_9_tlb2_v_ETC___d64 =
	      tlb2_valids_3;
    endcase
  end
  always@(IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91 or
	  mv_vm_xlate_va or pa___1__h7247 or pa___1__h7296 or pa___1__h7364)
  begin
    case (IF_SEL_ARR_tlb0_valids_0_tlb0_valids_1_tlb0_va_ETC___d91[65:64])
      2'd0: _theResult___fst__h7242 = pa___1__h7247;
      2'd1: _theResult___fst__h7242 = pa___1__h7296;
      2'd2: _theResult___fst__h7242 = pa___1__h7364;
      2'd3: _theResult___fst__h7242 = mv_vm_xlate_va;
    endcase
  end

  // handling of inlined registers

  always@(posedge CLK)
  begin
    if (tlb0_valids_0$EN)
      tlb0_valids_0 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_0$D_IN;
    if (tlb0_valids_1$EN)
      tlb0_valids_1 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_1$D_IN;
    if (tlb0_valids_10$EN)
      tlb0_valids_10 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_10$D_IN;
    if (tlb0_valids_11$EN)
      tlb0_valids_11 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_11$D_IN;
    if (tlb0_valids_12$EN)
      tlb0_valids_12 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_12$D_IN;
    if (tlb0_valids_13$EN)
      tlb0_valids_13 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_13$D_IN;
    if (tlb0_valids_14$EN)
      tlb0_valids_14 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_14$D_IN;
    if (tlb0_valids_15$EN)
      tlb0_valids_15 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_15$D_IN;
    if (tlb0_valids_2$EN)
      tlb0_valids_2 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_2$D_IN;
    if (tlb0_valids_3$EN)
      tlb0_valids_3 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_3$D_IN;
    if (tlb0_valids_4$EN)
      tlb0_valids_4 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_4$D_IN;
    if (tlb0_valids_5$EN)
      tlb0_valids_5 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_5$D_IN;
    if (tlb0_valids_6$EN)
      tlb0_valids_6 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_6$D_IN;
    if (tlb0_valids_7$EN)
      tlb0_valids_7 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_7$D_IN;
    if (tlb0_valids_8$EN)
      tlb0_valids_8 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_8$D_IN;
    if (tlb0_valids_9$EN)
      tlb0_valids_9 <= `BSV_ASSIGNMENT_DELAY tlb0_valids_9$D_IN;
    if (tlb1_valids_0$EN)
      tlb1_valids_0 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_0$D_IN;
    if (tlb1_valids_1$EN)
      tlb1_valids_1 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_1$D_IN;
    if (tlb1_valids_2$EN)
      tlb1_valids_2 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_2$D_IN;
    if (tlb1_valids_3$EN)
      tlb1_valids_3 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_3$D_IN;
    if (tlb1_valids_4$EN)
      tlb1_valids_4 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_4$D_IN;
    if (tlb1_valids_5$EN)
      tlb1_valids_5 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_5$D_IN;
    if (tlb1_valids_6$EN)
      tlb1_valids_6 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_6$D_IN;
    if (tlb1_valids_7$EN)
      tlb1_valids_7 <= `BSV_ASSIGNMENT_DELAY tlb1_valids_7$D_IN;
    if (tlb2_valids_0$EN)
      tlb2_valids_0 <= `BSV_ASSIGNMENT_DELAY tlb2_valids_0$D_IN;
    if (tlb2_valids_1$EN)
      tlb2_valids_1 <= `BSV_ASSIGNMENT_DELAY tlb2_valids_1$D_IN;
    if (tlb2_valids_2$EN)
      tlb2_valids_2 <= `BSV_ASSIGNMENT_DELAY tlb2_valids_2$D_IN;
    if (tlb2_valids_3$EN)
      tlb2_valids_3 <= `BSV_ASSIGNMENT_DELAY tlb2_valids_3$D_IN;
  end

  // synopsys translate_off
  `ifdef BSV_NO_INITIAL_BLOCKS
  `else // not BSV_NO_INITIAL_BLOCKS
  initial
  begin
    tlb0_valids_0 = 1'h0;
    tlb0_valids_1 = 1'h0;
    tlb0_valids_10 = 1'h0;
    tlb0_valids_11 = 1'h0;
    tlb0_valids_12 = 1'h0;
    tlb0_valids_13 = 1'h0;
    tlb0_valids_14 = 1'h0;
    tlb0_valids_15 = 1'h0;
    tlb0_valids_2 = 1'h0;
    tlb0_valids_3 = 1'h0;
    tlb0_valids_4 = 1'h0;
    tlb0_valids_5 = 1'h0;
    tlb0_valids_6 = 1'h0;
    tlb0_valids_7 = 1'h0;
    tlb0_valids_8 = 1'h0;
    tlb0_valids_9 = 1'h0;
    tlb1_valids_0 = 1'h0;
    tlb1_valids_1 = 1'h0;
    tlb1_valids_2 = 1'h0;
    tlb1_valids_3 = 1'h0;
    tlb1_valids_4 = 1'h0;
    tlb1_valids_5 = 1'h0;
    tlb1_valids_6 = 1'h0;
    tlb1_valids_7 = 1'h0;
    tlb2_valids_0 = 1'h0;
    tlb2_valids_1 = 1'h0;
    tlb2_valids_2 = 1'h0;
    tlb2_valids_3 = 1'h0;
  end
  `endif // BSV_NO_INITIAL_BLOCKS
  // synopsys translate_on
endmodule  // mkTLB

