//
// Generated by Bluespec Compiler, version 2019.05.beta2 (build a88bf40db, 2019-05-24)
//
//
//
//
// Ports:
// Name                         I/O  size props
// m_clint_addr_base              O    32 const
// m_clint_addr_size              O    32 const
// m_clint_addr_lim               O    32 const
// m_plic_addr_base               O    32 const
// m_plic_addr_size               O    32 const
// m_plic_addr_lim                O    32 const
// m_uart0_addr_base              O    32 const
// m_uart0_addr_size              O    32 const
// m_uart0_addr_lim               O    32 const
// m_gpio0_addr_base              O    32 const
// m_gpio0_addr_size              O    32 const
// m_gpio0_addr_lim               O    32 const
// m_boot_rom_addr_base           O    32 const
// m_boot_rom_addr_size           O    32 const
// m_boot_rom_addr_lim            O    32 const
// m_mem0_controller_addr_base    O    32 const
// m_mem0_controller_addr_size    O    32 const
// m_mem0_controller_addr_lim     O    32 const
// m_itcm_addr_base               O    32 const
// m_itcm_addr_size               O    32 const
// m_itcm_addr_lim                O    32 const
// m_is_itcm_addr                 O     1
// m_dtcm_addr_base               O    32 const
// m_dtcm_addr_size               O    32 const
// m_dtcm_addr_lim                O    32 const
// m_is_dtcm_addr                 O     1
// m_is_tcm_addr                  O     1
// m_is_mem_addr                  O     1
// m_is_IO_addr                   O     1
// m_is_nmio_addr                 O     1
// m_is_clint_addr                O     1
// m_pc_reset_value               O    64 const
// m_mtvec_reset_value            O    64 const
// m_nmivec_reset_value           O    64 const
// CLK                            I     1 unused
// RST_N                          I     1 unused
// m_is_itcm_addr_addr            I    32
// m_is_dtcm_addr_addr            I    32
// m_is_tcm_addr_addr             I    32
// m_is_mem_addr_addr             I    32
// m_is_IO_addr_addr              I    32
// m_is_nmio_addr_addr            I    32
// m_is_clint_addr_addr           I    32
//
// Combinational paths from inputs to outputs:
//   m_is_itcm_addr_addr -> m_is_itcm_addr
//   m_is_dtcm_addr_addr -> m_is_dtcm_addr
//   m_is_tcm_addr_addr -> m_is_tcm_addr
//   m_is_mem_addr_addr -> m_is_mem_addr
//   m_is_IO_addr_addr -> m_is_IO_addr
//   m_is_nmio_addr_addr -> m_is_nmio_addr
//   m_is_clint_addr_addr -> m_is_clint_addr
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

module mkSoC_Map(CLK,
		 RST_N,

		 m_clint_addr_base,

		 m_clint_addr_size,

		 m_clint_addr_lim,

		 m_plic_addr_base,

		 m_plic_addr_size,

		 m_plic_addr_lim,

		 m_uart0_addr_base,

		 m_uart0_addr_size,

		 m_uart0_addr_lim,

		 m_gpio0_addr_base,

		 m_gpio0_addr_size,

		 m_gpio0_addr_lim,

		 m_boot_rom_addr_base,

		 m_boot_rom_addr_size,

		 m_boot_rom_addr_lim,

		 m_mem0_controller_addr_base,

		 m_mem0_controller_addr_size,

		 m_mem0_controller_addr_lim,

		 m_itcm_addr_base,

		 m_itcm_addr_size,

		 m_itcm_addr_lim,

		 m_is_itcm_addr_addr,
		 m_is_itcm_addr,

		 m_dtcm_addr_base,

		 m_dtcm_addr_size,

		 m_dtcm_addr_lim,

		 m_is_dtcm_addr_addr,
		 m_is_dtcm_addr,

		 m_is_tcm_addr_addr,
		 m_is_tcm_addr,

		 m_is_mem_addr_addr,
		 m_is_mem_addr,

		 m_is_IO_addr_addr,
		 m_is_IO_addr,

		 m_is_nmio_addr_addr,
		 m_is_nmio_addr,

		 m_is_clint_addr_addr,
		 m_is_clint_addr,

		 m_pc_reset_value,

		 m_mtvec_reset_value,

		 m_nmivec_reset_value);
  input  CLK;
  input  RST_N;

  // value method m_clint_addr_base
  output [31 : 0] m_clint_addr_base;

  // value method m_clint_addr_size
  output [31 : 0] m_clint_addr_size;

  // value method m_clint_addr_lim
  output [31 : 0] m_clint_addr_lim;

  // value method m_plic_addr_base
  output [31 : 0] m_plic_addr_base;

  // value method m_plic_addr_size
  output [31 : 0] m_plic_addr_size;

  // value method m_plic_addr_lim
  output [31 : 0] m_plic_addr_lim;

  // value method m_uart0_addr_base
  output [31 : 0] m_uart0_addr_base;

  // value method m_uart0_addr_size
  output [31 : 0] m_uart0_addr_size;

  // value method m_uart0_addr_lim
  output [31 : 0] m_uart0_addr_lim;

  // value method m_gpio0_addr_base
  output [31 : 0] m_gpio0_addr_base;

  // value method m_gpio0_addr_size
  output [31 : 0] m_gpio0_addr_size;

  // value method m_gpio0_addr_lim
  output [31 : 0] m_gpio0_addr_lim;

  // value method m_boot_rom_addr_base
  output [31 : 0] m_boot_rom_addr_base;

  // value method m_boot_rom_addr_size
  output [31 : 0] m_boot_rom_addr_size;

  // value method m_boot_rom_addr_lim
  output [31 : 0] m_boot_rom_addr_lim;

  // value method m_mem0_controller_addr_base
  output [31 : 0] m_mem0_controller_addr_base;

  // value method m_mem0_controller_addr_size
  output [31 : 0] m_mem0_controller_addr_size;

  // value method m_mem0_controller_addr_lim
  output [31 : 0] m_mem0_controller_addr_lim;

  // value method m_itcm_addr_base
  output [31 : 0] m_itcm_addr_base;

  // value method m_itcm_addr_size
  output [31 : 0] m_itcm_addr_size;

  // value method m_itcm_addr_lim
  output [31 : 0] m_itcm_addr_lim;

  // value method m_is_itcm_addr
  input  [31 : 0] m_is_itcm_addr_addr;
  output m_is_itcm_addr;

  // value method m_dtcm_addr_base
  output [31 : 0] m_dtcm_addr_base;

  // value method m_dtcm_addr_size
  output [31 : 0] m_dtcm_addr_size;

  // value method m_dtcm_addr_lim
  output [31 : 0] m_dtcm_addr_lim;

  // value method m_is_dtcm_addr
  input  [31 : 0] m_is_dtcm_addr_addr;
  output m_is_dtcm_addr;

  // value method m_is_tcm_addr
  input  [31 : 0] m_is_tcm_addr_addr;
  output m_is_tcm_addr;

  // value method m_is_mem_addr
  input  [31 : 0] m_is_mem_addr_addr;
  output m_is_mem_addr;

  // value method m_is_IO_addr
  input  [31 : 0] m_is_IO_addr_addr;
  output m_is_IO_addr;

  // value method m_is_nmio_addr
  input  [31 : 0] m_is_nmio_addr_addr;
  output m_is_nmio_addr;

  // value method m_is_clint_addr
  input  [31 : 0] m_is_clint_addr_addr;
  output m_is_clint_addr;

  // value method m_pc_reset_value
  output [63 : 0] m_pc_reset_value;

  // value method m_mtvec_reset_value
  output [63 : 0] m_mtvec_reset_value;

  // value method m_nmivec_reset_value
  output [63 : 0] m_nmivec_reset_value;

  // signals for module outputs
  wire [63 : 0] m_mtvec_reset_value, m_nmivec_reset_value, m_pc_reset_value;
  wire [31 : 0] m_boot_rom_addr_base,
		m_boot_rom_addr_lim,
		m_boot_rom_addr_size,
		m_clint_addr_base,
		m_clint_addr_lim,
		m_clint_addr_size,
		m_dtcm_addr_base,
		m_dtcm_addr_lim,
		m_dtcm_addr_size,
		m_gpio0_addr_base,
		m_gpio0_addr_lim,
		m_gpio0_addr_size,
		m_itcm_addr_base,
		m_itcm_addr_lim,
		m_itcm_addr_size,
		m_mem0_controller_addr_base,
		m_mem0_controller_addr_lim,
		m_mem0_controller_addr_size,
		m_plic_addr_base,
		m_plic_addr_lim,
		m_plic_addr_size,
		m_uart0_addr_base,
		m_uart0_addr_lim,
		m_uart0_addr_size;
  wire m_is_IO_addr,
       m_is_clint_addr,
       m_is_dtcm_addr,
       m_is_itcm_addr,
       m_is_mem_addr,
       m_is_nmio_addr,
       m_is_tcm_addr;

  // value method m_clint_addr_base
  assign m_clint_addr_base = 32'h02000000 ;

  // value method m_clint_addr_size
  assign m_clint_addr_size = 32'h0000C000 ;

  // value method m_clint_addr_lim
  assign m_clint_addr_lim = 32'd33603584 ;

  // value method m_plic_addr_base
  assign m_plic_addr_base = 32'h0C000000 ;

  // value method m_plic_addr_size
  assign m_plic_addr_size = 32'h00400000 ;

  // value method m_plic_addr_lim
  assign m_plic_addr_lim = 32'd205520896 ;

  // value method m_uart0_addr_base
  assign m_uart0_addr_base = 32'h62300000 ;

  // value method m_uart0_addr_size
  assign m_uart0_addr_size = 32'h00000080 ;

  // value method m_uart0_addr_lim
  assign m_uart0_addr_lim = 32'd1647313024 ;

  // value method m_gpio0_addr_base
  assign m_gpio0_addr_base = 32'h6FFF0000 ;

  // value method m_gpio0_addr_size
  assign m_gpio0_addr_size = 32'h00000080 ;

  // value method m_gpio0_addr_lim
  assign m_gpio0_addr_lim = 32'd1878982784 ;

  // value method m_boot_rom_addr_base
  assign m_boot_rom_addr_base = 32'h70000000 ;

  // value method m_boot_rom_addr_size
  assign m_boot_rom_addr_size = 32'h00001000 ;

  // value method m_boot_rom_addr_lim
  assign m_boot_rom_addr_lim = 32'd1879052288 ;

  // value method m_mem0_controller_addr_base
  assign m_mem0_controller_addr_base = 32'h90000000 ;

  // value method m_mem0_controller_addr_size
  assign m_mem0_controller_addr_size = 32'h10000000 ;

  // value method m_mem0_controller_addr_lim
  assign m_mem0_controller_addr_lim = 32'hA0000000 ;

  // value method m_itcm_addr_base
  assign m_itcm_addr_base = 32'hC0000000 ;

  // value method m_itcm_addr_size
  assign m_itcm_addr_size = 32'd131072 ;

  // value method m_itcm_addr_lim
  assign m_itcm_addr_lim = 32'hC0020000 ;

  // value method m_is_itcm_addr
  assign m_is_itcm_addr =
	     m_is_itcm_addr_addr >= 32'hC0000000 &&
	     m_is_itcm_addr_addr < 32'hC0020000 ;

  // value method m_dtcm_addr_base
  assign m_dtcm_addr_base = 32'hC8000000 ;

  // value method m_dtcm_addr_size
  assign m_dtcm_addr_size = 32'd131072 ;

  // value method m_dtcm_addr_lim
  assign m_dtcm_addr_lim = 32'hC8020000 ;

  // value method m_is_dtcm_addr
  assign m_is_dtcm_addr =
	     m_is_dtcm_addr_addr >= 32'hC8000000 &&
	     m_is_dtcm_addr_addr < 32'hC8020000 ;

  // value method m_is_tcm_addr
  assign m_is_tcm_addr =
	     m_is_tcm_addr_addr >= 32'hC0000000 &&
	     m_is_tcm_addr_addr < 32'hC0020000 ||
	     m_is_tcm_addr_addr >= 32'hC8000000 &&
	     m_is_tcm_addr_addr < 32'hC8020000 ;

  // value method m_is_mem_addr
  assign m_is_mem_addr =
	     m_is_mem_addr_addr >= 32'h70000000 &&
	     m_is_mem_addr_addr < 32'd1879052288 ||
	     m_is_mem_addr_addr >= 32'h90000000 &&
	     m_is_mem_addr_addr < 32'hA0000000 ||
	     m_is_mem_addr_addr >= 32'hC0000000 &&
	     m_is_mem_addr_addr < 32'hC0020000 ||
	     m_is_mem_addr_addr >= 32'hC8000000 &&
	     m_is_mem_addr_addr < 32'hC8020000 ;

  // value method m_is_IO_addr
  assign m_is_IO_addr =
	     m_is_IO_addr_addr >= 32'h02000000 &&
	     m_is_IO_addr_addr < 32'd33603584 ||
	     m_is_IO_addr_addr >= 32'h0C000000 &&
	     m_is_IO_addr_addr < 32'd205520896 ||
	     m_is_IO_addr_addr >= 32'h62300000 &&
	     m_is_IO_addr_addr < 32'd1647313024 ;

  // value method m_is_nmio_addr
  assign m_is_nmio_addr =
	     m_is_nmio_addr_addr >= 32'h02000000 &&
	     m_is_nmio_addr_addr < 32'd33603584 ||
	     m_is_nmio_addr_addr >= 32'h0C000000 &&
	     m_is_nmio_addr_addr < 32'd205520896 ;

  // value method m_is_clint_addr
  assign m_is_clint_addr =
	     m_is_clint_addr_addr >= 32'h02000000 &&
	     m_is_clint_addr_addr < 32'd33603584 ;

  // value method m_pc_reset_value
  assign m_pc_reset_value = 64'h00000000C0000000 ;

  // value method m_mtvec_reset_value
  assign m_mtvec_reset_value = 64'h0000000000001000 ;

  // value method m_nmivec_reset_value
  assign m_nmivec_reset_value = 64'hAAAAAAAAAAAAAAAA ;
endmodule  // mkSoC_Map
