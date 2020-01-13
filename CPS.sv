//============================================================================
//  FPGAGen port to MiSTer
//  Copyright (c) 2017,2018 Sorgelig
//
//  YM2612 implementation by Jose Tejada Gomez. Twitter: @topapate
//  Original Genesis code: Copyright (c) 2010-2013 Gregory Estrade (greg@torlus.com) 
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================

module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [44:0] HPS_BUS,
	
	output [3:0]  sconf,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	output  [7:0] VIDEO_ARX,
	output  [7:0] VIDEO_ARY,

	input			PATT_CLK_IN,
	
	input  [11:0] PATT_X_IN,
	input  [11:0] PATT_Y_IN,
	
	input			  PATT_HS_IN,
	input			  PATT_VS_IN,
	input			  PATT_DE_IN,
	
	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output [1:0]  VGA_SL,
	
	input			  BTN_USER,

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	// I/O board button press simulation (active high)
	// b[1]: user button
	// b[0]: osd button
	output  [1:0] BUTTONS,	

	output signed [15:0] AUDIO_L,
	output signed [15:0] AUDIO_R,
	output        AUDIO_S, // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)
	input         TAPE_IN,

	//ADC
	inout   [3:0] ADC_BUS,
	
	// SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,
	
	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..6 - USR2..USR6
	// Set USER_OUT to 1 to read from USER_IN.
	input   [6:0] USER_IN,
	output  [6:0] USER_OUT,

	input         OSD_STATUS	
);

assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
//assign {SDRAM_DQ, SDRAM_A, SDRAM_BA, SDRAM_CLK, SDRAM_CKE, SDRAM_DQML, SDRAM_DQMH, SDRAM_nWE, SDRAM_nCAS, SDRAM_nRAS, SDRAM_nCS} = 'Z;

//assign VIDEO_ARX = status[9] ? 8'd16 : 8'd4;
//assign VIDEO_ARY = status[9] ? 8'd9  : 8'd3;
assign VIDEO_ARX = 8'd4;
assign VIDEO_ARY = 8'd3;

assign LED_DISK  = 0;
assign LED_POWER = 0;
assign LED_USER  = ioctl_download;

`include "build_id.v"
localparam CONF_STR = {
	"CPS;;",
	"-;",
	"F,BINROM ;",
	"-;",
	"O69,Layer,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15;",
	"-;",
	"-;",
	"-;",
	"O13,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%,CRT 75%;",
	"-;",
	"O4,Swap joysticks,No,Yes;",
	"O5,6 buttons mode,No,Yes;",
	"-;",
	"R0,Reset;",
	"J1,1,2,3,4,coin,start,service,test;",
	"V,v1.51.",`BUILD_DATE
};

wire [3:0] layer = status[9:6];

wire [31:0] status;
wire  [1:0] buttons;
wire [15:0] joystick_0;
wire [15:0] joystick_1;
wire        ioctl_download;
wire        ioctl_wr;
wire [24:0] ioctl_addr;
wire [15:0] ioctl_data;
wire  [7:0] ioctl_index;
reg         ioctl_wait;
wire        forced_scandoubler;
wire [10:0] ps2_key;

hps_io #(.STRLEN($size(CONF_STR)>>3), .PS2DIV(1000), .WIDE(1)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.conf_str(CONF_STR),
	.joystick_0(joystick_0),
	.joystick_1(joystick_1),
	.buttons(buttons),
	.forced_scandoubler(forced_scandoubler),

	.status(status),
	.status_in(status),
	.status_set(region_set),

	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_data),
	.ioctl_wait(ioctl_wait),

	.ps2_key(ps2_key)
);


///////////////////////////////////////////////////
wire clk_sys, clk_ntsc, pll_locked;

pll pll
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(clk_sys),
	.outclk_1(clk_ntsc),
	.locked(pll_locked)
);

///////////////////////////////////////////////////
wire [3:0] r, g, b;
wire vs,hs;
wire ce_pix;
wire hblank, vblank;
wire interlace;


wire [12:0] audio_l, audio_r;


(*keep*) wire reset = RESET | status[0] | !BTN_USER | ioctl_download;

reg [19:0] CLK_DIV;
always @(posedge clk_sys or posedge reset)
if (reset) CLK_DIV <= 20'd0;
else CLK_DIV <= CLK_DIV + 1;


reg fx68k_enPhi1;
reg fx68k_enPhi2;

reg as_n_1;
wire as_n_falling = as_n_1 && !fx68k_as_n;

reg [1:0] clkdiv;
always @(posedge clk_sys) begin
	clkdiv <= clkdiv + 2'd1;
	
	as_n_1 <= fx68k_as_n;
	
	fx68k_enPhi1 <= 1'b0;
	fx68k_enPhi2 <= 1'b0;
	
	//if (clkdiv==0) begin
		//fx68k_enPhi1 <= 1'b1;
	//end

	if (clkdiv==1) begin
		fx68k_enPhi1 <= 1'b1;
	end
	
	//if (clkdiv==2) begin
		//fx68k_enPhi2 <= 1'b1;
	//end
	
	if (clkdiv==3) begin
		fx68k_enPhi2 <= 1'b1;
	end
end


(*keep*) wire fx68k_clk = clk_sys;
(*keep*) wire fx68k_rst = reset;

(*keep*) wire fx68k_rw;
(*keep*) wire fx68k_as_n;
(*keep*) wire fx68k_lds_n;
(*keep*) wire fx68k_uds_n;
(*keep*) wire fx68k_e;
(*keep*) wire fx68k_vma_n;

(*keep*) wire fx68k_berr_n = 1'b1;

(*keep*) wire [2:0] fx68k_fc;

//wire fx68k_vpa_n = 1'b1;							// vpa_n tied High, means it's NOT using auto-vector for the interrupt. ElectronAsh.
(*keep*) wire fx68k_vpa_n = !(fx68k_fc==7);	// vpa_n driven Low when the interrupt is being serviced, which will trigger Auto-vectored interrupt.


(*keep*) wire [23:1] fx68k_addr;										// WORD address. LSB not used! (like on the real 68000).

(*keep*) wire [23:0] fx68k_byte_addr = {fx68k_addr, 1'b0};	// LSB added, and tied low. Makes it easier for SignalTap debugging, and address decoding.

(*keep*) wire [15:0] fx68k_di;
(*keep*) wire [15:0] fx68k_do;

(*keep*) wire fx68k_dtack_n = (CPU_ROM_CS /*| CPU_RAM_CS*/) ? !sdram_68k_ready : 1'b0;


(*keep*) wire fx68k_ipl_n_2 = !HBLANK_INT;
(*keep*) wire fx68k_ipl_n_1 = !VBLANK_INT;
(*keep*) wire fx68k_ipl_n_0 = 1'b1;


(*keep*) wire fx68k_bg_n;

(*keep*) wire fx68k_br_n = 1'b1;			// Bus Request.
(*keep*) wire fx68k_bgack_n = 1'b1;		// Bus Grant Acknowledge.

(*keep*) wire fx68k_write_pulse = !fx68k_as_n && !fx68k_rw && fx68k_enPhi1 && (!fx68k_uds_n | !fx68k_lds_n);


fx68k fx68k_inst
(
	.clk( fx68k_clk ) ,			// input  clk
	
	.extReset( fx68k_rst ) ,	// input  extReset
	.pwrUp( fx68k_rst ) ,		// input  pwrUp
	
	.enPhi1( fx68k_enPhi1 ) ,	// input  enPhi1
	.enPhi2( fx68k_enPhi2 ) ,	// input  enPhi2
	
	.eRWn( fx68k_rw ) ,			// output  eRWn
	.ASn( fx68k_as_n ) ,			// output  ASn
	.LDSn( fx68k_lds_n ) ,		// output  LDSn
	.UDSn( fx68k_uds_n ) ,		// output  UDSn
	.E( fx68k_e ) ,				// output  E
	.VMAn( fx68k_vma_n ) ,		// output  VMAn
	
	.FC0( fx68k_fc[0] ) ,		// output  FC0
	.FC1( fx68k_fc[1] ) ,		// output  FC1
	.FC2( fx68k_fc[2] ) ,		// output  FC2
	
	.oRESETn( ) ,					// output  oRESETn
	.oHALTEDn( ) ,					// output  oHALTEDn
	
	.DTACKn( fx68k_dtack_n ) ,	// input  DTACKn
	
	.VPAn( fx68k_vpa_n ) ,		// input  VPAn - Tied HIGH on the real Jag.
	
	.BERRn( fx68k_berr_n ) ,	// input  BERRn - Tied HIGH on the real Jag.
	
	.BRn( fx68k_br_n ) ,			// input  BRn
	.BGn( fx68k_bg_n ) ,			// output  BGn
	.BGACKn( fx68k_bgack_n ) ,	// input  BGACKn
	
	.IPL0n( fx68k_ipl_n_0 ) ,	// input  IPL0n
	.IPL1n( fx68k_ipl_n_1 ) ,	// input  IPL1n
	.IPL2n( fx68k_ipl_n_2 ) ,	// input  IPL2n
	
	.iEdb( fx68k_di ) ,			// input [15:0] iEdb
	.oEdb( fx68k_do ) ,		// output [15:0] oEdb
	
	.eab( fx68k_addr ) 		// output [23:1] eab
);


/*
cpu_rom_hi	cpu_rom_hi_inst (
	.clock ( clk_sys ),
	.address ( fx68k_addr[17:1] ),
	.q ( ROM_HI_DO )
);
wire [7:0] ROM_HI_DO;


cpu_rom_lo	cpu_rom_lo_inst (
	.clock ( clk_sys ),
	.address ( fx68k_addr[17:1] ),
	.q ( ROM_LO_DO )
);
wire [7:0] ROM_LO_DO;
*/

sdram sdram
(
	.init(~pll_locked),
	
	//.clk( clk_ram ),
	.clk( clk_sys ),				// Apparently don't need the phase shift any more? DDIO is used to generate SDRAM_CLK instead.

	.SDRAM_DQ( SDRAM_DQ ),		// 16 bit bidirectional data bus
	.SDRAM_A( SDRAM_A) ,			// 13 bit multiplexed address bus
	.SDRAM_DQML( SDRAM_DQML ) ,// two byte masks
	.SDRAM_DQMH( SDRAM_DQMH ) ,// 
	.SDRAM_BA( SDRAM_BA ),		// two banks
	.SDRAM_nCS( SDRAM_nCS ),	// a single chip select
	.SDRAM_nWE( SDRAM_nWE ),	// write enable
	.SDRAM_nRAS( SDRAM_nRAS ),	// row address select
	.SDRAM_nCAS( SDRAM_nCAS ),	// columns address select
	.SDRAM_CKE( SDRAM_CKE ),	// clock enable
	.SDRAM_CLK( SDRAM_CLK ),	// clock for chip
	
	// Port 1.
	.ch1_addr( ioctl_addr[24:1] ),	// WORD address!! [26:1]
	.ch1_dout(  ),							// output [63:0]
	.ch1_rnw( 1'b0 ),
	.ch1_din( {ioctl_data[7:0],ioctl_data[15:8]} ),	// input [15:0]
	.ch1_req( ioctl_wr ),	
	.ch1_ready( rom_wrack ),	
	
	// Port 2.
	.ch2_addr(  ),		// WORD address!! [26:1]
	.ch2_dout(  ),		// output [31:0]
	.ch2_rnw(  ),
	.ch2_din(  ),		// input [15:0]
	.ch2_req( 1'b0 ),
	.ch2_ready(  ),
	
	// Port 3.
	.ch3_addr( sdram_addr ),	// WORD address!! [26:1]
	.ch3_dout( sdram_dout ),	// output [15:0]
	.ch3_rnw( sdram_rd ),
	.ch3_din( sdram_din ),		// input [15:0]
	.ch3_req( sdram_68k_req ),
	.ch3_udqm_n( 1'b0 ),
	.ch3_ldqm_n( 1'b0 ),
	.ch3_ready( ch3_ready ),
);

(*keep*) wire [15:0] sdram_din = fx68k_do;
(*keep*) wire [15:0] sdram_dout;

wire ch3_ready;

reg fx68k_as_n_old;
wire fx68k_as_n_falling = fx68k_as_n_old & !fx68k_as_n;

(*keep*) wire sdram_68k_req = CPU_ROM_CS && fx68k_as_n_falling && fx68k_rw;

(*noprune*) reg sdram_68k_ready;

always @(posedge clk_sys or posedge reset)
if (reset) begin
	sdram_68k_ready <= 1'b0;
end
else begin
	fx68k_as_n_old <= fx68k_as_n;

	if (sdram_68k_req) sdram_68k_ready <= 1'b0;
	else if (ch3_ready) sdram_68k_ready <= 1'b1;
end

								
wire [20:1] sdram_addr = /*(CPU_RAM_CS) ? {5'b10000, fx68k_addr[15:1]} :*/	// Force 68000 RAM to map to 0x100000-0x10FFFF (WORD address!) in SDRAM.
													    fx68k_addr[20:1];					// Allow reading of 68000 ROM.

//wire sdram_wrh = CPU_RAM_CS && !fx68k_uds_n && !fx68k_as_n && !fx68k_rw && fx68k_enPhi1;
//wire sdram_wrl = CPU_RAM_CS && !fx68k_lds_n && !fx68k_as_n && !fx68k_rw && fx68k_enPhi1;

wire sdram_rd = !fx68k_as_n && fx68k_rw && (CPU_ROM_CS/* | CPU_RAM_CS*/);


//wire  [1:0] sdram_be = 2'b11;


// BYTE Addresses for SDRAM...
//
// 0x000000-0x0FFFFF = 68000 Program ROM. (1MB)
// 0x100000-0x10FFFF = 68000 RAM.         (64KB)
// 0x110000-0x11FFFF = Z80 Program ROM.   (64KB)
// 0x120000-0x15FFFF = PCM Sample ROM.    (256KB)
// 0x160000-onwards  = Tile ROMs.         (quite a lot)
//


//assign rom_wrack = 1'b1;
//assign rom_rdack = 1'b1;


(*keep*) wire CPU_RAM_WREN = CPU_RAM_CS && fx68k_write_pulse;
(*keep*) wire [1:0] CPU_RAM_BE = {!fx68k_uds_n, !fx68k_lds_n};

cpu_ram	cpu_ram_inst (
	.clock ( clk_sys ),
	.address ( fx68k_addr[15:1] ),
	
	.data ( fx68k_do ),
	.wren ( CPU_RAM_WREN ),
	.byteena( CPU_RAM_BE ),
	
	.q ( CPU_RAM_DO )
);
(*keep*) wire [15:0] CPU_RAM_DO;


(*noprune*) reg [10:0] KEEP_REG;
always @(posedge clk_sys) begin
	KEEP_REG <= {CPU_ROM_CS, PLAYER_CS, SERV_CS, NOPR_CS, COINCTRL_CS, CPSA_CS, CPSB_CS, SOUNDCMD_CS, SOUNDFADE_CS, GFX_RAM_CS, CPU_RAM_CS};
end

// Note: The LSB bit of fx68k_byte_addr is tied Low, so the comparisons for some of these ODD addresses will be ignored. ElectronAsh.

(*keep*) wire CPU_ROM_CS	= (fx68k_byte_addr>=24'h000000 && fx68k_byte_addr<=24'h3FFFFF);
(*keep*) wire PLAYER_CS		= (fx68k_byte_addr>=24'h800000 && fx68k_byte_addr<=24'h800007);
(*keep*) wire SERV_CS		= (fx68k_byte_addr>=24'h800018 && fx68k_byte_addr<=24'h800019);
(*keep*) wire DIPA_CS		= (fx68k_byte_addr>=24'h80001A && fx68k_byte_addr<=24'h80001B);
(*keep*) wire DIPB_CS		= (fx68k_byte_addr>=24'h80001C && fx68k_byte_addr<=24'h80001D);
(*keep*) wire DIPC_CS		= (fx68k_byte_addr>=24'h80001E && fx68k_byte_addr<=24'h80001F);
(*keep*) wire NOPR_CS		= (fx68k_byte_addr>=24'h800020 && fx68k_byte_addr<=24'h800021);
(*keep*) wire COINCTRL_CS	= (fx68k_byte_addr>=24'h800030 && fx68k_byte_addr<=24'h800037);
(*keep*) wire CPSA_CS		= (fx68k_byte_addr>=24'h800100 && fx68k_byte_addr<=24'h80013F);
(*keep*) wire CPSB_CS		= (fx68k_byte_addr>=24'h800140 && fx68k_byte_addr<=24'h80017F);
(*keep*) wire SOUNDCMD_CS	= (fx68k_byte_addr>=24'h800180 && fx68k_byte_addr<=24'h800187);
(*keep*) wire SOUNDFADE_CS	= (fx68k_byte_addr>=24'h800188 && fx68k_byte_addr<=24'h80018F);
(*keep*) wire GFX_RAM_CS	= (fx68k_byte_addr>=24'h900000 && fx68k_byte_addr<=24'h92FFFF);
(*keep*) wire CPU_RAM_CS 	= (fx68k_byte_addr>=24'hFF0000 && fx68k_byte_addr<=24'hFFFFFF);


// CPS-A Registers...
//
// All regs are write-only, apparently.
wire CPS_A_OBJ_CS = 			(fx68k_byte_addr>=24'h800100 && fx68k_byte_addr<=24'h800101);
wire CPS_A_SCR1_CS = 		(fx68k_byte_addr>=24'h800102 && fx68k_byte_addr<=24'h800103);
wire CPS_A_SCR2_CS = 		(fx68k_byte_addr>=24'h800104 && fx68k_byte_addr<=24'h800105);
wire CPS_A_SCR3_CS = 		(fx68k_byte_addr>=24'h800106 && fx68k_byte_addr<=24'h800107);
wire CPS_A_RSCR_CS =			(fx68k_byte_addr>=24'h800108 && fx68k_byte_addr<=24'h800109);
wire CPS_A_PAL_CS =			(fx68k_byte_addr>=24'h80010a && fx68k_byte_addr<=24'h80010b);
wire CPS_A_SCR1_X_CS =		(fx68k_byte_addr>=24'h80010c && fx68k_byte_addr<=24'h80010d);
wire CPS_A_SCR1_Y_CS =		(fx68k_byte_addr>=24'h80010e && fx68k_byte_addr<=24'h80010f);
wire CPS_A_SCR2_X_CS =		(fx68k_byte_addr>=24'h800110 && fx68k_byte_addr<=24'h800111);
wire CPS_A_SCR2_Y_CS =		(fx68k_byte_addr>=24'h800112 && fx68k_byte_addr<=24'h800113);
wire CPS_A_SCR3_X_CS =		(fx68k_byte_addr>=24'h800114 && fx68k_byte_addr<=24'h800115);
wire CPS_A_SCR3_Y_CS =		(fx68k_byte_addr>=24'h800116 && fx68k_byte_addr<=24'h800117);
wire CPS_A_STAR1_X_CS =		(fx68k_byte_addr>=24'h800118 && fx68k_byte_addr<=24'h800119);
wire CPS_A_STAR1_Y_CS =		(fx68k_byte_addr>=24'h80011a && fx68k_byte_addr<=24'h80011b);
wire CPS_A_STAR2_X_CS =		(fx68k_byte_addr>=24'h80011c && fx68k_byte_addr<=24'h80011d);
wire CPS_A_STAR2_Y_CS =		(fx68k_byte_addr>=24'h80011e && fx68k_byte_addr<=24'h80011f);
wire CPS_A_RSCR_OFFS_CS =	(fx68k_byte_addr>=24'h800120 && fx68k_byte_addr<=24'h800121);
wire CPS_A_vCONT_CS =		(fx68k_byte_addr>=24'h800122 && fx68k_byte_addr<=24'h800123);


reg [15:0] CPS_A_OBJ_REG;
reg [15:0] CPS_A_SCR1_REG;
reg [15:0] CPS_A_SCR2_REG;
reg [15:0] CPS_A_SCR3_REG;
reg [15:0] CPS_A_RSCR_REG;
reg [15:0] CPS_A_PAL_REG;
reg [15:0] CPS_A_SCR1_X_REG;
reg [15:0] CPS_A_SCR1_Y_REG;
reg [15:0] CPS_A_SCR2_X_REG;
reg [15:0] CPS_A_SCR2_Y_REG;
reg [15:0] CPS_A_SCR3_X_REG;
reg [15:0] CPS_A_SCR3_Y_REG;
reg [15:0] CPS_A_STAR1_X_REG;
reg [15:0] CPS_A_STAR1_Y_REG;
reg [15:0] CPS_A_STAR2_X_REG;
reg [15:0] CPS_A_STAR2_Y_REG;
reg [15:0] CPS_A_RSCR_OFFS_REG;
reg [15:0] CPS_A_vCONT_REG;


// CPS_B Registers...
//
// (example from Street Fighter 2, since each game has a different address map for the CPS-B regs).
wire CPS_B_LAYERCON_CS = (fx68k_byte_addr>=24'h800166 && fx68k_byte_addr<=24'h800167);
wire CPS_B_PRIMASK1_CS = (fx68k_byte_addr>=24'h800168 && fx68k_byte_addr<=24'h800169);
wire CPS_B_PRIMASK2_CS = (fx68k_byte_addr>=24'h80016a && fx68k_byte_addr<=24'h80016b);
wire CPS_B_PRIMASK3_CS = (fx68k_byte_addr>=24'h80016c && fx68k_byte_addr<=24'h80016d);
wire CPS_B_PRIMASK4_CS = (fx68k_byte_addr>=24'h80016e && fx68k_byte_addr<=24'h80016f);
wire CPS_B_PALCONT_CS  = (fx68k_byte_addr>=24'h800170 && fx68k_byte_addr<=24'h800171);

(*keep*) wire CPS_B_SF2_ID_CS = (fx68k_byte_addr>=24'h800172 && fx68k_byte_addr<=24'h800173);

reg [15:0] CPS_B_LAYERCON_REG;
reg [15:0] CPS_B_PRIMASK1_REG;
reg [15:0] CPS_B_PRIMASK2_REG;
reg [15:0] CPS_B_PRIMASK3_REG;
reg [15:0] CPS_B_PRIMASK4_REG;
reg [15:0] CPS_B_PALCONT_REG;


reg VBLANK_INT;
reg HBLANK_INT;
reg HSYNC_PULSE_1;
wire HSYNC_PULSE = (!HSYNC_PULSE_1 && CLK_DIV[11]);
always @(posedge clk_sys or posedge reset)
if (reset) begin
	VBLANK_INT <= 0;
	HSYNC_PULSE_1 <= 0;
end
else begin
	HSYNC_PULSE_1 <= CLK_DIV[11];
	
	if (HSYNC_PULSE) HBLANK_INT <= 1'b1;
	else if (fx68k_fc==3'd7) HBLANK_INT <= 1'b0;	// If the 68K services the Interrupt, clear the HBLANK_INT flag.
	
	if (CLK_DIV==0) VBLANK_INT <= 1'b1;
	else if (fx68k_fc==3'd7) VBLANK_INT <= 1'b0;	// If the 68K services the Interrupt, clear the VBLANK_INT flag.
	
	// CPS_A Register WRITES...
	if (CPS_A_OBJ_CS			&& fx68k_write_pulse) CPS_A_OBJ_REG			<= fx68k_do;
	if (CPS_A_SCR1_CS			&& fx68k_write_pulse) CPS_A_SCR1_REG		<= fx68k_do;
	if (CPS_A_SCR2_CS			&& fx68k_write_pulse) CPS_A_SCR2_REG		<= fx68k_do;
	if (CPS_A_SCR3_CS			&& fx68k_write_pulse) CPS_A_SCR3_REG		<= fx68k_do;
	if (CPS_A_RSCR_CS			&& fx68k_write_pulse) CPS_A_RSCR_REG		<= fx68k_do;
	if (CPS_A_PAL_CS			&& fx68k_write_pulse) CPS_A_PAL_REG			<= fx68k_do;
	if (CPS_A_SCR1_X_CS		&& fx68k_write_pulse) CPS_A_SCR1_X_REG		<= fx68k_do;
	if (CPS_A_SCR1_Y_CS		&& fx68k_write_pulse) CPS_A_SCR1_Y_REG		<= fx68k_do;
	if (CPS_A_SCR2_X_CS		&& fx68k_write_pulse) CPS_A_SCR2_X_REG		<= fx68k_do;
	if (CPS_A_SCR2_Y_CS		&& fx68k_write_pulse) CPS_A_SCR2_Y_REG		<= fx68k_do;
	if (CPS_A_SCR3_X_CS		&& fx68k_write_pulse) CPS_A_SCR3_X_REG		<= fx68k_do;
	if (CPS_A_SCR3_Y_CS		&& fx68k_write_pulse) CPS_A_SCR3_Y_REG		<= fx68k_do;
	if (CPS_A_STAR1_X_CS		&& fx68k_write_pulse) CPS_A_STAR1_X_REG	<= fx68k_do;
	if (CPS_A_STAR1_Y_CS		&& fx68k_write_pulse) CPS_A_STAR1_Y_REG	<= fx68k_do;
	if (CPS_A_STAR2_X_CS		&& fx68k_write_pulse) CPS_A_STAR2_X_REG	<= fx68k_do;
	if (CPS_A_STAR2_Y_CS		&& fx68k_write_pulse) CPS_A_STAR2_Y_REG	<= fx68k_do;
	if (CPS_A_RSCR_OFFS_CS	&& fx68k_write_pulse) CPS_A_RSCR_OFFS_REG <= fx68k_do;
	if (CPS_A_vCONT_CS		&& fx68k_write_pulse) CPS_A_vCONT_REG		<= fx68k_do;

	// CPS_B Register WRITES...
	if (CPS_B_LAYERCON_CS	&& fx68k_write_pulse) CPS_B_LAYERCON_REG	<= fx68k_do;
	if (CPS_B_PRIMASK1_CS	&& fx68k_write_pulse) CPS_B_PRIMASK1_REG	<= fx68k_do;
	if (CPS_B_PRIMASK2_CS	&& fx68k_write_pulse) CPS_B_PRIMASK2_REG	<= fx68k_do;
	if (CPS_B_PRIMASK3_CS	&& fx68k_write_pulse) CPS_B_PRIMASK3_REG	<= fx68k_do;
	if (CPS_B_PRIMASK4_CS	&& fx68k_write_pulse) CPS_B_PRIMASK4_REG	<= fx68k_do;
	if (CPS_B_PALCONT_CS		&& fx68k_write_pulse) CPS_B_PALCONT_REG	<= fx68k_do;
end



// Player controls are all active-HIGH here!...
wire P1_RIGHT	= joystick_0[0];
wire P1_LEFT	= joystick_0[1];
wire P1_DOWN	= joystick_0[2];
wire P1_UP		= joystick_0[3];
wire P1_BUT1	= joystick_0[4];
wire P1_BUT2	= joystick_0[5];
wire P1_BUT3	= joystick_0[6];
wire P1_UNK		= joystick_0[7];

wire P2_RIGHT	= joystick_1[0];
wire P2_LEFT	= joystick_1[1];
wire P2_DOWN	= joystick_1[2];
wire P2_UP		= joystick_1[3];
wire P2_BUT1	= joystick_1[4];
wire P2_BUT2	= joystick_1[5];
wire P2_BUT3	= joystick_1[6];
wire P2_UNK		= joystick_1[7];

wire [15:0] JOYSTICKS = ~{P2_UNK, P2_BUT3, P2_BUT2, P2_BUT1, P2_UP, P2_DOWN, P2_LEFT, P2_RIGHT,
								  P1_UNK, P1_BUT3, P1_BUT2, P1_BUT1, P1_UP, P1_DOWN, P1_LEFT, P1_RIGHT};


// Switches / DIP Switches are all active-HIGH here!...
wire COIN1	= joystick_0[8];
wire START1	= joystick_0[9];

wire COIN2	= joystick_1[8];
wire START2	= joystick_1[9];

wire SERV1	= joystick_0[10];
wire SERVSW	= joystick_0[11];
wire [7:0] SERV = ~{1'b0, SERVSW, START2, START1, 1'b0, SERV1, COIN2, COIN1};


// DIPSW A.
wire [2:0] COINAGE = 3'b000;	// 1 Coin/1 Credit.
wire COINSLOTS = 1'b0;			// 1 Coin Slot.
wire [7:0] DIPA = ~{COINAGE, COINSLOTS, 6'b000000};


// DIPSW B.
wire [2:0] DIFF = 3'b110;		// Normal.
wire [1:0] VS_MODE = 2'b00;	// 1 Game Match;
wire [7:0] DIPB = ~{DIFF, VS_MODE, 6'b000000};


// DIPSW C.
wire FREEZE = 1'b0;			// Definitely keep this OFF! lol
wire FLIP_SCREEN = 1'b0;

//wire DEMO_SOUNDS = 1'b1;
wire DEMO_SOUNDS = 1'b0;	// TESTING. Not sure if this stuff is inverted?

wire GAME_MODE = 1'b0;		// 0=GAME. 1=TEST.
wire [7:0] DIPC = ~{3'b000, FREEZE, FLIP_SCREEN, DEMO_SOUNDS, 1'b0, GAME_MODE};



//(*keep*) wire [15:0] ROM_DATA_FULL = {ROM_HI_DO, ROM_LO_DO};	// On-chip.


(*keep*) assign fx68k_di = //(CPU_ROM_CS) ? ROM_DATA_FULL : 
									(CPU_ROM_CS) ? sdram_dout :

									(CPU_RAM_CS) ? CPU_RAM_DO :
									//(CPU_RAM_CS) ? sdram_dout :
									 
									(PLAYER_CS) ? JOYSTICKS :
									 
									(SERV_CS)  ? {SERV, 8'hFF} :	// Coin/Start/Service button bits are already active-LOW here. Mapped to the upper bits of the 68K!
									
									(DIPA_CS)  ? {DIPA, 8'hFF} :
									(DIPB_CS)  ? {DIPB, 8'hFF} :
									(DIPC_CS)  ? {DIPC, 8'hFF} :
									 
									(CPS_A_OBJ_CS) 		? CPS_A_OBJ_REG :			// Offset: 0x800100.
									(CPS_A_SCR1_CS) 		? CPS_A_SCR1_REG :		// Offset: 0x800102.
									(CPS_A_SCR2_CS) 		? CPS_A_SCR2_REG :		// Offset: 0x800104.
									(CPS_A_SCR3_CS) 		? CPS_A_SCR3_REG :		// Offset: 0x800106.
									(CPS_A_RSCR_CS) 		? CPS_A_RSCR_REG :		// Offset: 0x800108.
									(CPS_A_PAL_CS) 		? CPS_A_PAL_REG :			// Offset: 0x80010A.
									(CPS_A_SCR1_X_CS) 	? CPS_A_SCR1_X_REG :		// Offset: 0x80010C.
									(CPS_A_SCR1_Y_CS) 	? CPS_A_SCR1_Y_REG :		// Offset: 0x80010E.
									(CPS_A_SCR2_X_CS) 	? CPS_A_SCR2_X_REG :		// Offset: 0x800110.
									(CPS_A_SCR2_Y_CS) 	? CPS_A_SCR2_Y_REG:		// Offset: 0x800112.
									(CPS_A_SCR3_X_CS) 	? CPS_A_SCR3_X_REG :		// Offset: 0x800114.
									(CPS_A_SCR3_Y_CS) 	? CPS_A_SCR3_Y_REG :		// Offset: 0x800116.
									(CPS_A_STAR1_X_CS) 	? CPS_A_STAR1_X_REG :	// Offset: 0x800118.
									(CPS_A_STAR1_Y_CS) 	? CPS_A_STAR1_Y_REG :	// Offset: 0x80011A.
									(CPS_A_STAR2_X_CS) 	? CPS_A_STAR2_X_REG :	// Offset: 0x80011C.
									(CPS_A_STAR2_Y_CS) 	? CPS_A_STAR2_Y_REG :	// Offset: 0x80011E.
									(CPS_A_RSCR_OFFS_CS) ? CPS_A_RSCR_OFFS_REG :// Offset: 0x800120.
									(CPS_A_vCONT_CS) 	? CPS_A_vCONT_REG :		// Offset: 0x800122.
									 
									// CPS_B regs have different offsets for each game!
									(CPS_B_LAYERCON_CS) ? CPS_B_LAYERCON_REG :
									(CPS_B_PRIMASK1_CS) ? CPS_B_PRIMASK1_REG :
									(CPS_B_PRIMASK2_CS) ? CPS_B_PRIMASK2_REG :
									(CPS_B_PRIMASK3_CS) ? CPS_B_PRIMASK3_REG :
									(CPS_B_PRIMASK4_CS) ? CPS_B_PRIMASK4_REG :
									(CPS_B_PALCONT_CS)  ? CPS_B_PALCONT_REG :
									 
									(CPS_B_SF2_ID_CS)	? 16'h0401 :
									 
									(GFX_RAM_CS) ? GFX_RAM_DO :
														 16'hzzzz;
/*
gfx_ram	gfx_ram_inst (
	.clock ( clk_sys ),
	.address ( fx68k_addr[17:1] ),
	
	.data ( fx68k_do ),
	.wren ( GFX_RAM_WREN ),
	.byteena( GFX_RAM_BE ),
	
	.q ( GFX_RAM_DO )
);
*/

gfx_ram_dp	gfx_ram_dp_inst (
	.clock_a ( clk_sys ),
	.address_a ( fx68k_addr[17:1] ),
	.data_a ( fx68k_do ),
	.wren_a ( GFX_RAM_WREN ),
	.byteena_a ( GFX_RAM_BE ),
	.q_a ( GFX_RAM_DO ),
	
	.clock_b ( PATT_CLK_IN ),
	.address_b ( GFX_RAM_READ_ADDR ),
//	.data_b ( GFX_RAM_READ_DI ),
	.wren_b ( 1'b0 ),
	.q_b ( GFX_RAM_READ_DO )
);

(*keep*) wire [15:0] GFX_RAM_DO;

(*keep*) wire GFX_RAM_WREN = GFX_RAM_CS && !fx68k_as_n && !fx68k_rw;
(*keep*) wire [1:0] GFX_RAM_BE = {!fx68k_uds_n, !fx68k_lds_n};



wire [16:0] GFX_RAM_READ_ADDR = (layer<<13) | {PATT_Y_IN[11:3], PATT_X_IN[11:4]};

wire [15:0] GFX_RAM_READ_DO;

assign VGA_R = (PATT_DE_IN) ? {GFX_RAM_READ_DO[15:11], 3'b000} : 8'h00;
assign VGA_G = (PATT_DE_IN) ? {GFX_RAM_READ_DO[10:5],  2'b00}  : 8'h00;
assign VGA_B = (PATT_DE_IN) ? {GFX_RAM_READ_DO[4:0],   3'b000} : 8'h00;

assign CLK_VIDEO = PATT_CLK_IN;
assign CE_PIXEL = 1'b1;

/*
wire [23:0] osd_rgb_out;
wire [1:0] cable_type;
wire osd_enable;
wire cont_disable;

wire [1:0] lr_filter;
wire [1:0] hr_filter;
wire [3:0] scanline_mode;

wire [7:0] osd_ctrl;
wire osdframe;

my_osd my_osd_inst
(
	.reset_n( !reset ) ,			// input  reset_n (active LOW).
	
	.pix_clk( PATT_CLK_IN ) ,	// input  pixel clk 
	.sys_clk( clk_sys ) ,		// input  system clk

	.hsync( PATT_HS_IN ) ,		// input  hsync (active HIGH!)
	.vsync( PATT_VS_IN ) ,		// input  vsync (active HIGH!)
	
	.rgb_in( 24'h000000 ) ,		// input [23:0] rgb_in.
	.rgb_out( osd_rgb_out ) ,	// output [23:0] rgb_out.

	.osd_ctrl( osd_ctrl ) ,		// input [7:0] osd_ctrl
	
	._scs( OSD_CS_N ) ,			// input  _scs		// ESP8266, pin "GPIO 16" (SPI /Chip Select for the OSD module)
	.sdo( OSD_MISO ) ,			// inout  sdo		// ESP8266, pin "GPIO 12" (ESP MISO. ie. Data IN to the ESP!)
	
	.sdi( SPARE ) ,				// input  sdi		// ESP8266, pin "GPIO 13" (ESP MOSI. ie. Data OUT of the ESP!)
										// Had to change to the SPARE pin, as the OSD_MOSI pin (13) on the Cyc III got zapped. :(
										// Must be the ground-loop thing between the PC and board. Need series resistors on ALL SPI signals, and JTAG!!
	
	.sck( EEPROM_DAT ) ,			// input  sck		// ESP8266, pin "GPIO 14" (SCK. SPI Clock)
										// Had to change to the EEPROM_DAT pin, as the OSD_SCLK pin on the Cyc III ALSO got zapped. :(
	
	.horbeam( PATT_X_IN ) ,		// input [11:0] horbeam.
	.verbeam( PATT_Y_IN ) ,		// input [11:0] verbeam.
	
	.osdframe( osdframe ) ,		// output osdframe
	
	.scanline_mode( scanline_mode ) ,// output [3:0] scanline
	
	.cable_type( cable_type ) ,		// output [1:0] cable_type
	.osd_enable( osd_enable ) ,		// output  osd_enable
	.cont_disable( cont_disable ) ,	// output  cont_disable
	
	.lr_filter( lr_filter ) ,	// output [1:0] lr_filter
	.hr_filter( hr_filter )		// output [1:0] hr_filter
);
assign VGA_R = (PATT_DE_IN) ? osd_rgb_out[23:16] : 8'h00;
assign VGA_G = (PATT_DE_IN) ? osd_rgb_out[15:8] : 8'h00;
assign VGA_B = (PATT_DE_IN) ? osd_rgb_out[7:0] : 8'h00;
*/
assign VGA_HS = !PATT_HS_IN;
assign VGA_VS = !PATT_VS_IN;
assign VGA_DE = PATT_DE_IN;

/*
cps_a cps_a_inst
(
	.RESET_N(!reset) ,		// input  RESET_N
	.CLK(PATT_CLK_IN) ,		// input  CLK
	.HBLANK_N(HBLANK_N) ,	// input  HBLANK_N
	.VBLANK_N(VBLANK_N) ,	// input  VBLANK_N
	.CSB(CSB) ,					// input  CSB
	.WRB(WRB) ,					// input  WRB
	.CA(CA) ,					// inout [23:0] CA
	.CDIN(CDIN) ,				// input [15:0] CDIN
	.CDOUT(CDOUT) ,			// output [15:0] CDOUT
	.BRB(BRB) ,					// output  BRB
	.BGACKB(BGACKB) ,			// input  BGACKB
	.PATT_X(PATT_X) ,			// input [11:0] PATT_X
	.PATT_Y(PATT_Y) ,			// input [11:0] PATT_Y
	.CK125(CK125) ,			// output  CK125
	.CK250(CK250) ,			// output  CK250
	.ROMA(ROMA) ,				// output [22:0] ROMA
	.GFX_RAM_ADDR(GFX_RAM_ADDR) ,	// output [16:0] GFX_RAM_ADDR
	.GFX_RAM_DO(GFX_RAM_DO) ,	// input [15:0] GFX_RAM_DO
	.GFX_RAM_DI(GFX_RAM_DI) 	// output [15:0] GFX_RAM_DI
);
*/

wire aud_cmd_clk = fx68k_clk;
wire aud_cmd_addr = fx68k_byte_addr[3];
wire [7:0] aud_cmd_din = fx68k_do[7:0];
wire aud_cmd_wr = (SOUNDCMD_CS | SOUNDFADE_CS) & fx68k_write_pulse;

cps_audio cps_audio_inst
(
	.clock( clk_ntsc ) ,	// input  clock
	.reset( reset ) ,		// input  reset
	
	.cmd_clk( aud_cmd_clk ) ,	// input  cmd_clk
	.cmd_addr( aud_cmd_addr ) ,// input  cmd_addr
	.cmd_din( aud_cmd_din ) ,	// input [7:0] cmd_din
	.cmd_wr( aud_cmd_wr ) ,		// input  cmd_wr
	
	.out_l( AUDIO_L ) ,	// output [15:0] out_l
	.out_r( AUDIO_R ) 	// output [15:0] out_r
);

assign AUDIO_S = 1;
assign AUDIO_MIX = 0;



assign sconf = status[11:10];

/*
video_mixer #(.LINE_LENGTH(320), .HALF_DEPTH(1)) video_mixer
(
    .clk_sys(CLK_VIDEO),
    .ce_pix(~old_ce_pix & ce_pix),
    .ce_pix_out(CE_PIXEL),

    .scanlines(0),
    .scandoubler(~interlace && (scale || forced_scandoubler)),
    .hq2x(scale==1),

    .mono(0),

    .R(r),
    .G(g),
    .B(b),

    // Positive pulses.
    .HSync(hs),
    .VSync(vs),
    .HBlank(hblank),
    .VBlank(vblank),
    
    .VGA_R( VGA_R ),
    .VGA_G( VGA_G ),
    .VGA_B( VGA_B ),
    .VGA_VS( VGA_VS ),
    .VGA_HS( VGA_HS ),
    .VGA_DE( VGA_DE )
);
*/


/*
compressor compressor
(
	clk_sys,
	audio_l[12:1], audio_r[12:1],
	AUDIO_L,       AUDIO_R
);
*/


///////////////////////////////////////////////////

wire [22:1] rom_addr;
wire [15:0] rom_data;
wire rom_rd;
wire rom_rdack;
/*

assign DDRAM_CLK = clk_ram;

ddram ddram
(
	.*,

   .wraddr(ioctl_addr),
   .din({ioctl_data[7:0],ioctl_data[15:8]}),
   .we_req(rom_wr),
   //.we_ack(rom_wrack),
	.we_ack(),

   .rdaddr(use_map ? {map[rom_addr[21:19]], rom_addr[18:1]} : rom_addr),
   .dout(rom_data),
   .rd_req(rom_rd),
   .rd_ack(rom_rdack)
);
*/

reg  rom_wr;
wire rom_wrack;

always @(posedge clk_sys) begin
	reg old_download, old_reset;
	old_download <= ioctl_download;
	old_reset <= reset;

	if(~old_reset && reset) ioctl_wait <= 0;
	if(~old_download && ioctl_download) rom_wr <= 0;
	else begin
		if(ioctl_wr) begin
			ioctl_wait <= 1;
			rom_wr <= ~rom_wr;
		end else if(ioctl_wait && (rom_wr == rom_wrack)) begin
			ioctl_wait <= 0;
		end
	end
end


/*
(*keep*)wire bus_eof;
(*keep*)wire [6:0] bus_vbl;
(*keep*)wire [8:0] bus_vpos;
(*keep*)wire bus_dma_ena;
(*keep*)wire bus_frd_ena;

(*keep*)wire vid_clk_ena;
(*keep*)wire vid_eol;
(*keep*)wire vid_eof;
(*keep*)wire [10:0] vid_hpos;
(*keep*)wire [10:0] vid_vpos;
(*keep*)wire vid_hsync;
(*keep*)wire vid_vsync;
(*keep*)wire vid_dena;

cps_video_beam cps_video_beam_inst
(
	.bus_rst( reset ) ,		// input  bus_rst
	.bus_clk( clk_sys ) ,	// input  bus_clk
	
	.ram_ref(ram_ref) ,		// input  ram_ref
	.ram_cyc(ram_cyc) ,		// input [3:0] ram_cyc
	.ram_acc(ram_acc) ,		// input [3:0] ram_acc
	.ram_slot(ram_slot) ,	// input [8:0] ram_slot
	
	.slot_rst(slot_rst) ,	// input  slot_rst
	
	.bus_eof(bus_eof) ,		// output  bus_eof
	.bus_vbl(bus_vbl) ,		// output [6:0] bus_vbl
	.bus_vpos(bus_vpos) ,	// output [8:0] bus_vpos
	.bus_dma_ena(bus_dma_ena) ,	// output  bus_dma_ena
	.bus_frd_ena(bus_frd_ena) ,	// output  bus_frd_ena
	
	.vid_rst( reset ) ,		// input  vid_rst
	.vid_clk( clk_sys ) ,		// input  vid_clk
	.vid_clk_ena(vid_clk_ena) ,	// output [2:0] vid_clk_ena
	.vid_eol(vid_eol) ,		// output  vid_eol
	.vid_eof(vid_eof) ,		// output  vid_eof
	.vid_hpos(vid_hpos) ,	// output [10:0] vid_hpos
	.vid_vpos(vid_vpos) ,	// output [10:0] vid_vpos
	.vid_hsync(vid_hsync) ,	// output  vid_hsync
	.vid_vsync(vid_vsync) ,	// output  vid_vsync
	.vid_dena(vid_dena) 		// output  vid_dena
);
*/

endmodule
