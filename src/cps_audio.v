module cps_audio (
	input clock,
	input reset,
	
	input cmd_clk,
	input cmd_addr,
	input [7:0] cmd_din,
	input cmd_wr,
	
	output [15:0] out_l,
	output [15:0] out_r
);


// Z80
wire T80_RESET_N = !reset;
wire T80_CLKEN = 1'b1;
wire T80_WAIT_N = 1'b1;
wire T80_INT_N = JT51_IRQ_N;
wire T80_NMI_N = 1'b1;
wire T80_BUSRQ_N = 1'b1;

wire T80_M1_N;
wire T80_MREQ_N;
wire T80_IORQ_N;
wire T80_RD_N;
wire T80_WR_N;
wire T80_BUSAK_N;

wire [15:0] T80_ADDR;
wire [7:0] T80_DI;
wire [7:0] T80_DO;

T80s T80s_inst
(
	.RESET_n( T80_RESET_N ) ,	// input  RESET_n
	.CLK( clock ) ,			// input  CLK
	.CEN( T80_CLKEN ) ,			// input  CEN
	.WAIT_n( T80_WAIT_N ) ,		// input  WAIT_n
	.INT_n( T80_INT_N ) ,		// input  INT_n
	.NMI_n( T80_NMI_N ) ,		// input  NMI_n
	.BUSRQ_n( T80_BUSRQ_N ) ,	// input  BUSRQ_n
	.M1_n( T80_M1_N ) ,			// output  M1_n
	.MREQ_n( T80_MREQ_N ) ,		// output  MREQ_n
	.IORQ_n( T80_IORQ_N ) ,		// output  IORQ_n
	.RD_n( T80_RD_N ) ,			// output  RD_n
	.WR_n( T80_WR_N ) ,			// output  WR_n
//	.RFSH_n(RFSH_n) ,				// output  RFSH_n
//	.HALT_n(HALT_n) ,				// output  HALT_n
	.BUSAK_n( T80_BUSAK_N ) ,	// output  BUSAK_n
//	.OUT0(OUT0) ,					// input  OUT0
	.A( T80_ADDR ) ,				// output [15:0] A
	.DI( T80_DI ) ,				// input [7:0] DI
	.DO( T80_DO ) 					// output [7:0] DO
);


wire [15:0] Z80_ROM_ADDR = (Z80_BANK1_CS) ? {1'b1, Z80_BANK_REG, T80_ADDR[13:0]} :	// <- 0x8000 to 0xBFFF.
																						T80_ADDR[15:0];	// <- Just use the full T80_ADDR range for 0x0000 to 0x7FFF.

z80_rom	z80_rom_inst (
	.clock ( clock ),
	.address ( Z80_ROM_ADDR ),
	.q ( Z80_ROM_DO )
);
wire [7:0] Z80_ROM_DO;


z80_ram	z80_ram_inst (
	.clock ( clock ),
	.address ( T80_ADDR[10:0] ),
		
	.data ( Z80_RAM_DI ),
	.wren ( Z80_RAM_WE ),
	
	.q ( Z80_RAM_DO )
);
wire [7:0] Z80_RAM_DI = T80_DO;
wire [7:0] Z80_RAM_DO;

wire Z80_RAM_WE = Z80_RAM_CS && !T80_MREQ_N && !T80_WR_N;


wire Z80_ROM_CS  = (T80_ADDR>=16'h0000 && T80_ADDR<=16'h7FFF);
wire Z80_BANK1_CS= (T80_ADDR>=16'h8000 && T80_ADDR<=16'hBFFF);
wire Z80_RAM_CS  = (T80_ADDR>=16'hD000 && T80_ADDR<=16'hD7FF);

wire Z80_JT51_CS = (T80_ADDR>=16'hF000 && T80_ADDR<=16'hF001);
wire Z80_OKI_CS  = (T80_ADDR>=16'hF002 && T80_ADDR<=16'hF003);
wire Z80_BANK_CS = (T80_ADDR>=16'hF004 && T80_ADDR<=16'hF005);
wire Z80_SEL_CS  = (T80_ADDR>=16'hF006 && T80_ADDR<=16'hF007);
wire Z80_CMD_CS  = (T80_ADDR>=16'hF008 && T80_ADDR<=16'hF009);
wire Z80_FADE_CS = (T80_ADDR>=16'hF00A && T80_ADDR<=16'hF00B);


assign T80_DI = (Z80_ROM_CS)		? Z80_ROM_DO :
					 (Z80_BANK1_CS)	? Z80_ROM_DO :
					 (Z80_RAM_CS)		? Z80_RAM_DO :
					 (Z80_JT51_CS)		? JT51_DO : 
					 //(Z80_OKI_CS)	? PCM_DO :
					 (Z80_OKI_CS)		? 8'hF0 :
					 (Z80_CMD_CS)		? Z80_CMD_REG :
					 (Z80_FADE_CS)		? Z80_FADE_REG :
											  8'h00;


// 68K to Z80 command regs / latches...
reg [7:0] Z80_CMD_REG;
reg [7:0] Z80_FADE_REG;
always @(posedge cmd_clk or posedge reset)
if (reset) begin
	Z80_CMD_REG <= 8'h00;
	Z80_FADE_REG <= 8'h00;
end
else begin
	if (cmd_addr==1'b0 && cmd_wr) Z80_CMD_REG <= cmd_din;
	if (cmd_addr==1'b1 && cmd_wr) Z80_FADE_REG <= cmd_din;
end


reg Z80_BANK_REG;
reg PCM_SS_REG;

reg [15:0] DELAY;
reg [3:0] CNT;
always @(posedge clock or posedge reset)
if (reset) begin
	//Z80_CMD_REG <= 8'h00;
	//Z80_FADE_REG <= 8'h00;
	//DELAY <= 16'hFFFF;
	//CNT <= 0;
	Z80_BANK_REG <= 1'b0;
	PCM_SS_REG <= 1'b1;
end
else begin
	//DELAY <= DELAY - 1;
	
	if (Z80_BANK_CS && !T80_WR_N) Z80_BANK_REG <= T80_DO[0];
	//if (Z80_SEL_CS && !T80_WR_N) PCM_SS_REG <= T80_DO[0];
	
	/*
	case (CNT)
	0: if (!DELAY) begin
		Z80_CMD_REG <= 8'hF0;	// Probably the "silence" / stop music command.
		CNT <= CNT + 1;
	end
	
	1: if (!DELAY) begin
		Z80_CMD_REG <= 8'hFF;
		CNT <= CNT + 1;
	end

	2: if (!DELAY) begin
		Z80_CMD_REG <= 8'hF7;	// Probably the "music select" command.
		CNT <= CNT + 1;
	end

	3: if (!DELAY) begin
		Z80_CMD_REG <= 8'hFF;
		CNT <= CNT + 1;
	end

	4: if (!DELAY) begin
		//Z80_CMD_REG <= 8'h01;	// Music selection. (SF2 - Ryu's theme).
		//Z80_CMD_REG <= 8'h04;	// Music selection. (SF2 - Ken's theme.
		//Z80_CMD_REG <= 8'h05;	// Music selection. (SF2 - Guile's theme).
		//Z80_CMD_REG <= 8'h06;	// Music selection. (SF2 - Chun-Li's theme).
		//Z80_CMD_REG <= 8'h07;	// Music selection. (SF2 - Zangief's theme).
		//Z80_CMD_REG <= 8'h08;	// Music selection. (SF2 - Dhalsim's theme).
		Z80_CMD_REG <= 8'h09;	// Music selection. (SF2 - Balrog's theme).
		//Z80_CMD_REG <= 8'h0A;	// Music selection. (SF2 - Vega's theme).
		//Z80_CMD_REG <= 8'h0E;	// Music selection. (SF2 - Character Select Menu song).
		//Z80_CMD_REG <= 8'h16;	// Music selection. (SF2 - Intro song).

		//Z80_CMD_REG <= 8'h0C;	// Music selection. (Ghouls - Level 1).
		//Z80_CMD_REG <= 8'h0D;	// Music selection. (Ghouls - Boss fight?).
		//Z80_CMD_REG <= 8'h0E;	// Music selection. (Ghouls - ).
		
		CNT <= CNT + 1;
	end

	5: if (!DELAY) begin
		Z80_CMD_REG <= 8'hFF;
		CNT <= CNT + 1;
	end

	default:;
	endcase
	*/
end


wire JT51_CLK = clock;
wire JT51_RsT = reset;
wire JT51_CS_N = !Z80_JT51_CS;
wire JT51_WR_N = !(Z80_JT51_CS && !T80_WR_N && !T80_MREQ_N);
wire JT51_A0 = T80_ADDR[0];
wire [7:0] JT51_DI = T80_DO;
wire [7:0] JT51_DO;
wire JT51_CT1;
wire JT51_CT2;
wire JT51_IRQ_N;
wire JT51_P1;
wire JT51_SAMPLE;
(*keep*) wire signed [15:0] JT51_L;
(*keep*) wire signed [15:0] JT51_R;
(*keep*) wire signed [15:0] JT51_XLEFT;
(*keep*) wire signed [15:0] JT51_XRIGHT;
(*keep*) wire [15:0] JT51_DACLEFT;
(*keep*) wire [15:0] JT51_DACRIGHT;


jt51 jt51_inst
(
	.clk( JT51_CLK ) ,			// input  clk
	.rst( JT51_RsT ) ,			// input  rst
	
	.cs_n( JT51_CS_N ) ,			// input  cs_n
	.wr_n( JT51_WR_N ) ,			// input  wr_n
	.a0( JT51_A0 ) ,				// input  a0
	
	.d_in( JT51_DI ) ,			// input [7:0] d_in
	
	.d_out( JT51_DO ) ,			// output [7:0] d_out
	
	.ct1( JT51_CT1 ) ,			// output  ct1
	.ct2( JT51_CT2 ) ,			// output  ct2
	.irq_n( JT51_IRQ_N ) ,		// output  irq_n
	.p1( JT51_P1 ) ,				// output  p1
	
	.sample( JT51_SAMPLE ) ,	// output  sample
	
	.left( JT51_L ) ,				// output [15:0] left
	.right( JT51_R ) ,			// output [15:0] right
	.xleft( JT51_XLEFT ) ,		// output [15:0] xleft
	.xright( JT51_XRIGHT ) ,	// output [15:0] xright
	.dacleft( JT51_DACLEFT ) ,	// output [15:0] dacleft
	.dacright( JT51_DACRIGHT ) // output [15:0] dacright
);


/*
wire PCM_RESET_N = !reset;
wire PCM_CLK = clock;

wire [7:0] PCM_DI = T80_DO;
wire [7:0] PCM_DO;

wire PCM_CS_N = !Z80_OKI_CS;
wire PCM_RD_N = !(Z80_OKI_CS && !T80_RD_N);
wire PCM_WR_N = !(Z80_OKI_CS && !T80_WR_N);

wire PCM_SS = PCM_SS_REG;

wire [17:0] PCM_ROM_ADDR;
wire [7:0] PCM_ROM_DATA;

wire signed [21:0] PCM_SOUND_OUT;

wire signed [17:0] V1_SAMP_OUT;
wire signed [17:0] V2_SAMP_OUT;
wire signed [17:0] V3_SAMP_OUT;
wire signed [17:0] V4_SAMP_OUT;

wire signed [12:0] V1_SIGNAL;
wire signed [12:0] V2_SIGNAL;
wire signed [12:0] V3_SIGNAL;
wire signed [12:0] V4_SIGNAL;

msm6295 msm6295_inst
(
	.RESET_N( PCM_RESET_N ) ,		// input  RESET_N
	.CLK( PCM_CLK ) ,					// input  CLK
	
	.CPU_DI( PCM_DI ) ,				// input [7:0] CPU_DI
	
	.CPU_DO( PCM_DO ) ,				// output [7:0] CPU_DO
		
	.CS_N( PCM_CS_N ) ,				// input  CS_N
	.RD_N( PCM_RD_N ) ,				// input  RD_N
	.WR_N( PCM_WR_N ) ,				// input  WR_N
	
	.SS( PCM_SS ) ,					// input  SS
	
	.ROM_ADDR( PCM_ROM_ADDR ) ,	// output [17:0] ROM_ADDR
	.ROM_DATA( PCM_ROM_DATA ) ,	// input [7:0] ROM_DATA
	
	.SOUND_OUT( PCM_SOUND_OUT ) ,	// output [21:0] SOUND_OUT
	
//	.V1_STATE(V1_STATE) ,	// output [3:0] V1_STATE
//	.VOICE_SLOT(VOICE_SLOT) ,	// output [2:0] VOICE_SLOT
	
//	.V1_SA(V1_SA) ,			// output [17:0] V1_SA
//	.V1_EA(V1_EA) ,			// output [17:0] V1_EA
	
//	.V1_GATE(V1_GATE) ,		// output  V1_GATE
//	.V2_GATE(V2_GATE) ,		// output  V2_GATE
//	.V3_GATE(V3_GATE) ,		// output  V3_GATE
//	.V4_GATE(V4_GATE) ,		// output  V4_GATE
	
//	.WR_N_RISING(WR_N_RISING) ,	// output  WR_N_RISING
	
//	.WRITE_STATE(WRITE_STATE) ,	// output  WRITE_STATE
	
//	.CPU_DATA_DBG(CPU_DATA_DBG) ,	// output [7:0] CPU_DATA_DBG
	
//	.V1_NIB(V1_NIB) ,	// output [3:0] V1_NIB
//	.V2_NIB(V2_NIB) ,	// output [3:0] V2_NIB
//	.V3_NIB(V3_NIB) ,	// output [3:0] V3_NIB
//	.V4_NIB(V4_NIB) ,	// output [3:0] V4_NIB

	.V1_SAMP_OUT( V1_SAMP_OUT ) ,	// output [17:0] V1_SAMP_OUT
	.V2_SAMP_OUT( V2_SAMP_OUT ) ,	// output [17:0] V2_SAMP_OUT
	.V3_SAMP_OUT( V3_SAMP_OUT ) ,	// output [17:0] V3_SAMP_OUT
	.V4_SAMP_OUT( V4_SAMP_OUT ) ,	// output [17:0] V4_SAMP_OUT

	.V1_SIGNAL( V1_SIGNAL ) ,		// output [12:0] V1_SIGNAL
	.V2_SIGNAL( V2_SIGNAL ) ,		// output [12:0] V2_SIGNAL
	.V3_SIGNAL( V3_SIGNAL ) ,		// output [12:0] V3_SIGNAL
	.V4_SIGNAL( V4_SIGNAL ) ,		// output [12:0] V4_SIGNAL
	
	.SAMP_PULSE(SAMP_PULSE) 	// output  SAMP_PULSE

);

pcm_rom	pcm_rom_inst (
	.clock ( CLK_DIV[2] ),		// Need to run this at a faster clock multiple than the msm6295 atm. TODO - fix this! ElectronAsh.
	
	.address ( PCM_ROM_ADDR ),
	.q ( PCM_ROM_DATA )
);
*/


//assign AUDIO_S = 1;
//assign AUDIO_MIX = 0;

//wire signed [16:0] MIX_L = PCM_SOUND_OUT[21:5];

//wire signed [16:0] MIX_L = {JT51_XLEFT[15],  JT51_XLEFT}  + {PCM_SOUND_OUT[21], PCM_SOUND_OUT[21:5]};
//wire signed [16:0] MIX_R = {JT51_XRIGHT[15], JT51_XRIGHT} + {PCM_SOUND_OUT[19], PCM_SOUND_OUT[19:4]};

//wire signed [16:0] MIX_L = JT51_XLEFT  + PCM_SOUND_OUT[19:4];
//wire signed [16:0] MIX_R = JT51_XRIGHT + PCM_SOUND_OUT[19:4];


//assign AUDIO_L = MIX_L[16:1];
//assign AUDIO_R = MIX_R[16:1];

// Original chip quality...
assign out_l = JT51_XLEFT;
assign out_r = JT51_XRIGHT;

// Better quality?...
//assign AUDIO_L = JT51_XLEFT;
//assign AUDIO_R = JT51_XRIGHT;

//assign AUDIO_R = PCM_SOUND_OUT[19:4];

endmodule
