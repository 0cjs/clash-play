module system_top #(
	parameter NUM_DIGITS = 3,
	parameter NUM_SEGS = 8,
	parameter NUM_BUTTONS = 4
) (
	input                               OSC_50M,
	output         reg [NUM_DIGITS-1:0] DIGIT,
	output           reg [NUM_SEGS-1:0] SEG,
	input             [NUM_BUTTONS-1:0] BUTTON
);

	wire clk = OSC_50M;

	reg [17:0] count = 0;
	reg [NUM_SEGS-1:0] segs0, segs1, segs2;
	initial DIGIT = ~1;

	always @* begin
		segs0 = BUTTON;
		segs1 = ~BUTTON;
		segs2 = { BUTTON, ~BUTTON };
	end

	always @* begin
		if      (DIGIT[0]) SEG = segs0;
		else if (DIGIT[1]) SEG = segs1;
		else               SEG = segs2;
	end

	always @(posedge clk) begin
		count <= count + 1'b1;
		if (!count)
			DIGIT <= { DIGIT[0], DIGIT[NUM_DIGITS-1:1] };
	end

endmodule
