`timescale 1ns/10ps

module tb;
   wire [31:0] out1, out2;
   reg [31:0]  in;

   localparam  SCALE = 'h100_0000;

   recip4_ dut1(.x(in), .y(out1));
   recip4  dut2(.x(in), .y(out2));

   task t(input real x);
      begin
         in = x * SCALE;
         #1;
         $display("%f:\t%.9f\t%.9f", x, 1.0 * out1 / SCALE, 1.0 * out2 / SCALE);
      end
   endtask

   initial begin
      $display("input:\t\trecip4_\t\trecip4");
      t(0.1);
      t(0.3);
      t(0.5);
      t(0.7);
      t(1.0);
      t(1.8);
      t(2.5);
      t(3.0);

      $finish();
   end

endmodule
