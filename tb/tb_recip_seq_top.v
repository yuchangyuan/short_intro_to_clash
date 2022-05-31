`timescale 1ns/10ps

module tb;
   reg clk, rst;

   localparam  SCALE = 'h100_0000;

   reg [31:0]  in_data;
   reg [31:0]  in_esp;
   reg         in_valid;
   wire        in_ready;

   wire [31:0] out_data;
   wire [ 7:0] out_cnt;
   wire        out_valid;
   reg         out_ready;

   recip_seq_top dut
     (// clock & reset
      .clk(clk),
      .rst(rst),

      // in
      .in_data(in_data),
      .in_esp(in_esp),
      .in_valid(in_valid),
      .in_ready(in_ready),

      // out
      .out_ready(out_ready),
      .out_data(out_data),
      .out_cnt(out_cnt),
      .out_valid(out_valid));

   task t(input real x, input real esp);
      begin
         in_data = x * SCALE;
         in_esp = esp * SCALE;
         in_valid = 1'b1;

         // wait input ready
         wait (in_valid && in_ready);
         @(posedge clk) in_valid = 1'b0;

         // wait output valid
         out_ready = 1'b1;
         wait (out_valid && out_ready);

         // display
         $display("%.2f %.9f cnt:%0d esp:%g err:%+.3g",
                  x, 1.0 * out_data / SCALE, out_cnt, esp,
                  1 / x - 1.0 * out_data / SCALE);

         // clear up
         @(posedge clk) out_ready = 1'b0;
      end
   endtask

   // 50MHz
   always #10 clk = !clk;

   initial begin
      in_data = 32'b0;
      in_esp =  32'b0;
      in_valid = 1'b0;
      out_ready = 1'b0;

      //$dumpfile("recip_seq_top.fst");
      //$dumpvars(0, tb);

      clk = 1'b0;
      rst = 1'b1;
      #55 rst = 1'b0;
   end

   initial begin
      wait(!rst);
      @(posedge clk);

      t(0.1, 1e-7);
      t(0.3, 1e-7);
      t(0.5, 1e-7);
      t(0.7, 1e-7);
      t(1.0, 1e-7);
      t(1.8, 1e-7);
      t(2.5, 1e-7);
      t(3.0, 1e-7);

      $finish();
   end

endmodule
