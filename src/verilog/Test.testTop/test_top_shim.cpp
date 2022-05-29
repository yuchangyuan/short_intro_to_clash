#include <cstdlib>

#include <verilated.h>

#include "Vtest_top.h"

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  Vtest_top *top = new Vtest_top;

  while(!Verilated::gotFinish()) {
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}

