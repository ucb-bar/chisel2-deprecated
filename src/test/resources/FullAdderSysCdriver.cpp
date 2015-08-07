#include <systemc>
using namespace std;
using namespace sc_core;
using namespace sc_dt;
#include "SCWrappedFullAdder.cpp"
#include <cstdlib>

SC_MODULE(Generator){
  sc_fifo<cs_io_in_bits_i> out;
  sc_fifo<cs_io_in_bits_i> src;

  SC_HAS_PROCESS(Generator);
  Generator(sc_module_name a_name) : sc_module(a_name), out(1){
    SC_THREAD(generator_thread);
  }

  void generator_thread(void){
	cs_io_in_bits_i dat;

    for(int i=1; i<100; i++){
      dat.FullAdder__io_in_bits_a = rand() & 0x01;
      dat.FullAdder__io_in_bits_b = rand() & 0x01;
      dat.FullAdder__io_in_bits_cin = rand() & 0x01;
      out.write(dat);
      src.write(dat);
      wait(20.0, SC_SEC);
    }
  }
};

SC_MODULE(Eater){
  sc_fifo<cs_io_in_bits_i> *src;
  sc_fifo<cs_io_out_bits_o>* in;

  SC_HAS_PROCESS(Eater);
  Eater(sc_module_name a_name) : sc_module(a_name){
    SC_THREAD(eater_thread);
  }

  void eater_thread(void){
    while(true){
      cs_io_in_bits_i test = src->read();
      printf("Input a = %llu\n", test.FullAdder__io_in_bits_a.values[0]);
      printf("Input b = %llu\n", test.FullAdder__io_in_bits_b.values[0]);
      printf("Input cin = %llu\n", test.FullAdder__io_in_bits_cin.values[0]);
      cs_io_out_bits_o data = in->read();
      printf("Output sum = %llu\n", data.FullAdder__io_out_bits_sum.values[0]);
      printf("Output cout = %llu\n", data.FullAdder__io_out_bits_cout.values[0]);
    }
  }
};

int sc_main(int sc_argc, char* sc_argv[1])
{
  // See if we've been provided with a SEED.
  for (int i = 0; i < sc_argc; i += 1) {
    if (strcmp(sc_argv[i], "--SEED") == 0) {
      srand(atoi(sc_argv[i + 1]));
    }
  }
  //Create components
  Generator generator("mygenerator");
  Eater eater("muncher");
  SCWrappedFullAdder filter("myfilter");

  //Connect components
  filter.in = &generator.out;
  eater.in = filter.out;
  eater.src = &generator.src;

  //Simulate
  sc_start(1000.0, SC_SEC);
  return 0;    
}
