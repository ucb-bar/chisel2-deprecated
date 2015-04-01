#include <systemc>
using namespace std;
using namespace sc_core;
using namespace sc_dt;
#include "SCWrappedAddFilter.cpp"

SC_MODULE(Counter){
  sc_fifo<dat_t<16> > out;

  SC_HAS_PROCESS(Counter);
  Counter(sc_module_name a_name) : sc_module(a_name), out(1){
    SC_THREAD(counter_thread);
  }

  void counter_thread(void){    
    for(int i=1; i<100; i++){
      out.write(i);
      wait(20.0, SC_SEC);
    }
  }
};

SC_MODULE(Eater){
  sc_fifo<dat_t<16> >* in;

  SC_HAS_PROCESS(Eater);
  Eater(sc_module_name a_name) : sc_module(a_name){
    SC_THREAD(eater_thread);
  }

  void eater_thread(void){
    while(true){
      dat_t<4> data = in->read();      
      printf("Output = %llu\n", data.values[0]);
    }
  }
};

int sc_main(int sc_argc, char* sc_argv[1]){

  //Create components
  Counter counter("mycounter");
  Eater eater("muncher");
  SCWrappedAddFilter filter("myfilter");

  //Connect components
  filter.a = &counter.out;
  eater.in = filter.b;


  //Simulate
  sc_start(1000.0, SC_SEC);
  return 0;    
}
