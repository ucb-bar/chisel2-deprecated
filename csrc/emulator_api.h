// Header for Chisel emulator API
#ifndef __IS_EMULATOR_API__
#define __IS_EMULATOR_API__

#include <string>
#include <map>

using namespace std;

template<int w> class dat_accessor {
	string get_value();
	void set_value(string value);
};

template<int w, int d> class mem_accessor {
	string get_value(string index);
	void set_value(string index, string value);
};

struct dat_record {
	void* dat_t_ptr;
	string path;
	int width;
};

struct dat_record {
	void* mem_t_ptr;
	string path;
	int width;
	int depth;
};

class mod_api_t {
public:
	virtual mod_t get_module();

	// Helper functions


	// API basic functions
	string get_host_name() {return "C++ Emulator API";}
	string get_api_version() {return "0";}
	string get_api_support() {return "PeekPoke Introspection";}

	// API basic peek/poke functions
	string node_peek(string node_path);
	void node_poke(string node_path, string value);
	string mem_peek(string mem_path, string mem_index);
	void mem_poke(string mem_path, string mem_index, string value);
	void clock(string cycles);
	void reset();

	// API introspection functions
	string list_nodes();
	string list_mems();
	string node_width(string node_path);
	string node_type(string node_path);
	string mem_width(string mem_path);
	string mem_depth(string mem_path);

	// External access functions
	string eval_command(string command);
	void read_eval_print_loop(FILE *f);

private:
	// Mapping table functions
	virtual init_mapping_table();

	map<string, dat_record> dat_table;
	map<string, mem_record> mem_table;
};

#endif
