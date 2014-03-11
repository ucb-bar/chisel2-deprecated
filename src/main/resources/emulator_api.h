// Header for Chisel emulator API
#ifndef __IS_EMULATOR_API__
#define __IS_EMULATOR_API__

#include "emulator_mod.h"

#include <string>
#include <sstream>
#include <map>

/**
 * Converts an integer to a std::string without needing additional libraries
 * or C++11.
 */
static std::string itos(int in) {
	std::stringstream out;
	out << in;
	return out.str();
}

/**
 * Copy one val_t array to another.
 * nb must be the exact number of bits the val_t represents.
 */
static void val_cpy(val_t* dst, val_t* src, int nb) {
    for (int i=0; i<val_n_words(nb); i++) {
        dst[i] = src[i];
    }
}

/**
 * Empty a val_t array (sets to zero).
 * nb must be the exact number of bits the val_t represents.
 */
static void val_empty(val_t* dst, int nb) {
    for (int i=0; i<val_n_words(nb); i++) {
        dst[i] = 0;
    }
}

/**
 * Set a val_t array to a integer number. Obviously, the maximum integer
 * is capped by the width of a single val_t element.
 * nb must be the exact number of bits the val_t represents.
 */
static void val_set(val_t* dst, val_t nb, val_t num) {
    val_empty(dst, nb);
    dst[0] = num;
}

/**
 * Sets a dat_t from a string, where the input radix is automatically determined
 * from the string (or defaults to 10).
 * Returns true on success.
 */
template <int w>
bool dat_from_str(std::string in, dat_t<w>& res, int pos = 0) {
    int radix = 10;

    if (!in.substr(pos, 1).compare("d")) {
        radix = 10;
        pos++;
    } else if (!in.substr(pos, 1).compare("h")
               || !in.substr(pos, 1).compare("x")) {
        radix = 16;
        pos++;
    } else if (!in.substr(pos, 2).compare("0h")
               || !in.substr(pos, 2).compare("0x")) {
        radix = 16;
        pos += 2;
    } else if (!in.substr(pos, 1).compare("b")) {
        radix = 2;
        pos++;
    } else if (!in.substr(pos, 2).compare("0b")) {
        radix = 2;
        pos += 2;
    }

    val_t radix_val = radix;
    val_t temp_prod[val_n_words(w)];
    val_t curr_base[val_n_words(w)];
    val_t temp_alias[val_n_words(w)];
    val_t *dest_val = res.values;
    val_set(curr_base, w, 1);
    val_empty(dest_val, w);

    for (int rpos=in.length()-1; rpos>=pos; rpos--) {
        char c = in[rpos];
        val_t c_val = 0;
        if (c == '_') {
            continue;
        }
        if (c >= '0' && c <= '9') {
            c_val = c - '0';
        } else if (c >= 'a' && c <= 'z') {
            c_val = c - 'a' + 10;
        } else if (c >= 'A' && c <= 'Z') {
            c_val = c - 'A' + 10;
        } else {
            std::cerr << "dat_from_str: Invalid character '" << c << "'" <<
            		std::endl;
            return false;
        }
        if (c_val > radix /* || c_val < 0 */) {
            std::cerr << "dat_from_str: Invalid character '" << c << "'" <<
            		std::endl;
            return false;
        }

        mul_n(temp_prod, curr_base, &c_val, w, w, val_n_bits());
        val_cpy(temp_alias, dest_val, w);   // copy to prevent aliasing on add
        add_n(dest_val, temp_alias, temp_prod, val_n_words(w), w);
        val_cpy(temp_alias, curr_base, w);
        mul_n(curr_base, temp_alias, &radix_val, w, w, val_n_bits());
    }
    return true;
}

// API base class, providing common functions
class api_base {
public:
	api_base(std::string new_name, std::string new_path) :
		name(new_name),
		path(new_path)
	{}
	// returns the fully qualified name of this object (path + dot + name)
	std::string get_pathname() {
		if (path.empty()) {
			return name;
		} else {
			return path + "." + name;
		}
	}
	// returns the short name of this object
	std::string get_name() {
		return name;
	}
	// returns the path of this object (without a trailing dot)
	std::string get_path() {
		return path;
	}
protected:
	std::string name;
	std::string path;
};

// API base (non width templated) class for API accessors to dat_t
class dat_api_base : public api_base {
public:
	dat_api_base(std::string new_name, std::string new_path) :
		api_base(new_name, new_path)
	{}
	// returns the value of this wire as a string, or empty string on failure
	virtual std::string get_value() = 0;
	// sets the value of this wire from a string, returning true on success
	virtual bool set_value(std::string value) = 0;
	// returns the bitwidth of this wire
	virtual std::string get_width() = 0;
};

// dat_api dummy class, does nothing except for return errors
// to be used when a real dat_api object can't be found
class dat_dummy : public dat_api_base {
public:
	dat_dummy() :
		dat_api_base("", "")
	{}
	std::string get_value() {
		return "";
	}

	bool set_value(std::string value) {
		return false;
	}

	std::string get_width() {
		return "";
	}
};

template<int w> class dat_api : public dat_api_base {
public:
	dat_api(dat_t<w>* new_dat, std::string new_name, std::string new_path) :
		dat_api_base(new_name, new_path),
		dat_ptr(new_dat)
	{}

	std::string get_value() {
		return dat_ptr->to_str();
	}

	bool set_value(std::string value) {
		return dat_from_str<w>(value, *dat_ptr);
	}

	std::string get_width() {
		return itos(w);
	}

protected:
	dat_t<w>* dat_ptr;
};

// API base (non width/depth templated) class for API accessors to mem_t
class mem_api_base : public api_base {
public:
	mem_api_base(std::string new_name, std::string new_path) :
		api_base(new_name, new_path)
	{}
	// return the value of an element as a string, or empty string on failure
	virtual std::string get_element(std::string index) = 0;
	// sets the value of an element from a string, returning true on success
	virtual bool set_element(std::string index, std::string value) = 0;
	// returns the bitwidth of a memory element
	virtual std::string get_width() = 0;
	// returns the number of memory elements
	virtual std::string get_depth() = 0;
};

// mem_api dummy class, does nothing except for return errors
// to be used when a real mem_api object can't be found
class mem_dummy : public mem_api_base {
public:
	mem_dummy() :
		mem_api_base("", "")
	{}
	string get_element(std::string index) {
		return "";
	}

	bool set_element(std::string index, std::string value) {
		return false;
	}

	std::string get_width() {
		return "";
	}

	std::string get_depth() {
		return "";
	}
};

template<int w, int d> class mem_api : public mem_api_base {
public:
	mem_api(mem_t<w, d>* new_mem, std::string new_name, std::string new_path) :
		mem_api_base(new_name, new_path),
		mem_ptr(new_mem)
	{}

	string get_element(std::string index) {
		int index_int = atoi(index.c_str());
		return mem_ptr->contents[index_int].to_str();
	}

	bool set_element(std::string index, std::string value) {
		int index_int = atoi(index.c_str());
		return dat_from_str<w>(mem_ptr->contents[index_int]);
	}

	std::string get_width() {
		return itos(w);
	}

	std::string get_depth() {
		return itos(d);
	}

protected:
	mem_t<w, d>* mem_ptr;
};

class mod_api_t {
public:
	void init(mod_t* new_module) {
		module = new_module;
		init_mapping_table();
	}

	mod_t* get_module() {
		return module;
	}

	// API basic functions
	std::string get_host_name() {return "C++ Emulator API";}
	std::string get_api_version() {return "0";}
	std::string get_api_support() {return "PeekPoke Introspection";}

	// External access functions & helpers
	std::vector< std::string > tokenize(std::string str) {
	    std::vector< std::string > res;
	    int i = 0;
	    int c = ' ';
	    while ( i < str.size() ) {
	      while (isspace(c)) {
	        if (i >= str.size()) return res;
	        c = str[i++];
	      }
	      std::string s;
	      while (!isspace(c) && i < str.size()) {
	        s.push_back(c);
	        c = str[i++];
	      }
	      if (i >= str.size()) s.push_back(c);
	      if (s.size() > 0)
	        res.push_back(s);
	    }
	    return res;
	}

	// helper to verify command length, returning false and printing an error
	// to stderr if the length isn't in the specified range
	bool check_command_length(std::vector<std::string>& tokenized_command,
			int min_args, int max_args=-1) {
		if (tokenized_command.size() - 1 < min_args) {
			std::cerr << tokenized_command[0] << " expects at least " << min_args
					<< " args, got " << tokenized_command.size() - 1
					<< std::endl;
			return false;
		} else if (max_args >= 0 && tokenized_command.size() - 1 > max_args) {
			std::cerr << tokenized_command[0] << " expects at most " << max_args
					<< " args, got " << tokenized_command.size() - 1
					<< std::endl;
			return false;
		}
		return true;
	}

	std::string eval_command(string command) {
		std::vector<std::string> tokens = tokenize(command);
		if (tokens[0] == "get_host_name") {
			if (!check_command_length(tokens, 0, 0)) { return ""; }
			return get_host_name();
		} else if (tokens[0] == "get_api_version") {
			if (!check_command_length(tokens, 0, 0)) { return ""; }
			return get_api_version();
		} else if (tokens[0] == "get_api_support") {
			if (!check_command_length(tokens, 0, 0)) { return ""; }
			return get_api_support();

		} else if (tokens[0] == "clock" || tokens[0] == "step") {
			if (!check_command_length(tokens, 1, 1)) { return ""; }
			int cycles = atoi(tokens[1].c_str());
		    for (int i=0; i<cycles; i++) {
		    	module->clock_lo(dat_t<1>(0));
		    	module->clock_hi(dat_t<1>(0));
		    }
		    module->clock_lo(dat_t<1>(0));
		    return itos(cycles);

		} else if (tokens[0] == "reset") {
			if (!check_command_length(tokens, 0, 1)) { return ""; }
			int cycles = 1;
			if (tokens.size() >= 2) {
				cycles = atoi(tokens[1].c_str());
			}
			for (int i=0; i<cycles; i++) {
			   	module->clock_lo(dat_t<1>(1));
			   	module->clock_hi(dat_t<1>(1));
		    }
		    module->clock_lo(dat_t<1>(0));
		    return itos(cycles);

		} else if (tokens[0] == "peek") {
			if (!check_command_length(tokens, 1, 2)) { return ""; }
			cerr << "peek is deprecated, use node_peek or mem_peek" << std::endl;
			if (tokens.size() == 2) {
				return get_dat_by_name(tokens[1])->get_value();
			} else if (tokens.size() == 3) {
				return get_mem_by_name(tokens[1])->get_element(tokens[2]);
			}
		} else if (tokens[0] == "poke") {
			if (!check_command_length(tokens, 2, 3)) { return ""; }
			cerr << "poke is deprecated, use node_poke or mem_poke" << std::endl;
			bool success;
			if (tokens.size() == 3) {
				success = get_dat_by_name(tokens[1])->set_value(tokens[2]);
			} else if (tokens.size() == 4) {
				success = get_mem_by_name(tokens[1])->set_element(tokens[2], tokens[3]);
			}
			return success ? "true" : "false";

		} else if (tokens[0] == "node_peek") {
			if (!check_command_length(tokens, 1, 1)) { return ""; }
			return get_dat_by_name(tokens[1])->get_value();
		} else if (tokens[0] == "node_poke") {
			if (!check_command_length(tokens, 2, 2)) { return ""; }
			bool success = get_dat_by_name(tokens[1])->set_value(tokens[2]);
			return success ? "true" : "false";
		} else if (tokens[0] == "mem_peek") {
			if (!check_command_length(tokens, 2, 2)) { return ""; }
			return get_mem_by_name(tokens[1])->get_element(tokens[2]);
		} else if (tokens[0] == "mem_poke") {
			if (!check_command_length(tokens, 3, 3)) { return ""; }
			bool success = get_mem_by_name(tokens[1])->set_element(tokens[2], tokens[3]);
			return success ? "true" : "false";

		} else if (tokens[0] == "list_nodes") {
			if (!check_command_length(tokens, 0, 0)) { return ""; }
			std::string out = "";
			for (std::map<string, dat_api_base*>::iterator it = dat_table.begin(); it != dat_table.end(); it++) {
				out = out + it->second->get_pathname() + " ";
			}
			if (out.size() >= 1) {
				return out.substr(0, out.size() - 1);
			} else {
				return "";
			}

		} else if (tokens[0] == "list_mems") {
			if (!check_command_length(tokens, 0, 0)) { return ""; }
			std::string out = "";
			for (std::map<string, mem_api_base*>::iterator it = mem_table.begin(); it != mem_table.end(); it++) {
				out = out + it->second->get_pathname() + " ";
			}
			if (out.size() >= 1) {
				return out.substr(0, out.size() - 1);
			} else {
				return "";
			}

		} else if (tokens[0] == "node_width") {
			if (!check_command_length(tokens, 1, 1)) { return ""; }
			return get_dat_by_name(tokens[1])->get_width();
		} else if (tokens[0] == "mem_width") {
			if (!check_command_length(tokens, 1, 1)) { return ""; }
			return get_mem_by_name(tokens[1])->get_width();
		} else if (tokens[0] == "mem_depth") {
			if (!check_command_length(tokens, 1, 1)) { return ""; }
			return get_mem_by_name(tokens[1])->get_depth();

		} else {
			std::cerr << "Unknown command: '" << tokens[0] << "'" << std::endl;
		}
		return "";
	}

	void read_eval_print_loop() {
		while (true) {
		    std::string str_in;
		    getline(cin, str_in);
		    if (str_in == "quit") {
		    	break;
		    } else {
		    	cout << eval_command(str_in) << std::endl;
		    }
		}
	}

protected:
	mod_t* module;

	// Mapping table functions
	virtual void init_mapping_table() = 0;

	dat_api_base* get_dat_by_name(std::string name) {
		if (dat_table.find(name) != dat_table.end()) {
			return dat_table[name];
		} else {
			std::cerr << "Unable to find dat '" << name << "'" << std::endl;
			return &this_dat_dummy;
		}
	}
	mem_api_base* get_mem_by_name(std::string name) {
		if (mem_table.find(name) != mem_table.end()) {
			return mem_table[name];
		} else {
			std::cerr << "Unable to find mem '" << name << "'" << std::endl;
			return &this_mem_dummy;
		}
	}

	std::map<string, dat_api_base*> dat_table;
	std::map<string, mem_api_base*> mem_table;
	dat_dummy this_dat_dummy;
	mem_dummy this_mem_dummy;
};

#endif
