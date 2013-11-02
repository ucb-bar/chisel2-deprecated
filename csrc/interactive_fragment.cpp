#include "emulator.h"   // TODO: replace with generated module header

#include <iostream>
#include <sstream>

using namespace std;

// shamelessly copied from http://stackoverflow.com/questions/236129/how-to-split-a-string-in-c
std::vector<std::string> &split(const std::string &s, char delim, std::vector<std::string> &elems) {
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) {
        elems.push_back(item);
    }
    return elems;
}
std::vector<std::string> split(const std::string &s, char delim) {
    std::vector<std::string> elems;
    split(s, delim, elems);
    return elems;
}

int main() {
    cout << "Init ... " << endl; 
    mod_t mod;  // TODO: replace with generated module class
    mod.init();
    cout << "  done" << endl; 
    
    vector<string> nodes_str = mod.get_nodes();
    cout << "Nodes: " << nodes_str.size() << endl; 
    for (int i=0; i<nodes_str.size(); i++) {
        cout << "  " << nodes_str[i] << endl;
    }
    
    vector<string> mems_str = mod.get_mems();
    cout << "Mems: " << mems_str.size() << endl; 
    for (int i=0; i<mems_str.size(); i++) {
        cout << "  " << mems_str[i] << endl;
    }
    
    mod.clock(dat_t<1>(1));
    
    while(1) {
        string str;
        cout << "> ";
        getline(cin, str);
        vector<string> command = split(str, ' ');
        if (command.size() < 1) {
            cout << "No command" << endl;
            continue;
        }
        if (command[0] == "rd") {
            if (command.size() < 2) {
                cout << "Invalid format" << endl;
                continue;
            }
            cout << mod.node_read(command[1]) << endl;
        } else if (command[0] == "wd") {
            if (command.size() < 3) {
                cout << "Invalid format" << endl;
                continue;
            }
            cout << mod.node_write(command[1], command[2]) << endl;
        } else if (command[0] == "cl") {
            mod.clock(dat_t<1>(0));
            cout << "Cycle" << endl;
        } else if (command[0] == "cleq") {
            if (command.size() < 3) {
                cout << "Invalid format" << endl;
                continue;
            }
            int cycles = 0;
            while (mod.node_read(command[1]) != command[2]) {
                mod.clock(dat_t<1>(0));
                cycles++;
            }
            cout << "Ran " << cycles << "cycles";
        } else if (command[0] == "clne") {
            if (command.size() < 3) {
                cout << "Invalid format" << endl;
                continue;
            }
            int cycles = 0;
            while (mod.node_read(command[1]) == command[2]) {
                mod.clock(dat_t<1>(0));
                cycles++;
            }
            cout << "Ran " << cycles << "cycles";
        } else {
            cout << "Invalid command " << command[0] << endl;
        }
    }
}
