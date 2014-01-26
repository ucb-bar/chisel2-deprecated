################################################
# Setup tcl script for delay backannotation    #
#    Donggyu Kim(dgkim@eecs.berkeley.edu)      #
#                                              #
# This file does:                              #
#    1. Specify libraries                      #
#    2. Set variables to preserve signal names #
#    3. Load a generated tcl file              #
################################################

set chisel_out_home [getenv CHISEL_OUT_HOME]

#----------------------------------------------------------
# Specify libraries
# You should appropriately modify this part
# accourding to your system environment
#----------------------------------------------------------
set ucb_vlsi_home [getenv UCB_VLSI_HOME]
set stdcells_home $ucb_vlsi_home/stdcells/[getenv UCB_STDCELLS]
set search_path "$stdcells_home/db ../"
set target_librarly "celss.db"
set synthetic_library "standard.sldb"
set link_library "* $target_library $synthetic_library"
set alib_library_analysis_path "alib"

#----------------------------------------------------------
# Set variables to preserve signals
# We recommend not to edit this part
#----------------------------------------------------------
set hdlin_infer_mux all
set hdlin_keep_signal_name all

#----------------------------------------------------------
# Load a generated tcl file from the Chisel backend
#----------------------------------------------------------
source gendctcl.tcl

# Quit DC
exit
