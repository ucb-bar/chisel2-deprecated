#################################################
# Setup tcl script for delay backannotation     #
#    Donggyu Kim(dgkim@eecs.berkeley.edu)       #
#                                               #
# This file does:                               #
#    1. Specify libraries                       #
#    2. Set variables to preserve signal names  #
#    3. Read the designs                        #
#    4. Compile                                 #
#    5. Load a generated tcl file               #
#################################################

#----------------------------------------------------------
# Specify libraries
# You should appropriately modify this part
# accourding to your system environment
#----------------------------------------------------------
set ucb_vlsi_home [getenv UCB_VLSI_HOME]
set stdcells_home $ucb_vlsi_home/stdcells/[getenv UCB_STDCELLS]
set search_path "$stdcells_home/db ./ ../"
set target_library "cells.db"
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
# Read the designs
#----------------------------------------------------------
set design_name [getenv DESIGN_NAME]
analyze -format verilog "$design_name.v"
elaborate "$design_name"
link
check_design
create_clock clk -name ideal_clock1 -period 1

#----------------------------------------------------------
# Read the designs
#----------------------------------------------------------
compile -only_hold_time

#----------------------------------------------------------
# Load a generated tcl file from the Chisel backend
# - Generate timing reports
#----------------------------------------------------------
set dctcl_name [getenv DCTCL_NAME]
source "$dctcl_name"

#----------------------------------------------------------
# Quit DC
#----------------------------------------------------------
exit
