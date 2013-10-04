#/bin/bash -f 

for i in 00_installation 01_the_basics 02_bits_and_uint 03_module_instantiation 04_writing_chisel_testcases 05_creating_projects 06_registers_memories 07_scripting_construction 
do
  (cd $i; pdflatex $i.tex)
done
