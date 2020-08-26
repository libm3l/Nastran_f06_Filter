# Nastran_f06_Filter
This file is a simple filter which reads in Nastran f06 file, exports GRID poins to an ASCII file and then modal displacements for each mode to a separate file

To compile do something like

gfortran -o Nastran_f06_Filter.out Nastran_f06_Filter.f90 

Input is the name of file and number of modes

