#!/usr/bin/env csh

##################################
# Module setup script for JLab   #
# Run using `source` from ifarm. #
##################################

module unload gcc
module unload cmake
module unload python3

module load gcc/7.2.0
module load cmake/3.22.1
# Use the system Python instead 
#module load python3/3.9.7
module load maven/3.5.2

# TODO: Remove this after Maven module is fixed (CCPR was submitted).
setenv PATH $PATH\:/apps/maven/apache-maven-3.5.0/bin

setenv  CC `which gcc`
setenv CXX `which g++`

which cmake
which gcc
which g++
which python3
