#!/usr/bin/env csh

##################################
# Module setup script for JLab   #
# Run using `source` from ifarm. #
##################################

# Unload modules
module unload gcc
module unload cmake
module unload maven

# Load modules
module load gcc/7.2.0
module load cmake/3.22.1
module load maven/3.5.0

# Set environment variables for CMake
setenv  CC `which gcc`
setenv CXX `which g++`

# Print paths to tools
which cmake
which gcc
which g++
which mvn
