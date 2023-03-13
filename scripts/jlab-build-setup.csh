module unload gcc
module unload cmake
module unload python3

module load gcc/7.2.0
module load cmake/3.22.1
module load python3/3.9.7
module load maven/3.5.2

setenv PATH $PATH\:/apps/maven/PRO/bin

setenv  CC `which gcc`
setenv CXX `which g++`

which cmake
which gcc
which g++
which python3
