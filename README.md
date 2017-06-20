# hps-mc

## Prerequisites

There are a number of required programs and tools you must have installed on your system to build and run hps-mc.

- gcc - at least version 4.8
- CMake - at least version 3.0
- Maven - at least version 3.0
- python - tested with Python 2.7.13
- Java - tested with Java 1.8

The [slic](https://github.com/slaclab/slic) program should be pre-installed and avialable in the system path.

## Installation

You need to have CMake installed on your system as well as gcc (preferably version 4.8 or greater).

To build the project:

```
cd hps-mc; mkdir build; cd build
cmake -DCMAKE_INSTALL_PREFIX=../install ..
make install
```

This should install all of the tools to `hps-mc/install`.  

Change `CMAKE_INSTALL_PREFIX` above if you wish to install to a different directory.

## Environment Setup

Before running scripts, the environment script should be sourced.

This sources the bash environment setup script:

```
. hps-mc/install/bin/hps-mc-env.sh
```

This is the corresponding command for c-shell:

```
. hps-mc/install/bin/hps-mc-env.csh
```

The SLIC executable and DST Maker are "sold separately" so these commands should be setup in your environment.

You can check if they are available using:

```
which slic
which dst_maker
```

You should also check that typing `slic` and `dst_maker` executes these commands correctly.

## Running Job Scripts

You will want to run jobs from a scratch directory as many of components create a lot of files:

```
mkdir /scratch/myjob
cd /scratch/myjob
```

Now you can execute one of the test jobs:

```
python hps-mc/python/test/egs5_test.py
```

In the above command insert the full path to your hps-mc installation instead of `hps-mc` or you can copy the python script to the scratch directory.

## Help

You can post any questions or report problems on the [HPS Slack](https://heavyphotonsearch.slack.com/) in the [montecarlo](https://heavyphotonsearch.slack.com/messages/C47LLBP5F) channel.

The HPS software mailing list can also be used for help.
