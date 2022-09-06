# hps-mc installation guide  {#readme}
### Prerequisites

There are a number of required programs and tools you must have installed on your system to build and run hps-mc.

- wget
- git
- gcc >= 4.8
- CMake >= 3.18
- Maven >= 3.0
- python >= 3.6
- Java =1.8
- GSL =[1.16](https://ftp.gnu.org/gnu/gsl/gsl-1.16.tar.gz)

Most of these dependencies can be installed on Ubuntu using:

```
sudo apt-get install build-essential maven gfortran cmake python3 libgsl-dev git wget
```

You may want to [download](https://cmake.org/download/) and install the latest version of CMake instead of using the one from your package manager so that it is up to date.

A few Python libraries need to be installed as well:

```
pip install psutil jinja2
```

The [slic](https://github.com/slaclab/slic) program should be pre-installed and available in the system path.

Some scripts may also require that the [DST Maker](https://github.com/JeffersonLab/hps-dst) is installed and available in the system path.

### Installation

You need to have CMake installed on your system as well as gcc (preferably version 4.8 or greater).

To build the project:

```
cd hps-mc; mkdir build; cd build
cmake -DCMAKE_INSTALL_PREFIX=../install -DGSL_ROOT_DIR=$GSL_ROOT_DIR ..
make install
```

The `GSL_ROOT_DIR` variable should be set to the GSL installation prefix which was used when you configured it (directory should contain the directories bin, include, etc.).

To build with additional external dependencies installed automatically:

```
cmake -DCMAKE_INSTALL_PREFIX=$(realpath ../install) -DGSL_ROOT_DIR=/work/slac/sw/gsl/gsl-1.16-install/ -DENABLE_INSTALL_GENERATORS=ON -DENABLE_INSTALL_FIELDMAPS=ON -DENABLE_INSTALL_LCIO=ON -DENABLE_INSTALL_HPSJAVA=ON -DENABLE_INSTALL_CONDITIONS=ON ..
```

This should install all of the tools to `hps-mc/install`.  

Change `CMAKE_INSTALL_PREFIX` above if you wish to install to a different directory.

### Environment Setup

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

### Running Job Scripts

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

### Documentation
The source code documentation can be sound [here](https://jeffersonlab.github.io/hps-mc/). For additional information of how to use the job scripts, refer to the examples directory and the comments in there.


### Quick Start Guide

The first step to setting up an system to run jobs via hps-mc is first producing `~/.hpsmc` which will set all of the system level configuration parameters for all types of jobs. An example of what a system level .hpsmc file should look like is provided at `config/bravo_sdf.cfg`. One can simply copy this to `~/.hpsmc` of the system and update the paths to point to the relevant locations on that particular system.

The system level configuration file should not be confused with a job level configuration file, typcially kept in the directory used to prepare and submit the jobs. hps-mc will go looking in a few standard locations for configuration files named `.hpsmc` and load any configurations in them. The local directory where the submission command is issued is the last place searched and parameters in this location will override all others, if any conflicts exist. An example of a job level configuration file is provided at `config/job.cfg` as well as more in the various example jobs provided in the examples directory.

The next step of the process is to build up a jobs.json file with all of the relevant job configurations. Looking in `examples/tritrig_gen` one can see a few different scripts as examples of how one can put together a jobs.json file to be used to submit/run jobs using the various available computing resources hps-mc knows how to leverage. Following the example of `examples/tritrig_gen` one can navigate to this directory and inspect `mkjobs.sh` to see the command for automatically generating a jobs.json configuration. The provided job.json configuration only defines a single job, and can not be used to submit jobs to a batch system and is used for local testing purposes. `hps-mc-job-template` takes in a few different types of arguements to tell it how you want to generate your jobs. 

Parameters one might want to iterate over in jobs can be entered into lists in vars.json, the example uses this to generate the sample with two different target positions, just to illustrate this feature. Another important arguement for `hps-mc-job-template` is job.json.tmpl. This is the filename for the job template to be used to define all the parameters for one particular job. Variables provided in the vars.json file can be accessed in job.json.tmpl inside of double curly braces, and jinja2 support is available for processing the variables. An example of this feature is the seed parameter in `examples/tritrig_gen` where an offset is added to `job_id` (which is always an available template variable) to make a unique seed number for each job. Jinja2 is a well documented templating engine on which this part of hps-mc is based. 

After one has generated a jobs.json file, the jobs it defines can be ran locally, in a pool, or submitted to one of the various batch systems it supports via `hps-mc-batch`. In `examples/tritrig_gen` there are a few examples of submission commands in `run_pool.sh`, `submit_lsf.sh`, and `run_slurm.sh`. Slurm in particular will generate script to use to submit each individual job. With some small modifications one can use these to prepare a job array to submit to slurm, if one wants to use that particular feature of the Slurm system. Ultimately, hps-mc just prepares a single run command using `hps-mc-job` which the batch tool knows how to prepare and call properly, given the configuration and parameters of each job, to get the job running in any of the supported run modes (local, pool, or some batch system). 

It is recommended to slowly build up jobs and tests them locally first before submitting a whole set to a batch system to run. This can be with commands nearly identical to the commands used to submit the jobs to a batch system (local or pool). The system is pretty abstracted so you might even find a new clever way to leverage the features of hps-mc to speed up the process of defining job sets, or even just getting the jobs ran.

### Help

You can post any questions or report problems on the [HPS Slack](https://heavyphotonsearch.slack.com/) in the [montecarlo](https://heavyphotonsearch.slack.com/messages/C47LLBP5F) channel.

The HPS software mailing list can also be used for help.
