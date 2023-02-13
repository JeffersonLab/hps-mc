pede minimizer {#pede}
==============
This example uses pede_job.py to optimize the alignment parameters. The inputs are recursively searched for `*.bin` files that we assume were prepared by hps-java during the tracking step.
These data files are then used to run the minimization scheme within pede.
Finally, the results form pede are applied to the detector description file (`compact.xml`), generating a new iteration of the detector.

# Parameters
There are several special parameters that are separated depending on if they are required and how often they change between jobs.

#### Requried Config
These parameters are required but do not change often so they are put into a config file (e.g. `.hpsmc` in this directory) rather than the job JSON.

| param              | description                                                              |
|--------------------|--------------------------------------------------------------------------|
| param\_map         | map of SVT sensors listing names, ID numbers, and other location details |
| pede\_minimization | pede minimization settings to append to its steering file                |
| java\_dir          | root directory of hps-java for applying pede results                     |


#### Required Job
These parameters are required and change often depending on the iteration and what alignment you are studying, so they are in the job JSON.

| param         | description                                                                 |
|---------------|-----------------------------------------------------------------------------|
| inputs        | list of `bin` files or directories in which to recursively search for them  |
| output\_files | mapping the pede output files to their destination names                    |
| to\_float     | list of pede parameters to allow to float during minimization               |
| detector      | name of detector that tracking was run on                                   |

#### Optional Job
These parameters are optional and only some of them are tested. They are provided in the job JSON as well.

| param                 | description                                                                       |
|-----------------------|-----------------------------------------------------------------------------------|
| subito                | if true, use the `-s` pede flag which is a rougher minimization focusing on speed |
| constraint\_file      | path to external constraint file pede should load before running minimizer        |
| previous\_fit         | path to previous results (`res`) file to start fit from                           |
| beamspot\_constraints | if true, apply the beamspot constraints                                           |
| survey\_constraints   | if true, apply the survey constraints                                             |
| res\_file             | name of millepede.res file (if not "millepede.res", only helpful if separate)     |
| bump                  | if false, *do not* update the iteration number of detector when applying results  |
| force                 | if true, ignore any existing detector when creating a new directory               |

