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
| input\_files  | text file listng `bin` files or the list of `bin` files themselves          |
| output\_files | mapping the pede output files to their destination names                    |
| to\_float     | list of pede parameters to allow to float during minimization (see below)   |
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
| next\_detector        | if set, use this name for the new iteration of the detector (with parameters applied) |
| no\_copy      | don't use the destination file as `inputs` to pede, use the source          |

The `no\_copy` parameter is helpful when you have a local set of `bin` files that you want to run over. Often times, the `pede` program tries to run over the input files before they are done being loaded into the kernel cache and so you see a "Open error" (example below). This parameter can avoid this error by just having `pede` look at the original source file rather than the copy in the scratch directory.
```
 Open error for file events.bin           2
STOP FILETC: open error                                      
hpsmc.job:INFO Execution of pede took 0.0129 second(s) with return code: 0
```

## to\_float
The to\_float job parameter is more complicated than the others, so it deserves its own section.
It is a list of different so-called Pattern each of which can be an ID number of a composition of
several different boolean operations in order to construct a group of paramters to float. If a
single alignment parameter "matches" _any_ of the Patterns in the list, then it will be floated.

More detail about the Pattern syntax is given in its documentation: `hpsmc.alignment._pattern.Pattern`

#### Example Patterns
Within each pattern, different operations are separated by `&` to reflect that all of them must be true
for a Parameter to match that Pattern.
- `"direction=u & operation=t"` would match Parameters that translate along the local u axis
  - this is so common, it has an alias: `"tu"`
- `"top & front & tu"` would match Parameters that are individual sensors in front (first 4 layers), top half
  of the detector and translate along the local u axis

You can also select parameters using logical or by separating Patterns into different elements of the list.
- `["top & front & tu", "top & front & rw"]` would match Parameters that are individual sensors in the front,
  top half of the detector and either translate along u or rotate around w.
  - `"rw"` is an alias for `"direction=w & operation=r"`
