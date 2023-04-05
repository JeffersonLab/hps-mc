misalignment {#misalign}
============
This example uses the misalign\_job.py to construct a new hps-java detector after 
intentionally misaligning certain millepede parameters. This job is relatively
simple and short, but it opens the door for users to use hps-mc's jinja engine
to quickly construct many detectors following different patterns.

# Parameters
There are several special parameters that are separated depending on if they are required and how often they change between jobs.

#### Requried Config
These parameters are required but do not change often so they are put into a config file (e.g. `~/.hpsmc`).

| param              | description                                                              |
|--------------------|--------------------------------------------------------------------------|
| param\_map         | map of SVT sensors listing names, ID numbers, and other location details |
| java\_dir          | root directory of hps-java for applying pede results                     |


#### Required Job
These parameters are required and change often depending on the iteration and what alignment you are studying, so they are in the job JSON.

| param          | description                                                                 |
|----------------|-----------------------------------------------------------------------------|
| parameters     | dictionary of pede parameters and their shifts in values (see below)        |
| detector       | name of detector that we should base our misaligned detector on             |
| next\_detector | name of detector to create (including the misalignments)                    |

#### Optional Job
These parameters are optional and only some of them are tested. They are provided in the job JSON as well.

| param                 | description                                                                       |
|-----------------------|-----------------------------------------------------------------------------------|
| force                 | if true, ignore any existing detector when creating a new directory               |

## parameters
The **parameters** job parameter is more complicated than the others, so it deserves its own section.
It is a dictionary of different so-called Patterns each of which can be an ID number or a composition of
several different boolean operations in order to construct a group of paramters to shift. If a
single alignment parameter "matches" a Pattern in the dictionary, it will be given that shift.

More detail about the Pattern syntax is given in its documentation: `hpsmc.alignment._pattern.Pattern`

#### Example Patterns
Within each pattern, different operations are separated by `&` to reflect that all of them must be true
for a Parameter to match that Pattern.
- `"direction=u & operation=t" : 0.01` would shift all Parameters that translate along the local u access by 0.01mm (or 10um).
- `"top & front & tu" : 0.02` would shift all Parameters that translate along the local u axis in the front (first 4 layers), top half of the detector by 0.02mm (20um)

