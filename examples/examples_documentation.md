Examples  {#examples}
========

HPS-MC contains an extensive list of examples of running different components. These examples are designed to run on every machine that has a working installation of hps-mc. In addition to that, they should run relatively fast so that they allow new users to get a quick and extensive overview hps-mc and allow easy testing of functionality. Please see below for a list of all examples. The list contains links to the documentation of each example.

- @subpage apgentoslic
- @subpage beamcoords
- @subpage beamgen
- @subpage beamslic
- @subpage datacnv
- @subpage dummy
- @subpage feeslictorecon
- @subpage hpstr
- @subpage mollergen
- @subpage radgen
- @subpage readoutrecon
- @subpage simp
- @subpage slictoanaMC
- @subpage tritrigbeam
- @subpage tritriggen
- @subpage tritrigslicfullchain
- @subpage wabgensample

#### Job parameters    {#params}
Each job in hps-mc is run with a `job.json` parameter list. These configuration files contain several parameters, the most common of which are explained below.

| param         |                                                                                        |
|---------------|----------------------------------------------------------------------------------------|
| input_files   | input files used by job:<br>"input_file": "internal_input_name"                        |
| output_files  | files that are written to the output_dir:<br>"internal_output_name": "output_file"     |
| output_dir    | output directory                                                                       |
| nevents       | number of events generated/processed                                                   |
| bunches       | number of bunches?                                                                     |
| seed          | seed for random number generation                                                      |
| run_params    | key for run parameter lookup in python/hpsmc/run\_params.py                            |
| target_z      | z position of target                                                                   |
| run_number    | ?                                                                                      |

#### Job template
Some examples include a job template (`job.json.tmpl`) and a script (`mkjobs.sh`) to create a `jobs.json` file that contains the configuration for multiple jobs. The template uses several variables that are defined in `vars.json`. There is a dedicated example directory for using the job template, @subpage jobtemplate.