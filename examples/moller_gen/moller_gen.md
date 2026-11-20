Moller event generation {#mollergen}
=======================

This is an example of how to use moller_gen_job.py. The job generates moller events using egs5 and transforms the output to beam coordinates.

#### Job parameters
There are no special run parameters that need to be set for this job other than the general ones discussed on the [examples main page](@ref examples).

#### Job template
This example includes a job template. The variables used in this template mostly have the same name and meaning as the job parameters above. Their values are set in `vars.json`.
#### Moller scattering off bound electrons (moller_v5)
EGS5 treats the struck target electron as free and at rest. The `moller_v5` program samples it instead from
the shell-wise Hartree-Fock Compton profiles of the target atom (impulse approximation, the "Levchuk effect"),
which smears the Moller opening angle and pair mass by about 1.6% (W target). Select it and configure it with
these optional job parameters:

| Parameter | Default | Meaning |
|-----------|---------|---------|
| `egs5_program` | `moller_v3` | EGS5 program; use `moller_v5` for bound electrons |
| `bound_electrons` | on in `moller_v5` | 1 = bound electrons, 0 = free electrons at rest (identical to `moller_v4`) |
| `bound_target_z` | 74 | atomic number for `data/shellwise_Compton_profile/zNNN.dat` |
| `bound_ekin_min` | 1.0 | minimum kinetic energy (MeV) of the Moller secondary for the correction |
| `bound_debug` | 0 | number of corrected interactions written to `bound_debug.txt` |

For example, add to `job.json`:
```
    "egs5_program": "moller_v5",
    "bound_electrons": 1,
```
The algorithm and its validation are described in `generators/egs5/test/README.md`.
