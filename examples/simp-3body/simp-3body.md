SIMP-3Body Generation
=====================

This is an example of how to use the `simp-3body` MadGraph generator.
The job generates SIMP events using MadGraph5 having the dark vector meson
undergo a 3-body decay rather than a resonant 2-body decay.

#### Job parameters
The parameters for this generation are the same for the 2-body decay generation.
The main difference is changing the name from `simp` to `simp-3body` in two key places.
1. In the constructor of the `MG5` job step. `name = "simp-3body"` so the 3-body decay generator is called.
2. In the output file name whever it is referenced (either in the JSON job config or in the next job step).
```
simp_unweighted_events.lhe.gz -> simp-3body_unweighted_events.lhe.gz
```
