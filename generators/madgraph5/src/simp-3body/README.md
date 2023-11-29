# 3-Body SIMP Decay

![Feynman Diagram](SubProcesses/P1_emn_emnap_ap_rhodpid_rhod_pidepem/matrix1.ps)

The MadGraph model (and the FeynRules model it was derived from) were given to me (Tom Eichlersmith)
by Nikita Blinov originally developed for [ArXiV:1801.05805](https://arxiv.org/abs/1801.05805).

## Difference Relative to 2-Body SIMP
The main, physical difference between this generator and the
[resonant, 2-body decay one](../simp) is that we intentionally asked MadGraph
for a 3-body decay rather than a 2-body decay.
```
generate e- n > e- n ap /z h DQND=0, (ap > rhod pid, rhod > pid e+ e-)
```
rather than
```
generate e- n > e- n ap /z h DQND=0, (ap > rhod pid, rhod > e+ e-)
```
when creating this MadEvent workspace.

The astute among you may also notice other differences, specifically in the
ufomodel that is used. These other differences are due to the fact that the
model used here was generated with a later version of Mathematica and FeynRules
compared to the model used in the resonant, 2-body one. The differences do
not seem to affect the actual kinematic distributions and so they are ignored.
