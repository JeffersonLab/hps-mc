# Tests for moller_v5 (Moller scattering off bound electrons)

## Physics
EGS5 samples Moller scattering off free electrons at rest. `moller_v5` (`config/src/moller_v5.f`) adds,
in `ausgab(iarg=9)` right after each Moller interaction and before transport:

1. The free collision on the stack fixes the incoming electron, b = p1 + p2 - (m, 0). EGS5's Moller
   kinematics conserve 4-momentum exactly.
2. Boost to the free CM frame and record the CM angles (theta*, phi*) of electron 1 about the beam.
3. Sample the struck electron: shell i with probability n_i / Z, |p| from the isotropic radial density
   f(p) = -2 p dJ_i/dp, an isotropic direction, and t = (m - B_i, p). The ion absorbs -p.
4. In the CM frame of b + t, place the pair back to back at the same (theta*, phi*), with
   p* = sqrt(s'/4 - m^2).
5. Boost back to the lab and overwrite the stack. EGS5 then transports both electrons through the rest
   of the target, and the output cuts act on the corrected kinematics.

J_i are the Biggs, Mendelsohn and Mann (1975) shell profiles shipped in
`data/shellwise_Compton_profile/zNNN.dat`. Each is continued beyond the table end (100 a.u.) with a
hydrogen-like tail (1 + q^2/p0^2)^-3 matched to the last two points; about 15% of the W K-shell
electrons lie beyond the table. For E_beam >> m the rate per electron does not depend on p
(sigma ~ 1/s, flux ~ s), so no event weighting is needed.

Interactions are left free in two cases:
- the secondary's kinetic energy is below `bound_ekin_min` (default 1 MeV), where the impulse
  approximation does not hold;
- s' < 4 m^2, which needs the momentum component along the beam close to m (0.07% of hard collisions,
  K-shell tail only).

## Unit test: test_bound_moller.f
The test feeds exact free Moller collisions to `bndmol` and writes the before and after kinematics.
Build it in the EGS5 config build directory, which provides `include/` and the RNG sources:
```
cd <build>/egs5/config
awk '/^!-------------------------------bndini/{f=1} f' src/moller_v5.f > bnd_lib.f
cp <hps-mc>/generators/egs5/src/egs/randomset.f <hps-mc>/generators/egs5/src/egs/rluxinit.f .
gfortran -O2 -fno-automatic -o test_bound_moller.exe <hps-mc>/generators/egs5/test/test_bound_moller.f \
    bnd_lib.f randomset.f rluxinit.f
mkdir run && cd run && ln -s <install>/share/generators/egs5/data data
echo "1 74 1.0 0" > bound.dat
echo "500000 1920.0 0.26 0.5 777" | ../test_bound_moller.exe   # N, E_beam [MeV], x_lo, x_hi, seed
```
Results at 1.92 GeV, secondary fraction 0.26-0.5 (both electrons above 0.5 GeV), 500k collisions:

| Check | Result |
|-------|--------|
| 4-momentum balance, incl. t = (m - B, p) | max deviation 1.5e-7 MeV |
| Shell frequencies vs occupation | within statistics |
| Per-shell p_z vs J_i (KS test) | worst shell at 0.97 of the 1% critical value |
| sqrt(tan th1 tan th2), bound / free | core sigma 1.60% (0.369 mrad); 16.6% of events beyond 4% |
| Pair mass, bound / free | core sigma 1.60% (0.71 MeV at 44.3 MeV) |
| Closure: `bound_electrons = 0` vs moller_v4 (same seed) | identical events |
