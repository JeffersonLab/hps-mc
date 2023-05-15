# iDM
**i**elastic **D**ark **M**atter

Credit to Stefania Gori from UCSC for original development of MadGraph5 Model
for [ArXiV:1804.00661](https://arxiv.org/abs/1804.00661).

Another helpful resource is [ArXiV:1807.01730](https://arxiv.org/abs/1807.01730)
which provides some more background on the theoretical landscape. My reading
of this paper has led me to conclude that this model should, more accurately,
be called "Pseudo-Dirac Inelastic Dark matter" and the mass splitting $\Delta$
(or here `dMchi`) will need to be larger than $2m_e$ to keep the lifetime of
the dark photon short enough for the decay to be visible by HPS.

## Set Up
Playing with this model requires a local copy of MadGraph5.
1. Download MG5 from [its launchpad](https://launchpad.net/mg5amcnlo).
2. Unpack it somewhere `tar xzvf MG5_aMC_v3.4.2.tar.gz && rm MG5_aMC_v3.4.2.tar.gz`
3. Symlink this directory into the `models` directory of MG5.
    - The name of the model to use with the `import model` command in MG5
      is the same as the name of the directory in `models`, for this README,
      I will assume that this model has been linked without changing its name.
4. Import the model into MG5 `MG5_aMC>import model idm --modelname`
    - Using the `--modelname` option informs MG5 to use the names for particles
      from the model

## Brief MG/ME Primer
The general flow is
1. Define model (particles and interactions between particles): `import model <sub-dir-of-models>`
2. Use model to define diagrams (MadGraph - MG part): `generate` command
3. Output model into MadEvent (ME) code: `output` command
4. Run ME code to generate events and compute cross sections: 
   `launch` command or `./bin/generate_events` directly

# Notes on Model

### Generate Events and Widths
Full iDM cascade (requiring a real dark photon and a real heavy dark fermion)
```
generate e- n > e- n ap, (ap > chi2 chi1, chi2 > chi1 e+ e-)
```
Heavier dark fermion width
```
generate chi2 > chi1 e+ e-
```
Dark bremsstrahlung (produce dark photon)
```
generate e- n > e- n ap
```
Dark photon width
```
generate ap > chi2 chi1
```

### New Particles
Particle | ID      | Description
---------|---------|---------------
n        | 9000002 | target nucleus
ap       | 1023    | dark photon
chi1     | 1000022 | lighter DM
chi2     | 1000023 | heavier DM

### Parameters
Listed here are the relevent entries in the param card.
Each entry in the param card is in a certain block and has
a short name after it in a comment.

Short Name | Block | Value | Description
-----------|---|-------|-------------
Mchi | dm | 1e-1 | Avg DM Mass $m_\chi = (m_2+m_1)/2$
dMchi | dm | 2e-2 | DM Mass Splitting $\Delta = m_2-m_1$
Map | hidden | 1 | dark photon mass
WAp | decay | 8.252e-4 | dark photon decay width
Wchi2 | decay | 1e-3 | heavier DM decay width
MHS | hidden | 200 | dark higgs mass (can ignore)
epsilon | hidden | 1e-2 | kinetic mixing (can ignore / want to study independently)
kap | hidden | 1e-9 | dark higgs quartic (can ignore)
aXM1 | hidden | 1.279e2 | $1/\alpha_D$ (set to $1/\alpha_{EW}$)
GAN | frblock | 3.028177e-1 | nucleus and standard photon coupling
Anuc | frblock | 184 | atomic weight of nucleus (set to tungsten)
Znuc | frblock | 74 | atomic number of nucleus (set to tungsten)

## Generate MadEvent Workspace
```
./bin/mg5_aMC
import model idm --modelname
generate e- n > e- n ap, (ap > chi2 chi1, chi2 > chi1 e+ e-)
output eN-iDM
```
Now the directory `eN-iDM` is a "stand-alone" MadEvent "program"
where we can tune the model parameters in `Cards/param_card.dat`
and change the run parameters in `Cards/run_card.dat`.

### param\_card
- Adjust `Mchi`, `dMchi`, and `Map` to study model of interest

### run\_card
*Necessary*:
- Lower `ebeam1` (energy of incident electron) to 2.3 GeV from 500.0
- Lower `ebeam2` (energy of target nucleus) to tungsten mass 174 GeV to have it be stationary
- Remove `ptl`, `etal`, and `drll` cuts to open up phase space
  - Applying fiducial cuts will be done later in the HPS simulation pipeline

*Optional*:
- Update run tag to something helpful

