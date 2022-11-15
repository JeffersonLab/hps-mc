Production scripts {#prod}
==================

This directory is meant to document the scripts and samples in a clean way. For specific examples on how to use hps-mc, please refer to @ref examples.
The production pipeline is split into three stages:
- generation (gen): event generation using MadGraph or egs5
- slic: simulation of the detector response and all necessary steps leading up to this, e.g.
  - adding mother particles
  - rotating into beam coordinates
  - sampling of event
- readout and reconstruction (recon): simulation of detector readout and subsequent reconstruction of event
  - includes conversion of output files to root format
  - can be extended to include analysis

The production directory is separated into production using SLAC or JLab machines.
