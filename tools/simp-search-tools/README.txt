In this directory, we include all the tools to perform tight selection optimization with the 2021 analysis.
It includes the following subdirectories: slurm-running, bdt-training ,ann-training, and plot-making. Here is a brief description of how to run everything.

slurm-running
This contains the 5 scripts to run tight selection optimization on slurm's sbatch. Except for a repository to contain signal and background distributions, the bdt joblib file, and the ann's scaling npz file and classifier and adversary pickle files, this should be self contained and runable. I suggest you toggle the other inputs to your desiring. Here is a brief description of each file and dependendcies:

scanner_run3.sh: submits ~12 files to sbatch slurm with a different index correspdoning to one of the scan values (either determining epsilon, mass, or one of the scan values for projected significance and/or ann, intuition cut, or bdt cut value.

scan_yields_array_v9_ALL.sh: slurm job that actually is run by roma, milano etc. Just evaluates the upcoming function writer for a copy of mass, epsilon, etc. You set the output directory of text files continaing persisting signal and background numbers here.

write_final_yields_v9_ALL3.py: the workhorse for this analysis. Takes dependences from decayLength8sel.py and bk_eff_selection.py to estimate the signal and background abundancies first and then apply fractional cuts to this. The input parameters feed into this and it applies consecutive tight selection (psum, proj sig, hit category, and ann/bdt/min y0) fractions to this and finally writes a text file with surviving signal, background, significance, and scan values used.

decayLength8sel.py: contains tools required to calculate F(z) (fractional acceptance), radiative fraction and acceptance fits, mass resolution curves, branching fractions, and effectively the vast majority of things required to calculate signal abundance.

bk_eff_selection.y: contains any tools deemed entirely corresponding to background. Largely obsolete, but should retain and you will need to input background root file locations in here.


plot-making
This file contains many/most of the scripts required for plotting tight optimization estimated yields (after optimizing things) as well as roc curves for individual scans (ann,bdt,miny0) to compare things relatively. The scripts contained therein are: ann_score_data_mc_overlay.py  make_maxZbi_grid_worker_v2.py  submit_maxZbi_grid.sh  write_roc_overlay_all3.py

ann_score_data_mc_overlay.py:
Plots the ann response curve for data and data-like MC on top of eachother given the locations of ann npz scaling files and classifier pickle files. Is used as a final test to establish that indeed it is not learning data MC discriminating features rather than signal background discriminating features.
 
make_maxZbi_grid_worker_v2.py: 
This file reads in the text files created by write_final_yields_v9_ALL3.py for cut, ann, bdt, and all hit categories and makes significance, signal, background yield plots for mass and epsilon as well as what cut values were used for those bins. Run with submit_maxZbi_grid.sh

submit_maxZbi_grid.sh:
Submits 12 jobs to run make_maxZbi_grid_worker_v2.py concurrently. Making the plot (from reading txt files) actually takes a decent bit, long enough that parrallelization acrosss slurm batches is necessary.

write_roc_overlay_all3.py:
Writes a comparative roc curve for the ann, bdt, and miny0 curves. Necessary to evaluate whether the slurm-running code is going to work (one often runs the long slurm job in there, finds one bungled the ann variable order in training and got very poor performance, and wasted 2 hours of slurm time and alot of machine resources if one doesn't do this first).

ann-training
This repository contains the code used to train the ann to be adversarial to mass training. It only contains the most recent version (i.e. the one that does mixed background and signal samples). I use a special command to run these python notebooks; I will include said argument here too.


