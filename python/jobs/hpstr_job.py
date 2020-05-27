from hpsmc.tools import HPSTR 

# Convert LCIO to ROOT
cnv = HPSTR(cfg='recon')

# Run an analysis on the ROOT file
ana = HPSTR(cfg='ana')

job.add([cnv, ana])
