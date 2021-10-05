from hpsmc.tools import HPSTR 

job.description = 'fit rawsvthit 2dhistos to get offline baselines'

# Fit baselines
bl = HPSTR(run_mode=1, cfg='fit')

job.add([bl])
