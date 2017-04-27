from hpsmc import run_params
from run_params import RunParameters
from generators import EGS5

print "----- EGS5 test job -----"

rp = RunParameters(key="4pt4")
egs5 = EGS5("beam_v5")
egs5.run_params = rp 
egs5.setup()
egs5.run()