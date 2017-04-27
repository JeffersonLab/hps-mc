from hpsmc import run_params
from run_params import RunParameters
from generators import EGS5

print "----- EGS5 test job -----"

# FIXME: hard-coded number of bunches
beam_bunches = 5e5

# FIXME: hard-coded beam energy
beam_energy = "4pt4"

# FIXME: hard-coded program name
program = "beam_v5"

# FIXME: hard-coded job num used for seed data (I think???)
num = 1

rp = RunParameters(key=beam_energy)

egs5 = EGS5("beam_v5")
egs5.run_params = rp 
egs5.setup()
egs5.run()