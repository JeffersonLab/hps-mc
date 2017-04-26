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

print "target_z = %d" % target_z
print "num_electrons = %d" % num_electrons
print "ebeam = %d" % ebeam
print "num_electrons * bunches = %d" % total_electrons

egs5 = EGS5()
egs5.run_params = rp 
egs5.setup()
egs5.run()