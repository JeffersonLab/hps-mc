from hpsmc.generators import MG4, StdHepConverter
from hpsmc.base import Job
from hpsmc.run_params import RunParameters
from hpsmc.tools import Unzip

ap = MG4(description="Generate A-prime events with APMASS param",
         name="ap",
         run_card="run_card_1pt05.dat",
         params={"APMASS": 40.0},
         outputs=["ap"],
         nevets=1000)

unzip = Unzip(inputs=["ap_events.lhe.gz"])

# ${stdhep_dir}/lhe_tridents_displacetime in.lhe out.stdhep -s{seed} -l1
          
job = Job(name="AP test",
    components=[ap, unzip])

job.setup()
job.run()
job.cleanup()
