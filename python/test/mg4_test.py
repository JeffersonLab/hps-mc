from hpsmc.generators import MG4, StdHepConverter
from hpsmc.base import Job
from hpsmc.run_params import RunParameters

mg4 = MG4(description="Generate tritrig events using MG4",
          name="tritrig",
          run_card="run_card_1pt05.dat",
          outputs=["tritrig"],
          nevents=1000)

stdhep_cnv = StdHepConverter(description="Convert LHE events to StdHep using EGS5",
                             run_params=RunParameters(key="1pt05"),
                             inputs=["tritrig_events.lhe.gz"],
                             outputs=["tritrig.stdhep"])

job = Job(name="MG4 test",
    components=[mg4, stdhep_cnv])

job.setup()
job.run()
job.cleanup()
