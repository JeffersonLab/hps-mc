from hpsmc.generators import MG4
from hpsmc.base import Job

mg4 = MG4(description="Generate tritrig events using MG4",
          name="tritrig",
          run_card="run_card_1pt05.dat",
          outputs=["tritrig"],
          nevents=100000)

job = Job(name="MG4 test",
    components=[mg4])

job.setup()
job.run()
job.cleanup()
