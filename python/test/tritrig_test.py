from hpsmc.generators import MG4, StdHepConverter
from hpsmc.run_params import RunParameters 
from hpsmc.base import Job

mg4 = MG4(name="tritrig", run_card="run_card_1pt05.dat", outputs=["tritrig"])

stdhep_cnv = StdHepConverter(run_params=RunParameters(key="1pt05"),
    inputs=["tritrig.stdhep"], outputs="tritrig.stdhep")

job = Job(name="tritrig test", components=[mg4, stdhep_cnv])

job.setup()
job.run()
job.cleanup()
