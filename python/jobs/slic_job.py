from hpsmc.tools import SLIC
#from hpsmc.tools import SLIC, LCIOCount, LCIODumpEvent

job.description = 'SLIC'

job.add(SLIC())

#job.add(LCIOCount())

#job.add(LCIODumpEvent())
