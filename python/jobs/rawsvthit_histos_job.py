from hpsmc.tools import EvioToLcio, HPSTR

job.description = "EVIO converter -> HPSTR ntuple -> HPSTR RawSvtHit 2dhistos | used for performing offline baseline fits"

cnv = EvioToLcio(steering='recon')

tuple = HPSTR(run_mode=1, cfg='tuple')

hh = HPSTR(run_mode=1, cfg='hh')

job.add([cnv, tuple, hh])
