"""
Hadd root files
"""

from hpsmc.tools import Hadd
from os import listdir
from os.path import isfile, join

job.description = 'Hadd root files'

if 'infile_dir' in job.params:
    infile_dir = job.params['infile_dir']
else:
    print("ERROR. NO INPUT FILE DIR SPECIFIED")
hh_files = ["%s/%s"%(infile_dir,f) for f in listdir(infile_dir) if (isfile(join(infile_dir,f)) and 'hh' in f)]

hadd = Hadd(inputs=hh_files, outputs=['2dhistos_hadd.root'])

job.add([hadd])
