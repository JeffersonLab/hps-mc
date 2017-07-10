#!/usr/bin/env python

"""
Job for concatenating LCIO files together.
"""

import os
from hpsmc.job import Job
from hpsmc.tools import LCIOConcat

job = Job(name="LCIO concat job")
job.initialize()

input_files = sorted(job.params.input_files.keys())
if len(input_files) < 2:
    raise Exception("Not enough input files were provided (at least 2 required).")

"""
If the name of the output file and its destination are the same, then the file numbers
used in the job are appended to it.  In the case that the destination file has a different
name, it is assumed that the user wants to use that file name explicitly rather than have
these numbers automatically added.
"""
output_src = job.params.output_files.keys()[0]
if output_src == job.params.output_files[output_src]:
    start_filenum = os.path.splitext(input_files[0])[1][1:]
    end_filenum = os.path.splitext(input_files[-1])[1][1:]
    output_basename = os.path.splitext(os.path.basename(output_src))[0]
    try:
        int(start_filenum)
    except:
        raise Exception("Start file number '%s' has bad format." % start_filenum)
    try:
        int(end_filenum)
    except:
        raise Exception("End file number '%s' has bad format." % end_filenum)
    output_dest = output_basename + "_" + start_filenum + "-" + end_filenum + ".slcio"
    job.params.output_files[output_src] = output_dest
    
concat = LCIOConcat(inputs=input_files, outputs=[output_src])

job.components = [concat]

job.run()
