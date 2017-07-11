#!/usr/bin/env python

"""
Job for concatenating LCIO files together.

This script ignores the provided output destination and automatically names the output file
from the input source with the sequence numbers of the input files processed appended.
"""

import os
from hpsmc.job import Job
from hpsmc.tools import LCIOConcat

def split_file_number(filename):
    basefile = os.path.basename(os.path.splitext(filename)[0])
    file_number = basefile[basefile.rfind('_')+1:]
    basefile = basefile[:basefile.rfind('_')]
    return basefile, file_number

job = Job(name="LCIO concat job")
job.initialize()

input_files = sorted(job.params.input_files.keys())
if len(input_files) < 2:
    raise Exception("Not enough input files were provided (at least 2 required).")

"""
If no output file mapping is provided explicitly, then an auto-naming scheme is used which
concatenates the start and end file numbers onto the base name of the first input file.
"""
if not len(job.params.output_files):
    
    output_basename,start_filenum = split_file_number(job.params.input_files[input_files[0]])
    dontcare,end_filenum = split_file_number(job.params.input_files[input_files[-1]])
    
    print output_basename
    print start_filenum
    print end_filenum
    
    try:
        int(start_filenum)
    except:
        raise Exception("Start file number '%s' has bad format." % start_filenum)
    try:
        int(end_filenum)
    except:
        raise Exception("End file number '%s' has bad format." % end_filenum)
    output_src = output_basename + "_" + start_filenum + "-" + end_filenum + ".slcio"
    job.params.output_files[output_src] = output_src
else:
    output_src = job.params.output_files.keys()[0]
    
concat = LCIOConcat(inputs=input_files, outputs=[output_src])

job.components = [concat]
job.run()
