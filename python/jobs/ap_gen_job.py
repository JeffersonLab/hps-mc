#!/usr/bin/env python

"""
Job script to generate A-prime events, convert to StdHep and apply transformations.

Run with '--job-steps 1' to only generate the untransformed LHE output.
"""

import sys, os, argparse

from hpsmc.job import Job
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import Unzip, StdHepTool, MoveFiles

job = Job()

# Generate A-prime events using MadGraph4
ap = MG4(name="ap")

# Unzip the LHE events to a local file, excluding the unweighted event file
unzip = Unzip(exclude=['unweighted'])

# Create a stdhep file, displacing the time of decay using the ctau param
displace = StdHepTool(name="lhe_tridents_displacetime")

# Rotate events into beam coordinates and move the vertices
rotate = StdHepTool(name="beam_coords",
                    append="_rotated")

# Print the final events
dump = StdHepTool(name="print_stdhep")

# Add components to the job
job.add([ap, unzip, displace, rotate, dump])

# Run the job
job.run()
