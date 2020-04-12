"""
Job script to generate A-prime events, convert to StdHep and apply transformations.

Run with '--job-steps 1' to only generate the untransformed LHE output.
"""

import sys, os, argparse

from hpsmc.job import Job
from hpsmc.generators import MG4, StdHepConverter
from hpsmc.tools import Unzip, StdHepTool, DisplaceTime, FileFilter, BeamCoords

job = Job()

# Generate A-prime events using MadGraph4
ap = MG4(name="ap")

# Filter out unweighted events
filt = FileFilter(excludes=['unweighted'])

# Unzip the LHE events to a local file, excluding the unweighted event file
unzip = Unzip()

# Create a stdhep file, displacing the time of decay using the ctau param
displace = DisplaceTime()

# Rotate events into beam coordinates and move the vertices
rotate = BeamCoords()

# Add components to the job
job.add([ap, filt, unzip, displace, rotate])

# Run the job
job.run()
