"""
Job script to generate A-prime events, convert to StdHep, and apply transformations.
"""

from hpsmc.generators import MG4
from hpsmc.tools import Unzip, StdHepTool, DisplaceTime, BeamCoords

# Generate A-prime events using MadGraph4
ap = MG4(name="ap", event_types=['unweighted'])

# Unzip the LHE events to a local file
unzip = Unzip()

# Create a stdhep file, displacing the time of decay using the ctau param
displace = DisplaceTime()

# Rotate events into beam coordinates and move the vertices
rotate = BeamCoords()

# Add components to the job
job.add([ap, unzip, displace, rotate])
