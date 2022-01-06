"""Miscellaneous math functions."""

import gzip
import logging

logger = logging.getLogger("hpsmc.func")

"""
Calculate integrated luminosity.
"""
def lint(run_params, density = 6.306e-2):
    w = run_params.get("target_z")
    ne = run_params.get("num_electrons")
    return density*w*ne

"""
Extract cross-section from gzipped LHE file.
"""
def csection(filename):

    logger.info("Using gzip to open '%s'" % filename)

    with gzip.open(filename, 'rb') as in_file:
        lines = in_file.readlines()

    for line in lines:
        if "Integrated weight" in line:
            xs = float(line[line.rfind(":")+1:].strip())
            break

    if "xs" not in locals():
        raise Exception("Could not find 'Integrated weight' in LHE input file.")

    return xs

"""
Calculate mu = number of events per bunch.
"""
def mu(filename, run_params):
    return lint(run_params) * 1e-12 * csection(filename)

"""
Read number of events in file from LHE header and optionally confirm by counting <event> blocks.
"""
def nevents(filename, confirm = False):

    with gzip.open(filename, 'rb') as in_file:
        lines = in_file.readlines()

    for line in lines:
        if "nevents" in line:
            nevents = int(line.split()[0])

    if "nevents" not in locals():
        raise Exception("Could not find 'nevents' in LHE input file.")

    if confirm:
        event_count = 0
        for line in lines:
            if "<event>" in line:
                event_count += 1
        if event_count != nevents:
            raise Exception("The number of events %d from header does not match the count %d in file '%s'." % (nevents, event_count, filename))

    return nevents

"""
Get the approximate number of beam bunches represented by an LHE file from its event count.
"""
def nbunches(filename, run_params):
    n = nevents(filename)
    m = mu(filename, run_params)
    return int(n/m)

# TODO: wab LHE file fixup
"""
echo "Transmuting A's to photons..."
gunzip -f wab.lhe.gz
sed -i 's/\([:blank:]*\)622 /\1 22 /' wab.lhe
gzip wab.lhe
"""
