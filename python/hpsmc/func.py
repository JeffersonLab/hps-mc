"""! Miscellaneous math functions."""

import gzip
import logging

logger = logging.getLogger("hpsmc.func")


def lint(run_params, density=6.306e-14):
    """!
    Calculate integrated luminosity.
    @param run_params  run parameter
    @param density  1/(cm*pb), default value is for tungsten
    @return integrated luminosity in 1/pb
    """
    w = run_params.get("target_z")
    ne = run_params.get("num_electrons")
    return density * w * ne


def csection(filename):
    """!
    Extract cross-section from gzipped LHE file.
    WARNING: This function does not work!

    \todo remove or replace by more useful function
    @param filename  name of input file
    """
    logger.info("Using gzip to open '%s'" % filename)

    with gzip.open(filename, 'rb') as in_file:
        lines = in_file.readlines()

    for line in lines:
        if "Integrated weight" in line:
            xs = float(line[line.rfind(":") + 1:].strip())
            break

    if "xs" not in locals():
        raise Exception("Could not find 'Integrated weight' in LHE input file.")

    return xs


def mu(filename, run_params):
    """!
    Calculate mu = number of events per bunch.
    WARNING: This function does not work properly because csection() is broken!

    @param filename  name of input LHE input file containing cross section
    @param run_params  run parameters to calculate integrated luminosity from
    @return number of events per bunch (L_int[1/pb] * xsec[pb])
    """
    return lint(run_params) * csection(filename)


def nevents(filename, confirm=False):
    """!
    Read number of events in file from LHE header and optionally confirm by counting <event> blocks.
    @param filename  name of input LHE file
    @param confirm  set to True to confirm number of events
    """
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


def nbunches(filename, run_params):
    """!
    Get the approximate number of beam bunches represented by an LHE file from its event count.
    @param filename  name of input LHE file
    @param run_params  run parameter
    """
    n = nevents(filename)
    m = mu(filename, run_params)
    return int(n / m)


# TODO: wab LHE file fixup
"""
echo "Transmuting A's to photons..."
gunzip -f wab.lhe.gz
sed -i 's/\\([:blank:]*\\)622 /\1 22 /' wab.lhe
gzip wab.lhe
"""
