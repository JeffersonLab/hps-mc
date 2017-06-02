import gzip
from hpsmc.run_params import RunParameters

"""
Calculate integrated luminosity.
"""
def lint(run_params, density = 6.306e-2):
    w = run_params.get("target_z")    
    ne = run_params.get("num_electrons")
    #print "lint=%f"%(density*w*ne)
    return density*w*ne

"""
Extract cross-section from gzipped LHE file.
"""
def csection(filename):
    with gzip.open(filename, 'rb') as in_file:
        lines = in_file.readlines()

    for line in lines:
        if "Integrated weight" in line:
            xs = float(line[line.rfind(":")+1:].strip())          
            break

    if "xs" not in locals():
        raise Exception("Could not find 'Integrated weight' in LHE input file.")

    #print "xs=%d"%xs

    return xs

"""
Calculate mu = number of events per 500k bunches (???).
"""
def mu(run_params, filename):
    return lint(run_params) * 1e-12 * csection(filename)
