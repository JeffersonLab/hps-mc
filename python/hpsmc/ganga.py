#!/usr/bin/env python

#################################################
#                                               #
# Staggered submission of LSF jobs using Ganga. # 
#                                               #
#################################################

import argparse, time, sys
from ganga import *

def subjobs(wf, nsub, waittime, maxjobs):

    if not jobtree.exists("/" + wf):
        raise Exception("The workflow dir '/" + wf + "' does not exist!")
    jobtree.cd("/" + wf)

    subjobs = []
    subcount = 0
    while True:
        jobsrunning = True
        runMonitoring()
        if len(subjobs) != 0:
            for jobid in subjobs:
                if jobs(jobid).status not in ['running', 'failed', 'completed']:
                    print "Some jobs from the last submission have not run yet."
                    jobsrunning = False
                    break
        newjobs = sorted(jobtree.getjobs().select(status='new'), key=lambda j: j.id)
        if len(newjobs) != 0:
            if jobsrunning:
                subjobs = []
                for j in newjobs[0:nsub]:
                    subjobs.append(j.id)
                print "Submitting next " + str(len(subjobs)) + " jobs."
                for j in subjobs:
                    print "Submitting job: " + str(j)
                    try:
                        jobs(j).submit()
                        subcount += 1
                        if subcount >= maxjobs:
                            print "Reached max jobs of %d." % maxjobs
                            break
                    except Exception as e:
                        print "Error during job submission: " + e.message
                        if jobs(j).status != 'new':
                            jobs(j).force_status('failed')
                print "Submitted jobs: " + str(subjobs)
            if subcount < maxjobs:
                print "Sleeping for " + str(waittime) + " seconds ..."
                time.sleep(waittime)
                print "Done sleeping!"
        else:
            print "No more jobs to submit."
            break
        if subcount >= maxjobs:
            break

if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Staggered job submission to LSF using Ganga interface")
    parser.add_argument("-w", "--workflow", nargs="?", help="Name of the workflow (e.g. name of top-level job dir in Ganga)", required=True)
    parser.add_argument("-n", "--njobs", nargs="?", type=int, default=200, help="Number of jobs to submit in one batch")
    parser.add_argument("-t", "--time", nargs="?", type=int, default=60*5, help="Number of seconds to wait between batch submissions")
    parser.add_argument("-m", "--max", nargs="?", type=int, default=sys.maxint, help="Maximum number of jobs to submit")
    cl = parser.parse_args()

    wf = cl.workflow
    nsub = cl.njobs
    waittime = cl.time
    maxjobs = cl.max

    print "Submitting workflow <" + wf + "> with batch size " + str(nsub) + " and wait time " + str(waittime)

    subjobs(wf, nsub, waittime, maxjobs)
