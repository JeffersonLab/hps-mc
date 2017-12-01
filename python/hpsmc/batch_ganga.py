#!/usr/bin/env python

#################################################
#                                               #
# Staggered submission of LSF jobs using Ganga. # 
#                                               #
#################################################

import argparse, time, sys, os
import ganga
from hpsmc.workflow import Workflow

def to_ascii(str):
    """Coerce unicode strings to ASCII for Ganga.""" 
    return str.encode('ascii', 'ignore')

def add_to_ganga(wf, job_dir, workdir):
    
    file_checker_path = os.path.dirname(os.path.realpath(__file__)) + os.path.sep + "file_checker.py"
    file_checker = ganga.CustomChecker(module = file_checker_path)
  
    job_names = wf.get_job_names()
    ganga.jobtree.mkdir(job_dir)
    ganga.jobtree.cd(job_dir)
    jobs = wf.get_jobs()
    for k in job_names:
        jobdict = wf.get_jobs()[k]
        gj = ganga.Job()
        gj.application.exe = 'python'
        jobfile = to_ascii(k + ".json")
        jobfile = wf.workdir + "/" + jobfile 
        gj.comment = to_ascii("job " + str(int(jobdict["job_id"])) + " in '" + wf.name + "'")
        gj.application.args = [to_ascii(wf.job_script), to_ascii(jobfile)]
        gj.backend = ganga.LSF()
        gj.backend.queue = 'long'
        gj.backend.extraopts = '-W 24:0'
        gj.name = to_ascii(k)
        gj.parallel_submit = True
        gj.postprocessors.append(file_checker)
        ganga.jobtree.add(gj)
        print "Added Ganga job <" + k + "> with id " + str(gj.id) + " and JSON config <" + jobfile + ">"          

def subjobs(wf, nsub, waittime, maxjobs):

    if not ganga.jobtree.exists("/" + wf.name):
        raise Exception("The workflow dir '/" + wf + "' does not exist!")
    ganga.jobtree.cd("/" + wf.name)

    subjobs = []
    subcount = 0
    while True:
        jobsrunning = True
        ganga.runMonitoring()
        if len(subjobs) != 0:
            for jobid in subjobs:
                if ganga.jobs(jobid).status not in ['running', 'failed', 'completed']:
                    print "Some jobs from the last submission have not run yet."
                    jobsrunning = False
                    break
        newjobs = sorted(ganga.jobtree.getjobs().select(status='new'), key=lambda j: j.id)
        if len(newjobs) != 0:
            if jobsrunning:
                subjobs = []
                for j in newjobs[0:nsub]:
                    subjobs.append(j.id)
                print "Submitting next " + str(len(subjobs)) + " jobs."
                for j in subjobs:
                    print "Submitting job: " + str(j)
                    try:
                        ganga.jobs(j).submit()
                        subcount += 1
                        if subcount >= maxjobs:
                            print "Reached max jobs of %d." % maxjobs
                            break
                    except Exception as e:
                        print "Error during job submission: " + e.message
                        if ganga.jobs(j).status != 'new':
                            ganga.jobs(j).force_status('failed')
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
    parser.add_argument("-n", "--njobs", nargs="?", type=int, default=1000, help="Number of jobs to submit in one batch")
    parser.add_argument("-t", "--time", nargs="?", type=int, default=0, help="Number of seconds to wait between batch submissions")
    parser.add_argument("-m", "--max", nargs="?", type=int, default=sys.maxint, help="Maximum number of jobs to submit")
    parser.add_argument("-w", "--workdir", nargs=1, help="Work dir where JSON and XML files will be saved", required=False)
    parser.add_argument("jobstore", nargs=1, help="Job store in JSON format")
    cl = parser.parse_args()

    wf = Workflow(cl.jobstore[0])

    nsub = cl.njobs
    waittime = cl.time
    maxjobs = cl.max
    workdir = cl.workdir

    print "Adding jobs to Ganga ..."
    add_to_ganga(wf, wf.name, workdir)
 
    print "Submitting workflow <" + wf.name + "> with batch size " + str(nsub) + " and wait time " + str(waittime)
    subjobs(wf, nsub, waittime, maxjobs)
