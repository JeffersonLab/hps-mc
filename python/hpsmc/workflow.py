#!/usr/bin/env python

import argparse, os, json, glob

from hpsmc.job import JobParameters

class Workflow:
    
    base_params = ["job_id", "seed", "output_dir", "input_files", "output_files"]
    
    def __init__(self, json_file = None):
        jobs = []
        
        # TODO: make command line arg
        self.job_id_pad = 4

        if json_file:
            self.load(json_file)
    
    def parse_args(self):
        
        parser = argparse.ArgumentParser(description="Create a workflow with one or more jobs")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job number", default=1)
        parser.add_argument("-n", "--num-jobs", nargs="?", type=int, help="Number of jobs", default=1)
        parser.add_argument("-o", "--output-dir", nargs="?", help="Job output directory", default=os.getcwd())
        parser.add_argument("-r", "--random-seed", nargs="?", type=int, help="Base number for random seed", default=1)
        parser.add_argument("-w", "--workflow", nargs="?", help="Name of workflow", default="jobs")
        parser.add_argument("params", nargs="?", help="Job param template in JSON format", default="job.json")
        cl = parser.parse_args()
        
        self.job_start = cl.job_start
        self.num_jobs = cl.num_jobs
        self.params = JobParameters(cl.params)
        self.seed = cl.random_seed
        if cl.output_dir:
            self.output_dir = cl.output_dir
        else:
            self.output_dir = os.getcwd()
        self.workflow = cl.workflow
        self.job_store = cl.workflow + ".json"
        
        dir(self)
        
    def build(self):
        
        jobs = {self.workflow: {}}
                                        
        input_file_lists = {}
        input_file_count = {}
        for dest,src in self.params.input_files.iteritems():
            if isinstance(src, dict):
                src_files,ntoread = src.iteritems().next()
            else:
                src_files = src
                ntoread = 1
            
            flist = glob.glob(src_files)
            flist.sort()
            input_file_lists[dest] = flist
            input_file_count[dest] = ntoread
                
        for jobid in range(self.job_start, self.job_start + self.num_jobs):
            job = {}
            job["job_id"] = jobid
            job["seed"] = int(str(self.seed) + str(jobid))
            job["output_dir"] = self.output_dir
            job["input_files"] = {}
            
            if input_file_lists:
                for dest,src in input_file_lists.iteritems():
                    ntoread = input_file_count[dest]
                    if ntoread > 1:
                        for i in range(1, ntoread + 1):
                            job["input_files"][dest+"."+str(i)] = src.pop(0)
                    else:
                        job["input_files"][dest] = src.pop(0)
            
            job["output_files"] = {}
            job_id_padded = ("%0" + str(self.job_id_pad) + "d") % jobid                                            
            for src,dest in self.params.output_files.iteritems():
                base,ext = os.path.splitext(dest)
                if "." in base:
                    ext = dest[dest.find("."):]
                    base = base[:base.find(".")]
                dest_file = base + "_" + job_id_padded + ext
                job["output_files"][src] = dest_file            
            
            for k,v in self.params.json_dict.iteritems():
                if k not in Workflow.base_params:
                    job[k] = v
            
            jobs[self.workflow][self.workflow + "_"+ job_id_padded] = job
                
        with open(self.job_store, "w") as jobfile:
            json.dump(jobs, jobfile, indent=4, sort_keys=True)
            
        print json.dumps(jobs, indent=4, sort_keys=True)
    
    def load(self, json_store):
        print "loading JSON from '%s'" % json_store
        print
        rawdata = open(json_store, "r").read()
        data = json.loads(rawdata)
        self.jobs = data.itervalues().next()
                
if __name__ == "__main__":
    workflow = Workflow()
    workflow.parse_args()
    workflow.build()
