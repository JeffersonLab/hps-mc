#!/usr/bin/env python

import argparse, os, json, glob
from collections import OrderedDict

from hpsmc.job import JobParameters
from hpsmc.db import Database, Productions

class Workflow:
    
    base_params = ["job_id", "seed", "output_dir", "input_files", "output_files"]
    
    def __init__(self, json_file = None):
        jobs = []
        
        if json_file:
            self.load(json_file)
    
    def parse_args(self):
        
        parser = argparse.ArgumentParser(description="Create a workflow with one or more jobs")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job number", default=1)
        parser.add_argument("-n", "--num-jobs", nargs="?", type=int, help="Number of jobs", default=1)
        parser.add_argument("-w", "--workflow", nargs="?", help="Name of workflow", required=True)
        parser.add_argument("-p", "--pad", nargs=1, type=int, help="Padding spaces for filenames (default is 4)", default=4)
        parser.add_argument("-d", "--database", help="Name of SQLite db file")
        parser.add_argument("params", nargs="?", help="Job template in JSON format")
        cl = parser.parse_args()
        
        if cl.params:
            self.params = JobParameters(cl.params)
        else:
            parser.print_usage()
            raise Exception("Missing param file argument.")
        
        self.job_start = cl.job_start
        self.num_jobs = cl.num_jobs    
        self.workflow = cl.workflow
        self.job_store = cl.workflow + ".json"
        self.job_id_pad = cl.pad
        
        if cl.database:
            self.db = Database(cl.database)
        else:
            self.db = None
        
        dir(self)
        
    def setup(self):
        if self.db:
            self.db.connect()
            self.db.create()
            
    def cleanup(self):
        if self.db:
            self.db.commit()
            self.db.close()
        
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
            job["seed"] = self.params.seed
            job["output_dir"] = self.params.output_dir
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
        
        if self.db:
            prod = self.db.productions()
            jobdb = self.db.jobs()
            prod.insert(self.workflow, self.job_store)
            prod_id = prod.prod_id(self.workflow)
            d = jobs[self.workflow] 
            for k in sorted(d):
                j = d[k]
                jobdb.insert(j['job_id'], prod_id, str(j))
    
    def load(self, json_store):
        print "loading JSON from '%s'" % json_store
        print
        rawdata = open(json_store, "r").read()
        data = json.loads(rawdata)
        self.jobs = data.itervalues().next()
                
if __name__ == "__main__":
    workflow = Workflow()
    workflow.parse_args()
    workflow.setup()
    workflow.build()
    workflow.cleanup()
