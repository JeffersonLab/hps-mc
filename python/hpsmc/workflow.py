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
        
        parser = argparse.ArgumentParser(description="Manage and create workflows with multiple jobs")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job number", default=1)
        parser.add_argument("-n", "--num-jobs", nargs="?", type=int, help="Number of jobs", default=1)
        parser.add_argument("-w", "--workflow", nargs="?", help="Name of workflow", required=True)
        parser.add_argument("-p", "--pad", nargs="?", type=int, help="Padding spaces for filenames (default is 4)", default=4)
        parser.add_argument("-d", "--database", help="Name of SQLite db file")
        parser.add_argument("script", help="Python job script")
        parser.add_argument("params", help="Job template in JSON format")
        cl = parser.parse_args()
                
        if not os.path.isfile(cl.params):
            raise Exception("The params file '%s' does not exist.")
        self.params = JobParameters(cl.params)
        
        self.job_script = os.path.abspath(cl.script)
        if not os.path.isfile(self.job_script):
            raise Exception("The job script '%s' does not exist.")
                
        self.job_start = cl.job_start
        self.num_jobs = cl.num_jobs    
        self.name = cl.workflow
        self.job_store = self.name + ".json"
        self.job_id_pad = cl.pad
        
        if cl.database:
            self.db = Database(cl.database)
            self.db.connect()
            self.db.create()
        else:
            self.db = None
        
    def build(self):
        
        jobs = {self.name: {}}
        jobs["job_script"] = self.job_script
                                                
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
            
        seed = self.params.seed
        output_dir = os.path.abspath(self.params.output_dir)
                
        for jobid in range(self.job_start, self.job_start + self.num_jobs):
            job = {}
            job["job_id"] = jobid
            job["seed"] = seed
            job["output_dir"] = output_dir
            job["input_files"] = {}
            
            if input_file_lists:
                for dest,src in input_file_lists.iteritems():
                    ntoread = input_file_count[dest]
                    if ntoread > 1:
                        for i in range(1, ntoread + 1):
                            job["input_files"][dest+"."+str(i).zfill(3)] = src.pop(0)
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
            
            jobs[self.name][self.name + "_"+ job_id_padded] = job
            
            seed += 1
                
        with open(self.job_store, "w") as jobfile:
            json.dump(jobs, jobfile, indent=4, sort_keys=True)

        print "Created '%s' for workflow '%s'" % (self.job_store, self.name)
        print json.dumps(jobs, indent=4, sort_keys=True)
        
        if self.db:  
            self.db.productions.insert(self.name, self.job_store)
            prod_id = self.db.productions.prod_id(self.name)
            d = jobs[self.name]
            for k in sorted(d):
                j = d[k]
                self.db.jobs.insert(j['job_id'], prod_id, str(j), k)
            self.db.commit()
    
    def load(self, json_store):
        rawdata = open(json_store, "r").read()
        data = json.loads(rawdata)
        self.job_script = data.pop("job_script", None)
        if not self.job_script:
            raise Exception("JSON file is missing 'job_script' key.")
        self.name = data.keys()[0]
        self.jobs = data[self.name]
        
    def delete(self):
        if self.name:
            if self.db:
                self.db.productions.delete(self.name)
                self.db.commit()
                print "Deleted workflow <%s>" % self.name
            else:
                raise Exception("The database is not enabled.")
        else:
            raise Exception("The name field is not set.")
