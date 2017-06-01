import argparse, os, json, glob

from job import JobParameters

class Workflow:
    
    base_params = ["job_num", "seed", "output_dir", "input_files", "output_files"]
    
    def __init__(self):
        jobs = []
        
        # TODO: make command line arg
        self.job_num_pad = 4
    
    def parse_args(self):
        
        parser = argparse.ArgumentParser(description="Create a workflow with one or more jobs")
        parser.add_argument("-j", "--job-start", nargs=1, type=int, help="Starting job number", default=1)
        parser.add_argument("-n", "--num-jobs", nargs=1, type=int, help="Number of jobs", default=1)
        parser.add_argument("-o", "--output-dir", nargs=1, help="Job output directory")
        parser.add_argument("-r", "--random-seed", nargs=1, type=int, help="Base number for random seed", default=1)
        parser.add_argument("-s", "--store", nargs=1, help="File for storing job params", default="jobs.json")
        parser.add_argument("script", nargs=1, help="Python job script")
        parser.add_argument("params", nargs=1, help="Job param template in JSON format")
        cl = parser.parse_args()
        
        self.job_start = cl.job_start[0]
        self.num_jobs = cl.num_jobs[0]
        self.job_store = cl.store[0]
        self.script = cl.script[0]
        self.params = JobParameters(cl.params[0])
        self.seed = cl.random_seed[0]
        if cl.output_dir:
            self.output_dir = cl.output_dir[0]
        else:
            self.output_dir = os.getcwd()
        
        dir(self)
        
    def build(self):
        
        jobs = {"jobs": []}
                    
        input_file_lists = {}
        for dest,src in self.params.input_files.iteritems():
            print src
            flist = glob.glob(src)
            flist.sort()
            input_file_lists[dest] = flist
        #print input_file_lists                    

        ifile = 0                
        for jobnum in range(self.job_start, self.job_start + self.num_jobs):
            job = {}
            job["job_num"] = jobnum
            job["seed"] = str(self.seed) + str(jobnum)
            job["output_dir"] = self.output_dir
            job["input_files"] = {}
            jobs["jobs"].append(job)
            if input_file_lists:
                for dest,src in input_file_lists.iteritems():
                    job["input_files"][dest] = src[ifile]
            job["output_files"] = {}
            for src,dest in self.params.output_files.iteritems():
                base,ext = os.path.splitext(dest)
                dest_file = base + "_" + (("%0" + str(self.job_num_pad) + "d") % jobnum) + ext
                job["output_files"][src] = dest_file
            
            for k,v in self.params.json_dict.iteritems():
                if k not in Workflow.base_params:
                    job[k] = v 
                                
            ifile += 1
                
        with open(self.job_store, "w") as jobfile:
            json.dump(jobs, jobfile, indent=4, sort_keys=False)
            
        print json.dumps(jobs, indent=4, sort_keys=False)

if __name__ == "__main__":
    workflow = Workflow()
    workflow.parse_args()
    workflow.build()
            
        
    
        
    