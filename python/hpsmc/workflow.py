import argparse, os, json, glob

from hpsmc.job import JobParameters

def to_ascii(str):
    """Coerce unicode strings to ASCII for Ganga.""" 
    return str.encode('ascii', 'ignore')

class Workflow:
    """Represents a set of MC job parameters and a job script to be managed 
    and run by a batch system.  The workflow data is stored in a single JSON
    file which can be unpacked into individual configuration files for each job.
    
    Example command line usage to create a workflow:
    
    python workflow.py -j 1 -n 10 -w ap -g -d workdir -u -p 6 ap_job.py job.json
    
    This will create 10 jobs, numbered from 1, in a workflow called 'ap' with a working
    directory called 'workdir'.  It will automatically submit the workflow to Ganga
    and also unpacks the job configs to individual JSON files.  The 'ap_job.py' script
    is used to run the actual job, and the job parameters for the script are contained 
    in a file called 'job.json' which is used to create individual JSON configurations.  
    Job numbers will be padded using 6 characters.
    
    The standard argparse switch can be used to print out detailed help info describing
    all the available options:
    
    python workflow.py --help
    
    Should the jobs not be unpacked during the workflow creation step by using the '-u' flag, 
    then this step would need to be done manually before running on the batch system.
    """
    
    base_params = ["job_id", "seed", "output_dir", "input_files", "output_files"]
    
    def __init__(self, json_file = None):
        if json_file:
            self.load(json_file)
    
    def parse_args(self):
        """Parse command line arguments to build and configure workflow."""
        
        parser = argparse.ArgumentParser(description="Manage and create workflows with multiple jobs")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job number for file naming", default=1)
        parser.add_argument("-n", "--num-jobs", nargs="?", type=int, help="Number of jobs", default=1)
        parser.add_argument("-w", "--workflow", nargs="?", help="Name of the workflow", required=True)
        parser.add_argument("-p", "--pad", nargs="?", type=int, help="Number of padding spaces for job numbers (default is 4)", default=4)
        parser.add_argument("-g", "--ganga", action='store_true', help="Automatically create Ganga jobs from this workflow")
        parser.add_argument("-d", "--workdir", nargs="?", default=os.getcwd(), help="Working dir for storing JSON files")
        parser.add_argument("-u", "--unpack", action='store_true', help="Unpack workflow into individual JSON files in the work dir")
        parser.add_argument("script", help="Python job script")
        parser.add_argument("params", help="Job template in JSON format")
        cl = parser.parse_args()
                
        if not os.path.isfile(cl.params):
            raise Exception("The job param file '%s' does not exist.")
        self.params = JobParameters(cl.params)
        
        self.job_script = to_ascii(os.path.abspath(cl.script))
        if not os.path.isfile(self.job_script):
            raise Exception("The job script '%s' does not exist.")
                
        self.job_start = cl.job_start
        self.num_jobs = cl.num_jobs
        self.name = cl.workflow
        self.job_store = self.name + ".json"
        self.job_id_pad = cl.pad
        self.enable_ganga = cl.ganga
        self.workdir = os.path.abspath(cl.workdir)
        self.do_unpack = cl.unpack
        
    def build(self):
        """Build a workflow from the input parameters and write out a single JSON file containing all job definitions."""
        
        self.jobs = {self.name: {}}
        self.jobs["job_script"] = self.job_script
                                          
        # configure input files depending on how they are defined in the job params                                                
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
        
        # TODO: Should rand seed instead be a param to workflow?
        seed = self.params.seed
        
        output_dir = os.path.abspath(self.params.output_dir)

        # build configuration for each job in the workflow                
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
            
            self.get_jobs()[self.name + "_"+ job_id_padded] = job
            
            seed += 1
        
        # write out entire workflow to single JSON file
        with open(self.job_store, "w") as jobfile:
            json.dump(self.jobs, jobfile, indent=2, sort_keys=True)

        # register the jobs with Ganga so they are runnable in LSF
        if self.enable_ganga:
            print "Adding jobs to Ganga"
            self.add_to_ganga("/" + self.name, self.workdir)
        
        # unpack workflow into individual JSON config files
        if self.do_unpack:
            print "Unpacking jobs to <" + self.workdir + ">"
            self.unpack()
    
    def load(self, json_store):
        """Load JSON job store into this workflow."""
        rawdata = open(json_store, "r").read()
        data = json.loads(rawdata)
        self.job_script = data.pop("job_script", None)
        if not self.job_script:
            raise Exception("JSON file is missing 'job_script' key.")
        self.name = data.keys()[0]
        self.jobs = data[self.name]
        
    def get_jobs(self):
        """Get the dictionary representing all the individual jobs."""
        return self.jobs[self.name]
    
    def get_job_names(self):
        """Get a sorted list of job names defined by this workflow."""
        return sorted(self.jobs[self.name])

    def add_to_ganga(self, job_dir, work_dir):
        """Add all jobs from this workflow to Ganga."""
        try:
            import ganga
        except:
            raise Exception("Ganga is not available in your python installation, or it did not import successfully.")
        from ganga import Job, LSF, jobtree
        
        job_names = sorted(self.jobs[self.name])
        jobtree.mkdir(job_dir)
        jobtree.cd(job_dir)
        jobs = self.get_jobs()
        for k in job_names:
            jobdict = self.get_jobs()[k]
            gj = Job()
            gj.application.exe = 'python'
            jobfile = to_ascii(k + ".json")
            jobfile = work_dir + "/" + jobfile 
            gj.application.args = [self.job_script, jobfile]
            gj.backend = LSF()
            gj.backend.queue = 'long'
            gj.name = to_ascii(k)
            gj.parallel_submit = True
            jobtree.add(gj)
            print "Added Ganga job <" + k + "> with id " + str(gj.id) + " and JSON config <" + jobfile + ">"          
            
    def unpack(self):
        """Unpack the workflow into individual JSON job configuration files."""
        for k in self.get_job_names():
            jobdict = self.get_jobs()[k]
            jobfile = self.workdir + "/" + k + ".json"            
            with open(jobfile, "w") as outfile:
                json.dump(jobdict, outfile, indent=2, sort_keys=True)
            print "Wrote JSON config <" + jobfile + "> for job <" + k + ">"
            
# run from command line            
if __name__ == "__main__":
    workflow = Workflow()
    workflow.parse_args()
    workflow.build()
        