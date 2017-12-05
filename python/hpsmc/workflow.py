import argparse, os, json, glob, collections

from hpsmc.job import JobParameters

class Workflow:
    """Represents a set of MC job parameters and a job script to be managed 
    and run by a batch system.  The workflow data is stored in a single JSON
    file which can be unpacked into individual configuration files for each job.
    
    Example command line usage to create a workflow:
    
    python workflow.py -j 1 -n 10 -w ap -g -d workdir -u -p 6 ap_job.py job.json
    
    This will create 10 jobs, numbered from 1, in a workflow called 'ap' with a working
    directory called 'workdir'.  The 'ap_job.py' script is used to run the actual job, 
    and the job parameters for the script are contained in a file called 'job.json' which 
    is used to create individual JSON configurations.  Job numbers will be padded 
    using 6 characters.
    
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
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job number for file naming", default=0)
        parser.add_argument("-n", "--num-jobs", nargs="?", type=int, help="Number of jobs", default=1)
        parser.add_argument("-w", "--workflow", nargs="?", help="Name of the workflow", required=True)
        parser.add_argument("-p", "--pad", nargs="?", type=int, help="Number of padding spaces for job numbers (default is 4)", default=4)
        parser.add_argument("-d", "--workdir", nargs="?", default=None, help="Working dir for storing JSON files")
        parser.add_argument("-r", "--seed", nargs="?", type=int, help="Starting random seed", default=1)
        parser.add_argument("script", help="Python job script")
        parser.add_argument("params", help="Job template in JSON format")
        cl = parser.parse_args()

        if not os.path.isfile(cl.params):
            raise Exception("The job param file '%s' does not exist.")
        self.params = JobParameters(cl.params)
        
        self.job_script = os.path.abspath(cl.script)
        if not os.path.isfile(self.job_script):
            raise Exception("The job script '%s' does not exist.")
                
        self.job_start = cl.job_start
        self.num_jobs = cl.num_jobs
        self.name = cl.workflow
        self.job_store = self.name + ".json"
        self.job_id_pad = cl.pad
        self.seed = cl.seed

        self.workdir= cl.workdir
        if not self.workdir:
            self.workdir = os.getcwd() + os.path.sep + self.name
        self.workdir = os.path.abspath(self.workdir)
        
    def build(self):
        """Build a workflow from the input parameters and write out a single JSON file containing all job definitions."""
        
        self.data = {self.name: {}}
        self.data["job_script"] = self.job_script
        self.data["workdir"] = self.workdir

        if not os.path.isdir(self.workdir):
            os.makedirs(self.workdir)

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
        
        seed = self.seed
        
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
            json.dump(self.data, jobfile, indent=2, sort_keys=True)

        # unpack workflow into individual JSON config files
        print "Unpacking jobs to <" + self.workdir + ">"
        self.unpack()
    
    def load(self, json_store):
        """Load JSON job store into this workflow."""
        rawdata = open(json_store, "r").read()
        data = json.loads(rawdata)
        self.job_script = data.pop("job_script", None)
        self.workdir = data.pop("workdir", None)
        self.name = data.keys()[0]
        self.data = data
        
    def get_jobs(self):
        """Get the dictionary representing all the individual jobs."""
        return collections.OrderedDict(sorted(self.data[self.name].items()))
    
    def get_job_names(self):
        """Get a sorted list of job names defined by this workflow."""
        return sorted(self.data[self.name])
    
    def unpack(self):
        """Unpack the workflow into individual JSON job configuration files."""
        for k in self.get_job_names():
            jobdict = self.get_jobs()[k]
            jobfile = self.workdir + os.path.sep + k + ".json"            
            with open(jobfile, "w") as outfile:
                json.dump(jobdict, outfile, indent=2, sort_keys=True)
            print "Wrote JSON config <" + jobfile + "> for job <" + k + ">"
            
# run from command line            
if __name__ == "__main__":
    workflow = Workflow()
    workflow.parse_args()
    workflow.build()
