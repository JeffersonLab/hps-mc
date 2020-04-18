import argparse, os, json, glob, collections, logging, itertools
from string import Template
import hpsmc.util as util

logger = logging.getLogger("hpsmc.job_store")

class JobStore:
    """
    Create a JSON job store using a JSON template and variable substitutions. 
    
    The user may provide a file containing a list of input files to process as 
    well as a JSON file containing variable lists. All combinations of the 
    input files and variables are combined together using itertools and the 
    template is processed to expand in a full set of jobs, which are written 
    into a JSON job store. The input file list and the variable file are both
    optional but at least one of them must be provided.
    """
        
    def __init__(self, path=None):        
        self.path = path
        if path:
            logger.info("Initializing job store from '%s'" % self.path)
            self.load(path)
    
    def parse_args(self):
        """Parse command line arguments to build and configure the job store."""
        
        parser = argparse.ArgumentParser(description="Create a job store with multiple jobs")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job ID", default=0)
        parser.add_argument("-p", "--pad", nargs="?", type=int, help="Number of padding spaces for job IDs (default is 4)", default=4)
        parser.add_argument("-r", "--seed", nargs="?", type=int, help="Starting random seed, incremented by 1 for each job", default=1)
        parser.add_argument("-i", "--input-files", help="Input file list (text file)")
        parser.add_argument("-o", "--output-file", help="Output file written by the job script")
        parser.add_argument("-a", "--var-file", help="Variables in JSON format for iteration")
        parser.add_argument("json_template_file", help="Job template in JSON format")
        parser.add_argument("job_store", help="Output file containing the JSON job store")
        cl = parser.parse_args()

        self.json_template_file = cl.json_template_file
        if not os.path.isfile(self.json_template_file):
            raise Exception("The JSON template file '%s' does not exist.")
        
        self.job_store = cl.job_store
                        
        self.job_start = cl.job_start
        self.job_id_pad = cl.pad
        self.seed = cl.seed

        self.input_files = []
        input_files = cl.input_files
        with open(input_files) as f:
            self.input_files = f.read().splitlines()

        if cl.output_file:           
            self.output_file = cl.output_file
        else:
            self.output_file = None
        
        if cl.var_file:
            var_file = cl.var_file
            print("Iteration variables will be read from '%s'" % var_file)
            if not os.path.exists(var_file):
                raise Exception("The var file '%s' does not exist." % var_file)
            with open(var_file, 'r') as f:
                self.itervars = json.load(f)
        else:
            self.itervars = None
 
    def get_iter_vars(self):
        """
        Return all combinations of the (optional) input files with the iteration variables.
        """
        var_list = []
        var_names = []
        if len(self.input_files):
            var_names.append('input_file')
            var_list.append(self.input_files)
        if self.itervars:
            var_names.extend(sorted(self.itervars.keys()))
            for k in sorted(self.itervars.keys()):
                var_list.append(self.itervars[k])
        prod = itertools.product(*var_list)
        return var_names, list(prod)
                
    def build(self):
        """
        Build a job store from the JSON template and variable iterations.
        """ 
        
        # Get the iteration variable names and value lists
        var_names, var_lists = self.get_iter_vars()

        # Setup the basic variable mapping for the template
        job_id = self.job_start
        mapping = {
            "job_id": None,
            "seed": None
        }
        if self.output_file:
            mapping["output_file"] = self.output_file
        seed = self.seed

        # Dict which will contain all jobs
        jobs = {}
        
        # Read in the template file
        with open(self.json_template_file, 'r') as tmpl_file:
            tmpl = Template(tmpl_file.read())
        
        # Loop over the variable lists and substitute into the template
        for var_list in var_lists:
            for varname, value in zip(var_names, var_list):
                mapping[varname] = value                
                #print('%s=%s' % (str(varname), str(value)))
            job_id_padded = ("%0" + str(self.job_id_pad) + "d") % job_id
            mapping['job_id'] = job_id_padded
            mapping['seed'] = seed
            job_str = tmpl.substitute(mapping)
            job_json = json.loads(job_str)
            jobs[str(job_id)] = job_json
            job_id +=1
            seed +=1 
        
        self.data = jobs
        
        # Dump the results to a file
        with open(self.job_store, 'w') as f:
            json.dump(jobs, f, indent=4)
                
        print("Wrote %d jobs to job store '%s'" % (len(self.data), self.job_store))
            
    def load(self, json_store):
        """Load raw JSON data into this job store."""
        rawdata = open(json_store, "r").read()
        self.data = json.loads(rawdata)
        
    def get_job(self, job_id):
        """Get a job by its job ID."""
        return self.data[str(job_id)]
        
    def get_job_data(self):
        """Get the raw dict containing all the job data."""
        return self.data
        
    def get_job_ids(self):
        """Get a sorted list of job IDs."""
        return sorted(self.data.keys())
    
    def has_job_id(self, job_id):
        """Return true if the job ID exists in the store."""
        return job_id in self.data.keys()
    
# Run from the command line to create a new job store
if __name__ == "__main__":
    jobstore = JobStore()
    jobstore.parse_args()
    jobstore.build()
