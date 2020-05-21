import argparse, os, json, glob, collections, logging, itertools
from string import Template
import hpsmc.util as util

logger = logging.getLogger("hpsmc.job_store")

# TODO: Add option to read N input files per job and create vars input_file_1, input_file_2, etc.
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
        parser.add_argument("-s", "--seed", nargs="?", type=int, help="Starting random seed, incremented by 1 for each job", default=1)
        parser.add_argument("-i", "--input-file-list", action='append', nargs=2, help="Input file lists and number of reads per job")
        parser.add_argument("-o", "--output-file", help="Output file written by the job script")
        parser.add_argument("-a", "--var-file", help="Variables in JSON format for iteration")
        parser.add_argument("-r", "--repeat", type=int, help="Repeat each iteration N times", default=1)
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
        
        # TODO: handle multiple input file lists here
        self.input_lists = [] 
        if cl.input_file_list is not None:
            for f in cl.input_file_list:
                self.input_lists.append([f[0], int(f[1])])
#            print(self.input_lists)
        self.list_reader = FileListReader(self.input_lists)
        
#        self.input_files = []
#        if cl.input_files:
#            with open(cl.input_files) as f:
#                self.input_files = f.read().splitlines()

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

        self.repeat = cl.repeat
        if self.repeat < 1:
            raise Exception('Bad value %d for repeat argument.' % self.repeat)
 
    def get_iter_vars(self):
        """
        Return all combinations of the (optional) input files with the iteration variables.
        """
        var_list = []
        var_names = []
        # TODO: remove this from itervars (handle it in the job creation loop)
        #if len(self.input_files):
        #    var_names.append('input_file')
        #    var_list.append(self.input_files)
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
        mapping = {
        }
        if self.output_file:
            mapping["output_file"] = self.output_file

        # List which will contain all the jobs
        jobs = []
        
        # Read in the template file
        with open(self.json_template_file, 'r') as tmpl_file:
            tmpl = Template(tmpl_file.read())
        
        self.list_reader.open()
        
        # Loop over the variable lists and substitute into the template
        seed = self.seed
        job_id = self.job_start
        for var_list in var_lists:
            # TODO: handle multiple input file lists here
            for varname, value in zip(var_names, var_list):
                mapping[varname] = value
            for r in range(0, self.repeat):
                
                mapping['seed'] = seed
                mapping['job_id'] = job_id
                mapping['sequence'] = r + 1
                
                file_vars = self.list_reader.read_next()
                print("read next files: " + str(file_vars))
                for name,path in file_vars.iteritems():
                    mapping[name] = path
                
                job_str = tmpl.substitute(mapping)
                job_json = json.loads(job_str)
                job_json["job_id"] = job_id
                jobs.append(job_json)
                                
                job_id +=1
                seed +=1
                
        self.data = jobs
        
        self.list_reader.close()
        
        # Dump the results to a file
        with open(self.job_store, 'w') as f:
            json.dump(jobs, f, indent=4)
                
        print("Wrote %d jobs to '%s'" % (len(self.data), self.job_store))
        
        # TODO: Load the store here
            
    def load(self, json_store):
        """Load raw JSON data into this job store."""
        with open(json_store, 'r') as f:
            json_data = json.loads(f.read())
        self.data = {}
        for j in json_data:
            self.data[j['job_id']] = j
        print("Loaded %d jobs from job store '%s'" % (len(self.data), json_store))
        
    def get_job(self, job_id):
        """Get a job by its job ID."""
        return self.data[int(job_id)]
        
    def get_job_data(self):
        """Get the raw dict containing all the job data."""
        return self.data
        
    def get_job_ids(self):
        """Get a sorted list of job IDs."""
        return sorted(self.data.keys())
    
    def has_job_id(self, job_id):
        """Return true if the job ID exists in the store."""
        return job_id in self.data.keys()
    
class FileListReader:
    
    def __init__(self, flists):
        self.flists = flists
        self.fhandles = {}

    def open(self):
        for flist,nread in self.flists:
            list_name = os.path.splitext(os.path.basename(flist))[0]
            if self.fhandles.has_key(list_name):
                raise Exception("Duplicate file list name: %s" % list_name)
            f = open(flist, 'r')
            self.fhandles[list_name] = [f, nread]
        print(self.flists)
        print(self.fhandles)
                
    def read_next(self):
        file_vars = {}
        for name,f in self.fhandles.iteritems():
            for i in range(f[1]):
                file_vars['_'.join([name, str(i + 1)])] = f[0].readline().strip()
        return file_vars
    
    def close(self):
        for name,f in self.fhandles.iteritems():
            try:
                f.close()
            except:
                pass
    
# Run from the command line to create a new job store
if __name__ == "__main__":
    jobstore = JobStore()
    jobstore.parse_args()
    jobstore.build()
