import argparse, os, json, glob, collections

from string import Template

import hpsmc.util as util

class JobStore:
    """
    Create a JSON job store using a JSON template and list of files.
    """
        
    def __init__(self, json_file=None):
        if json_file:
            self.load(json_file)
    
    def parse_args(self):
        """Parse command line arguments to build and configure the job store."""
        
        parser = argparse.ArgumentParser(description="Create a job store with multiple jobs")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job ID", default=0)
        parser.add_argument("-p", "--pad", nargs="?", type=int, help="Number of padding spaces for job IDs (default is 4)", default=4)
        parser.add_argument("-r", "--seed", nargs="?", type=int, help="Starting random seed, incremented by 1 for each job", default=1)
        parser.add_argument("-i", "--input-files", help="Input file list (text file)", required=True)
        parser.add_argument("-o", "--output-file", help="Output file written by the job script", required=True)
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
            
        self.output_file = cl.output_file
            
    def build(self):
        """
        Build a job store from the JSON template and input file list.
        """ 
               
        job_id = self.job_start
        mapping = {
            "job_id": None,
            "input_file": None,
            "output_file": self.output_file,
            "seed": None
        }
        seed = self.seed
        jobs = {}
        with open(self.json_template_file, 'r') as tmpl_file:
            tmpl = Template(tmpl_file.read())
            
        print("Processing %d input files ..." % len(self.input_files))
        for input_file in self.input_files:
            job_id_padded = ("%0" + str(self.job_id_pad) + "d") % job_id
            mapping['job_id'] = job_id_padded
            mapping['input_file'] = input_file
            mapping['seed'] = seed
            job_str = tmpl.substitute(mapping)
            job_json = json.loads(job_str)
            jobs[str(job_id)] = job_json
            job_id +=1
            seed +=1
                
        with open(self.job_store, 'w') as f:
            json.dump(jobs, f, indent=4)
        
        print("Wrote job store to '%s'" % self.job_store)
            
    def load(self, json_store):
        """Load raw JSON data into this job store."""
        rawdata = open(json_store, "r").read()
        self.data = json.loads(rawdata)
        
    def get_job(self, job_id_str):
        """Get a job by its job ID."""
        return self.data[job_id_str]
        
    def get_job_data(self):
        """Get the raw dict containing all the job data."""
        return self.data
    
    def get_job_ids(self):
        """Get a sorted list of job ID strings."""
        return sorted(self.data.keys())
    
# Run from the command line to create a job store
if __name__ == "__main__":
    jobstore = JobStore()
    jobstore.parse_args()
    jobstore.build()
