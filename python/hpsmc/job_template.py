import sys
import os
import itertools
import copy
import json

from jinja2 import Template, Environment, FileSystemLoader

def basename(path):
    return os.path.splitext(os.path.basename(path))[0]
    
class JobData(object):
    
    def __init__(self):
        self.input_files = {}
    
    def set(self, name, value):
        setattr(self, name, value)

class JobTemplate:
    
    def __init__(self, template_file=None, output_file='jobs.json'):
        self.template_file = template_file
        self.env = Environment(loader=FileSystemLoader('.'))
        self.env.filters['basename'] = basename
        self.job_id_start = 0;
        self.input_files = {}
        self.itervars = {}
        self.repeat = -1
        self.output_file = output_file
        
    def add_input_files(self, key, file_list, nreads=1):
        if self.input_files.has_key(key):
            raise Exception('Input file key already exists: %s' % key)
        self.input_files[key] = (file_list, nreads)
    
    def add_itervar(self, name, vals):
        if self.itervars.has_key(name):
            raise Exception('The iter var already exists: %s' % name)
        self.itervars[name] = vals
        
    def add_itervars(self, d):
        for k,v in d:
            add_itervar(k, v)
            
    def add_itervars_json(self, json_file):
        add_itervars(json.load(json_file))
        
    def get_itervars(self):
        """
        Return all combinations of the iteration variables.
        """
        var_list = []
        var_names = []
        if self.itervars:
            var_names.extend(sorted(self.itervars.keys()))
            for k in sorted(self.itervars.keys()):
                var_list.append(self.itervars[k])
        prod = itertools.product(*var_list)
        return var_names,list(prod)
            
    def run(self):
        self.template = self.env.get_template(self.template_file)
        jobs = []
        for job in self.create_jobs():
            vars = {'job': job}
            s = self.template.render(vars)
            #print(s)
            job_json = json.loads(s)
            jobs.append(job_json)
        #print(json.dumps(job_list, indent=4, sort_keys=True))
        # Dump the results to a file
        with open(self.output_file, 'w') as f:
            json.dump(jobs, f, indent=4)
            print('Wrote %d jobs to: %s' % (len(jobs), self.output_file))
 
    def create_jobs(self):
        
        jobs = []
        
        var_names, var_vals = self.get_itervars()
        nvars = len(var_names)
                
        job_id = self.job_id_start
        
        # loop over itervars
        for var_index in range(len(var_vals)):
            jobdata = JobData()
            vars = var_vals[var_index]
            for j in range(nvars):
                jobdata.set(var_names[j], var_vals[var_index][j])
            input_files = copy.deepcopy(self.input_files)
            # repeat N times
            for r in range(self.repeat):
                job_input_files = []    
                for input_name in input_files.keys():
                    nreads = input_files[input_name][1]
                    flist = input_files[input_name][0]
                    for iread in range(nreads):
                        input_file = input_files[input_name][0].pop(0)
                        job_input_files.append(input_file)
                jobdata.input_files[input_name] = job_input_files
                jobdata.set('job_id', job_id)
                jobdata_copy = copy.deepcopy(jobdata)
                jobs.append(jobdata_copy)
                job_id += 1
        return jobs
 
    """
    def parse_args(self):
        
        parser = argparse.ArgumentParser(description="Create a job store with multiple jobs")

        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job ID", default=0)
        
        parser.add_argument("-p", "--pad", nargs="?", type=int, 
                            help="Number of padding spaces for job IDs (default is 0 for no padding)", default=0)
        
        parser.add_argument("-i", "--input-file-list", action='append', nargs=2, 
                            metavar=('FILE', 'N_READS'), help="Input file list and N reads per event")
        
        parser.add_argument("-g", "--glob", action='append', nargs=3,
                            metavar=('NAME', 'GLOB', 'NREADS'),
                            help="Glob pattern to read input files (use '\\\\*' for wildcard character)")
        
        parser.add_argument("-a", "--var-file", help="Variables in JSON format for iteration")
        
 #       parser.add_argument("-r", "--repeat", type=int, help="Repeat each iteration N times", default=1)
        
        parser.add_argument("json_template_file", help="Job template in JSON format")
        
        parser.add_argument("job_store", help="Output file containing the JSON job store")
        
        cl = parser.parse_args()
        
        self.json_template_file = cl.json_template_file
        if not os.path.isfile(self.json_template_file):
            raise Exception('The JSON template file does not exist: %s' % self.json_template_file)
        
        self.job_store = cl.job_store
                        
        self.job_start = cl.job_start
        self.pad = cl.pad
        self.seed = cl.seed
        
        self.input_lists = [] 
        if cl.input_file_list is not None:
            for f in cl.input_file_list:
                self.input_lists.append([f[0], int(f[1])])
        self.list_reader = FileListReader(self.input_lists)
                
        if cl.var_file:
            var_file = cl.var_file
            print("Iteration variables will be read from file: %s" % var_file)
            if not os.path.exists(var_file):
                raise Exception('The var file does not exist: %s' % var_file)
            with open(var_file, 'r') as f:
                self.itervars = json.load(f)
        else:
            self.itervars = None

        self.repeat = cl.repeat
        if self.repeat < 1:
            raise Exception('Bad value for repeat argument: %d' % self.repeat)
        
        self.glob_readers = [] 
        if cl.glob is not None:
            for g in cl.glob:
                name = g[0]
                wildcard = g[1]
                nreads = int(g[2])
                self.glob_readers.append(GlobReader(name, wildcard, nreads))
    """
        
if __name__ == '__main__':
    
    if len(sys.argv) < 2:
        raise Exception('Missing name of template file')
    
    job_tmpl = JobTemplate(sys.argv[1])
    
    # test vars
    job_tmpl.add_itervar('run_number', [10666, 1194500])
    job_tmpl.add_itervar('detector', ['HPS-PhysicsRun2019-v2-4pt5'])
    
    # test input events
    job_tmpl.add_input_files('test_events', ['/fake/path/events_1.stdhep', '/fake/path/events_2.stdhep'])
    
    # test repeat
    job_tmpl.repeat = 2
        
    job_tmpl.run()

### Example input template 
"""
{
    "job_id": {{ job.job_id }},
    "seed": {{ job.job_id + 1000000 }},
    "run_number": {{ job.run_number }},
    "detector": "{{ job.detector }}",
    "input_files": {
        "{{ job.input_files['test_events'][0] }}": "events.stdhep"
    },
    "output_files": {
        "fake_output_events.slcio": "{{ job.input_files['test_events'][0] | basename }}.slcio"    
    }
}

"""