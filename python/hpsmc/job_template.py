import sys
import os
import itertools
import copy
import json
import argparse

from jinja2 import Template, Environment, FileSystemLoader

def basename(path):
    """Filter to return a file base name stripped of dir and extension. 
    
    Useful for using base names of input files in the output file names within jinja2 template.
    """
    return os.path.splitext(os.path.basename(path))[0]

def extension(path):
    """Filter to get file extension from string."""
    return os.path.splitext(path)[1]

def dirname(path):
    """Filter to get dir name from string."""
    return os.path.dirname(path)

# TODO:
# filenum filter - try to get file num by looking for _%d in file name
# filetype filter - return type of file e.g. 'lcio', 'root', etc.

#def pwd():
#    return os.getcwd()
    
class JobData(object):
    """Very simple key-value object for storing data for each job."""
    
    def __init__(self):
        self.input_files = {}
    
    def set(self, name, value):
        setattr(self, name, value)

class JobTemplate:
    """Template engine for transforming input job template into JSON job store.
    
    Accepts a set of iteration variables of which all combinations will be turned into jobs.
    
    Also accepts lists of input files with a unique key from which one or more can be read
    per job.
    
    The user's template should be a JSON dict with jinja2 markup, which can use the 'job' variable 
    to access job params for a particular job.
    """
    
    def __init__(self, template_file=None, output_file='jobs.json'):
        self.template_file = template_file
        self.env = Environment(loader=FileSystemLoader('.'))
        self.env.filters['basename'] = basename
        self.job_id_start = 0;
        self.input_files = {}
        self.itervars = {}
        self.output_file = output_file
        
    def add_input_files(self, key, file_list, nreads=1):
        if key in self.input_files:
            raise Exception('Input file key already exists: %s' % key)
        self.input_files[key] = (file_list, nreads)
    
    def add_itervar(self, name, vals):
        if name in self.itervars:
            raise Exception('The iter var already exists: %s' % name)
        self.itervars[name] = vals
        
    def add_itervars(self, d):
        for k,v in d.items():
            self.add_itervar(k, v)
            
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
        """
        Generate the JSON jobs from processing the template and write to file.
        """
        self.template = self.env.get_template(self.template_file)
        jobs = []
        for job in self._create_jobs():
            vars = {'job': job}
            s = self.template.render(vars)
            #print(s)
            job_json = json.loads(s)
            jobs.append(job_json)
        #print(json.dumps(job_list, indent=4, sort_keys=True))
        with open(self.output_file, 'w') as f:
            json.dump(jobs, f, indent=4)
            print('Wrote %d jobs to: %s' % (len(jobs), self.output_file))
            
    def _get_max_iterations(self):
        max_iter = -1
        for input_name in list(self.input_files.keys()):
            nreads = self.input_files[input_name][1]
            flist = self.input_files[input_name][0]
            n_iter = len(flist) / nreads
            if n_iter > max_iter:
                max_iter = n_iter
        return max_iter
 
    def _create_jobs(self):
        
        jobs = []
        
        var_names, var_vals = self.get_itervars()
        nvars = len(var_names)
        print("Number of var combinations: %d" % len(var_vals))
                
        job_id = self.job_id_start
        
        # loop over itervars
        repeat = self._get_max_iterations()
        print("Max iterations: %d" % repeat)
        if repeat < 1:
            repeat = 1
        for var_index in range(len(var_vals)):
            jobdata = JobData()
            vars = var_vals[var_index]
            for j in range(nvars):
                jobdata.set(var_names[j], var_vals[var_index][j])
            input_files = copy.deepcopy(self.input_files)
            for r in range(repeat): # TODO: allow settable repeat param here
                job_input_files = []
                for input_name in list(input_files.keys()):
                    nreads = input_files[input_name][1]
                    flist = input_files[input_name][0]
                    for iread in range(nreads):
                        input_file = input_files[input_name][0].pop(0)
                        job_input_files.append(input_file)
                jobdata.input_files[input_name] = job_input_files
                jobdata.set('job_id', job_id)
                #jobdata.set('sequence', sequence)
                jobdata_copy = copy.deepcopy(jobdata)
                jobs.append(jobdata_copy)
                job_id += 1
                #sequence += 1
        return jobs
    
    def _read_input_file_list(self, input_file_list):
        """Read the input file list from arg parsing."""
        for f in input_file_list:
            name = f[0]
            if name in list(self.input_files.keys()):
                raise Exception('Duplicate input file list name: %s' % name)
            file = f[1]
            nreads = int(f[2])
            input_file_list = []
            with open(file, 'r') as f:
                lines = f.readlines()
                for line in lines:
                    if len(line.strip()):
                        input_file_list.append(line.strip())
                if not len(input_file_list):
                    raise Exception('Failed to read any input files from file: %s' % file)
                self.input_files[name] = (input_file_list, nreads)

    def parse_args(self):
        """Parse arguments for template engine."""        
        
        parser = argparse.ArgumentParser(description="Create a JSON job store from a jinja2 template")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job ID", default=0)
        parser.add_argument("-a", "--var-file", help="Variables in JSON format for iteration")
        parser.add_argument("-i", "--input-file-list", action='append', nargs=3, 
                            metavar=('NAME', 'FILE', 'NREADS'), help="Unique name of input file list, path on disk, number of files to read per job")
        parser.add_argument("template_file", help="Job template in JSON format with jinja2 markup")
        parser.add_argument("output_file", help="Output file containing the generated JSON job store")
        
        # TODO:
        # - add repeat CL arg (for event gen or can override default from input files)
        # - add max num jobs arg (job generation should quit once it reaches this num of jobs)
        
        cl = parser.parse_args()
        
        self.job_id_start = cl.job_start
        
        self.template_file = cl.template_file
        if not os.path.isfile(self.template_file):
            raise Exception('The template file does not exist: %s' % self.json_template_file)
        
        self.output_file = cl.output_file
        
        self.input_files = {}
        if cl.input_file_list is not None:
            self._read_input_file_list(cl.input_file_list)
                
        if cl.var_file:
            var_file = cl.var_file
            if not os.path.exists(var_file):
                raise Exception('The var file does not exist: %s' % var_file)
            with open(var_file, 'r') as f:
                self.add_itervars(json.load(f))
                print("Loaded iter vars from file: %s" % var_file)
                print(self.itervars)
        
if __name__ == '__main__':
    job_tmpl = JobTemplate()
    job_tmpl.parse_args()
    job_tmpl.run()

"""
def _old_main():
    if len(sys.argv) < 2:
        raise Exception('Missing name of template file')
    
    job_tmpl = JobTemplate(sys.argv[1])
    
    # test vars
    job_tmpl.add_itervar('run_number', [10666, 1194500])
    job_tmpl.add_itervar('detector', ['HPS-PhysicsRun2019-v2-4pt5'])
    
    # test input events
    nfiles = 10
    input_files = []
    for i_file in range(nfiles):
        input_files.append('/fake/path/events_%d.stdhep' % (i_file+1))
    job_tmpl.add_input_files('test_events', input_files)
            
    job_tmpl.run()
"""