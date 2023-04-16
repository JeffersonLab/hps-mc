"""! @package job_template
Expand a Jinja job template into a full list of jobs in JSON format."""

import sys
import os
import itertools
import copy
import json
import argparse
import math
import uuid as _uuid
import subprocess
import re

from jinja2 import Template, Environment, FileSystemLoader


def basename(path):
    """! Filter to return a file base name stripped of dir and extension."""
    return os.path.splitext(os.path.basename(path))[0]


def extension(path):
    """! Filter to get file extension from string."""
    return os.path.splitext(path)[1]


def dirname(path):
    """! Filter to get dir name from string."""
    return os.path.dirname(path)


def pad(num, npad=4):
    """! Filter to pad a number."""
    return format(num, format(npad, '02'))


def uuid():
    """! Function to get a uuid within a template."""
    return str(_uuid.uuid4())[:8]


def runnumber(path):
    """! Filter to get a run number by inspecting first event in slcio file."""
    event_dump = subprocess.run(
        ["dumpevent", path, "1"],  # dump the first event from the file
        check=True,  # throw exception if returns non-0 exit code
        stdout=subprocess.PIPE,  # keep the output in the object rather than printing it out
        stderr=subprocess.PIPE
        )
    # search output for run number
    match = re.search('run:\\s*(\\d+)', event_dump.stdout.decode('utf-8'))
    if not match:
        raise ValueError(f'Unable to find run number from dump of first event in {path}')
    # group 0 is the entire match, group 1 is what is in the parentheses above
    return int(match.group(1))


def filenum(path):
    """! Filter to get the trailing number of a file

    This will extract the number between the last underscore and the extension.
    For example 'file_name_is_number.root' will return 'number' if 'number' is
    actually a integer.
    """
    # use our other function to remove the extention and directory
    filename = basename(path)
    # entries in a filename are split by '_', we take the last one (index -1),
    #  and then attempt to convert it to an int
    return int(filename.split('_')[-1])

# def pwd():
#    return os.getcwd()


class JobData(object):
    """! Very simple key-value object for storing data for each job."""

    def __init__(self):
        self.input_files = {}
        self.params = {}
        self.job_id = 0

    def set(self, name, value):
        setattr(self, name, value)

    def set_param(self, name, value):
        self.params[name] = value


class MaxJobsException(Exception):
    """! Exception if max jobs are reached."""

    def __init__(self, max_jobs):
        super().__init__("Reached max jobs: {}".format(max_jobs))


class JobTemplate:
    """! Template engine for transforming input job template into JSON job store.

    Accepts a set of iteration variables of which all combinations will be turned into jobs.
    Also accepts lists of input files with a unique key from which one or more can be read
    per job.
    The user's template should be a JSON dict with jinja2 markup.
    """

    def __init__(self, template_file=None, output_file='jobs.json'):
        ## template file from which parameters are read
        self.template_file = template_file
        self.env = Environment(loader=FileSystemLoader('.'))
        self.env.filters['basename'] = basename
        self.env.filters['pad'] = pad
        self.env.filters['uuid'] = uuid
        self.env.filters['extension'] = extension
        self.env.filters['dirname'] = dirname
        self.env.filters['runnumber'] = runnumber
        self.env.filters['filenum'] = filenum
        ## start ID for jobs
        self.job_id_start = 0
        ## dict of input files
        self.input_files = {}
        ## dict of iteration variables
        self.itervars = {}
        ## name of output file
        self.output_file = output_file

    def add_input_files(self, key, file_list, nreads=1):
        """! Add new input files to dict of input files.
        @param key  key under which new input files are added
        @param file_list  list of new input files to be added
        @param nreads  nbr of times the input files are read \todo check if this is correct
        """
        if key in self.input_files:
            raise Exception('Input file key already exists: %s' % key)
        self.input_files[key] = (file_list, nreads)

    def add_itervar(self, name, vals):
        """! Add new iteration variable to dict of iteration variables.
        @param name  name of new variable
        @param vals  list of values for iteration variable
        """
        if name in self.itervars:
            raise Exception('The iter var already exists: %s' % name)
        self.itervars[name] = vals

    def add_itervars(self, iter_dict):
        """! Add several iter variables at once.
        @param iter_dict  new dict of iteration variables to be added
        """
        for k, v in iter_dict.items():
            self.add_itervar(k, v)

    def add_itervars_json(self, json_file):
        """! Add iter variables from json file.
        @param json_file  name of json file
        """
        self.add_itervars(json.load(json_file))

    def get_itervars(self):
        """!
        Return all combinations of the iteration variables.
        """
        var_list = []
        var_names = []
        if self.itervars:
            var_names.extend(sorted(self.itervars.keys()))
            for k in sorted(self.itervars.keys()):
                var_list.append(self.itervars[k])
        prod = itertools.product(*var_list)
        return var_names, list(prod)

    def run(self):
        """!
        Generate the JSON jobs from processing the template and write to file.
        """
        self.template = self.env.get_template(self.template_file)
        self.template.globals['uuid'] = uuid
        jobs = []
        for job in self._create_jobs():
            job_vars = {'job': job,
                        'job_id': job.job_id,
                        'sequence': job.sequence,
                        'input_files': job.input_files}
            for k, v in job.params.items():
                if k in job_vars:
                    raise Exception("Illegal variable name: {}".format(k))
                job_vars[k] = v
            s = self.template.render(job_vars)
            job_json = json.loads(s)
            job_json['job_id'] = job.job_id

            jobs.append(job_json)
        with open(self.output_file, 'w') as f:
            json.dump(jobs, f, indent=4)
            print('Wrote %d jobs to: %s' % (len(jobs), self.output_file))

    def _get_max_iterations(self):
        """!
        Get the maximum number of iterations based on file input parameters.
        """
        max_iter = -1
        for input_name in list(self.input_files.keys()):
            nreads = self.input_files[input_name][1]
            flist = self.input_files[input_name][0]
            n_iter = int(math.floor(len(flist) / nreads))
            if n_iter > max_iter:
                max_iter = n_iter
        return max_iter

    def _create_jobs(self):

        jobs = []

        var_names, var_vals = self.get_itervars()
        nvars = len(var_names)

        job_id = self.job_id_start

        max_iter = self._get_max_iterations()
        if max_iter < 1:
            max_iter = self.repeat
        else:
            max_iter = max_iter * self.repeat

        njobs = 0
        try:
            for var_index in range(len(var_vals)):
                jobdata = JobData()
                for j in range(nvars):
                    jobdata.set_param(var_names[j], var_vals[var_index][j])
                input_files = copy.deepcopy(self.input_files)
                for r in range(max_iter):
                    jobdata.set('job_id', job_id)
                    jobdata.set('sequence', r)
                    if (len(input_files.keys())):
                        for input_name in list(input_files.keys()):
                            job_input_files = []
                            nreads = input_files[input_name][1]
                            for iread in range(nreads):
                                input_file = input_files[input_name][0].pop(0)
                                job_input_files.append(input_file)
                            jobdata.input_files[input_name] = job_input_files
                    jobdata_copy = copy.deepcopy(jobdata)
                    jobs.append(jobdata_copy)
                    job_id += 1
                    njobs += 1
                    if njobs >= self.max_jobs:
                        raise MaxJobsException(self.max_jobs)
        except MaxJobsException as mje:
            print(mje)

        return jobs

    def _read_input_file_list(self, input_file_list):
        """! Read the input file list from arg parsing."""
        for f in input_file_list:
            name = f[0]
            if name in list(self.input_files.keys()):
                raise Exception('Duplicate input file list name: %s' % name)
            input_file = f[1]
            nreads = int(f[2])
            input_file_list = []
            with open(input_file, 'r') as f:
                lines = f.readlines()
                for line in lines:
                    if len(line.strip()):
                        input_file_list.append(line.strip())
                if not len(input_file_list):
                    raise Exception('Failed to read any input files from file: %s' % input_file)
                self.input_files[name] = (input_file_list, nreads)

    def parse_args(self):
        """! Parse arguments for template engine."""

        parser = argparse.ArgumentParser(description="Create a JSON job store from a jinja2 template")
        parser.add_argument("-j", "--job-start", nargs="?", type=int, help="Starting job ID", default=0)
        parser.add_argument("-a", "--var-file", help="Variables in JSON format for iteration")
        parser.add_argument("-i", "--input-file-list", action='append', nargs=3,
                            metavar=('NAME', 'FILE', 'NREADS'), help="Unique name of input file list, path on disk, number of files to read per job")
        parser.add_argument("-r", "--repeat", type=int, help="Number of times to repeat job parameters", default=1)
        parser.add_argument("-m", "--max-jobs", type=int, help="Max number of jobs to generate", default=sys.maxsize)
        parser.add_argument("template_file", help="Job template in JSON format with jinja2 markup")
        parser.add_argument("output_file", help="Output file containing the generated JSON job store")

        cl = parser.parse_args()

        self.job_id_start = cl.job_start

        self.repeat = cl.repeat

        self.max_jobs = cl.max_jobs

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


if __name__ == '__main__':
    job_tmpl = JobTemplate()
    job_tmpl.parse_args()
    job_tmpl.run()
