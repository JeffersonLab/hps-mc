"""! Utilities for writing config files for jobs from environment variables."""

import argparse

from tools import *
from generators import *

from job import Job
from help import _ignore

def write_config(filename, component_names, include_defaults, fail_on_missing):
    """! Write a config file from environment variables.
    @param filename  name of config file
    @param component_names  list of components in job
    @param include_defaults  set true if defaults should be included
    @param fail_on_missing  if true: method fails if environment variable is missing
                            if false: throws warning if environment variable is missing"""
    lines = []
    if include_defaults:
        lines.extend(_get_job_defaults())
    for k in sorted(globals().keys()):
        v = globals()[k]
        if isinstance(v, Component.__class__):
            if v.__name__ not in _ignore:
                if len(component_names) and v.__name__ not in component_names:
                    #print('Skipping ' + v.__name__)
                    continue
                #print('Config env for ' + v.__name__)
                obj = eval(v.__name__)()
                section = '[%s]\n' % v.__name__
                lines += section
                for c in obj.required_config():
                    env_var = c.upper()
                    if env_var in os.environ:
                        s = '%s = %s\n' % (c, os.environ[env_var])
                        lines += s
                    elif fail_on_missing:
                        raise Exception('Missing env var: %s' % env_var)
                    else:
                        print("WARNING: Missing env var: %s" % env_var)
                lines += '\n'
    with open(filename, 'w') as f:
        f.writelines(lines)
    print('Wrote config: %s' % filename)

def write_config_for_job(job_script, filename, include_defaults, fail_on_missing):
    """! Write a config file for a specific job script.
    @param job_script  job script
    @param filename  currently unused
    @param include_defaults  set true if defaults should be included
    @param fail_on_missing  if true: method fails if environment variable is missing
                            if false: throws warning if environment variable is missing
    """
    j = Job()
    j.script = job_script
    j._load_script()
    component_names = [c.__class__.__name__ for c in j.components]
    write_config('job.cfg', component_names, include_defaults, fail_on_missing)

def _get_job_defaults():
    """! Get default job class settings."""

    lines = []
    lines += '[Job]\n'
    j = Job()
    for cj in j._config_names:
        v = getattr(j, cj)
        lines += '%s = %s\n' % (cj, v)
    lines += '\n'
    return lines

if __name__ == '__main__':

    parser = argparse.ArgumentParser("config_writer.py")
    parser.add_argument('-I', '--ignore-missing', action='store_true', help='Do not crash if env var is missing.')
    parser.add_argument('-J', '--job-defaults', action='store_true', help="Add job defaults to the config")
    parser.add_argument('script', nargs='?', help="Job script")
    parser.add_argument('config', nargs='?', help="Config file")

    cl = parser.parse_args()

    if not cl.script:
        raise Exception('Missing required job script argument.')

    if cl.config:
        config = cl.config
    else:
        config = 'job.cfg'

    write_config_for_job(cl.script, config, cl.job_defaults, (not cl.ignore_missing))
