"""Simple utility for converting a short name into the path to an HPSMC job script."""

import glob, os, sys

class JobScriptDatabase:

    def __init__(self):
        if 'HPSMC_DIR' not in os.environ:
            raise Exception('HPSMC_DIR is not set in the environ.')
        hps_mc_dir = os.environ['HPSMC_DIR']
        script_dir = os.path.join(hps_mc_dir, 'lib', 'python', 'jobs')
        self.scripts = {}
        for f in glob.glob(os.path.join(script_dir, '*_job.py')):
            script_name = os.path.splitext(os.path.basename(f))[0].replace('_job', '')
            self.scripts[script_name] = f
    
    def get_script_path(self, name):
        return self.scripts[name]
        
    def get_script_names(self):
        return self.scripts.keys()
    
    def get_scripts(self):
        return self.scripts
        
    def exists(self, name):
        return name in self.scripts
