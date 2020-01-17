import os, sys, time, shutil, argparse, getpass, json, logging, subprocess
from component import Component

logger = logging.getLogger("hpsmc.job")

import hpsmc.config as config

class Job:

    def __init__(self, **kwargs):
        
        if "name" in kwargs:
            self.name = kwargs["name"] 
        else:
            self.name = "HPS MC Job"
            
        self.delete_rundir = False
        if "rundir" in kwargs:
            self.rundir = kwargs["rundir"]
        else:
            if "LSB_JOBID" in os.environ:
                self.rundir = os.path.join("/scratch", getpass.getuser(), os.environ["LSB_JOBID"])
                self.delete_rundir = True
            else:
                self.rundir = os.getcwd()
        logger.info("Run dir set to '%s'" % self.rundir)                

        if "components" in kwargs:
            self.components = kwargs["components"]
        else:
            self.components = []            
            
        if "job_id_pad" in kwargs:
            self.job_id_pad = kwargs["job_id_pad"]
        else:
            self.job_id_pad = 4
                        
        if "delete_existing" in kwargs:
            self.delete_existing = kwargs["delete_existing"]
        else:
            self.delete_existing = False
            
        if "set_component_seeds" in kwargs:
            self.set_component_seeds = kwargs["set_component_seeds"]
        else:
            self.set_component_seeds = True

        self.params = None
        self.default_params = {}
        
        self.log_out = sys.stdout
        self.log_err = sys.stderr
        
        self.input_files = {}
        self.output_files = {}
        
        self.seed = 1
        
        self.output_dir = os.getcwd()
        
        self.job_id = 1
        
        self.enable_copy_output_files = True
        self.enable_copy_input_files = True

    def set_default_params(self, default_params):
        self.default_params = default_params
                    
    def parse_args(self):
        
        parser = argparse.ArgumentParser(description=self.name)
        parser.add_argument("-c", "--config", nargs=1, help="Config file location")
        parser.add_argument("-o", "--out", nargs=1, help="Log file for job stdout")
        parser.add_argument("-e", "--err", nargs=1, help="Log file for job stderr")
        parser.add_argument("-L", "--level", nargs=1, help="Global log level")
        parser.add_argument("--job-steps", type=int, default=-1)
        parser.add_argument("params", nargs=1, help="Job params in JSON format")
        cl = parser.parse_args()
        
        self.config = cl.config
        
        if cl.level:
            level = logging.getLevelName(cl.level[0])
            logging.basicConfig(level=level)
        
        if cl.out:
            self.out_file = os.path.join(self.rundir, cl.out[0])
            logger.info("Stdout will be redirected to '%s'" % self.out_file)
        else:
            self.out_file = None
            
        if cl.err:
            self.err_file = os.path.join(self.rundir, cl.err[0])
            logger.info("Stderr will be redirected to '%s'" % self.err_file)
        else:
            self.err_file = None
        
        self.job_steps = cl.job_steps
        
        if cl.params:
            logger.info("Loading job params from '%s'" % cl.params[0])
            self.params = JobParameters(cl.params[0], defaults = self.default_params)
            logger.info(json.dumps(self.params.json_dict, indent=4, sort_keys=False))
        else:
            raise Exception("Missing required JSON file with job params.")
            
        self.input_files = self.params.input_files
        self.output_files = self.params.output_files
        self.seed = self.params.seed
        self.output_dir = self.params.output_dir
        if not os.path.isabs(self.output_dir):
            self.output_dir= os.path.abspath(self.output_dir)
            logger.info("Changed output dir to abs path '%s'" % self.output_dir)
        self.job_id = self.params.job_id

    def initialize(self):
                                
        self.parse_args()

        if self.config:
            logger.info("Reading config from '%s'" % self.config)
            config.parser.read(self.config)

        if not os.path.exists(self.rundir):
            logger.info("Creating run dir '%s'" % self.rundir)
            os.makedirs(self.rundir)

        os.chdir(self.rundir)

        if self.out_file:
            self.log_out = open(self.out_file, "w")
        if self.err_file:
            self.log_err = open(self.err_file, "w")
       
        if self.enable_copy_input_files: 
            self.copy_input_files()
            
    def run(self): 
        
        if not len(self.components):
            raise Exception("Job has no components.")

        if not hasattr(self, "params"):
            raise Exception("Job params were never parsed.")

        logger.info("Job parameters: " + str(self.params))
        
        self.configure()
        self.setup()
        self.execute()
        if self.enable_copy_output_files:
            self.copy_output_files()
        self.cleanup()
                      
    def execute(self):
        logger.info("Running '%s'" % self.name)
        if not len(self.components):
            raise Exception("Job has no components to execute.")
                
        for c in self.components:
            logger.info("Executing '%s' with inputs %s and outputs %s" % (c.name, str(c.inputs), str(c.outputs)))
            start = time.time()
            returncode = c.execute(self.log_out, self.log_err)
            end = time.time()
            elapsed = end - start
            logger.info("Execution of '%s' took %d second(s)" % (c.name, elapsed))
            logger.info("Return code of '%s' was %d" % (c.name, returncode))
            # TODO: figure out if this can be used
            # if not self.ignore_returncode and proc.returncode:
            #     raise Exception("Component: error code %d returned by '%s'" % (proc.returncode, self.name))
            
    def configure(self):
        for c in self.components:
            logger.info("job is configuring '%s'" % c.name)
            c.config()

    def setup(self):
        # limit components according to job steps
        if self.job_steps > 0:
            self.components = self.components[0:self.job_steps]
            logger.info("Job is limited to first %d steps." % self.job_steps)

        # run setup methods of each component
        for c in self.components:
            logger.info("Setting up '%s'" % (c.name))
            c.rundir = self.rundir
            if self.set_component_seeds:
                logger.info("Setting seed on '%s' to %d" % (c.name, self.seed))
                c.seed = self.seed
            #os.chdir(self.rundir)
            c.setup()
#            if not c.cmd_exists():
#                raise Exception("Command '%s' does not exist for '%s'." % (c.command, c.name))

    def cleanup(self):
        for c in self.components:
            logger.info("Running cleanup for '%s'" % str(c.name))
            c.cleanup()
        if self.delete_rundir:
            logger.info("Deleting run dir '%s'" % self.rundir)
            shutil.rmtree(self.rundir)
        if self.log_out != sys.stdout:
            self.log_out.close()
        if self.log_err != sys.stderr:
            self.log_err.close()
    
    def copy_output_files(self):
                
        if not os.path.exists(self.output_dir):
            logger.info("Creating output dir '%s'" % self.output_dir)
            os.makedirs(self.output_dir, 0755)

        # debug
        #logger.info("pwd: " + os.getcwd())
        #p = subprocess.Popen(['ls', os.getcwd()], shell=True, stdout=subprocess.PIPE)
        #out, err = p.communicate()
        #print "dir list..."
        #print out
        
        if isinstance(self.output_files, dict):
            for src,dest in self.output_files.iteritems():
                self.copy_output_file([src, dest])
        elif isinstance(self.output_files, list):
            for f in self.output_files:
                self.copy_output_file(f)
                                         
    def copy_output_file(self, src_dest):
        if isinstance(src_dest, list):
            src = src_dest[0]
            dest = src_dest[1]
        else:
            src = src_dest
            dest = src_dest
                    
        src_file = os.path.join(self.rundir, src)
        dest_file = os.path.join(self.output_dir, dest)
        
        # Check if the file is already there and does not need copying (e.g. if running in local dir)
        samefile = False
        if os.path.exists(dest_file):
            if os.path.samefile(src_file, dest_file):
                samefile = True

        # If target file already exists then see if it can be deleted; otherwise raise an error
        if os.path.isfile(dest_file):
            if self.delete_existing:
                logger.info("Deleting existing file at '%s'" % dest_file)
                os.remove(dest_file)
            else:
                raise Exception("Output file '%s' already exists." % dest_file)

        # Copy the file to the destination dir if not already created by the job
        logger.info("Copying '%s' to '%s'" % (src_file, dest_file))
        if not samefile:
            shutil.copyfile(src_file, dest_file)
        else:
            logger.warning("Skipping copy of '%s' to '%s' because they are the same file!" % (src_file, dest_file))
            
    def copy_input_files(self):
        if isinstance(self.input_files, dict):
            for dest,src in self.input_files.iteritems():
                if not os.path.isabs(src):
                    raise Exception("The input source file '%s' is not an absolute path." % src)
                if os.path.dirname(dest):
                    raise Exception("The input file destination '%s' is not valid." % dest)
                logger.info("Copying input '%s' to '%s'" % (src, os.path.join(self.rundir, dest)))
                shutil.copyfile(src, os.path.join(self.rundir, dest))
        elif isinstance(self.input_files, list):
            logger.warning("Skipping copy for input file list.")
        else:
            raise Exception("Invalid input files - must be dict or list.")
            
class JobParameters:
    
    def __init__(self, filename = None, defaults = {}):
        if filename:
            self.load(filename)

        if not hasattr(self, "input_files"):
            self.input_files = {}

        if not hasattr(self, "output_files"):
            self.output_files = {}

        if not hasattr(self, "seed"):
            self.seed = 1

        if not hasattr(self, "output_dir"):
            self.output_dir = os.getcwd()

        if not hasattr(self, "job_id"):
            self.job_id = 1

        if not hasattr(self, "nevents"):
            # WARNING: This might cause certain components to blow up if not overridden!
            self.nevents = -1

        self.defaults = defaults

    def load(self, filename):
        rawdata = open(filename, 'r').read()
        self.json_dict = json.loads(rawdata)

    def __getattr__(self, attr):
        if attr in self.json_dict:
            return self.json_dict[attr]
        else:
            raise AttributeError("%r has no attribute '%s'" % (self.__class__, attr))

    def __getitem__(self, key):
        if key in self.json_dict:
            # from the JSON file
            return self.json_dict[key]
        elif key in self.defaults:
            # from defaults supplied by the job script
            return self.defaults[key]
        elif key in vars(self):
            # from a variable on the params (e.g. for job_id, seed, etc.)
            return vars(self)[key]
        else:
            # parameter was not set
            raise Exception("%r has no item '%s'" % (self.__class__, key))

    def __contains__(self, item):
        return item in self.json_dict or item in self.defaults or item in vars(self)

    def __str__(self):
        return "job params: " + str(self.json_dict) + ", defaults: " + str(self.defaults)
     
