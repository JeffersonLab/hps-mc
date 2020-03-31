import os, sys, time, shutil, argparse, getpass, json, logging, subprocess, collections
from component import Component

logger = logging.getLogger("hpsmc.job")
logger.setLevel(logging.DEBUG)

import hpsmc.config as config

def load_json_file(filename):
    rawdata = open(filename, 'r').read()
    return json.loads(rawdata)

class Job:
    """
    Primary class to run HPS jobs from a Python script.
    """

    def __init__(self, **kwargs):
        
        if "name" in kwargs:
            self.name = kwargs["name"] 
        else:
            self.name = "HPS MC Job"
        
        self.components = []

        # FIXME: This doesn't belong here and the run dir should not be set with keyword args.
        if "rundir" in kwargs:
            self.rundir = kwargs["rundir"]
        else:
            # Special config for running in LSF.
            # TODO: Better to put this in batch.py instead since env check would not be necessary
            if "LSB_JOBID" in os.environ:
                self.rundir = os.path.join("/scratch", getpass.getuser(), os.environ["LSB_JOBID"])
                self.delete_rundir = True
            else:
                self.rundir = os.getcwd()
        logger.info("Run dir set to '%s'" % self.rundir)    
        
        # TODO: put this in config instead
        if "job_id_pad" in kwargs:
            self.job_id_pad = kwargs["job_id_pad"]
        else:
            self.job_id_pad = 4
                                
        # TODO: put this in config instead    
        if "set_component_seeds" in kwargs:
            self.set_component_seeds = kwargs["set_component_seeds"]
        else:
            self.set_component_seeds = True

        self.params = {}        
        
        self.log_out = sys.stdout
        self.log_err = sys.stderr
        
        self.input_files = {}
        self.output_files = {}
        
        self.out_file = None
        self.err_file = None
        
        self.seed = 1
        
        self.output_dir = os.getcwd()
        
        self.rundir = os.getcwd()
        
        self.job_id = 1
        
        self.enable_copy_output_files = True
        self.enable_copy_input_files = True
        self.delete_existing = False
        self.delete_existing = False
        self.delete_rundir = False
        self.dry_run = False
        
        self.__initialize()

    def add(self, component):
        """
        Public method for adding components to the job.
        """
        if isinstance(component, collections.Sequence) and not isinstance(obj, basestring):
            self.components.extend(component)
        else:
            self.components.append(component)

    def add_parameters(self, params):
        """
        Add parameters to the job, overriding values if they exist already.
        
        This method can be used in job scripts to define default values.
        """
        for k,v in params.iteritems():
            if k in self.params:
                logger.info("Setting new value '%s' for parameter '%s' with existing value '%s'."
                            % (str(v), str(k), params[k]))
            self.params[k] = v
                    
    def __parse_args(self):
        
        parser = argparse.ArgumentParser(description=self.name)
        parser.add_argument("-c", "--config", nargs=1, help="Config file location")
        parser.add_argument("-o", "--out", nargs=1, help="Log file for job stdout")
        parser.add_argument("-e", "--err", nargs=1, help="Log file for job stderr")
        parser.add_argument("-L", "--level", nargs=1, help="Global log level")
        parser.add_argument("-s", "--job-steps", type=int, default=-1, 
                            help="Job steps to run (single number)")
        parser.add_argument("-d", "--run-dir", nargs=1, help="Job run dir")
        parser.add_argument("params", nargs=1, help="Job params in JSON format")
        
        # TODO: CL option to disable automatic copying of ouput files
        # parser.add_argument("--no-copy-output-files", help="Disable copying of output files")
        #parser.add_argument('--feature', dest='feature', action='store_true')
        #parser.add_argument('--no-feature', dest='feature', action='store_false')
        #parser.set_defaults(feature=True)
        
        cl = parser.parse_args()
            
        self.config = cl.config
        
        if cl.level:
            level = logging.getLevelName(cl.level[0])
            logger.info("Setting log level to '%s'" % level)
            logging.basicConfig(level=level)
        
        if cl.out:
            self.out_file = os.path.join(self.rundir, cl.out[0])
            logger.info("Stdout will be redirected to '%s'" % self.out_file)
                    
        if cl.err:
            self.err_file = os.path.join(self.rundir, cl.err[0])
            logger.info("Stderr will be redirected to '%s'" % self.err_file)
        
        self.job_steps = cl.job_steps
        
        if cl.params:
            self.param_file = cl.params[0]
            logger.info("Loading job params from '%s'" % self.param_file)
            self.add_parameters(load_json_file(cl.params[0]))
            logger.info(json.dumps(self.params, indent=4, sort_keys=False))
        
        if 'output_dir' in self.params:
            self.output_dir = self.params['output_dir']
        if not os.path.isabs(self.output_dir):
            self.output_dir = os.path.abspath(self.output_dir)
            logger.info("Changed output dir to abs path '%s'" % self.output_dir)
        
        if 'job_id' in self.params:
            self.job_id = self.params['job_id']
        
        if cl.run_dir:
            self.rundir = cl.run_dir[0]
  
    def __initialize(self):
        """
        Perform basic initialization before adding job components.
        """
                                
        self.__parse_args()

        if self.config:
            logger.info("Reading config from '%s'" % self.config)
            config.parser.read(self.config)

        if not os.path.exists(self.rundir):
            logger.info("Creating run dir '%s'" % self.rundir)
            os.makedirs(self.rundir)

        logger.info("Changing to run dir '%s'" % self.rundir)
        os.chdir(self.rundir)

        if self.out_file:
            self.log_out = open(self.out_file, "w")
        if self.err_file:
            self.log_err = open(self.err_file, "w")
        
        if 'input_files' in self.params:
            self.input_files = self.params['input_files']
        if 'output_files' in self.params:
            self.output_files = self.params['output_files']
                   
    def run(self): 
        """
        This is the primary execution method that should be called at the end of a job script.
        It will configure, setup, and execute this class and the components and then
        perform cleanup after the job finishes.
        """
        
        if not len(self.components):
            raise Exception("Job has no components to execute.")

        # Print job parameters.
        logger.info("Job parameters: " + str(self.params))
        
        # This will configure the Job class and its components by copying
        # information into them from the .hpsmc config file.
        self.__configure()
        
        # Set component parameters from job JSON file.                
        self.__set_parameters()
        
        # Perform component setup to prepare for execution.
        # May use config and parameters set from above.
        self.__setup()
        
        # Copy the input files to the run dir if not in dry run mode
        # and this feature is enabled.
        if not self.dry_run:
            if self.enable_copy_input_files: 
                self.__copy_input_files()
                
        # Execute the job.
        self.__execute()
        
        # Copy the output files to the output dir not in dry run mode
        # and this feature is enable.
        if not self.dry_run:
            if self.enable_copy_output_files:
                self.__copy_output_files()
            
            # Perform job cleanup.
            self.__cleanup()
                      
    def __execute(self):
        
        logger.info("Running job '%s'" % self.name)
            
        if not self.dry_run:
            for c in self.components:
                logger.info("Executing '%s' with inputs %s and outputs %s" % 
                            (c.name, str(c.inputs), str(c.outputs)))
                start = time.time()
                returncode = c.execute(self.log_out, self.log_err)
                end = time.time()
                elapsed = end - start
                logger.info("Execution of '%s' took %d second(s)" % (c.name, elapsed))
                if returncode is not None:
                    logger.info("Return code of '%s' was %d" % (c.name, returncode))
                else:
                    logger.info("No return code from '%s'" % c.name)
                    
                # TODO: Add check here on component outputs and raise exception if do not exist.    
                
                # FIXME: All return codes ignored for now.
                # if not self.ignore_returncode and proc.returncode:
                #     raise Exception("Component: error code %d returned by '%s'" % (proc.returncode, self.name))
        else:
            # Dry run mode. Just print component info but do not execute.
            logger.info("Dry run enabled. Components will NOT be executed!")
            for c in self.components:
                logger.info("'%s' with args: %s (NOT EXECUTED)" % (c.name, ' '.join(c.cmd_args())))
                            
    def __configure(self):
            
        p = config.parser  
        
        default = 'Job'
        
        try:            
            self.enable_copy_output_files = p.getboolean(default, 'copy_output_files')
            logger.debug("enable_copy_output_files=%s" % str(self.enable_copy_output_files))
        except:
            pass
                
        try:        
            self.enable_copy_input_files = p.getboolean(default, 'copy_input_files')
            logger.debug("enable_copy_input_files=%s" % str(self.enable_copy_input_files))
        except:
            pass
        
        try:
            self.delete_existing = p.getboolean(default, 'delete_existing')
            logger.debug("delete_existing=%s" % str(self.delete_existing))
        except:
            pass
        
        try:
            self.dry_run = p.getboolean(default, 'dry_run')
            logger.debug("dry_run=%s" % str(self.dry_run))
        except:
            pass
                
        for c in self.components:
            logger.info("Configuring job component '%s'" % c.name)
            c.config()

    def __setup(self):
        # limit components according to job steps
        if self.job_steps > 0:
            self.components = self.components[0:self.job_steps]
            logger.info("Job is limited to first %d steps." % self.job_steps)

        self.__config_file_pipeline()

        # run setup methods of each component
        for c in self.components:
            logger.info("Setting up '%s'" % (c.name))
            c.rundir = self.rundir
            
            if self.set_component_seeds:
                logger.info("Setting seed on '%s' to %d" % (c.name, self.seed))
                c.seed = self.seed
            c.setup()
#            if not c.cmd_exists():
#                raise Exception("Command '%s' does not exist for '%s'." % (c.command, c.name))

    def __config_file_pipeline(self):
        for i in range(0, len(self.components)):
            logger.info("Configuring file IO for component %d" % i)
            c = self.components[i]
            if i == 0:
                logger.info("Setting inputs on '%s' to: %s"
                            % (c.name, str(self.input_files.values())))
                c.inputs = self.input_files.values()
            elif i - 1 > 0:
                logger.info("Setting inputs on '%s' to: %s"
                            % (c.name, str(self.components[i - 1].output_files())))
                c.inputs = self.components[i - 1].output_files()
                            
    def __set_parameters(self):
        """
        Push JSON job parameters to components.
        """
        for c in self.components:
            c.set_parameters(self.params)

    def __cleanup(self):
        """
        Perform post-job cleanup.
        """
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
    
    def __copy_output_files(self):
                
        if not os.path.exists(self.output_dir):
            logger.info("Creating output dir '%s'" % self.output_dir)
            os.makedirs(self.output_dir, 0755)

        # debug
        #logger.info("pwd: " + os.getcwd())
        #p = subprocess.Popen(['ls', os.getcwd()], shell=True, stdout=subprocess.PIPE)
        #out, err = p.communicate()
        #print "dir list..."
        #print out
        
        for src,dest in self.output_files.iteritems():
            self.__copy_output_file(src, dest)
                                         
    def __copy_output_file(self, src, dest):
                    
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
            
    def __copy_input_files(self):
        for src,dest in self.input_files.iteritems():
            if not os.path.isabs(src):
                # FIXME: Could try and convert to abspath here.
                raise Exception("The input source file '%s' is not an absolute path." % src)            
            if os.path.dirname(dest):
                raise Exception("The input file destination '%s' is not valid." % dest)
            logger.info("Copying input '%s' to '%s'" % (src, os.path.join(self.rundir, dest)))
            shutil.copyfile(src, os.path.join(self.rundir, dest))
         
            