"""Primary class for running and managing HPSMC jobs defined by a set of components."""

import os, sys, time, shutil, argparse, getpass, json, logging, subprocess, collections
import ConfigParser as configparser
from os.path import expanduser
from component import Component
from script_db import JobScriptDatabase
from job_store import JobStore
from util import convert_config_value, config_logging, load_json_data

logger = logging.getLogger('hpsmc.job')
logger.setLevel(logging.DEBUG)

class JobConfig(object):
    """Wrapper for accessing config information from parser."""
    
    def __init__(self, config_files=[], include_default_locations=True):
        self.config_files = []
        if include_default_locations:
            self.config_files.extend([os.path.join(expanduser("~"), ".hpsmc"),
                                      os.path.abspath(".hpsmc")])
        if len(config_files):
            self.config_files.extend(config_files)
        
        if not len(self.config_files):
            raise Exception('No config file locations provided.')
        
        self._load()
         
    def _load(self):
        """Load config from the list of possible locations."""

        # Read in config files and crash if none are found from list
        self.parser = configparser.ConfigParser()
        logger.info("Checking for config files: %s" % str(self.config_files))
        parsed = self.parser.read(self.config_files)
        if not len(parsed):
            raise Exception('No config files found in locations: %s' % str(self.config_files))
        
        # Print detailed config info to the log
        parser_lines = ['Successfully read config from: %s' % str(parsed)]
        for section in self.parser.sections():
            parser_lines.append("[" + section + "]")
            for i,v in self.parser.items(section): 
                parser_lines.append("%s=%s" % (i, v))
        parser_lines.append('')
        logger.info('\n'.join(parser_lines))
            
    def config(self, obj, section=None, required_names=[], allowed_names=[], require_section=True):
        """Push config into an object by setting an attribute."""    
        if not section:
            section = obj.__class__.__name__
        if self.parser.has_section(section):
            # Check that required settings are there.
            for req in required_names:
                if req not in parser.items(section):
                    raise Exception("Missing required config '%s'" % req)            
            # Push each config item into the object by setting attribute.
            for name,value in self.parser.items(section):
                if len(allowed_names) and name not in allowed_names:
                    raise Exception("Config name '%s' is not allowed for '%s'" % (name, section))
                setattr(obj, name, convert_config_value(value))
                logger.info("%s:%s:%s=%s" % (obj.__class__.__name__, 
                                             name,
                                             getattr(obj, name).__class__.__name__, 
                                             getattr(obj, name)))
        elif require_section:
            raise Exception("Missing required config section '%s'" % section)
        else:
            logger.warning('Config section not found: %s' % section)

        
class Job(object):
    """
    Primary class to run HPS jobs from a Python script by executing
    a series of components which are configured using a config file 
    with parameters provided by a JSON job file.
    """

    # List of config names to be read for the Job class (all optional).
    _config_names = ['enable_copy_output_files',
                     'enable_copy_input_files',
                     'delete_existing',
                     'delete_rundir',
                     'dry_run',
                     'ignore_return_codes',
                     'job_id_pad',
                     'check_output_files',
                     'enable_file_chaining',
                     'enable_ptags',
                     'enable_env_config']

    # Prefix to indicate ptag in job param file.
    _ptag_prefix = 'ptag:'

    def __init__(self, args=sys.argv, **kwargs):
                    
        self.description = "HPS MC Job" # Should be overridden by the job script
                
        self.components = []

        self.rundir = os.getcwd()
                                            
        self.params = {}
        
        self.log_out = sys.stdout
        self.log_err = sys.stderr
        
        self.input_files = {}
        self.output_files = {}
        
        self.out_file = None
        self.err_file = None
                
        self.output_dir = os.getcwd()
        
        self.rundir = os.getcwd()
        
        self.job_id = None
        
        # These are all settable by config file.
        self.job_id_pad = 4
        self.enable_copy_output_files = True
        self.enable_copy_input_files = True
        self.delete_existing = False
        self.delete_rundir = False
        self.dry_run = False
        self.ignore_return_codes = True
        self.check_output_files = True
        self.check_commands = False
        self.enable_file_chaining = True
        self.enable_ptags = False
        self.enable_env_config = False
        
        self.param_file = None
        
        self.args = args
        
        # Mapping of tags to output files
        self.ptags = {}
        
    def add(self, component):
        """
        Public method for adding components to the job.
        """
        if isinstance(component, collections.Sequence) and not isinstance(component, basestring):
            self.components.extend(component)
        else:
            self.components.append(component)

    def set_parameters(self, params):                
        """
        Add parameters to the job, overriding values if they exist already.
        
        This method can be used in job scripts to define default values.
        """
        for k,v in params.iteritems():
            if k in self.params:
                logger.debug("Setting new value '%s' for parameter '%s' with existing value '%s'."
                             % (str(v), str(k), params[k]))
            self.params[k] = v
                    
    def parse_args(self):
        """
        Configure the job from command line arguments.
        """
                
        parser = argparse.ArgumentParser(description=self.description)
        parser.add_argument("-c", "--config-file", help="Config file locations", action='append')
        parser.add_argument("-d", "--run-dir", nargs='?', help="Job run dir")
        parser.add_argument("-o", "--out", nargs='?', help="File for component stdout")
        parser.add_argument("-e", "--err", nargs='?', help="File for component stderr")
        parser.add_argument("-l", "--log", nargs='?', help="File for logging output (default is print to terminal)")
        parser.add_argument("-L", "--level", nargs='?', help="Global log level")
        parser.add_argument("-s", "--job-steps", type=int, default=-1, 
                            help="Job steps to run (single number)")
        parser.add_argument("-i", "--job-id", type=int, help="Job ID from JSON job store", default=None)
        parser.add_argument("script", nargs='?', help="Path or name of job script")
        parser.add_argument("params", nargs='?', help="Job param file in JSON format")
        
        # TODO: CL option to disable automatic copying of ouput files.
        #       The files should be symlinked if copying is disabled.
        # parser.add_argument("--no-copy-output-files", help="Disable copying of output files")
        #parser.add_argument('--feature', dest='feature', action='store_true')
        #parser.add_argument('--no-feature', dest='feature', action='store_false')
        #parser.set_defaults(feature=True)
        
        cl = parser.parse_args(self.args)
        
        if cl.run_dir:
            self.rundir = cl.run_dir
                             
        if cl.level:
            num_level = getattr(logging, cl.level.upper(), None)
            if not isinstance(num_level, int):
                raise ValueError('Invalid log level: %s' % num_level)
            logging.getLogger('hpsmc').setLevel(num_level)
            #print("Set log level of hpsmc: %s" % logging.getLevelName(logging.getLogger('hpsmc').getEffectiveLevel()))
        
        if cl.log:
            self.log_file = cl.log
            if not os.path.isabs(self.log_file):
                self.log_file = os.path.abspath(self.log_file)
            config_logging(stream=open(self.log_file, 'w'))
             
        if cl.out:
            self.out_file = cl.out
            if not os.path.isabs(self.out_file):
                self.out_file = os.path.abspath(self.out_file)
                logger.info('Changed stdout file to abs path: %s' % self.out_file)
                    
        if cl.err:
            self.err_file = cl.err
            if not os.path.isabs(self.err_file):
                self.err_file = os.path.abspath(self.err_file)
                logger.info('Changed stderr file to abs path: %s' % self.err_file)
        
        if cl.config_file:
            self.config_files = map(os.path.abspath, cl.config_file)
        else:
            self.config_files = []
                
        self.job_steps = cl.job_steps
        
        if cl.script:
            self.script = cl.script
        else:
            raise Exception('Missing required script name or location.')
        
        # Params are actually optional as some job scripts might not need them.
        if cl.params:
            self.param_file = os.path.abspath(cl.params)
            params = {}
            if cl.job_id:
                # Load data from a job store containing multiple jobs.
                self.job_id = cl.job_id
                logger.info("Loading job with ID %d from job store '%s'" % (self.job_id, self.param_file))
                jobstore = JobStore(self.param_file)
                if jobstore.has_job_id(self.job_id):
                    params = jobstore.get_job(self.job_id)
                else:
                    raise Exception("No job id %d was found in the job store '%s'" % (self.job_id, self.param_file))
            else:
                # Load data from a JSON file with a single job definition.
                logger.info('Loading job parameters from file: %s' % self.param_file)
                params = load_json_data(self.param_file)
                if not isinstance(params, dict):
                    raise Exception('Job ID must be provided when running from a job store.')

            self.__load_params(params)
                  
    def __load_params(self, params):
        """
        Load the job parameters from JSON data.
        """
    
        self.set_parameters(params)
        
        logger.info(json.dumps(self.params, indent=4, sort_keys=False))
        
        if 'output_dir' in self.params:
            self.output_dir = self.params['output_dir']
        if not os.path.isabs(self.output_dir):
            self.output_dir = os.path.abspath(self.output_dir)
            logger.info("Changed output dir to abs path: %s" % self.output_dir)
        
        if 'job_id' in self.params:
            self.job_id = self.params['job_id']
  
    def __initialize(self):
        """
        Perform basic initialization before the job script is loaded.
        """
        
        if not os.path.isabs(self.rundir):
            self.rundir = os.path.abspath(self.rundir)
            logger.info('Changed run dir to abs path: %s' % self.rundir)
            #raise Exception('The run dir is not an absolute path: %s' % self.rundir)
            
        # Set run dir if running inside LSF
        if "LSB_JOBID" in os.environ:
            self.rundir = os.path.join("/scratch", getpass.getuser(), os.environ["LSB_JOBID"])
            logger.info('Set run dir for LSF: %s' % self.rundir)
            self.delete_rundir = True

        if self.out_file:
            self.log_out = open(self.out_file, 'w')
        if self.err_file:
            self.log_err = open(self.err_file, 'w')
        
        if 'input_files' in self.params:
            self.input_files = self.params['input_files']
        if 'output_files' in self.params:
            self.output_files = self.params['output_files']
    
    def __configure(self):
        # Configure job class
        self.job_config = JobConfig(config_files=self.config_files)
        self.job_config.config(self, allowed_names=Job._config_names, require_section=False)
        
        # Configure each of the job components
        for c in self.components:
            c.config(self.job_config.parser) # Configure from supplied config files
            if self.enable_env_config:
                c.config_from_environ()      # Configure from env vars, if enabled
            c.check_config()                 # Check that the config is acceptable
                   
    def _load_script(self):
        if not self.script.endswith('.py'):
            # Script is a name.
            script_db = JobScriptDatabase()
            if not script_db.exists(self.script):
                raise Exception("The script name is not valid: %s" % self.script)
            script_path = script_db.get_script_path(self.script)
            logger.debug("Found script '%s' from name '%s'" % (script_path, self.script))
        else:
            # Script is a path.
            script_path = self.script

        if not os.path.exists(script_path):
            raise Exception('Job script does not exist: %s' % script_path)
        
        logger.debug('Loading job script: %s' % script_path)

        globals = {'job': self}       
        execfile(script_path, globals)
                       
    def run(self):
        """
        This is the primary execution method for running the job.
        """
        
        logger.info("Running job: %s" % self.description)
        
        # Initialize after CL parameters were parsed.
        self.__initialize()
                
        # Load the job components from the script
        self._load_script()
        
        if not len(self.components):
            raise Exception("Job has no components to execute.")

        # Print list of job components
        logger.info("Job components: " % [c.name for c in self.components])        

        # Print job parameters.
        logger.info("Job parameters: " + str(self.params))
        
        # This will configure the Job class and its components by copying
        # information into them from loaded config files.
        self.__configure()
        
        # Set component parameters from job JSON file.
        self.__set_parameters()

        # Perform component setup to prepare for execution.
        # May use config and parameters that were set from above.
        self.__setup()

        if not self.dry_run:
            if self.enable_copy_input_files: 
                # Copy input files to the run dir.
                self.__copy_input_files()
            else:
                # Symlink input files if copying is disabled.
                self.__symlink_input_files()
                
        # Save job start time
        start_time = time.time()
                 
        # Execute the job.
        self.__execute()
        
        # Copy the output files to the output dir if enabled and not in dry run.
        if not self.dry_run:
            
            # Print job timer info
            stop_time = time.time()
            elapsed = stop_time - start_time
            logger.info("Job execution took %f seconds" % elapsed)
                        

            # Copy by key set in the job script
            if self.enable_ptags:
                self.__copy_ptag_output_files()
            
            # Copy by file path
            if self.enable_copy_output_files:
                self.__copy_output_files()
            
            # Perform job cleanup.
            self.__cleanup()
                           
        logger.info('Successfully finished running job: %s' % self.description)
                    
    def __execute(self):
                    
        if not self.dry_run:
                       
            for c in self.components:
                logger.info("Executing '%s' with inputs %s and outputs %s" % 
                            (c.name, str(c.input_files()), str(c.output_files())))
                start = time.time()
                if self.log_out != sys.stdout:
                    self.log_out.write('==== %s ====\n' % c.name) # Add header to output file
                    self.log_out.flush()
                returncode = c.execute(self.log_out, self.log_err)
                end = time.time()
                elapsed = end - start                
                logger.info("Execution of '%s' took %f second(s)" % (c.name, elapsed))
                logger.info("Return code of '%s' was %s" % (c.name, str(returncode)))
                                     
                if not self.ignore_return_codes and proc.returncode:
                    raise Exception("Non-zero return code %d from '%s'" % (proc.returncode, c.name))
                
                if self.check_output_files:
                    for outputfile in c.output_files():
                        if not os.path.isfile(outputfile):
                            raise Exception("Output file '%s' is missing after execution." % outputfile)
        else:
            # Dry run mode. Just print component command but do not execute it.
            logger.info("Dry run enabled. Components will NOT be executed!")
            for c in self.components:
                logger.info("'%s' with args: %s (DRY RUN)" % (c.name, ' '.join(c.cmd_args())))
                                   
    def __setup(self):
        
         # Create run dir if it does not exist
        if not os.path.exists(self.rundir):
            logger.info('Creating run dir: %s' % self.rundir)
            os.makedirs(self.rundir)

         # Change to run dir
        logger.debug('Changing to run dir: %s' % self.rundir)
        os.chdir(self.rundir)
        
        # Limit components according to job steps
        if self.job_steps > 0:
            self.components = self.components[0:self.job_steps]
            logger.info("Job is limited to first %d steps." % self.job_steps)
        else:
            logger.info("No job steps specified so full job will be run.")

        if self.enable_file_chaining:
            self.__config_file_pipeline()

        # Run setup methods of each component
        for c in self.components:
            logger.debug('Setting up component: %s' % (c.name))
            c.rundir = self.rundir
            c.setup()
            if self.check_commands and not c.cmd_exists():
                raise Exception("Command '%s' does not exist for '%s'." % (c.command, c.name))

    def __config_file_pipeline(self):
        """
        Pipe component outputs to inputs automatically.
        """
        for i in range(0, len(self.components)):
            c = self.components[i]
            logger.debug("Configuring file IO for component '%s' with order %d" % (c. name, i))
            if i == 0:
                logger.debug("Setting inputs on '%s' to: %s"
                            % (c.name, str(self.input_files.values())))
                if not len(c.inputs):
                    c.inputs = self.input_files.values()
            elif i > -1:
                logger.debug("Setting inputs on '%s' to: %s"
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
            logger.debug('Running cleanup for component: %s' % str(c.name))
            c.cleanup()
        if self.delete_rundir:
            logger.debug('Deleting run dir: %s' % self.rundir)
            shutil.rmtree(self.rundir)
        if self.log_out != sys.stdout:
            self.log_out.close()
        if self.log_err != sys.stderr:
            self.log_err.close()
    
    def __copy_output_files(self):
        """
        Copy output files to output directory.
        """        
        
        if not os.path.exists(self.output_dir):
            logger.debug('Creating output dir: %s' % self.output_dir)
            os.makedirs(self.output_dir, 0755)
        
        for src,dest in self.output_files.iteritems():
            if not Job._is_ptag(src):
                self.__copy_output_file(src, dest)
                                         
    def __copy_output_file(self, src, dest):
        """
        Copy an output file from src to dest.
        """

        src_file = os.path.join(self.rundir, src)
        dest_file = os.path.join(self.output_dir, dest)
        
        # Create directory if not exists; this allows relative path segments
        # in output file strings.
        if not os.path.exists(os.path.dirname(dest_file)):
            os.makedirs(os.path.dirname(dest_file), 0755)
        
        # Check if the file is already there and does not need copying (e.g. if running in local dir)
        samefile = False
        if os.path.exists(dest_file):
            if os.path.samefile(src_file, dest_file):
                samefile = True

        # If target file already exists then see if it can be deleted; otherwise raise an error
        if os.path.isfile(dest_file):
            if self.delete_existing:
                logger.debug('Deleting existing file: %s' % dest_file)
                os.remove(dest_file)
            else:
                raise Exception('Output file already exists: %s' % dest_file)

        # Copy the file to the destination dir if not already created by the job
        logger.info("Copying '%s' to '%s'" % (src_file, dest_file))
        if not samefile:
            shutil.copyfile(src_file, dest_file)
        else:
            logger.warning("Skipping copy of '%s' to '%s' because they are the same file!" % (src_file, dest_file))
            
    def __copy_input_files(self):
        """
        Copy input files to the run dir.
        """        
        for src,dest in self.input_files.iteritems():
            if not os.path.isabs(src):
                # FIXME: Could try and convert to abspath here.
                raise Exception("The input source file '%s' is not an absolute path." % src)            
            if os.path.dirname(dest):
                raise Exception("The input file destination '%s' is not valid." % dest)
            logger.info("Copying input file: %s -> %s" % (src, os.path.join(self.rundir, dest)))
            shutil.copyfile(src, os.path.join(self.rundir, dest))
         
    def __symlink_input_files(self):
        """
        Symlink input files.
        """
        for src,dest in self.input_files.iteritems():
            if not os.path.isabs(src):
                raise Exception('The input source file is not an absolute path: %s' % src)            
            if os.path.dirname(dest):
                raise Exception('The input file destination is not valid: %s' % dest)
            logger.debug("Symlinking input '%s' to '%s'" % (src, os.path.join(self.rundir, dest)))
            os.symlink(src, os.path.join(self.rundir, dest))
            
    def ptag(self, tag, filename):
        """
        Map a key to an output file name so a user can reference it in their job params.
        """
        if not tag in self.ptags.keys():
            self.ptags[tag] = filename
            logger.info("Added ptag %s -> %s" % (tag, filename))
        else:
            raise Exception('The ptag already exists: %s' % tag)
        
    def __copy_ptag_output_files(self):
        if len(self.ptags):
            for src,dest in self.output_files.iteritems():
                if Job._is_ptag(src):
                    ptag_src = Job._get_ptag_src(src)
                    if ptag_src in self.ptags.keys():
                        src_file = self.ptags[ptag_src]
                        logger.info("Copying ptag '%s' from '%s' -> '%s'" % (ptag_src, src_file, dest))
                        self.__copy_output_file(src_file, dest)
                    else:
                        raise Exception('Invalid ptag in job params: %s' % ptag_src)

    @staticmethod
    def _is_ptag(src):
        return src.startswith(Job._ptag_prefix)

    @staticmethod
    def _get_ptag_src(src):
        if src.startswith(Job._ptag_prefix):
            return src[len(Job._ptag_prefix):]
        else:
            raise Exception('File src is not a ptag: %s' % src)
                                        
cmds = {
    'run': 'Run a job script',
    'avail': 'Print available job names',
    'components': 'Print detailed help for all components (or provide component name)'}

def print_usage():
    print("Usage: job.py [command] [args]")
    print("    command:")
    for name,descr in cmds.iteritems():
        print("        %s: %s" % (name,descr))

if __name__ == '__main__':
    if len(sys.argv) > 1:
        cmd = sys.argv[1]
        if cmd not in cmds.keys():
            print_usage()
            raise Exception('The job command is not valid: %s' % cmd)
        args = sys.argv[2:]
        if cmd == 'run':
            job = Job(args)
            job.parse_args()
            job.run()
        elif cmd == 'avail':
            scriptdb = JobScriptDatabase()
            print("Available job scripts: ")
            for name in sorted(scriptdb.get_script_names()):
                print('    %s: %s' % (name, scriptdb.get_script_path(name)))    
        elif cmd == 'components':
            if len(sys.argv) > 2:
                component_name = sys.argv[2]
                from hpsmc.help import print_component                
                print_component(component_name)
            else:
                from hpsmc.help import print_components
                print_components()
                    
    else:
        print_usage()
    
