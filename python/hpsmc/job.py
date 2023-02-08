"""! @package job
Primary class for running and managing HPSMC jobs defined by a set of components.
"""

import os
import sys
import json
import time
import shutil
import argparse
import getpass
import logging
import configparser
import glob
import subprocess

from collections.abc import Sequence
from os.path import expanduser

from hpsmc.util import convert_config_value, config_logging

## Initialize logger with default level
logger = logging.getLogger('hpsmc.job')
logger.setLevel(logging.INFO)


def check(j):
    jsonfile = j.application.args[1]
    with open(jsonfile) as datafile:
        data = json.load(datafile)
    output_dir = ""
    if "output_dir" in data:
        output_dir = data["output_dir"]
    if "output_files" in data:
        outputfiles = data["output_files"]
        for k, v in outputfiles.items():
            fpath = v
            if not os.path.isabs(v):
                fpath = output_dir + os.path.sep + v
            if not os.path.exists(fpath):
                return False
    return True


def load_json_data(filename):
    """! Load json data from file.
    @param filename  name of json file
    """
    rawdata = open(filename, 'r').read()
    return json.loads(rawdata)


class JobConfig(object):
    """! Wrapper for accessing config information from parser."""

    # , include_default_locations=True
    def __init__(self, config_files=[], include_default_locations=True):
        ## list of config files
        self.config_files = []
        if include_default_locations:
            self.config_files.extend([os.path.join(expanduser("~"), ".hpsmc"),
                                      os.path.abspath(".hpsmc")])

        if len(config_files):
            self.config_files.extend(config_files)

        # if not len(self.config_files):
        #    raise Exception('No config file locations provided.')

        self._load()

    def _load(self):
        """! Load config from the list of possible locations."""
        ## \todo implement or cleanup
        # Read in config files and crash if none are found from list
        ## configuration parser
        self.parser = configparser.ConfigParser()
        # logger.debug("Checking for config files: %s" % str(self.config_files))
        # parsed =
        self.parser.read(self.config_files)
        # if not len(parsed):
        #    raise Exception('No config files found in locations: %s' % str(self.config_files))

        # Print detailed config info to the log
        # if len(parsed):

    def __str__(self):
        parser_lines = ['JobConfig:']
        for section in self.parser.sections():
            parser_lines.append("[" + section + "]")
            for i, v in self.parser.items(section):
                parser_lines.append("%s=%s" % (i, v))
            parser_lines.append('')
        return '\n'.join(parser_lines)

    def config(self, obj, section=None, required_names=[], allowed_names=[], require_section=True):
        """! Push config into an object by setting an attribute.
        @param obj  object config is pushed to
        @param section  object name (basically name of the component the config is pushed to?)
        @param required_names  list of names of required configurations
        @param allowed_names  list of allowed config names
        """
        if not section:
            section = obj.__class__.__name__
        if self.parser.has_section(section):
            # Check that required settings are there.
            for req in required_names:
                if req not in dict(self.parser.items(section)):
                    raise Exception("Missing required config '%s'" % req)
            # Push each config item into the object by setting attribute.
            for name, value in self.parser.items(section):
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


class JobStore:
    """!
    Simple JSON based store of job data.
    """

    def __init__(self, path=None):
        self.path = path
        if self.path:
            # logger.info("Initializing job store from: {}".format(self.path))
            self.load(self.path)

    def load(self, json_store):
        """! Load raw JSON data into this job store.
        @param json_store  json file containing raw job data
        """
        if not os.path.exists(json_store):
            raise Exception('JSON job store does not exist: {}'.format(json_store))
        with open(json_store, 'r') as f:
            json_data = json.loads(f.read())
        ## dict of jobs, sorted by job id
        self.data = {}
        for j in json_data:
            self.data[j['job_id']] = j
        # logger.info("Loaded %d jobs from job store: %s" % (len(self.data), json_store))

    def get_job(self, job_id):
        """! Get a job by its job ID.
        @param job_id  job ID
        @return job"""
        return self.data[int(job_id)]

    def get_job_data(self):
        """! Get the raw dict containing all the job data."""
        return self.data

    def get_job_ids(self):
        """! Get a sorted list of job IDs."""
        return sorted(self.data.keys())

    def has_job_id(self, job_id):
        """! Return true if the job ID exists in the store."""
        return job_id in list(self.data.keys())


class JobScriptDatabase:
    """! Database of job scripts.
    """

    def __init__(self):
        if 'HPSMC_DIR' not in os.environ:
            raise Exception('HPSMC_DIR is not set in the environ.')
        hps_mc_dir = os.environ['HPSMC_DIR']
        script_dir = os.path.join(hps_mc_dir, 'lib', 'python', 'jobs')
        ## dict of paths to job scripts sorted by name
        self.scripts = {}
        for f in glob.glob(os.path.join(script_dir, '*_job.py')):
            script_name = os.path.splitext(os.path.basename(f))[0].replace('_job', '')
            self.scripts[script_name] = f

    def get_script_path(self, name):
        """! Get path to job script from job name.
        @param job name
        @return path to job script"""
        return self.scripts[name]

    def get_script_names(self):
        """! Get list of all script names."""
        return list(self.scripts.keys())

    def get_scripts(self):
        """! Get dict containing paths to scripts sorted by script names."""
        return self.scripts

    def exists(self, name):
        """! Test if job script exists in dict.
        @return True if job name is key in job dict"""
        return name in self.scripts


class Job(object):
    """!
    Primary class to run HPS jobs from a Python script.

    Jobs are run by executing a series of components
    which are configured using a config file with parameters
    provided by a JSON job file.
    """

    ## \todo is this still needed?
    ## List of config names to be read for the Job class (all optional).
    """
    _config_names = ['enable_copy_output_files',
                     'enable_copy_input_files',
                     'delete_existing',
                     'delete_rundir',
                     'dry_run',
                     'ignore_return_codes',
                     'check_output_files',
                     'enable_file_chaining',
                     'enable_env_config']
    """

    # Prefix to indicate ptag in job param file.
    PTAG_PREFIX = 'ptag:'

    def __init__(self, args=sys.argv, **kwargs):
        ## (passed) job arguments
        self.args = args
        ## short description of job, should be overridden by the job script
        self.description = "HPS MC Job"
        ## job ID
        self.job_id = None
        ## path to parameter file
        self.param_file = None
        ## list of components in job
        self.components = []
        ## rundir is current working directory
        self.rundir = os.getcwd()
        ## dict of parameters
        self.params = {}
        ## output_dir is current working directory
        self.output_dir = os.getcwd()
        ## dict of input files
        self.input_files = {}
        ## dict of output files
        self.output_files = {}
        ## dict with keys to output filenames
        self.ptags = {}
        ## log output
        self.log = sys.stdout
        ## system output
        self.out = sys.stdout
        ## error output
        self.err = sys.stderr

        ## These attributes can all be set in the config file.
        self.enable_copy_output_files = True
        self.enable_copy_input_files = True
        self.delete_existing = False
        self.delete_rundir = False
        self.dry_run = False
        self.ignore_return_codes = True
        self.check_output_files = True
        self.check_commands = False
        self.enable_file_chaining = True
        self.enable_env_config = False
        self.log_level = logging.INFO

    def add(self, component):
        """!
        Public method for adding components to the job.
        """
        if isinstance(component, Sequence) and not isinstance(component, str):
            self.components.extend(component)
        else:
            self.components.append(component)

    def set_parameters(self, params):
        """!
        Add parameters to the job, overriding values if they exist already.

        This method can be used in job scripts to define default values.
        """
        for k, v in params.items():
            if k in self.params:
                logger.debug("Setting new value '%s' for parameter '%s' with existing value '%s'."
                             % (str(v), str(k), params[k]))
            self.params[k] = v

    def parse_args(self):
        """!
        Configure the job from command line arguments.
        """

        parser = argparse.ArgumentParser(description=self.description)
        parser.add_argument("-c", "--config-file", help="Config file locations", action='append')
        parser.add_argument("-d", "--run-dir", nargs='?', help="Job run dir")
        parser.add_argument("-o", "--out", nargs='?', help="File for component stdout")
        parser.add_argument("-e", "--err", nargs='?', help="File for component stderr")
        parser.add_argument("-l", "--log", nargs='?', help="File for logging output (default is print to terminal)")
        parser.add_argument("-s", "--job-steps", type=int, default=-1,
                            help="Job steps to run (single number)")
        parser.add_argument("-i", "--job-id", type=int, help="Job ID from JSON job store", default=None)
        parser.add_argument("script", nargs='?', help="Path or name of job script")
        parser.add_argument("params", nargs='?', help="Job param file in JSON format")

        ## \todo: CL option to disable automatic copying of ouput files.
        # The files should be symlinked if copying is disabled.
        # parser.add_argument("--no-copy-output-files", help="Disable copying of output files")
        # parser.add_argument('--feature', dest='feature', action='store_true')
        # parser.add_argument('--no-feature', dest='feature', action='store_false')
        # parser.set_defaults(feature=True)

        cl = parser.parse_args(self.args)

        # Read in job configuration files
        config_files = []
        if cl.config_file:
            config_files = list(map(os.path.abspath, cl.config_file))
        self.job_config = JobConfig(config_files=config_files)

        # Configure job log level
        try:
            self.log_level = logging.getLevelName(
                self.job_config.parser.get('Job', 'log_level'))
        except BaseException:
            pass

        # Configure logging stream to file
        if cl.log:
            log_file = cl.log
            if not os.path.isabs(log_file):
                log_file = os.path.abspath(log_file)
            self.log = open(log_file, 'w')

        # Configure logging from command line parameters and configuration files,
        # including sub-modules that are used in job scripts
        config_logging(stream=self.log, level=self.log_level, logname='hpsmc.job')
        config_logging(stream=self.log, level=self.log_level, logname='hpsmc.component')
        config_logging(stream=self.log, level=self.log_level, logname='hpsmc.tools')
        config_logging(stream=self.log, level=self.log_level, logname='hpsmc.generators')

        # Set file for stdout from components
        if cl.out:
            out_file = cl.out
            if not os.path.isabs(out_file):
                out_file = os.path.abspath(out_file)
                logger.debug('Changed stdout file to abs path: %s' % out_file)
            self.out = open(out_file, 'w')

        # Set file for stderr from components
        if cl.err:
            err_file = cl.err
            if not os.path.isabs(err_file):
                err_file = os.path.abspath(err_file)
                logger.debug('Changed stderr file to abs path: %s' % err_file)
            self.err = open(err_file, 'w')

        # if cl.level:
        #    num_level = getattr(logging, cl.level.upper(), None)
        #    if not isinstance(num_level, int):
        #        raise ValueError('Invalid log level: %s' % num_level)
        #    logging.getLogger('hpsmc').setLevel(num_level)
        # print("Set log level of hpsmc: %s" % logging.getLevelName(logging.getLogger('hpsmc').getEffectiveLevel()))

        if cl.run_dir:
            self.rundir = cl.run_dir

        self.job_steps = cl.job_steps

        if cl.script:
            ## name of or path to script
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
                logger.debug("Loading job with ID %d from job store '%s'" % (self.job_id, self.param_file))
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

            self._load_params(params)

    def _load_params(self, params):
        """!
        Load the job parameters from JSON data.
        """

        self.set_parameters(params)

        # logger.info(json.dumps(self.params, indent=4, sort_keys=False))

        if 'output_dir' in self.params:
            self.output_dir = self.params['output_dir']
        if not os.path.isabs(self.output_dir):
            self.output_dir = os.path.abspath(self.output_dir)
            logger.debug("Changed output dir to abs path: %s" % self.output_dir)

        if 'job_id' in self.params:
            self.job_id = self.params['job_id']

        if 'input_files' in self.params:
            self.input_files = self.params['input_files']

        if 'output_files' in self.params:
            self.output_files = self.params['output_files']

    def __set_input_files(self):
        """!
        Prepare dictionary of input files.

        If a link to a download location is given as input, the file is downloaded into the run directory before the file name is added to the input_files dict. If a regular file is provided, it is added to the dict without any additional action.
        """
        input_files_dict = {}
        for file_key, file_name in self.input_files.items():
            if 'https' in file_key:
                logger.info("Downloading input file from %s" % file_key)
                file_name_path = self.rundir + "/" + file_name
                subprocess.check_output(['wget', '-O', file_name_path, file_key])
                input_files_dict.update({file_name: file_name})
            else:
                input_files_dict.update({file_key: file_name})
        self.input_files = input_files_dict

    def __initialize(self):
        """!
        Perform basic initialization before the job script is loaded.
        """

        if not os.path.isabs(self.rundir):
            self.rundir = os.path.abspath(self.rundir)
            logger.info('Changed run dir to abs path: %s' % self.rundir)
            # raise Exception('The run dir is not an absolute path: %s' % self.rundir)

        # Set run dir if running inside LSF
        if "LSB_JOBID" in os.environ:
            self.rundir = os.path.join("/scratch", getpass.getuser(), os.environ["LSB_JOBID"])
            logger.info('Set run dir for LSF: %s' % self.rundir)
            self.delete_rundir = True

        # Create run dir if it does not exist
        if not os.path.exists(self.rundir):
            logger.info('Creating run dir: %s' % self.rundir)
            os.makedirs(self.rundir)

    def __configure(self):
        """!
        Configure job class and components.
        """
        # Configure job class
        self.job_config.config(self, require_section=False)
        # allowed_names=Job._config_names,

        # Configure each of the job components
        for c in self.components:
            c.config(self.job_config.parser)  # Configure from supplied config files
            if self.enable_env_config:
                c.config_from_environ()      # Configure from env vars, if enabled
            # TODO: catch exception here...
            c.check_config()                 # Check that the config is acceptable

    def _load_script(self):
        """!
        Load the job script.
        """
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

        exec(compile(open(script_path, "rb").read(), script_path, 'exec'), {'job': self})

    def run(self):
        """!
        This is the primary execution method for running the job.
        """

        logger.info('Job ID: ' + str(self.job_id))
        logger.info('Description: %s' % self.description)

        # Print config to the log
        logger.info(str(self.job_config))

        # Initialize after CL parameters were parsed.
        self.__initialize()

        # Load the job components from the script
        self._load_script()

        if not len(self.components):
            raise Exception("Job has no components to execute.")

        # Print list of job components
        logger.info("Job components: %s" % ([c.name for c in self.components]))

        # Print job parameters.
        logger.info("Job parameters: %s" % str(self.params))

        # This will configure the Job class and its components by copying
        # information into them from loaded config files.
        self.__configure()

        # This (re)sets the input correctly
        self.__set_input_files()

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

            # Copy by file path or ptag
            if self.enable_copy_output_files:
                ## \todo: combine these methods
                self.__copy_output_files()
            else:
                logger.warning('Copy output files is disabled!')

            # Perform job cleanup.
            self.__cleanup()

        logger.info('Successfully finished running job: %s' % self.description)

    def __execute(self):
        """!
        Execute all components in job.

        If dry_run is set to True, the components will not be exectuted,
        list of components will be put out instead.
        """
        if not self.dry_run:

            for component in self.components:

                logger.info("Executing '%s' with command: %s" % (component.name, component.cmd_line_str()))
                logger.info("Inputs: %s" % str(component.input_files()))
                logger.info("Outputs: %s" % str(component.output_files()))

                # Print header to stdout
                self.out.write('================ Component: %s ================\n' % component.name)
                self.out.flush()

                # Print header to stderr if output is going to a file
                if self.err != sys.stderr:
                    self.err.write('================ Component: %s ================\n' % component.name)
                    self.err.flush()

                start = time.time()
                returncode = component.execute(self.out, self.err)
                end = time.time()
                elapsed = end - start
                logger.info("Execution of '%s' took %f second(s)" % (component.name, elapsed))
                logger.info("Return code of '%s' was %s" % (component.name, str(returncode)))

                if not self.ignore_return_codes and returncode:
                    raise Exception("Non-zero return code %d from '%s'" % (returncode, component.name))

                if self.check_output_files:
                    for outputfile in component.output_files():
                        if not os.path.isfile(outputfile):
                            raise Exception("Output file '%s' is missing after execution." % outputfile)
        else:
            # Dry run mode. Just print component command but do not execute it.
            logger.info("Dry run enabled. Components will NOT be executed!")
            for component in self.components:
                logger.info("'%s' with args: %s (DRY RUN)" % (component.name, ' '.join(component.cmd_args())))

    def __setup(self):
        """!
        Necessary setup before job can be executed.
        """

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
        for component in self.components:
            logger.debug('Setting up component: %s' % (component.name))
            component.rundir = self.rundir
            component.setup()
            if self.check_commands and not component.cmd_exists():
                raise Exception("Command '%s' does not exist for '%s'." % (component.command, component.name))

    def __config_file_pipeline(self):
        """!
        Pipe component outputs to inputs automatically.
        """
        for i in range(0, len(self.components)):
            component = self.components[i]
            logger.debug("Configuring file IO for component '%s' with order %d" % (component. name, i))
            if i == 0:
                logger.debug("Setting inputs on '%s' to: %s"
                             % (component.name, str(list(self.input_files.values()))))
                if not len(component.inputs):
                    component.inputs = list(self.input_files.values())
            elif i > -1:
                logger.debug("Setting inputs on '%s' to: %s"
                             % (component.name, str(self.components[i - 1].output_files())))
                if len(component.inputs) == 0:
                    component.inputs = self.components[i - 1].output_files()

    def __set_parameters(self):
        """!
        Push JSON job parameters to components.
        """
        for component in self.components:
            component.set_parameters(self.params)

    def __cleanup(self):
        """!
        Perform post-job cleanup.
        """
        for component in self.components:
            logger.debug('Running cleanup for component: %s' % str(component.name))
            component.cleanup()
        if self.delete_rundir:
            logger.debug('Deleting run dir: %s' % self.rundir)
            if os.path.exists("%s/__swif_env__" % self.rundir):
                for f in os.listdir(self.rundir):
                    if ('.log' not in f) and ('__swif_' not in f):
                        os.system('rm -r %s' % f)
            else:
                shutil.rmtree(self.rundir)
        if self.out != sys.stdout:
            self.out.close()
        if self.err != sys.stderr:
            self.err.close()

    def __copy_output_files(self):
        """!
        Copy output files to output directory, handling ptags if necessary.
        """

        if not os.path.exists(self.output_dir):
            logger.debug('Creating output dir: %s' % self.output_dir)
            os.makedirs(self.output_dir, 0o755)

        for src, dest in self.output_files.items():
            src_file = src
            if Job.is_ptag(src):
                ptag_src = Job.get_ptag_from_src(src)
                if ptag_src in list(self.ptags.keys()):
                    src_file = self.ptags[ptag_src]
                    logger.info("Resolved ptag: {} -> {}".format(ptag_src, src_file))
                else:
                    raise Exception('Undefined ptag used in job params: %s' % ptag_src)
            self.__copy_output_file(src_file, dest)

    def __copy_output_file(self, src, dest):
        """!
        Copy an output file from src to dest.
        """

        src_file = os.path.join(self.rundir, src)
        dest_file = os.path.join(self.output_dir, dest)

        # Create directory if not exists; this allows relative path segments
        # in output file strings.
        if not os.path.exists(os.path.dirname(dest_file)):
            os.makedirs(os.path.dirname(dest_file), 0o755)

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
        """!
        Copy input files to the run dir.
        """
        for src, dest in self.input_files.items():
            # if not os.path.isabs(src):
            ## \todo FIXME: Could try and convert to abspath here.
            #    raise Exception("The input source file '%s' is not an absolute path." % src)
            if os.path.dirname(dest):
                raise Exception("The input file destination '%s' is not valid." % dest)
            logger.info("Copying input file: %s -> %s" % (src, os.path.join(self.rundir, dest)))
            if '/mss/' in src:
                src = src.replace('/mss/', '/cache/')
            if os.path.exists(os.path.join(self.rundir, dest)):
                logger.info("The input file %s already exists at destination %s" % (dest, self.rundir))
                os.chmod(os.path.join(self.rundir, dest), 0o666)
            else:
                shutil.copyfile(src, os.path.join(self.rundir, dest))
            os.chmod(dest, 0o666)

    def __symlink_input_files(self):
        """!
        Symlink input files.
        """
        for src, dest in self.input_files.items():
            if not os.path.isabs(src):
                raise Exception('The input source file is not an absolute path: %s' % src)
            if os.path.dirname(dest):
                raise Exception('The input file destination is not valid: %s' % dest)
            logger.debug("Symlinking input '%s' to '%s'" % (src, os.path.join(self.rundir, dest)))
            os.symlink(src, os.path.join(self.rundir, dest))

    def ptag(self, tag, filename):
        """!
        Map a key to an output file name so a user can reference it in their job params.
        """
        if tag not in list(self.ptags.keys()):
            self.ptags[tag] = filename
            logger.info("Added ptag %s -> %s" % (tag, filename))
        else:
            raise Exception('The ptag already exists: %s' % tag)

    ## \todo is this still needed?
    def __copy_ptag_output_files(self):
        if len(self.ptags):
            for src, dest in self.output_files.items():
                if Job.is_ptag(src):
                    ptag_src = Job.get_ptag_from_src(src)
                    if ptag_src in list(self.ptags.keys()):
                        src_file = self.ptags[ptag_src]
                        logger.info("Copying ptag '%s' from '%s' -> '%s'" % (ptag_src, src_file, dest))
                        self.__copy_output_file(src_file, dest)
                    else:
                        raise Exception('Invalid ptag in job params: %s' % ptag_src)

    @staticmethod
    def is_ptag(src):
        return src.startswith(Job.PTAG_PREFIX)

    @staticmethod
    def get_ptag_from_src(src):
        if src.startswith(Job.PTAG_PREFIX):
            return src[len(Job.PTAG_PREFIX):]
        else:
            raise Exception('File src is not a ptag: %s' % src)

    def resolve_output_src(self, src):
        if Job.is_ptag(src):
            return self.ptags[Job.get_ptag_from_src(src)]
        else:
            return src


cmds = {
    'run': 'Run a job script',
    'script': 'Show list of available job scripts (provide script name for detailed info)',
    'component': 'Show list of available components (provide component name for detailed info)'}


def print_usage():
    print("Usage: job.py [command] [args]")
    print("    command:")
    for name, descr in cmds.items():
        print("        %s: %s" % (name, descr))


if __name__ == '__main__':
    if len(sys.argv) > 1:
        cmd = sys.argv[1]
        if cmd not in list(cmds.keys()):
            print_usage()
            raise Exception('The job command is not valid: %s' % cmd)
        args = sys.argv[2:]
        if cmd == 'run':
            job = Job(args)
            job.parse_args()
            job.run()
        elif cmd == 'script':
            if len(sys.argv) > 2:
                script = sys.argv[2]
                from hpsmc.help import print_job_script
                print_job_script(script)
            else:
                scriptdb = JobScriptDatabase()
                print("AVAILABLE JOB SCRIPTS: ")
                for name in sorted(scriptdb.get_script_names()):
                    print('    %s: %s' % (name, scriptdb.get_script_path(name)))
        elif cmd == 'component':
            if len(sys.argv) > 2:
                from hpsmc.help import print_component
                component_name = sys.argv[2]
                print_component(component_name)
            else:
                from hpsmc.help import print_components
                print_components()

    else:
        print_usage()
