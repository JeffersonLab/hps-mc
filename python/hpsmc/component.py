"""!
@package component
Defines the base interface that component classes should extend.
"""

import os
import sys
import subprocess
import logging

from ._config import convert_config_value
from hpsmc import global_config

logger = logging.getLogger("hpsmc.component")


class Component(object):
    """!
    Base class for components in a job.

    Do not perform any logging in the init method of Component subclasses,
    as this is not configured by the job manager until after the components
    are created.

    Optional parameters are: **nevents**, **seed**

    @param name  name of the component
    @param command  command to execute
    @param nevents  number of events to process
    @param seed  random seed
    @param inputs  list of input files
    @param outputs  list of output files
    @param append_tok  token to append to output file names
    @param output_ext  extension to append to output file names; format is .ext
    @param ignore_job_params  list of parameters to ignore when setting parameters
    @param kwargs  additional keyword arguments
    """

    def __init__(self,
                 name,
                 command=None,
                 nevents=None,
                 seed=1,
                 inputs=[],
                 outputs=None,
                 append_tok=None,
                 output_ext=None,
                 ignore_job_params=[],
                 **kwargs):

        self.name = name
        self.command = command
        self.nevents = nevents
        self.seed = seed
        self.inputs = inputs
        self.outputs = outputs
        self.append_tok = append_tok
        self.output_ext = output_ext

        ## \todo FIXME: This is hacky.
        self.ignore_job_params = ignore_job_params

        self.hpsmc_dir = os.getenv("HPSMC_DIR", None)
        if self.hpsmc_dir is None:
            raise Exception("The HPSMC_DIR is not set!")

        # Setup a logger specifically for this component. It will be configured later.
        self.logger = logging.getLogger("{}.{}".format(__name__, self.__class__.__name__))

    def cmd_line_str(self):
        cl = [self.command]
        cl.extend(self.cmd_args())
        return ' '.join(cl)

    def execute(self, log_out=sys.stdout, log_err=sys.stderr):
        """! Generic component execution method.

        Individual components may override this if specific behavior is required.

        @param log_out  name of log file for output
        @param log_err  name of log file for error
        @return error code
        """
        proc = subprocess.Popen(self.cmd_line_str(), shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()

        return proc.returncode

    def cmd_exists(self):
        """! Check if the component's assigned command exists."""
        return subprocess.call("type " + self.command, shell=True,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

    def cmd_args(self):
        """! Return the command arguments of this component."""
        return []

    def cmd_args_str(self):
        """! Return list of arguments, making sure they are all converted to strings."""
        return [str(c) for c in self.cmd_args()]

    def setup(self):
        """! Perform any necessary setup for this component to run such as making symlinks
        to required directories.
        """
        pass

    def cleanup(self):
        """! Perform post-job cleanup such as deleting temporary files."""
        pass

    def config_logging(self, parser):
        """!
        Configure the logging for a component.

        @param parser the ConfigParser object passed from the job manager
        """
        classname = self.__class__.__name__
        if classname in parser:
            if 'loglevel' in parser[classname]:
                loglevel = logging.getLevelName(parser[classname]['loglevel'])
                self.logger.setLevel(loglevel)

    def config(self, parser):
        """! Automatic configuration

        Automatically load attributes from config by reading in values from
        the section with the same name as the class in the config file and
        assigning them to class attributes with the same name.

        @param parser  config parser
        """
        section_name = self.__class__.__name__
        if parser.has_section(section_name):
            for name, value in parser.items(section_name):
                setattr(self, name, convert_config_value(value))
                logger.debug("%s:%s:%s=%s" % (self.name,
                                              name,
                                              getattr(self, name).__class__.__name__,
                                              getattr(self, name)))

    def set_parameters(self, params):
        """! Set class attributes for the component based on JSON parameters.

        Components should not need to override this method.

        @param params  parameters to setup component
        """

        # Set required parameters.
        for p in self.required_parameters():
            if p not in params:
                raise Exception("Required parameter '%s' is missing for component '%s'"
                                % (p, self.name))
            else:
                if p not in self.ignore_job_params:
                    setattr(self, p, params[p])
                    logger.debug("%s:%s=%s [required]" % (self.name, p, params[p]))
                else:
                    logger.debug("Ignored job param '%s'" % p)

        # Set optional parameters.
        for p in self.optional_parameters():
            if p in params:
                if p not in self.ignore_job_params:
                    setattr(self, p, params[p])
                    logger.debug("%s:%s=%s [optional]"
                                 % (self.name, p, params[p]))
                else:
                    logger.debug("Ignored job param '%s'" % p)

    def required_parameters(self):
        """!
        Return a list of required parameters.

        The job will fail if these are not present in the JSON file.
        """
        return []

    def optional_parameters(self):
        """!
        Return a list of optional parameters.

        Optional parameters are: **nevents**, **seed**
        """
        return ['nevents', 'seed']

    def required_config(self):
        """!
        Return a list of required configuration settings.

        There are none by default.
        """
        return []

    def check_config(self):
        """! Raise an exception on the first missing config setting for this component."""
        for c in self.required_config():
            if not hasattr(self, c):
                raise Exception('Missing required config attribute: %s:%s' % (self.__class__.__name__, c))
            if getattr(self, c) is None:
                raise Exception('Config was not set: %s:%s' % (self.__class__.__name__, c))

    def input_files(self):
        """! Get a list of input files for this component."""
        return self.inputs

    def output_files(self):
        """! Return a list of output files created by this component.

        By default, a series of transformations will be performed on inputs to
        transform them into outputs.
        """
        if self.outputs is not None and len(self.outputs):
            return self.outputs
        else:
            return self._inputs_to_outputs()

    def _inputs_to_outputs(self):
        """! This is the default method for automatically transforming input file names
        to outputs when output file names are not explicitly provided.
        """
        outputs = []
        for infile in self.input_files():
            f, ext = os.path.splitext(infile)
            if self.append_tok is not None:
                f += '_%s' % self.append_tok
            if self.output_ext is not None:
                ext = self.output_ext
            outputs.append('%s%s' % (f, ext))
        return outputs

    def config_from_environ(self):
        """! Configure component from environment variables which are just upper case
        versions of the required config names set in the shell environment."""
        for c in self.required_config():
            logger.debug("Setting config '%s' from environ" % c)
            if c.upper() in os.environ:
                setattr(self, c, os.environ[c.upper()])
                logger.debug("Set config '%s=%s' from env var '%s'" % (c, getattr(self, c), c.upper()))
            else:
                raise Exception("Missing config in environ for '%s'" % c)


class DummyComponent(Component):
    """! A dummy component that just prints some information instead of executing a program."""

    def __init__(self, **kwargs):
        Component.__init__(self, 'dummy', 'dummy', **kwargs)

    def execute(self, log_out=sys.stdout, log_err=sys.stderr):
        self.logger.debug("dummy debug")
        self.logger.info("dummy info")
        self.logger.warning("dummy warn")
        self.logger.critical("dummy critical")
        self.logger.error("dummy error")
