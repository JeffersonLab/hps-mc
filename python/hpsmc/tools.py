"""! Tools that can be used in HPSMC jobs."""

import os
import gzip
import shutil
import logging
import subprocess
import tarfile

from subprocess import PIPE

from component import Component
from run_params import RunParameters
import hpsmc.func as func

logger = logging.getLogger("hpsmc.tools")


class SLIC(Component):
    """!
    Run the SLIC Geant4 simulation.

    Optional parameters are: **nevents**, **macros**, **run_number** \n
    Required parameters are: **detector** \n
    Required configurations are: **slic_dir**, **hps_fieldmaps_dir**, **detector_dir**
    """

    def __init__(self, **kwargs):
        ## List of macros to run (optional)
        self.macros = []
        ## Run number to set on output file (optional)
        self.run_number = None
        ## To be set from config or install dir
        self.hps_fieldmaps_dir = None
        ## To be set from config or install dir
        self.detector_dir = None

        Component.__init__(self,
                           name='slic',
                           command='slic',
                           output_ext='.slcio',
                           **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        if not len(self.input_files()):
            raise Exception("No inputs given for SLIC.")

        args = ["-g", self.__detector_file(),
                "-i", self.input_files()[0],
                "-o", self.output_files()[0],
                "-d%s" % str(self.seed)]

        if self.nevents is not None:
            args.extend(["-r", str(self.nevents)])

        if self.run_number is not None:
            args.extend(["-m", "run_number.mac"])

        tbl = self.__particle_tbl()
        if os.path.exists(tbl):
            args.extend(["-P", tbl])
        else:
            raise Exception('SLIC particle.tbl does not exist: %s' % tbl)

        if len(self.macros):
            args = []
            for macro in self.macros:
                if macro == "run_number.mac":
                    raise Exception("Macro name '%s' is not allowed." % macro)
                if not os.path.isabs(macro):
                    raise Exception("Macro '%s' is not an absolute path." % macro)
                args.extend(["-m", macro])

        return args

    def __detector_file(self):
        """! Return path to detector file."""
        return os.path.join(self.detector_dir, self.detector, self.detector + ".lcdd")

    def __particle_tbl(self):
        """! Return path to particle table."""
        return os.path.join(self.slic_dir, "share", "particle.tbl")

    def config(self, parser):
        """! Configure SLIC component."""
        super().config(parser)

        if self.detector_dir is None:
            self.detector_dir = "{}/share/detectors".format(self.hpsmc_dir)
            if not os.path.isdir(self.detector_dir):
                raise Exception('Failed to find valid detector_dir')
            logger.debug("Using detector_dir from install: {}".format(self.detector_dir))

        # Set fieldmap dir to install location if not provided in config
        if self.hps_fieldmaps_dir is None:
            self.hps_fieldmaps_dir = "{}/share/fieldmap".format(self.hpsmc_dir)
            if not os.path.isdir(self.hps_fieldmaps_dir):
                raise Exception("The fieldmaps dir does not exist: {}".format(self.hps_fieldmaps_dir))
            logger.debug("Using fieldmap dir from install: {}".format(self.hps_fieldmaps_dir))
        else:
            logger.debug("Using fieldmap dir from config: {}".format(self.hps_fieldmaps_dir))

    def setup(self):
        """! Setup SLIC component."""
        if not os.path.exists(self.slic_dir):
            raise Exception("slic_dir does not exist: %s" % self.slic_dir)

        self.env_script = self.slic_dir + os.sep + "bin" + os.sep + "slic-env.sh"
        if not os.path.exists(self.env_script):
            raise Exception('SLIC setup script does not exist: %s' % self.name)

        logger.debug('Creating sym link to fieldmap dir: {}'.format(self.hps_fieldmaps_dir))
        if not os.path.islink(os.getcwd() + os.path.sep + "fieldmap"):
            os.symlink(self.hps_fieldmaps_dir, "fieldmap")
        else:
            logger.warning('Link to fieldmap dir already exists!')

        if self.run_number is not None:
            run_number_cmd = "/lcio/runNumber %d" % self.run_number
            run_number_mac = open("run_number.mac", 'w')
            run_number_mac.write(run_number_cmd)
            run_number_mac.close()

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **nevents**, **macros**, **run_number**
        @return  list of optional parameters
        """
        return ['nevents', 'macros', 'run_number']

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **detector**
        @return  list of required parameters
        """
        return ['detector']

    def required_config(self):
        """!
        Return list of required configurations.

        Required configurations are: **slic_dir**, **hps_fieldmaps_dir**, **detector_dir**
        @return  list of required configurations
        """
        return ['slic_dir', 'hps_fieldmaps_dir', 'detector_dir']

    def execute(self, log_out, log_err):
        """!
        Execute SLIC component.

        Component is executed by creating command line input
        from command and command arguments.
        @return  return code of process
        """
        # SLIC needs to be run inside bash as the Geant4 setup script is a piece of #@$@#$.
        cl = 'bash -c ". %s && %s %s"' % (self.env_script, self.command, ' '.join(self.cmd_args()))

        # logger.info("Executing '%s' with command: %s" % (self.name, cl))
        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()

        return proc.returncode


class JobManager(Component):
    """!
    Run the hps-java JobManager class.

    Input files have slcio format.

    Required parameters are: **steering_files** \n
    Optional parameters are: **detector**, **run_number**, **defs**
    """

    def __init__(self, steering=None, **kwargs):
        ## \todo verify these definitions
        ## run number
        self.run_number = None
        ## detector name
        self.detector = None
        ## event print interval
        self.event_print_interval = None
        ## \todo what is this?
        self.defs = None
        ## java arguments
        self.java_args = None
        ## file for config logging
        self.logging_config_file = None
        ## lcsim cache directory
        self.lcsim_cache_dir = None
        ## no idea
        self.conditions_user = None
        ## no idea
        self.conditions_password = None
        ## no idea
        self.conditions_url = None
        ## steering file
        self.steering = steering
        ## location of hps-java installation?
        self.hps_java_bin_jar = None

        if 'overlay_file' in kwargs:
            self.overlay_file = kwargs['overlay_file']
        else:
            self.overlay_file = None

        Component.__init__(self,
                           name='job_manager',
                           command='java',
                           description='HPS Java Job Manager',
                           output_ext='.slcio',
                           **kwargs)

        # Automatically append steering file key to output file name
        if self.append_tok is None:
            self.append_tok = self.steering
            logger.debug("Append token for '%s' automatically set to '%s' from steering key." % (self.name, self.append_tok))

    def config(self, parser):
        """! Configure JobManager component."""
        super().config(parser)
        # if installed these are set in the environment script...
        if self.hps_java_bin_jar is None:
            if os.getenv('HPS_JAVA_BIN_JAR', None) is not None:
                self.hps_java_bin_jar = os.getenv('HPS_JAVA_BIN_JAR', None)
                logger.debug('Set HPS_JAVA_BIN_JAR from environment: {}'.format(self.hps_java_bin_jar))
        if self.conditions_url is None:
            if os.getenv("CONDITIONS_URL", None) is not None:
                self.conditions_url = os.getenv("CONDITIONS_URL", None)
                logger.debug('Set CONDITIONS_URL from environment: {}'.format(self.hps_java_bin_jar))
        if self.hps_fieldmaps_dir is None:
            self.hps_fieldmaps_dir = "{}/share/fieldmap".format(self.hpsmc_dir)
            if not os.path.isdir(self.hps_fieldmaps_dir):
                raise Exception("The fieldmaps dir does not exist: {}".format(self.hps_fieldmaps_dir))
            logger.debug("Using fieldmap dir from install: {}".format(self.hps_fieldmaps_dir))
        else:
            logger.debug("Using fieldmap dir from config: {}".format(self.hps_fieldmaps_dir))

    def required_config(self):
        """!
        Return list of required configurations.

        Required configurations are: **hps_java_bin_jar**, **hps_fieldmaps_dir**
        @retun list of required configurations.
        """
        return ['hps_java_bin_jar', 'hps_fieldmaps_dir']

    def setup(self):
        """! Setup JobManager component."""
        if not len(self.input_files()):
            raise Exception("No inputs provided to hps-java.")

        if self.steering not in self.steering_files:
            raise Exception("Steering '%s' not found in: %s" % (self.steering, self.steering_files))
        self.steering_file = self.steering_files[self.steering]

        logger.debug('Creating sym link to fieldmap dir: {}'.format(self.hps_fieldmaps_dir))
        if not os.path.islink(os.getcwd() + os.path.sep + "fieldmap"):
            os.symlink(self.hps_fieldmaps_dir, "fieldmap")
        else:
            logger.warning('Link to fieldmap dir already exists!')

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []

        if self.java_args is not None:
            logger.debug('Setting java_args from config: %s' % self.java_args)
            args.append(self.java_args)

        if self.logging_config_file is not None:
            logger.debug('Setting logging_config_file from config: %s' % self.logging_config_file)
            args.append('-Djava.util.logging.config.file=%s' % self.logging_config_file)

        if self.lcsim_cache_dir is not None:
            logger.debug('Setting lcsim_cache_dir from config: %s' % self.lcsim_cache_dir)
            args.append('-Dorg.lcsim.cacheDir=%s' % self.lcsim_cache_dir)

        if self.conditions_user is not None:
            logger.debug('Setting conditions_user from config: %s' % self.conditions_user)
            args.append('-Dorg.hps.conditions.user=%s' % self.conditions_user)
        if self.conditions_password is not None:
            logger.debug('Setting conditions_password from config (not shown)')
            args.append('-Dorg.hps.conditions.password=%s' % self.conditions_password)
        if self.conditions_url is not None:
            logger.debug('Setting conditions_url from config: %s' % self.conditions_url)
            args.append('-Dorg.hps.conditions.url=%s' % self.conditions_url)

        args.append("-jar")
        args.append(self.hps_java_bin_jar)

        if self.event_print_interval is not None:
            args.append("-e")
            args.append(str(self.event_print_interval))

        if self.run_number is not None:
            args.append("-R")
            args.append(str(self.run_number))

        if self.detector is not None:
            args.append("-d")
            args.append(self.detector)

        if len(self.output_files()):
            args.append("-D")
            args.append("outputFile=" + os.path.splitext(self.output_files()[0])[0])

        if self.defs:
            for k, v in self.defs.items():
                args.append("-D")
                args.append(k + "=" + str(v))

        if not os.path.isfile(self.steering_file):
            args.append("-r")
            logger.debug("Steering does not exist at '%s' so assuming it is a resource." % self.steering_file)
        else:
            if not os.path.isabs(self.steering_file):
                raise Exception('Steering looks like a file but is not an abs path: %s' % self.steering_file)
        args.append(self.steering_file)

        if self.nevents is not None:
            args.append("-n")
            args.append(str(self.nevents))

        for input_file in self.input_files():
            args.append("-i")
            args.append(input_file)

        if self.overlay_file is not None:
            args.append("-D")
            args.append("overlayFile=" + os.path.splitext(self.overlay_file)[0])

        return args

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **steering_files**
        @return  list of required parameters
        """
        return ['steering_files']

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **detector**, **run_number**, **defs**
        @return list of optional parameters
        """
        return ['detector', 'run_number', 'defs']


class HPSTR(Component):
    """!
    Run the hpstr analysis tool.

    Required parameters are: **config_files** \n
    Optional parameters are: **year**, **is_data**, **nevents** \n
    Required configs are: **hpstr_install_dir**, **hpstr_base**
    """

    def __init__(self, cfg=None, is_data=0, year=None, **kwargs):
        ## configuration
        self.cfg = cfg
        ## run mode
        self.is_data = is_data
        ## year
        self.year = year

        Component.__init__(self,
                           name='hpstr',
                           command='hpstr',
                           **kwargs)

    def setup(self):
        """! Setup HPSTR component."""
        if not os.path.exists(self.hpstr_install_dir):
            raise Exception('hpstr_install_dir does not exist: %s' % self.hpstr_install_dir)
        self.env_script = self.hpstr_install_dir + os.sep + "bin" + os.sep + "hpstr-env.sh"

        # The config file to use is read from a dict in the JSON parameters.
        if self.cfg not in self.config_files:
            raise Exception("Config '%s' was not found in: %s" % (self.cfg, self.config_files))
        config_file = self.config_files[self.cfg]
        if len(os.path.dirname(config_file)):
            # If there is a directory name then we expect an absolute path not in the hpstr dir.
            if os.path.isabs(config_file):
                self.cfg_path = config_file
            else:
                # The config must be an abs path.
                raise Exception('The config has a directory but is not an abs path: %s' % self.cfg)
        else:
            # Assume the cfg file is within the hpstr base dir.
            self.cfg_path = os.path.join(self.hpstr_base, "processors", "config", config_file)
        logger.debug('Set config path: %s' % self.cfg_path)

        # For ROOT output, automatically append the cfg key from the job params.
        if os.path.splitext(self.input_files()[0])[1] == '.root':
            self.append_tok = self.cfg
            logger.debug('Automatically appending token to output file: %s' % self.append_tok)

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **config_files**
        @return  list of required parameters
        """
        return ['config_files']

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **year**, **is_data**, **nevents**
        @return  list of optional parameters
        """
        return ['year', 'is_data', 'nevents']

    def required_config(self):
        """!
        Return list of required configs.

        Required configs are: **hpstr_install_dir**, **hpstr_base**
        @return  list of required configs
        """
        return ['hpstr_install_dir', 'hpstr_base']

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = [self.cfg_path,
                "-t", str(self.is_data),
                "-i", self.input_files()[0],
                "-o", self.output_files()[0]]
        if self.nevents is not None:
            args.extend(["-n", str(self.nevents)])
        if self.year is not None:
            args.extend(["-y", str(self.year)])
        return args

    def output_files(self):
        """! Adjust names of output files."""
        f, ext = os.path.splitext(self.input_files()[0])
        if '.slcio' in ext:
            return ['%s.root' % f]
        else:
            return ['%s_%s.root' % (f, self.append_tok)]

    def execute(self, log_out, log_err):
        """! Execute HPSTR component."""
        args = self.cmd_args()
        cl = 'bash -c ". %s && %s %s"' % (self.env_script, self.command,
                                          ' '.join(self.cmd_args()))

        logger.debug("Executing '%s' with command: %s" % (self.name, cl))
        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()

        return proc.returncode

## \todo split this over several files -> move stdheptools in separate package


class StdHepTool(Component):
    """!
    Generic class for StdHep tools.
    """

    ## List of commands which accept a 'seed' argument.
    seed_names = ['beam_coords',
                  'beam_coords_old',
                  'lhe_tridents',
                  'lhe_tridents_displacetime',
                  'lhe_tridents_displaceuni',
                  'merge_poisson',
                  'mix_signal',
                  'random_sample']

    def __init__(self, name=None, **kwargs):

        Component.__init__(self,
                           name=name,
                           command="stdhep_" + name,
                           **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []

        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])

        if len(self.output_files()):
            args.insert(0, self.output_files()[0])
        elif len(self.output_files()) > 1:
            raise Exception("Too many outputs specified for StdHepTool.")

        if len(self.input_files()):
            for i in self.inputs[::-1]:
                args.insert(0, i)
        else:
            raise Exception("No inputs specified for StdHepTool.")

        return args


class BeamCoords(StdHepTool):
    """!
    Transform StdHep events into beam coordinates.

    Optional parameters are: **beam_sigma_x**, **beam_sigma_y**, **beam_rot_x**,
    **beam_rot_y**, **beam_rot_z**, **target_x**, **target_y**, **target_z**
    """

    def __init__(self, **kwargs):
        ## \todo clarify these
        ## beam sigma in x
        self.beam_sigma_x = None
        ## beam sigma in y
        self.beam_sigma_y = None
        ## target x position
        self.target_x = None
        ## target y position
        self.target_y = None
        ## target z position
        self.target_z = None
        ## beam rotation in x?
        self.beam_rot_x = None
        ## beam rotation in y?
        self.beam_rot_y = None
        ## beam rotation in z?
        self.beam_rot_z = None

        StdHepTool.__init__(self,
                            name='beam_coords',
                            append_tok='rot',
                            **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = StdHepTool.cmd_args(self)

        if self.beam_sigma_x is not None:
            args.extend(['-x', str(self.beam_sigma_x)])
        if self.beam_sigma_y is not None:
            args.extend(['-y', str(self.beam_sigma_y)])

        if self.beam_rot_x is not None:
            args.extend(['-u', str(self.beam_rot_x)])
        if self.beam_rot_y is not None:
            args.extend(['-v', str(self.beam_rot_y)])
        if self.beam_rot_z is not None:
            args.extend(['-w', str(self.beam_rot_z)])

        if self.target_x is not None:
            args.extend(['-X', str(self.target_x)])
        if self.target_y is not None:
            args.extend(['-Y', str(self.target_y)])
        if self.target_z is not None:
            args.extend(['-Z', str(self.target_z)])

        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **beam_sigma_x**, **beam_sigma_y**, **beam_rot_x**,
        **beam_rot_y**, **beam_rot_z**, **target_x**, **target_y**, **target_z**
        @return list of optional parameters
        """
        return ['beam_sigma_x', 'beam_sigma_y', 'beam_rot_x',
                'beam_rot_y', 'beam_rot_z',
                'target_x', 'target_y', 'target_z']


class RandomSample(StdHepTool):
    """!
    Randomly sample StdHep events into a new file.

    Optional parameters are: **nevents**, **mu**
    """

    def __init__(self, **kwargs):
        StdHepTool.__init__(self,
                            name='random_sample',
                            append_tok='sampled',
                            **kwargs)
        ## median of distribution?
        self.mu = None

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []

        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])

        args.extend(["-N", str(1)])

        if self.nevents is not None:
            args.extend(["-n", str(self.nevents)])

        if self.mu is not None:
            args.extend(["-m", str(self.mu)])

        if len(self.output_files()):
            args.insert(0, os.path.splitext(self.output_files()[0])[0])
        elif len(self.outputs) > 1:
            raise Exception("Too many outputs specified.")

        if len(self.input_files()):
            for i in self.inputs:
                args.insert(0, i)
        else:
            raise Exception("No inputs were provided.")

        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **nevents**, **mu**
        @return list of optional parameters
        """
        return ['nevents', 'mu']

    def execute(self, log_out, log_err):
        """! Execute RandomSample component"""
        returncode = Component.execute(self, log_out, log_err)

        # Move file to proper output file location.
        src = '%s_1.stdhep' % os.path.splitext(self.output_files()[0])[0]
        dest = '%s.stdhep' % os.path.splitext(self.output_files()[0])[0]
        logger.debug("Moving '%s' to '%s'" % (src, dest))
        shutil.move(src, dest)

        return returncode


class DisplaceTime(StdHepTool):
    """!
    Convert LHE files to StdHep, displacing the time by given ctau.

    Optional parameters are: **ctau**
    """

    def __init__(self, **kwargs):
        ## time shift
        self.ctau = None
        StdHepTool.__init__(self,
                            name='lhe_tridents_displacetime',
                            output_ext='.stdhep',
                            **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = StdHepTool.cmd_args(self)
        if self.ctau is not None:
            args.extend(["-l", str(self.ctau)])
        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **ctau**
        @return list of optional parameters
        """
        return ['ctau']


class DisplaceUni(StdHepTool):
    """!
    Convert LHE files to StdHep, displacing the time by given ctau.

    Optional parameters are: **ctau**
    """

    def __init__(self, **kwargs):
        ## time shift
        self.ctau = None
        StdHepTool.__init__(self,
                            name='lhe_tridents_displaceuni',
                            output_ext='.stdhep',
                            **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = StdHepTool.cmd_args(self)
        if self.ctau is not None:
            args.extend(["-l", str(self.ctau)])
        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **ctau**
        @return list of optional parameters
        """
        return ['ctau']


class AddMother(StdHepTool):
    """!
    Add mother particles for physics samples.
    """

    def __init__(self, **kwargs):
        StdHepTool.__init__(self,
                            name='add_mother',
                            append_tok='mom',
                            **kwargs)


class AddMotherFullTruth(StdHepTool):
    """! Add full truth mother particles for physics samples"""

    def __init__(self, **kwargs):
        StdHepTool.__init__(self,
                            'add_mother_full_truth',
                            append_tok='mom_full_truth',
                            **kwargs)
        if len(self.inputs) != 2:
            raise Exception("Must have 2 input files: a stdhep file and a lhe file in order")
        self.input_file_1 = self.inputs[0]
        base, ext = os.path.splitext(self.input_file_1)
        if ext != '.stdhep':
            raise Exception("The first input file must be a stdhep file")
        self.input_file_2 = self.inputs[1]
        base, ext = os.path.splitext(self.input_file_2)
        if ext != '.lhe':
            raise Exception("The second input file must be a lhe file")

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []

        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])

        if len(self.output_files()):
            args.insert(0, self.output_files()[0])
        elif len(self.output_files()) > 1:
            raise Exception("Too many outputs specified for StdHepTool.")

        args.insert(0, self.input_file_2)
        args.insert(0, self.input_file_1)

        return args


class MergePoisson(StdHepTool):
    """!
    Merge StdHep files, applying poisson sampling.

    Required parameters are: **run_params**
    """

    def __init__(self, xsec=0, **kwargs):
        ## cross section in pb
        self.xsec = xsec
        StdHepTool.__init__(self,
                            name='merge_poisson',
                            append_tok='sampled',
                            **kwargs)

    def setup(self):
        """! Setup MergePoisson component."""
        self.run_param_data = RunParameters(self.run_params)
        ## \todo: this function could just be inlined here
        if self.xsec > 0:
            self.mu = func.lint(self.run_param_data) * self.xsec
        else:
            raise Exception("Cross section is missing.")
        logger.info("mu is %f", self.mu)

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **run_params**
        @return list of required parameters
        """
        return ['run_params']

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []
        # self.mu = func.mu(self.lhe_file, self.run_param_data)
        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])

        args.extend(["-m", str(self.mu), "-N", str(1), "-n", str(self.nevents)])

        if len(self.output_files()):
            args.insert(0, os.path.splitext(self.output_files()[0])[0])
        elif len(self.outputs) > 1:
            raise Exception("Too many outputs specified.")

        if len(self.input_files()):
            for i in self.inputs:
                args.insert(0, i)
        else:
            raise Exception("No inputs were provided.")

        return args

    def execute(self, log_out, log_err):
        """! Execute MergePoisson component."""
        returncode = Component.execute(self, log_out, log_err)

        # Move file from tool to proper output file location.
        src = '%s_1.stdhep' % os.path.splitext(self.output_files()[0])[0]
        dest = '%s.stdhep' % os.path.splitext(self.output_files()[0])[0]
        logger.debug("Moving '%s' to '%s'" % (src, dest))
        shutil.move(src, dest)

        return returncode


class MergeFiles(StdHepTool):
    """!
    Merge StdHep files.

    Optional parameters are: none \n
    Required parameters are: none
    """

    def __init__(self, **kwargs):
        StdHepTool.__init__(self,
                            name='merge_files',
                            **kwargs)

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: none
        @return list of optional parameters
        """
        return []

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: none
        @return list of required parameters
        """
        return []


class StdHepCount(Component):
    """!
    Count number of events in a StdHep file.
    """

    def __init__(self, **kwargs):
        Component.__init__(self,
                           name='stdhep_count',
                           command='stdhep_count.sh',
                           **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        return [self.input_files()[0]]

    def execute(self, log_out, log_err):
        """! Execute StdHepCount component."""
        cl = [self.command]
        cl.extend(self.cmd_args())
        proc = subprocess.Popen(cl, stdout=PIPE)
        (output, err) = proc.communicate()

        nevents = int(output.split()[1])
        print("StdHep file '%s' has %d events." % (self.input_files()[0], nevents))

        return proc.returncode


class JavaTool(Component):
    """!
    Generic base class for Java based tools.
    """

    def __init__(self, name, java_class, **kwargs):
        ## java class
        self.java_class = java_class
        ## java arguments
        self.java_args = None
        ## tbd
        self.conditions_url = None
        Component.__init__(self,
                           name,
                           "java",
                           **kwargs)

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **hps_java_bin_jar**, **hps_fieldmaps_dir**
        @return list of required config
        """
        return ['hps_java_bin_jar', 'hps_fieldmaps_dir']

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []
        if self.java_args is not None:
            logger.debug("Setting java_args from config: %s" + self.java_args)
            args.append(self.java_args)
        if self.conditions_url is not None:
            logger.debug('Setting conditions_url from config: %s' % self.conditions_url)
            args.append('-Dorg.hps.conditions.url=%s' % self.conditions_url)
        args.append("-cp")
        args.append(self.hps_java_bin_jar)
        args.append(self.java_class)
        return args

    def config(self, parser):
        super().config(parser)
        if self.hps_fieldmaps_dir is None:
            self.hps_fieldmaps_dir = "{}/share/fieldmap".format(self.hpsmc_dir)
            if not os.path.isdir(self.hps_fieldmaps_dir):
                raise Exception("The fieldmaps dir does not exist: {}".format(self.hps_fieldmaps_dir))
            logger.debug("Using fieldmap dir from install: {}".format(self.hps_fieldmaps_dir))
        else:
            logger.debug("Using fieldmap dir from config: {}".format(self.hps_fieldmaps_dir))

    def setup(self):
        logger.debug('Creating sym link to fieldmap dir: {}'.format(self.hps_fieldmaps_dir))
        if not os.path.islink(os.getcwd() + os.path.sep + "fieldmap"):
            os.symlink(self.hps_fieldmaps_dir, "fieldmap")
        else:
            logger.warning('Link to fieldmap dir already exists!')


class EvioToLcio(JavaTool):
    """!
    Convert EVIO events to LCIO using the hps-java EvioToLcio command line tool.

    Input files have evio format (format used by DAQ system).

    Required parameters are: **detector**, **steering_files** \n
    Optional parameters are: **run_number**, **skip_events**, **nevents**, **event_print_interval**
    """

    def __init__(self, steering=None, **kwargs):
        ## detector name
        self.detector = None
        ## steering file
        self.steering = None
        ## run number
        self.run_number = None
        ## number of events that are skipped
        self.skip_events = None
        ## event print interval
        self.event_print_interval = None
        ## \todo is it necessary to set this twice?
        self.steering = steering

        JavaTool.__init__(self,
                          name='evio_to_lcio',
                          java_class='org.hps.evio.EvioToLcio',
                          output_ext='.slcio',
                          **kwargs)

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **detector**, **steering_files**
        @return list of required parameters
        """
        return ['detector', 'steering_files']

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **run_number**, **skip_events**, **nevents**, **event_print_interval**
        @return list of optional parameters
        """
        return ['run_number', 'skip_events', 'nevents', 'event_print_interval']

    def setup(self):
        """! Setup EvioToLcio component."""
        super().setup()
        if self.steering not in self.steering_files:
            raise Exception("Steering '%s' not found in: %s" % (self.steering, self.steering_files))
        self.steering_file = self.steering_files[self.steering]

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = JavaTool.cmd_args(self)
        if not len(self.output_files()):
            raise Exception('No output files were provided.')
        output_file = self.output_files()[0]
        args.append('-DoutputFile=%s' % os.path.splitext(output_file)[0])
        args.extend(['-d', self.detector])
        if self.run_number is not None:
            args.extend(['-R', str(self.run_number)])
        if self.skip_events is not None:
            args.extend(['-s', str(self.skip_events)])

        if not os.path.isfile(self.steering_file):
            args.append('-r')
            logger.debug("Steering does not exist at '%s' so assuming it is a resource." % self.steering_file)
        else:
            if not os.path.isabs(self.steering_file):
                raise Exception("Steering looks like a file but is not an abs path: %s" % self.steering_file)
        args.extend(['-x', self.steering_file])

        if self.nevents is not None:
            args.extend(['-n', str(self.nevents)])

        args.append('-b')

        for inputfile in self.input_files():
            args.append(inputfile)

        if self.event_print_interval is not None:
            args.extend(['-e', str(self.event_print_interval)])

        return args


class FilterBunches(JavaTool):
    """!
    Space MC events and apply energy filters to process before readout.

    Optional parameters are: **filter_ecal_hit_ecut**, **filter_event_interval**,
    **filter_nevents_read**, **filter_nevents_write**, **filter_no_cuts** \n
    Required config are: **hps_java_bin_jar**
    """

    def __init__(self, **kwargs):
        if 'filter_no_cuts' in kwargs:
            self.filter_no_cuts = kwargs['filter_no_cuts']
        else:
            # By default cuts are on
            self.filter_no_cuts = False

        if 'filter_ecal_pairs' in kwargs:
            self.filter_ecal_pairs = kwargs['filter_ecal_pairs']
        else:
            self.filter_ecal_pairs = False

        if 'filter_ecal_hit_ecut' in kwargs:
            self.filter_ecal_hit_ecut = kwargs['filter_ecal_hit_ecut']
        else:
            # No default ecal hit cut energy (negative val to be ignored)
            self.filter_ecal_hit_ecut = -1.0
            # self.filter_ecal_hit_ecut = 0.05

        if 'filter_event_interval' in kwargs:
            self.filter_event_interval = kwargs['filter_event_interval']
        else:
            # Default event filtering interval
            self.filter_event_interval = 250

        if 'filter_nevents_read' in kwargs:
            self.filter_nevents_read = kwargs['filter_nevents_read']
        else:
            # Default is no maximum nevents to read
            self.filter_nevents_read = -1

        if 'filter_nevents_write' in kwargs:
            self.filter_nevents_write = kwargs['filter_nevents_write']
        else:
            # Default is no maximum nevents to write
            self.filter_nevents_write = -1

        self.hps_java_bin_jar = None

        JavaTool.__init__(self,
                          name='filter_bunches',
                          java_class='org.hps.util.FilterMCBunches',
                          append_tok='filt',
                          **kwargs)

    def config(self, parser):
        """! Configure FilterBunches component."""
        super().config(parser)
        if self.hps_java_bin_jar is None:
            if os.getenv('HPS_JAVA_BIN_JAR', None) is not None:
                self.hps_java_bin_jar = os.getenv('HPS_JAVA_BIN_JAR', None)
                logger.debug('Set HPS_JAVA_BIN_JAR from environment: {}'.format(self.hps_java_bin_jar))

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = JavaTool.cmd_args(self)
        args.append("-e")
        args.append(str(self.filter_event_interval))
        for i in self.input_files():
            args.append(i)
        args.append(self.output_files()[0])
        if self.filter_ecal_pairs:
            args.append("-d")
        if self.filter_ecal_hit_ecut > 0:
            args.append("-E")
            args.append(str(self.filter_ecal_hit_ecut))
        if self.filter_nevents_read > 0:
            args.append('-n')
            args.append(str(self.filter_nevents_read))
        if self.filter_nevents_write > 0:
            args.append('-w')
            args.append(str(self.filter_nevents_write))
        if self.filter_no_cuts:
            args.append('-a')
        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **filter_ecal_hit_ecut**, **filter_event_interval**,
        **filter_nevents_read**, **filter_nevents_write**, **filter_no_cuts** \n
        @return list of optional parameters
        """
        return ['filter_ecal_hit_ecut',
                'filter_event_interval',
                'filter_nevents_read',
                'filter_nevents_write',
                'filter_no_cuts']

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **hps_java_bin_jar**
        @return list of required config
        """
        return ['hps_java_bin_jar']


class ExtractEventsWithHitAtHodoEcal(JavaTool):
    """!
    Apply hodo-hit filter and space MC events to process before readout.

    The nevents parameter is not settable from JSON in this class. It should
    be supplied as an init argument in the job script if it needs to be
    customized (the default nevents and event_interval used to apply spacing
    should usually not need to be changed by the user). \n

    Optional parameters are: **num_hodo_hits**, **event_interval**
    """

    def __init__(self, **kwargs):
        if "num_hodo_hits" in kwargs:
            self.num_hodo_hits = kwargs['num_hodo_hits']
        else:
            self.num_hodo_hits = 0

        if "event_interval" in kwargs:
            self.event_interval = kwargs['event_interval']
        else:
            self.event_interval = 250

        JavaTool.__init__(self,
                          name='filter_events',
                          java_class='org.hps.util.ExtractEventsWithHitAtHodoEcal',
                          append_tok='filt',
                          **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = JavaTool.cmd_args(self)
        args.append("-e")
        args.append(str(self.event_interval))
        for i in self.input_files():
            args.append(i)
        args.append(self.output_files()[0])
        if self.num_hodo_hits > 0:
            args.append("-M")
            args.append(str(self.num_hodo_hits))
        if self.nevents:
            args.append("-w")
            args.append(str(self.nevents))
        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **num_hodo_hits**, **event_interval**
        @return list of optional parameters
        """
        return ['num_hodo_hits', 'event_interval']


class Unzip(Component):
    """!
    Unzip the input files to outputs.
    """

    def __init__(self, **kwargs):
        Component.__init__(self,
                           name='unzip',
                           command='gunzip',
                           **kwargs)

    def output_files(self):
        """! Return list of output files."""
        return [os.path.splitext(i)[0] for i in self.input_files()]

    def execute(self, log_out, log_err):
        """! Execute Unzip component."""
        for inputfile in self.input_files():
            outputfile = os.path.splitext(inputfile)[0]
            with gzip.open(inputfile, 'rb') as in_file, open(outputfile, 'wb') as out_file:
                shutil.copyfileobj(in_file, out_file)
                logger.debug("Unzipped '%s' to '%s'" % (inputfile, outputfile))
        return 0


class LCIODumpEvent(Component):
    """!
    Dump LCIO event information.

    Required parameters are: none \n
    Required config are: **lcio_dir**
    """

    def __init__(self, **kwargs):
        ## lcio directory
        self.lcio_dir = None
        Component.__init__(self,
                           name='lcio_dump_event',
                           command='dumpevent',
                           **kwargs)

        if "event_num" in kwargs:
            self.event_num = kwargs["event_num"]
        else:
            self.event_num = 1

    def config(self, parser):
        """! Configure LCIODumpEvent component."""
        super().config(parser)
        if self.lcio_dir is None:
            self.lcio_dir = self.hpsmc_dir

    def setup(self):
        """! Setup LCIODumpEvent component."""
        self.command = self.lcio_dir + os.path.sep + "/bin/dumpevent"

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        if not len(self.input_files()):
            raise Exception("Missing required inputs for LCIODumpEvent.")
        args = []
        args.append(self.input_files()[0])
        args.append(str(self.event_num))
        return args

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **lcio_dir**
        @return list of required config
        """
        return ['lcio_dir']

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: none
        @return list of required parameters
        """
        return []


class LHECount(Component):
    """!
    Count events in an LHE file.
    """

    def __init__(self, minevents=0, fail_on_underflow=False, **kwargs):
        self.minevents = minevents
        Component.__init__(self,
                           name="lhe_count",
                           **kwargs)

    def setup(self):
        """! Setup LHECount component."""
        if not len(self.input_files()):
            raise Exception("Missing at least one input file.")

    def cmd_exists(self):
        """!
        Check if command exists.
        @return True if command exists
        """
        return True

    def execute(self, log_out, log_err):
        """! Execute LHECount component."""
        for i in self.inputs:
            with gzip.open(i, 'rb') as in_file:
                lines = in_file.readlines()

            nevents = 0
            for line in lines:
                if "<event>" in line:
                    nevents += 1

            print("LHE file '%s' has %d events." % (i, nevents))

            if nevents < self.minevents:
                msg = "LHE file '%s' does not contain the minimum %d events." % (i, nevents)
                if self.fail_on_underflow:
                    raise Exception(msg)
                else:
                    logger.warning(msg)
        return 0


class TarFiles(Component):
    """!
    Tar files into an archive.
    """

    def __init__(self, **kwargs):
        Component.__init__(self,
                           name='tar_files',
                           **kwargs)

    def cmd_exists(self):
        """!
        Check if command exists.
        @return True if command exists
        """
        return True

    def execute(self, log_out, log_err):
        """! Execute TarFiles component."""
        logger.debug("Opening '%s' for writing ..." % self.outputs[0])
        tar = tarfile.open(self.outputs[0], "w")
        for i in self.inputs:
            logger.debug("Adding '%s' to archive" % i)
            tar.add(i)
        tar.close()
        logger.info("Wrote archive '%s'" % self.outputs[0])
        return 0


class MoveFiles(Component):
    """!
    Move input files to new locations.
    """

    def __init__(self, **kwargs):
        Component.__init__(self,
                           name='move_files',
                           **kwargs)

    def cmd_exists(self):
        """!
        Check if command exists.
        @return True if command exists
        """
        return True

    def execute(self, log_out, log_err):
        """! Execute TarFiles component."""
        if len(self.inputs) != len(self.outputs):
            raise Exception("Input and output lists are not the same length!")
        for io in zip(self.inputs, self.outputs):
            src = io[0]
            dest = io[1]
            logger.info("Moving %s -> %s" % (src, dest))
            shutil.move(src, dest)
        return 0


class LCIOTool(Component):
    """!
    Generic component for LCIO tools.

    Required parameters are: none \n
    Required config are: **lcio_bin_jar**
    """

    def __init__(self, name=None, **kwargs):
        ## lcio bin jar (whatever this is)
        self.lcio_bin_jar = None
        Component.__init__(self,
                           name,
                           command='java',
                           **kwargs)

    def config(self, parser):
        """! Configure LCIOTool component."""
        super().config(parser)
        if self.lcio_bin_jar is None:
            self.config_from_environ()

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        return ['-jar', self.lcio_bin_jar, self.name]

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **lcio_bin_jar**
        @return list of required config
        """
        return ['lcio_bin_jar']

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: none
        @return list of required parameters
        """
        return []


class LCIOConcat(LCIOTool):
    """!
    Concatenate LCIO files together.
    """

    def __init__(self, **kwargs):
        LCIOTool.__init__(self,
                          name='concat',
                          **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = LCIOTool.cmd_args(self)
        if not len(self.input_files()):
            raise Exception("Missing at least one input file.")
        if not len(self.output_files()):
            raise Exception("Missing an output file.")
        for i in self.input_files():
            args.extend(["-f", i])
        args.extend(["-o", self.outputs[0]])
        return args


class LCIOCount(LCIOTool):
    """!
    Count events in LCIO files.

    Required parameters are: none \n
    Optional parameters are: none
    """

    def __init__(self, **kwargs):
        LCIOTool.__init__(self,
                          name='count',
                          **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = LCIOTool.cmd_args(self)
        if not len(self.inputs):
            raise Exception("Missing an input file.")
        args.extend(["-f", self.inputs[0]])
        return args

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: none
        @return list of required parameters
        """
        return []

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: none
        @return list of optional parameters
        """
        return []


class LCIOMerge(LCIOTool):
    """!
    Merge LCIO files.
    """

    def __init__(self, **kwargs):
        LCIOTool.__init__(self,
                          name='merge',
                          **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = LCIOTool.cmd_args(self)
        if not len(self.input_files()):
            raise Exception("Missing at least one input file.")
        if not len(self.output_files()):
            raise Exception("Missing an output file.")
        for i in self.input_files():
            args.extend(["-f", i])
        args.extend(["-o", self.outputs[0]])
        if self.nevents is not None:
            args.extend(['-n', str(self.nevents)])
        return args


class SimBase(Component):
    """!
    Generic base class for shared Geant4 sim config
    \todo Make SLIC extend this, too.

    Optional parameters are: **nevents**, **macros**, **run_number** \n
    Required parameters are: **detector**
    """

    def detector_file(self):
        """! Get path to detector file."""
        return os.path.join(self.detector_dir, self.detector, self.detector + ".lcdd")

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **nevents**, **macros**, **run_number**
        @return list of optional parameters
        """
        return ['nevents', 'macros', 'run_number']

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **detector**
        @return list of required parameters
        """
        return ['detector']

    def execute(self, log_out, log_err):
        """! Execute SimBase component."""
        # Program needs to be run inside bash to make the Geant4 setup script happy.
        cl = 'bash -c ". %s && %s %s"' % (self.env_script, self.command, ' '.join(self.cmd_args()))

        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()

        return proc.returncode


class Sim(SimBase):
    """!
    Run the hps-sim Geant4 simulation.

    Required config are: **hps_sim_dir**, **hps_fieldmaps_dir**, **detector_dir**
    """

    def __init__(self, **kwargs):
        ## List of macros to run (optional)
        self.macros = []
        ## Run number to set on output file (optional)
        self.run_number = None
        ## To be set from config or install dir
        self.hps_fieldmaps_dir = None
        ## To be set from config or install dir
        self.detector_dir = None
        Component.__init__(self,
                           name='hps-sim',
                           command='hps-sim',
                           output_ext='.slcio',
                           **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        if not len(self.input_files()):
            raise Exception("No inputs were provided for Sim.")

        return ['./run.macro']

    def config(self, parser):
        """! Configure Sim component."""
        super().config(parser)

        if self.detector_dir is None:
            self.detector_dir = "{}/share/detectors".format(self.hpsmc_dir)
            if not os.path.isdir(self.detector_dir):
                raise Exception('Failed to find valid detector_dir')
            logger.debug("Using detector_dir from install: {}".format(self.detector_dir))

        # Set fieldmap dir to install location if not provided in config
        if self.hps_fieldmaps_dir is None:
            self.hps_fieldmaps_dir = "{}/share/fieldmap".format(self.hpsmc_dir)
            if not os.path.isdir(self.hps_fieldmaps_dir):
                raise Exception("The fieldmaps dir does not exist: {}".format(self.hps_fieldmaps_dir))
            logger.debug("Using fieldmap dir from install: {}".format(self.hps_fieldmaps_dir))
        else:
            logger.debug("Using fieldmap dir from config: {}".format(self.hps_fieldmaps_dir))

    def setup(self):
        """! Setup Sim component."""
        if not os.path.exists(self.hps_sim_dir):
            raise Exception("hps_sim_dir does not exist: %s" % self.slic_dir)

        self.env_script = self.hps_sim_dir + os.sep + "bin" + os.sep + "hps-sim-env.sh"
        if not os.path.exists(self.env_script):
            raise Exception('hps-sim setup script does not exist: %s' % self.name)

        logger.debug('Creating sym link to fieldmap dir: {}'.format(self.hps_fieldmaps_dir))
        if not os.path.islink(os.getcwd() + os.path.sep + "fieldmap"):
            os.symlink(self.hps_fieldmaps_dir, "fieldmap")
        else:
            logger.warning('Link to fieldmap dir already exists!')

        self.write_run_macro()

    def write_run_macro(self, macro_name='./run.macro'):
        """! Write parameters to run macro."""
        macro_lines = []
        macro_lines.append("/lcdd/url {}".format(self.detector_file()))

        macro_lines.append("/run/initialize")

        if self.seed is not None:
            macro_lines.append("/random/seed {}".format(self.seed))

        ## \todo: Support LHE files, too
        macro_lines.append("/hps/generators/create StdHepGen STDHEP")
        for input_file in self.input_files():
            macro_lines.append("/hps/generators/StdHepGen/file {}".format(input_file))

        for macro in self.macros:
            macro_lines.append("/control/execute {}".format(macro))

        macro_lines.append("/hps/lcio/recreate")
        macro_lines.append("/hps/lcio/file {}".format(self.output_files()[0]))

        macro_lines.append("/run/beamOn {}".format(self.nevents))

        ## \todo: Set run number (no Geant4 built-in for this?)

        with open(macro_name, 'wt', encoding='utf-8') as run_macro:
            run_macro.write('\n'.join(macro_lines))

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **hps_sim_dir**, **hps_fieldmaps_dir**, **detector_dir**
        @return list of required config
        """
        return ['hps_sim_dir', 'hps_fieldmaps_dir', 'detector_dir']
