"""! Tools that can be used in HPSMC jobs."""

import json
import os
import gzip
import shutil
import subprocess
import tarfile

from subprocess import PIPE

from hpsmc.component import Component
import hpsmc.func as func


class SLIC(Component):
    """!
    Run the SLIC Geant4 simulation.

    Optional parameters are: **nevents**, **macros**, **run_number**, **disable_particle_table** \n
    Required parameters are: **detector** \n
    Required configurations are: **slic_dir**, **detector_dir**
    """

    def __init__(self, **kwargs):
        ## List of macros to run (optional)
        self.macros = []
        ## Run number to set on output file (optional)
        self.run_number = None
        ## To be set from config or install dir
        self.detector_dir = None
        ## Optionally disable loading of the particle table shipped with slic
        ## Note: This should not be used with a General Particle Source since the GPS
        ## in slic requires the particle table to function.
        self.disable_particle_table = False

        Component.__init__(
            self, name="slic", command="slic", output_ext=".slcio", **kwargs
        )

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        if not len(self.input_files()):
            raise Exception("No inputs given for SLIC.")

        args = [
            "-g",
            self.__detector_file(),
            # "-i", self.input_files()[0],
            "-o",
            self.output_files()[0],
            "-d%s" % str(self.seed),
        ]

        if self.nevents is not None:
            args.extend(["-r", str(self.nevents)])

        if self.run_number is not None:
            args.extend(["-m", "run_number.mac"])

        if not self.disable_particle_table:
            tbl = self.__particle_tbl()
            if os.path.exists(tbl):
                args.extend(["-P", tbl])
            else:
                raise Exception("SLIC particle.tbl does not exist: %s" % tbl)

        if len(self.macros):
            # args = []
            for macro in self.macros:
                if macro == "run_number.mac":
                    raise Exception("Macro name '%s' is not allowed." % macro)
                if not os.path.isabs(macro):
                    raise Exception("Macro '%s' is not an absolute path." % macro)
                args.extend(["-m", macro])
        else:
            args.extend(["-i", self.input_files()[0]])

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
                raise Exception("Failed to find valid detector_dir")
            self.logger.debug(
                "Using detector_dir from install: {}".format(self.detector_dir)
            )

    def setup(self):
        """! Setup SLIC component."""
        if not os.path.exists(self.slic_dir):
            raise Exception("slic_dir does not exist: %s" % self.slic_dir)

        self.env_script = self.slic_dir + os.sep + "bin" + os.sep + "slic-env.sh"
        if not os.path.exists(self.env_script):
            raise Exception("SLIC setup script does not exist: %s" % self.name)

        if self.run_number is not None:
            run_number_cmd = "/lcio/runNumber %d" % self.run_number
            run_number_mac = open("run_number.mac", "w")
            run_number_mac.write(run_number_cmd)
            run_number_mac.close()

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **nevents**, **macros**, **run_number**
        @return  list of optional parameters
        """
        return ["nevents", "macros", "run_number", "disable_particle_table"]

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **detector**
        @return  list of required parameters
        """
        return ["detector"]

    def required_config(self):
        """!
        Return list of required configurations.

        Required configurations are: **slic_dir**, **detector_dir**
        @return  list of required configurations
        """
        return ["slic_dir", "detector_dir"]

    def execute(self, log_out, log_err):
        """!
        Execute SLIC component.

        Component is executed by creating command line input
        from command and command arguments.
        @return  return code of process
        """
        # SLIC needs to be run inside bash as the Geant4 setup script is a piece of #@$@#$.
        cl = 'bash -c ". %s && %s %s"' % (
            self.env_script,
            self.command,
            " ".join(self.cmd_args()),
        )

        # self.logger.info("Executing '%s' with command: %s" % (self.name, cl))
        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()

        return proc.returncode


class SQLiteProc(Component):
    """!
    Copy the SQLite database file to the desired location.
    """

    def __init__(self, **kwargs):
        """!
        Initialize SQLiteProc to copy the SQLite file.

        This component simply copies source_file to destination_file (see execute); it does not run a command,
        so no command arguments are assembled here. Logging is deferred until after Component.__init__ has run,
        as required by the Component base class.
        """
        self.source_file = kwargs.get("source_file")
        self.destination_file = kwargs.get("destination_file")

        # Ensure to call the parent constructor properly
        Component.__init__(self, name="sqlite_file_copy", **kwargs)

    def cmd_args(self):
        """!
        Return dummy command arguments to satisfy the parent class.
        """
        cmd_args = ["(no-command-needed)"]

        if not all(isinstance(arg, str) for arg in cmd_args):
            raise ValueError("All arguments must be strings.")
        #  return ["(no-command-needed)"]
        return ["--source", self.source_file, "--destination", self.destination_file]

    def execute(self, log_out, log_err):
        """!
        Execute the file copy operation.
        """

        try:
            # Copy the file

            self.logger.info(
                f"Copying file from {self.source_file} to {self.destination_file}"
            )
            shutil.copy(self.source_file, self.destination_file)

            # Provide a job-local tmp dir (used e.g. as java.io.tmpdir by downstream Java tools).
            os.makedirs("tmp", exist_ok=True)

            # Log success
            self.logger.info(f"Successfully copied file to {self.destination_file}")

            return 0  # Success code

        except Exception as e:
            self.logger.error(f"Error during file copy: {e}")
            return 1  # Error code


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
        ## nevents
        self.nevents = None
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

        if "overlay_file" in kwargs:
            self.overlay_file = kwargs["overlay_file"]
        else:
            self.overlay_file = None

        Component.__init__(
            self,
            name="job_manager",
            command="java",
            description="HPS Java Job Manager",
            output_ext=".slcio",
            **kwargs,
        )

        # Automatically append steering file key to output file name
        if self.append_tok is None:
            self.append_tok = self.steering
            self.logger.debug(
                "Append token for '%s' automatically set to '%s' from steering key."
                % (self.name, self.append_tok)
            )

    def config(self, parser):
        """! Configure JobManager component."""
        super().config(parser)
        # if installed these are set in the environment script...
        if self.hps_java_bin_jar is None:
            if os.getenv("HPS_JAVA_BIN_JAR", None) is not None:
                self.hps_java_bin_jar = os.getenv("HPS_JAVA_BIN_JAR", None)
                self.logger.debug(
                    "Set HPS_JAVA_BIN_JAR from environment: {}".format(
                        self.hps_java_bin_jar
                    )
                )
            else:
                raise Exception(
                    "hps_java_bin_jar not set in environment or config file!"
                )
        if self.conditions_url is None:
            if os.getenv("CONDITIONS_URL", None) is not None:
                self.conditions_url = os.getenv("CONDITIONS_URL", None)
                self.logger.debug(
                    "Set CONDITIONS_URL from environment: {}".format(
                        self.hps_java_bin_jar
                    )
                )

    def required_config(self):
        """!
        Return list of required configurations.

        Required configurations are: **hps_java_bin_jar**
        @retun list of required configurations.
        """
        return ["hps_java_bin_jar"]

    def setup(self):
        """! Setup JobManager component."""
        if not len(self.input_files()):
            raise Exception("No inputs provided to hps-java.")

        if self.steering not in self.steering_files:
            raise Exception(
                "Steering '%s' not found in: %s" % (self.steering, self.steering_files)
            )
        self.steering_file = self.steering_files[self.steering]

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []

        if self.java_args is not None:
            self.logger.debug("Setting java_args from config: %s" % self.java_args)
            args.append(self.java_args)

        if self.logging_config_file is not None:
            self.logger.debug(
                "Setting logging_config_file from config: %s" % self.logging_config_file
            )
            args.append("-Djava.util.logging.config.file=%s" % self.logging_config_file)

        if self.lcsim_cache_dir is not None:
            self.logger.debug(
                "Setting lcsim_cache_dir from config: %s" % self.lcsim_cache_dir
            )
            args.append("-Dorg.lcsim.cacheDir=%s" % self.lcsim_cache_dir)

        if self.conditions_user is not None:
            self.logger.debug(
                "Setting conditions_user from config: %s" % self.conditions_user
            )
            args.append("-Dorg.hps.conditions.user=%s" % self.conditions_user)
        if self.conditions_password is not None:
            self.logger.debug("Setting conditions_password from config (not shown)")
            args.append("-Dorg.hps.conditions.password=%s" % self.conditions_password)
        if self.conditions_url is not None:
            self.logger.debug(
                "Setting conditions_url from config: %s" % self.conditions_url
            )
            args.append("-Dorg.hps.conditions.url=%s" % self.conditions_url)

        args.append("-jar")
        args.append(self.hps_java_bin_jar)

        ## \todo add event_print_interval to optional parameters?
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
            self.logger.debug(
                "Steering does not exist at '%s' so assuming it is a resource."
                % self.steering_file
            )
        else:
            if not os.path.isabs(self.steering_file):
                raise Exception(
                    "Steering looks like a file but is not an abs path: %s"
                    % self.steering_file
                )
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
        return ["steering_files"]

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **detector**, **run_number**, **defs**
        @return list of optional parameters
        """
        return ["detector", "run_number", "defs", "nevents"]


class ProcessMiniDst(Component):
    """!
    Run the make_mini_dst command on the input file.

    Required parameters are: **input_file**
    Required configs are: **minidst_install_dir**
    """

    def __init__(self, **kwargs):
        """!
        Initialize ProcessMiniDst with default input file and the command to run.
        """
        self.input_file = None
        self.minidst_args = None
        # Ensure to call the parent constructor properly
        Component.__init__(self, name='make_mini_dst',
                           command='make_mini_dst',
                           description='Create the MiniDST ROOT file',
                           output_ext='.root',
                           **kwargs)

    def setup(self):
        """! Setup the MiniDST component."""
        # Check if input files exist
        if not len(self.input_files()):
            raise Exception("No input files provided to make_mini_dst.")

        if not os.path.exists(self.minidst_install_dir):
            raise Exception("minidst_install_dir does not exist: %s" % self.minidst_install_dir)

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are only the standard "input_files".
        @return  list of required parameters
        """
        return []

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        There are currently no optional parameters.
        @return  list of optional parameters
        """
        return []

    def required_config(self):
        """!
        Return list of required configs.

        Required configs are: **minidst_install_dir**
        @return  list of required configs
        """
        return ["minidst_install_dir"]

    def output_files(self):
        """! Adjust names of output files."""
        if self.outputs is None:
            f, ext = os.path.splitext(self.input_files()[0])
            self.outputs = f"{f}_minidst.root"
            print(f"Set outputs to: {self.outputs}")

        return self.outputs

    def cmd_args(self):
        """!
        Setup command arguments for make_mini_dst.
        @return list of arguments
        """
        args = []

        print("===== Make MiniDST with input files: ", end=" ")
        for i in range(len(self.input_files())):
            print(f"{self.input_files()[i]}", end=", ")
        print(f" ==> {self.output_files()}")

        if self.minidst_args is not None:
            args.extend(self.minidst_args)

        args.extend(['-o', self.output_files()])
        args.extend(self.input_files())
        return args


class HPSTR(Component):
    """!
    Run the hpstr analysis tool.

    Required parameters are: **config_files** \n
    Optional parameters are: **year**, **is_data**, **nevents** \n
    Required configs are: **hpstr_install_dir**, **hpstr_base**
    """

    def __init__(self, cfg=None, is_data=0, year=None, tracking=None, **kwargs):
        ## configuration
        self.cfg = cfg
        ## run mode
        self.is_data = is_data
        ## year
        self.year = year
        ## tracking option (KF, GBL, BOTH)
        self.tracking = tracking

        self.hpstr_install_dir = None
        self.hpstr_base = None

        Component.__init__(self, name="hpstr", command="hpstr", **kwargs)

    def setup(self):
        """! Setup HPSTR component."""
        if not os.path.exists(self.hpstr_install_dir):
            raise Exception(
                "hpstr_install_dir does not exist: %s" % self.hpstr_install_dir
            )
        self.env_script = (
            self.hpstr_install_dir + os.sep + "bin" + os.sep + "hpstr-env.sh"
        )

        # The config file to use is read from a dict in the JSON parameters.
        if self.cfg not in self.config_files:
            raise Exception(
                "Config '%s' was not found in: %s" % (self.cfg, self.config_files)
            )
        config_file = self.config_files[self.cfg]
        if len(os.path.dirname(config_file)):
            # If there is a directory name then we expect an absolute path not in the hpstr dir.
            if os.path.isabs(config_file):
                self.cfg_path = config_file
            else:
                # The config must be an abs path.
                raise Exception(
                    "The config has a directory but is not an abs path: %s" % self.cfg
                )
        else:
            # Assume the cfg file is within the hpstr base dir.
            self.cfg_path = os.path.join(
                self.hpstr_base, "processors", "config", config_file
            )
        self.logger.debug("Set config path: %s" % self.cfg_path)

        # For ROOT output, automatically append the cfg key from the job params.
        if os.path.splitext(self.input_files()[0])[1] == ".root":
            self.append_tok = self.cfg
            self.logger.debug(
                "Automatically appending token to output file: %s" % self.append_tok
            )

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **config_files**
        @return  list of required parameters
        """
        return ["config_files"]

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **year**, **is_data**, **nevents**
        @return  list of optional parameters
        """
        return ["year", "is_data", "nevents", "tracking"]

    def required_config(self):
        """!
        Return list of required configs.

        Required configs are: **hpstr_install_dir**, **hpstr_base**
        @return  list of required configs
        """
        return ["hpstr_install_dir", "hpstr_base"]

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = [
            self.cfg_path,
            "-t",
            str(self.is_data),
            "-i",
            self.input_files()[0],
            "-o",
            self.output_files()[0],
        ]
        if self.nevents is not None:
            args.extend(["-n", str(self.nevents)])
        if self.year is not None:
            args.extend(["-y", str(self.year)])
        if self.tracking is not None:
            args.extend(["-w", str(self.tracking)])
        return args

    def output_files(self):
        """! Adjust names of output files."""
        f, ext = os.path.splitext(self.input_files()[0])
        if ".slcio" in ext:
            return ["%s.root" % f]
        else:
            if not self.append_tok:
                self.append_tok = self.cfg
            return ["%s_%s.root" % (f, self.append_tok)]

    def execute(self, log_out, log_err):
        """! Execute HPSTR component."""
        args = self.cmd_args()
        cl = 'bash -c ". %s && %s %s"' % (
            self.env_script,
            self.command,
            " ".join(self.cmd_args()),
        )

        self.logger.debug("Executing '%s' with command: %s" % (self.name, cl))
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
    seed_names = [
        "beam_coords",
        "beam_coords_old",
        "lhe_tridents",
        "lhe_tridents_displacetime",
        "lhe_tridents_displaceuni",
        "merge_poisson",
        "mix_signal",
        "random_sample",
    ]

    def __init__(self, name=None, **kwargs):

        Component.__init__(self, name=name, command="stdhep_" + name, **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []

        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])

        if len(self.output_files()) == 1:
            args.insert(0, self.output_files()[0])
        elif len(self.output_files()) > 1:
            raise Exception("Too many outputs specified for StdHepTool.")
        else:
            raise Exception("No outputs specified for StdHepTool.")

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

        StdHepTool.__init__(self, name="beam_coords", append_tok="rot", **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = StdHepTool.cmd_args(self)

        if self.beam_sigma_x is not None:
            args.extend(["-x", str(self.beam_sigma_x)])
        if self.beam_sigma_y is not None:
            args.extend(["-y", str(self.beam_sigma_y)])

        if self.beam_rot_x is not None:
            args.extend(["-u", str(self.beam_rot_x)])
        if self.beam_rot_y is not None:
            args.extend(["-v", str(self.beam_rot_y)])
        if self.beam_rot_z is not None:
            args.extend(["-w", str(self.beam_rot_z)])

        if self.target_x is not None:
            args.extend(["-X", str(self.target_x)])
        if self.target_y is not None:
            args.extend(["-Y", str(self.target_y)])
        if self.target_z is not None:
            args.extend(["-Z", str(self.target_z)])

        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **beam_sigma_x**, **beam_sigma_y**, **beam_rot_x**,
        **beam_rot_y**, **beam_rot_z**, **target_x**, **target_y**, **target_z**
        @return list of optional parameters
        """
        return [
            "beam_sigma_x",
            "beam_sigma_y",
            "beam_rot_x",
            "beam_rot_y",
            "beam_rot_z",
            "target_x",
            "target_y",
            "target_z",
        ]


class RandomSample(StdHepTool):
    """!
    Randomly sample StdHep events into a new file.

    Optional parameters are: **nevents**, **mu**
    """

    def __init__(self, **kwargs):
        StdHepTool.__init__(self, name="random_sample", append_tok="sampled", **kwargs)
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

        if len(self.output_files()) == 1:
            # only use file name, not extension because extension is added by tool
            args.insert(0, os.path.splitext(self.output_files()[0])[0])
        elif len(self.output_files()) > 1:
            raise Exception("Too many outputs specified for RandomSample.")
        else:
            raise Exception("No outputs specified for RandomSample.")

        if len(self.input_files()):
            for i in self.inputs[::-1]:
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
        return ["nevents", "mu"]

    def execute(self, log_out, log_err):
        """! Execute RandomSample component"""
        returncode = Component.execute(self, log_out, log_err)

        # Move file to proper output file location.
        src = "%s_1.stdhep" % os.path.splitext(self.output_files()[0])[0]
        dest = "%s.stdhep" % os.path.splitext(self.output_files()[0])[0]
        self.logger.debug("Moving '%s' to '%s'" % (src, dest))
        shutil.move(src, dest)

        return returncode


class Phi_LHE_to_STDHEP(StdHepTool):
    """!
    Convert LHE files to StdHep.
    """

    def __init__(self, **kwargs):
        ## time shift
        self.ctau = None
        StdHepTool.__init__(self,
                            name='lhe_phi',
                            output_ext='.stdhep',
                            **kwargs)

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = StdHepTool.cmd_args(self)
        return args


class DisplaceTime(StdHepTool):
    """!
    Convert LHE files to StdHep, displacing the time by given ctau.

    Optional parameters are: **ctau**
    """

    def __init__(self, **kwargs):
        ## time shift
        self.ctau = None
        StdHepTool.__init__(
            self, name="lhe_tridents_displacetime", output_ext=".stdhep", **kwargs
        )

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
        return ["ctau"]


class DisplaceUni(StdHepTool):
    """!
    Convert LHE files to StdHep, displacing the time by given ctau.

    Optional parameters are: **ctau**
    """

    def __init__(self, **kwargs):
        ## time shift
        self.ctau = None
        StdHepTool.__init__(
            self, name="lhe_tridents_displaceuni", output_ext=".stdhep", **kwargs
        )

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
        return ["ctau"]


class AddMother(StdHepTool):
    """!
    Add mother particles for physics samples.
    """

    def __init__(self, **kwargs):
        StdHepTool.__init__(self, name="add_mother", append_tok="mom", **kwargs)


class AddMotherFullTruth(StdHepTool):
    """! Add full truth mother particles for physics samples"""

    def __init__(self, **kwargs):
        StdHepTool.__init__(
            self, "add_mother_full_truth", append_tok="mom_full_truth", **kwargs
        )
        if len(self.inputs) != 2:
            raise Exception(
                "Must have 2 input files: a stdhep file and a lhe file in order"
            )
        self.input_file_1 = self.inputs[0]
        base, ext = os.path.splitext(self.input_file_1)
        if ext != ".stdhep":
            raise Exception("The first input file must be a stdhep file")
        self.input_file_2 = self.inputs[1]
        base, ext = os.path.splitext(self.input_file_2)
        if ext != ".lhe":
            raise Exception("The second input file must be a lhe file")

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        return super().cmd_args()


class MergePoisson(StdHepTool):
    """!
    Merge StdHep files, applying poisson sampling.

    Required parameters are: **target_thickness**, **num_electrons**
    """

    def __init__(self, xsec=0, **kwargs):
        ## cross section in pb
        self.xsec = xsec
        ## target thickness in cm
        self.target_thickness = None
        ## number of electrons per bunch
        self.num_electrons = None

        StdHepTool.__init__(self, name="merge_poisson", append_tok="sampled", **kwargs)

    def setup(self):
        """! Setup MergePoisson component."""
        if self.xsec > 0:
            self.mu = func.lint(self.target_thickness, self.num_electrons) * self.xsec
        else:
            raise Exception("Cross section is missing.")
        self.logger.info("mu is %f", self.mu)

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **target_thickness**, **num_electrons**
        @return list of required parameters
        """
        return ["target_thickness", "num_electrons"]

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []
        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])

        args.extend(["-m", str(self.mu), "-N", str(1), "-n", str(self.nevents)])

        if len(self.output_files()) == 1:
            # only use file name, not extension because extension is added by tool
            args.insert(0, os.path.splitext(self.output_files()[0])[0])
        elif len(self.output_files()) > 1:
            raise Exception("Too many outputs specified for MergePoisson.")
        else:
            raise Exception("No outputs specified for MergePoisson.")

        if len(self.input_files()):
            for i in self.inputs[::-1]:
                args.insert(0, i)
        else:
            raise Exception("No inputs were provided.")

        return args

    def execute(self, log_out, log_err):
        """! Execute MergePoisson component."""
        returncode = Component.execute(self, log_out, log_err)

        # Move file from tool to proper output file location.
        src = "%s_1.stdhep" % os.path.splitext(self.output_files()[0])[0]
        dest = "%s.stdhep" % os.path.splitext(self.output_files()[0])[0]
        self.logger.debug("Moving '%s' to '%s'" % (src, dest))
        shutil.move(src, dest)

        return returncode


class MergeFiles(StdHepTool):
    """!
    Merge StdHep files.

    Optional parameters are: none \n
    Required parameters are: none
    """

    def __init__(self, **kwargs):
        StdHepTool.__init__(self, name="merge_files", **kwargs)

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
        Component.__init__(
            self, name="stdhep_count", command="stdhep_count.sh", **kwargs
        )

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        ## \todo why does it only count the first input file?
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
        Component.__init__(self, name, "java", **kwargs)

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **hps_java_bin_jar**
        @return list of required config
        """
        return ["hps_java_bin_jar"]

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = []
        if self.java_args is not None:
            self.logger.debug("Setting java_args from config: %s" + self.java_args)
            args.append(self.java_args)
        if self.conditions_url is not None:
            self.logger.debug(
                "Setting conditions_url from config: %s" % self.conditions_url
            )
            args.append("-Dorg.hps.conditions.url=%s" % self.conditions_url)
        args.append("-cp")
        args.append(self.hps_java_bin_jar)
        args.append(self.java_class)
        return args

    def config(self, parser):
        super().config(parser)


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
        ## run number
        self.run_number = None
        ## number of events that are skipped
        self.skip_events = None
        ## event print interval
        self.event_print_interval = None
        ## steering file
        self.steering = steering

        JavaTool.__init__(
            self,
            name="evio_to_lcio",
            java_class="org.hps.evio.EvioToLcio",
            output_ext=".slcio",
            **kwargs,
        )

    def required_parameters(self):
        """!
        Return list of required parameters.

        Required parameters are: **detector**, **steering_files**
        @return list of required parameters
        """
        return ["detector", "steering_files"]

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **run_number**, **skip_events**, **nevents**, **event_print_interval**
        @return list of optional parameters
        """
        return ["run_number", "skip_events", "nevents", "event_print_interval"]

    def setup(self):
        """! Setup EvioToLcio component."""
        super().setup()
        if self.steering not in self.steering_files:
            raise Exception(
                "Steering '%s' not found in: %s" % (self.steering, self.steering_files)
            )
        self.steering_file = self.steering_files[self.steering]

    def cmd_args(self):
        """!
        Setup command arguments.
        @return  list of arguments
        """
        args = JavaTool.cmd_args(self)
        if not len(self.output_files()):
            raise Exception("No output files were provided.")
        output_file = self.output_files()[0]
        # Keep Java's scratch under the job dir (created by SQLiteProc) rather than the shared system /tmp.
        args.append("-Djava.io.tmpdir=./tmp")
        args.append("-DoutputFile=%s" % os.path.splitext(output_file)[0])
        # Fall back to a job-local SQLite conditions snapshot when no conditions URL was configured, so
        # offline/el9 running does not require the central conditions database. A configured conditions_url
        # (handled by JavaTool.cmd_args above) still takes precedence.
        if self.conditions_url is None:
            args.append("-Dorg.hps.conditions.url=jdbc:sqlite:./hps_local_conditions.db")
        args.extend(["-d", self.detector])
        if self.run_number is not None:
            args.extend(["-R", str(self.run_number)])
        if self.skip_events is not None:
            args.extend(["-s", str(self.skip_events)])

        if not os.path.isfile(self.steering_file):
            args.append("-r")
            self.logger.debug(
                "Steering does not exist at '%s' so assuming it is a resource."
                % self.steering_file
            )
        else:
            if not os.path.isabs(self.steering_file):
                raise Exception(
                    "Steering looks like a file but is not an abs path: %s"
                    % self.steering_file
                )
        args.extend(["-x", self.steering_file])

        if self.nevents is not None:
            args.extend(["-n", str(self.nevents)])

        args.append("-b")

        for inputfile in self.input_files():
            args.append(inputfile)

        if self.event_print_interval is not None:
            args.extend(["-e", str(self.event_print_interval)])

        return args


class FilterBunches(JavaTool):
    """!
    Space MC events and apply energy filters to process before readout.

    Optional parameters are: **filter_ecal_hit_ecut**, **filter_event_interval**,
    **filter_nevents_read**, **filter_nevents_write**, **filter_no_cuts** \n
    Required config are: **hps_java_bin_jar**
    """

    def __init__(self, **kwargs):
        if "filter_no_cuts" in kwargs:
            self.filter_no_cuts = kwargs["filter_no_cuts"]
        else:
            ## By default cuts are on
            self.filter_no_cuts = False

        if "filter_ecal_pairs" in kwargs:
            self.filter_ecal_pairs = kwargs["filter_ecal_pairs"]
        else:
            self.filter_ecal_pairs = False

        if "filter_ecal_hit_ecut" in kwargs:
            self.filter_ecal_hit_ecut = kwargs["filter_ecal_hit_ecut"]
        else:
            ## No default ecal hit cut energy (negative val to be ignored)
            self.filter_ecal_hit_ecut = -1.0
            # self.filter_ecal_hit_ecut = 0.05

        if "filter_event_interval" in kwargs:
            self.filter_event_interval = kwargs["filter_event_interval"]
        else:
            ## Default event filtering interval
            self.filter_event_interval = 250

        if "filter_nevents_read" in kwargs:
            self.filter_nevents_read = kwargs["filter_nevents_read"]
        else:
            ## Default is no maximum nevents to read
            self.filter_nevents_read = -1

        if "filter_nevents_write" in kwargs:
            self.filter_nevents_write = kwargs["filter_nevents_write"]
        else:
            ## Default is no maximum nevents to write
            self.filter_nevents_write = -1

        self.hps_java_bin_jar = None

        JavaTool.__init__(
            self,
            name="filter_bunches",
            java_class="org.hps.util.FilterMCBunches",
            append_tok="filt",
            **kwargs,
        )

    def config(self, parser):
        """! Configure FilterBunches component."""
        super().config(parser)
        if self.hps_java_bin_jar is None:
            if os.getenv("HPS_JAVA_BIN_JAR", None) is not None:
                self.hps_java_bin_jar = os.getenv("HPS_JAVA_BIN_JAR", None)
                self.logger.debug(
                    "Set HPS_JAVA_BIN_JAR from environment: {}".format(
                        self.hps_java_bin_jar
                    )
                )

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
            args.append("-n")
            args.append(str(self.filter_nevents_read))
        if self.filter_nevents_write > 0:
            args.append("-w")
            args.append(str(self.filter_nevents_write))
        if self.filter_no_cuts:
            args.append("-a")
        return args

    def optional_parameters(self):
        """!
        Return list of optional parameters.

        Optional parameters are: **filter_ecal_hit_ecut**, **filter_event_interval**,
        **filter_nevents_read**, **filter_nevents_write**, **filter_no_cuts** \n
        @return list of optional parameters
        """
        return [
            "filter_ecal_hit_ecut",
            "filter_event_interval",
            "filter_nevents_read",
            "filter_nevents_write",
            "filter_no_cuts",
        ]

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **hps_java_bin_jar**
        @return list of required config
        """
        return ["hps_java_bin_jar"]


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
            self.num_hodo_hits = kwargs["num_hodo_hits"]
        else:
            self.num_hodo_hits = 0

        if "event_interval" in kwargs:
            self.event_interval = kwargs["event_interval"]
        else:
            self.event_interval = 250

        JavaTool.__init__(
            self,
            name="filter_events",
            java_class="org.hps.util.ExtractEventsWithHitAtHodoEcal",
            append_tok="filt",
            **kwargs,
        )

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
        return ["num_hodo_hits", "event_interval"]


class Unzip(Component):
    """!
    Unzip the input files to outputs.
    """

    def __init__(self, **kwargs):
        Component.__init__(self, name="unzip", command="gunzip", **kwargs)

    def output_files(self):
        """! Return list of output files."""
        if self.outputs:
            return self.outputs
        return [os.path.splitext(i)[0] for i in self.input_files()]

    def execute(self, log_out, log_err):
        """! Execute Unzip component."""
        for i in range(0, len(self.input_files())):
            inputfile = self.input_files()[i]
            outputfile = self.output_files()[i]
            with gzip.open(inputfile, "rb") as in_file, open(
                outputfile, "wb"
            ) as out_file:
                shutil.copyfileobj(in_file, out_file)
                self.logger.debug("Unzipped '%s' to '%s'" % (inputfile, outputfile))
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
        Component.__init__(self, name="lcio_dump_event", command="dumpevent", **kwargs)

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
        self.command = self.lcio_dir + "/bin/dumpevent"

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
        return ["lcio_dir"]

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
        Component.__init__(self, name="lhe_count", **kwargs)

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
            with gzip.open(i, "rb") as in_file:
                lines = in_file.readlines()

            nevents = 0
            for line in lines:
                if "<event>" in line:
                    nevents += 1

            print("LHE file '%s' has %d events." % (i, nevents))

            if nevents < self.minevents:
                msg = "LHE file '%s' does not contain the minimum %d events." % (
                    i,
                    nevents,
                )
                if self.fail_on_underflow:
                    raise Exception(msg)
                else:
                    self.logger.warning(msg)
        return 0


class TarFiles(Component):
    """!
    Tar files into an archive.
    """

    def __init__(self, **kwargs):
        Component.__init__(self, name="tar_files", **kwargs)

    def cmd_exists(self):
        """!
        Check if command exists.
        @return True if command exists
        """
        return True

    def execute(self, log_out, log_err):
        """! Execute TarFiles component."""
        self.logger.debug("Opening '%s' for writing ..." % self.outputs[0])
        tar = tarfile.open(self.outputs[0], "w")
        for i in self.inputs:
            self.logger.debug("Adding '%s' to archive" % i)
            tar.add(i)
        tar.close()
        self.logger.info("Wrote archive '%s'" % self.outputs[0])
        return 0


class MoveFiles(Component):
    """!
    Move input files to new locations.
    """

    def __init__(self, **kwargs):
        Component.__init__(self, name="move_files", **kwargs)

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
            self.logger.info("Moving %s -> %s" % (src, dest))
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
        Component.__init__(self, name, command="java", **kwargs)

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
        if not self.name:
            raise Exception("Name required to write cmd args for LCIOTool.")
        return ["-jar", self.lcio_bin_jar, self.name]

    def required_config(self):
        """!
        Return list of required config.

        Required config are: **lcio_bin_jar**
        @return list of required config
        """
        return ["lcio_bin_jar"]

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
        LCIOTool.__init__(self, name="concat", **kwargs)

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
        LCIOTool.__init__(self, name="count", **kwargs)

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
        LCIOTool.__init__(self, name="merge", **kwargs)

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
            args.extend(["-n", str(self.nevents)])
        return args


"""
MergeROOT tool for hps-mc
Merges ROOT files using hadd with validation
"""


class MergeROOT(Component):
    """
    Merge ROOT files using hadd with event count validation.

    This component uses ROOT's hadd utility to merge multiple ROOT files
    into a single output file, and validates that all events are preserved.
    """

    def __init__(self, **kwargs):
        """
        Initialize MergeROOT component.

        Parameters
        ----------
        inputs : list
            List of input ROOT files to merge
        outputs : list
            List containing the output merged ROOT file name
        force : bool, optional
            Force overwrite of output file (default: True)
        compression : int, optional
            Compression level for output file (0-9, default: None uses hadd default)
        validate : bool, optional
            Validate event counts after merge (default: True)
        write_stats : bool, optional
            Write JSON stats file after merge (default: True when validate=True)
        job_id : int, optional
            Job ID to include in stats output
        """
        Component.__init__(self, **kwargs)

        # Set default command
        if not hasattr(self, "command") or self.command is None:
            self.command = "hadd"

        # Set force overwrite by default
        if not hasattr(self, "force"):
            self.force = True

        # Optional compression level
        if not hasattr(self, "compression"):
            self.compression = None

        # Enable validation by default
        if not hasattr(self, "validate"):
            self.validate = True

        # Write stats JSON (default: True when validate=True)
        if not hasattr(self, "write_stats"):
            self.write_stats = self.validate

        # Optional job ID for stats output
        if not hasattr(self, "job_id"):
            self.job_id = None

        # Store event counts
        self.input_tree_counts = {}
        self.output_tree_counts = {}

        # Track validation result
        self._validation_passed = None

    def cmd_args(self):
        """
        Build command line arguments for hadd.

        Returns
        -------
        list
            List of command arguments
        """
        import sys
        sys.stderr.write("MergeROOT DEBUG: cmd_args() called\n")
        sys.stderr.write("  self.force=%s, self.compression=%s\n" % (self.force, self.compression))
        sys.stderr.write("  self.inputs=%s\n" % self.inputs)
        sys.stderr.write("  self.outputs=%s\n" % self.outputs)
        sys.stderr.flush()

        args = []

        # Add force flag if enabled
        if self.force:
            args.append("-f")

        # Add compression level if specified
        if self.compression is not None:
            args.extend(["-fk", "-f%d" % self.compression])

        # Add output file
        if self.outputs and len(self.outputs) > 0:
            args.append(self.outputs[0])
        else:
            sys.stderr.write("MergeROOT DEBUG: ERROR - No output file specified!\n")
            sys.stderr.flush()
            raise RuntimeError("MergeROOT: No output file specified")

        # Add input files
        if self.inputs and len(self.inputs) > 0:
            args.extend(self.inputs)
        else:
            sys.stderr.write("MergeROOT DEBUG: ERROR - No input files specified!\n")
            sys.stderr.flush()
            raise RuntimeError("MergeROOT: No input files specified")

        sys.stderr.write("MergeROOT DEBUG: cmd_args() returning: %s\n" % args)
        sys.stderr.flush()
        return args

    def scan_root_file(self, filename, log_out=None):
        """
        Scan a ROOT file and extract TTree event counts.

        Parameters
        ----------
        filename : str
            Path to ROOT file
        log_out : file, optional
            Log file for output (used to report multiple key cycles)

        Returns
        -------
        dict
            Dictionary mapping tree names to entry counts
        """
        try:
            import ROOT
        except ImportError:
            raise RuntimeError(
                "MergeROOT: PyROOT is required for validation but not available"
            )

        tree_counts = {}
        tree_cycles = {}  # Track cycle numbers: {tree_name: [(cycle, entries), ...]}

        # Open ROOT file
        root_file = ROOT.TFile.Open(filename, "READ")
        if not root_file or root_file.IsZombie():
            raise RuntimeError("MergeROOT: Cannot open ROOT file: %s" % filename)

        # Iterate through all keys in the file
        for key in root_file.GetListOfKeys():
            obj = key.ReadObj()

            # Check if it's a TTree
            if obj.InheritsFrom("TTree"):
                tree_name = obj.GetName()
                cycle = key.GetCycle()
                num_entries = obj.GetEntries()

                if tree_name not in tree_cycles:
                    tree_cycles[tree_name] = []
                tree_cycles[tree_name].append((cycle, num_entries))

        root_file.Close()

        # Process collected cycles - use highest cycle number for each tree
        for tree_name, cycles in tree_cycles.items():
            if len(cycles) > 1:
                # Sort by cycle number (highest first)
                cycles.sort(key=lambda x: x[0], reverse=True)
                highest_cycle, highest_entries = cycles[0]
                if log_out:
                    log_out.write("  WARNING: Multiple key cycles found for tree '%s':\n" % tree_name)
                    for cyc, ent in cycles:
                        marker = " <-- using" if cyc == highest_cycle else ""
                        log_out.write("    Cycle %d: %d entries%s\n" % (cyc, ent, marker))
                tree_counts[tree_name] = highest_entries
            else:
                tree_counts[tree_name] = cycles[0][1]

        return tree_counts

    def scan_input_files(self, log_out):
        """
        Scan all input files and store tree event counts.

        Parameters
        ----------
        log_out : file
            Log file for output
        """
        log_out.write("\n" + "=" * 70 + "\n")
        log_out.write("MergeROOT: Scanning input files for TTrees\n")
        log_out.write("=" * 70 + "\n")

        for input_file in self.inputs:
            if not os.path.exists(input_file):
                raise RuntimeError("MergeROOT: Input file not found: %s" % input_file)

            log_out.write("\nScanning: %s\n" % input_file)
            tree_counts = self.scan_root_file(input_file, log_out)

            if not tree_counts:
                log_out.write("  WARNING: No TTrees found in this file\n")
            else:
                for tree_name, count in tree_counts.items():
                    log_out.write("  Tree '%s': %d events\n" % (tree_name, count))

            self.input_tree_counts[input_file] = tree_counts

        log_out.write("\n" + "=" * 70 + "\n")
        log_out.flush()

    def scan_output_file(self, log_out):
        """
        Scan output file and store tree event counts.

        Parameters
        ----------
        log_out : file
            Log file for output
        """
        output_file = self.outputs[0]

        log_out.write("\n" + "=" * 70 + "\n")
        log_out.write("MergeROOT: Scanning output file for TTrees\n")
        log_out.write("=" * 70 + "\n")
        log_out.write("\nScanning: %s\n" % output_file)

        self.output_tree_counts = self.scan_root_file(output_file, log_out)

        if not self.output_tree_counts:
            log_out.write("  WARNING: No TTrees found in output file\n")
        else:
            for tree_name, count in self.output_tree_counts.items():
                log_out.write("  Tree '%s': %d events\n" % (tree_name, count))

        log_out.write("\n" + "=" * 70 + "\n")
        log_out.flush()

    def validate_merge(self, log_out):
        """
        Validate that event counts match between input and output files.

        Parameters
        ----------
        log_out : file
            Log file for output

        Returns
        -------
        bool
            True if validation passes, False otherwise
        """
        log_out.write("\n" + "=" * 70 + "\n")
        log_out.write("MergeROOT: Validating merge results\n")
        log_out.write("=" * 70 + "\n\n")

        # Calculate sum of events per tree across all input files
        total_input_counts = {}

        for input_file, tree_counts in self.input_tree_counts.items():
            for tree_name, count in tree_counts.items():
                if tree_name not in total_input_counts:
                    total_input_counts[tree_name] = 0
                total_input_counts[tree_name] += count

        # Check that all input trees are in output
        all_valid = True

        if not total_input_counts:
            log_out.write("WARNING: No TTrees found in input files\n")
            return True

        log_out.write("Event count validation:\n")
        log_out.write("-" * 70 + "\n")
        log_out.write(
            "%-30s %15s %15s %10s\n"
            % ("Tree Name", "Input Events", "Output Events", "Status")
        )
        log_out.write("-" * 70 + "\n")

        for tree_name, input_count in sorted(total_input_counts.items()):
            output_count = self.output_tree_counts.get(tree_name, 0)

            if output_count == input_count:
                status = "✓ PASS"
            else:
                status = "✗ FAIL"
                all_valid = False

            log_out.write(
                "%-30s %15d %15d %10s\n"
                % (tree_name, input_count, output_count, status)
            )

        # Check for trees in output that weren't in input
        extra_trees = set(self.output_tree_counts.keys()) - set(
            total_input_counts.keys()
        )
        if extra_trees:
            log_out.write("\nWARNING: Output contains trees not found in inputs:\n")
            for tree_name in extra_trees:
                log_out.write(
                    "  - %s: %d events\n"
                    % (tree_name, self.output_tree_counts[tree_name])
                )

        log_out.write("-" * 70 + "\n")

        if all_valid:
            log_out.write("\n✓ VALIDATION PASSED: All event counts match!\n")
        else:
            log_out.write("\n✗ VALIDATION FAILED: Event count mismatch detected!\n")

        log_out.write("=" * 70 + "\n\n")
        log_out.flush()

        return all_valid

    def print_summary(self, log_out):
        """
        Print a summary of the merge operation.

        Parameters
        ----------
        log_out : file
            Log file for output
        """
        log_out.write("\n" + "=" * 70 + "\n")
        log_out.write("MergeROOT: Summary\n")
        log_out.write("=" * 70 + "\n")
        log_out.write("Input files: %d\n" % len(self.inputs))

        for i, input_file in enumerate(self.inputs, 1):
            log_out.write("  %d. %s\n" % (i, input_file))

        log_out.write("\nOutput file: %s\n" % self.outputs[0])
        log_out.write(
            "Compression level: %s\n"
            % (self.compression if self.compression else "default")
        )

        # Print total events per tree
        if self.output_tree_counts:
            log_out.write("\nTotal events in merged file:\n")
            for tree_name, count in sorted(self.output_tree_counts.items()):
                log_out.write("  %-30s: %d events\n" % (tree_name, count))

        log_out.write("=" * 70 + "\n")
        log_out.flush()

    def get_stats_filename(self):
        """
        Get the stats JSON filename based on the output ROOT filename.

        Returns
        -------
        str
            Path to stats JSON file (e.g., 'merged_X_job1.root' -> 'merged_X_job1_stats.json')
        """
        if not self.outputs or len(self.outputs) == 0:
            return None
        output_file = self.outputs[0]
        base, _ = os.path.splitext(output_file)
        return base + "_stats.json"

    def write_stats_json(self, log_out, validation_passed):
        """
        Write merge statistics to a JSON file.

        Parameters
        ----------
        log_out : file
            Log file for output
        validation_passed : bool
            Whether the validation passed
        """
        stats_file = self.get_stats_filename()
        if stats_file is None:
            log_out.write("WARNING: Cannot determine stats filename, skipping stats output\n")
            return

        log_out.write("\n" + "=" * 70 + "\n")
        log_out.write("MergeROOT: Writing stats to %s\n" % stats_file)
        log_out.write("=" * 70 + "\n")

        # Calculate total input events per tree
        total_input_events = {}
        for input_file, tree_counts in self.input_tree_counts.items():
            for tree_name, count in tree_counts.items():
                if tree_name not in total_input_events:
                    total_input_events[tree_name] = 0
                total_input_events[tree_name] += count

        # Build input files list with event counts
        input_files_list = []
        for input_file in self.inputs:
            tree_counts = self.input_tree_counts.get(input_file, {})
            input_files_list.append({
                "path": input_file,
                "events": tree_counts
            })

        # Build stats dictionary
        stats = {
            "job_id": self.job_id,
            "output_file": self.outputs[0] if self.outputs else None,
            "output_events": self.output_tree_counts,
            "input_files": input_files_list,
            "total_input_events": total_input_events,
            "validation_passed": validation_passed,
            "num_input_files": len(self.inputs)
        }

        # Write JSON file
        with open(stats_file, 'w') as f:
            json.dump(stats, f, indent=2)

        log_out.write("Stats written successfully\n")
        log_out.write("=" * 70 + "\n")
        log_out.flush()

    def execute(self, log_out, log_err):
        """
        Execute MergeROOT component using hadd.

        Parameters
        ----------
        log_out : file
            Log file for stdout
        log_err : file
            Log file for stderr

        Returns
        -------
        int
            Return code from hadd command
        """
        # Debug: Entry point
        log_out.write("\n" + "=" * 70 + "\n")
        log_out.write("MergeROOT: DEBUG - Entering execute()\n")
        log_out.write("=" * 70 + "\n")
        log_out.write("DEBUG: self.command = %s\n" % self.command)
        log_out.write("DEBUG: self.inputs = %s\n" % self.inputs)
        log_out.write("DEBUG: self.outputs = %s\n" % self.outputs)
        log_out.write("DEBUG: self.force = %s\n" % self.force)
        log_out.write("DEBUG: self.compression = %s\n" % self.compression)
        log_out.write("DEBUG: self.validate = %s\n" % self.validate)
        log_out.flush()

        # Check that hadd command exists
        log_out.write("\nDEBUG: Checking if hadd command exists...\n")
        log_out.flush()
        if not self.cmd_exists():
            raise RuntimeError("MergeROOT: hadd command not found in PATH")
        log_out.write("DEBUG: hadd command found\n")
        log_out.flush()

        # Check that input files exist
        log_out.write("\nDEBUG: Checking input files exist...\n")
        log_out.flush()
        for input_file in self.inputs:
            log_out.write("DEBUG: Checking: %s\n" % input_file)
            log_out.flush()
            if not os.path.exists(input_file):
                raise RuntimeError("MergeROOT: Input file not found: %s" % input_file)
            log_out.write("DEBUG:   -> exists (size: %d bytes)\n" % os.path.getsize(input_file))
            log_out.flush()

        # Scan input files before merge if validation is enabled
        log_out.write("\nDEBUG: Validation enabled = %s\n" % self.validate)
        log_out.flush()
        if self.validate:
            try:
                log_out.write("DEBUG: Starting input file scan...\n")
                log_out.flush()
                self.scan_input_files(log_out)
                log_out.write("DEBUG: Input file scan complete\n")
                log_out.flush()
            except Exception as e:
                log_out.write("\nWARNING: Could not scan input files: %s\n" % str(e))
                log_out.write("Proceeding with merge without validation.\n")
                self.validate = False

        # Build full command
        log_out.write("\nDEBUG: Building command arguments...\n")
        log_out.flush()
        cmd = [self.command] + self.cmd_args()
        log_out.write("DEBUG: cmd_args() returned: %s\n" % self.cmd_args())
        log_out.flush()

        # Log the command
        log_out.write("\n" + "=" * 70 + "\n")
        log_out.write("MergeROOT: Executing hadd\n")
        log_out.write("=" * 70 + "\n")
        log_out.write("Command: %s\n" % " ".join(cmd))
        log_out.write("=" * 70 + "\n\n")
        log_out.flush()

        # Execute hadd
        log_out.write("DEBUG: About to call subprocess.Popen...\n")
        log_out.flush()
        proc = subprocess.Popen(cmd, stdout=log_out, stderr=log_err)
        log_out.write("DEBUG: Popen returned, PID = %s\n" % proc.pid)
        log_out.flush()
        log_out.write("DEBUG: Waiting for process to complete...\n")
        log_out.flush()
        proc.wait()
        log_out.write("DEBUG: Process completed, returncode = %d\n" % proc.returncode)
        log_out.flush()

        # Check return code
        if proc.returncode != 0:
            log_out.write("DEBUG: hadd FAILED with return code %d\n" % proc.returncode)
            log_out.flush()
            raise RuntimeError(
                "MergeROOT: hadd failed with return code %d" % proc.returncode
            )

        # Verify output file was created
        log_out.write("DEBUG: Checking if output file exists: %s\n" % self.outputs[0])
        log_out.flush()
        if not os.path.exists(self.outputs[0]):
            raise RuntimeError(
                "MergeROOT: Output file was not created: %s" % self.outputs[0]
            )
        log_out.write("DEBUG: Output file exists, size = %d bytes\n" % os.path.getsize(self.outputs[0]))
        log_out.flush()

        log_out.write("\n✓ hadd completed successfully\n")
        log_out.flush()

        # Scan output file and validate if enabled
        log_out.write("\nDEBUG: Post-merge validation check, self.validate = %s\n" % self.validate)
        log_out.flush()
        validation_passed = True
        if self.validate:
            try:
                log_out.write("DEBUG: Starting output file scan...\n")
                log_out.flush()
                self.scan_output_file(log_out)
                log_out.write("DEBUG: Output file scan complete\n")
                log_out.flush()
                log_out.write("DEBUG: Starting merge validation...\n")
                log_out.flush()
                validation_passed = self.validate_merge(log_out)
                self._validation_passed = validation_passed
                log_out.write("DEBUG: Merge validation complete, passed = %s\n" % validation_passed)
                log_out.flush()

                if not validation_passed:
                    raise RuntimeError("MergeROOT: Event count validation failed!")

            except Exception as e:
                log_out.write("\nERROR during validation: %s\n" % str(e))
                log_out.flush()
                raise

        # Write stats JSON if enabled
        log_out.write("\nDEBUG: write_stats = %s\n" % self.write_stats)
        log_out.flush()
        if self.write_stats:
            try:
                self.write_stats_json(log_out, validation_passed)
            except Exception as e:
                log_out.write("\nWARNING: Could not write stats JSON: %s\n" % str(e))
                log_out.flush()

        # Print summary
        log_out.write("\nDEBUG: Printing summary...\n")
        log_out.flush()
        self.print_summary(log_out)

        log_out.write("\nDEBUG: MergeROOT.execute() returning %d\n" % proc.returncode)
        log_out.flush()
        return proc.returncode

    def output_files(self):
        """
        Return list of output files.

        Returns
        -------
        list
            List containing the merged output ROOT file and optionally the stats JSON
        """
        files = list(self.outputs) if self.outputs else []
        if self.write_stats:
            stats_file = self.get_stats_filename()
            if stats_file and stats_file not in files:
                files.append(stats_file)
        return files

    def required_config(self):
        """
        Return list of required configuration parameters.

        Returns
        -------
        list
            List of required config parameters (empty for MergeROOT)
        """
        return []
