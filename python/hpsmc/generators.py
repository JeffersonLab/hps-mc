"""! @package generators
Event generation tools."""

import os
import shutil
import glob
import gzip
import logging

from hpsmc.component import Component
from hpsmc.run_params import RunParameters

logger = logging.getLogger("hpsmc.generators")


class EventGenerator(Component):
    """! Event generator base class."""

    def __init__(self, name, command=None, **kwargs):
        Component.__init__(self, name, command=command, description='', **kwargs)

    def required_parameters(self):
        return ['nevents']

    def get_install_dir(self):
        if os.getenv("HPSMC_DIR") is None:
            raise Exception("HPSMC_DIR is not set!")
        return "{}/share/generators".format(os.getenv("HPSMC_DIR", None))


class EGS5(EventGenerator):
    """!
    Run the EGS5 event generator to produce a StdHep file.

    Required parameters are **seed**, **run_parameters** \n
    Optional parameters are: **bunches**, **target_thickness**
    """

    def __init__(self, name='', **kwargs):
        ## \todo is this the number of bunches or the number of particles per bunch?
        self.bunches = 5e5
        ## target thickness in $\mu$m, \todo is this correct?
        self.target_thickness = None
        ## egs5 installation directory
        self.egs5_dir = None
        EventGenerator.__init__(self, name, "egs5_" + name, **kwargs)

    def get_install_dir(self):
        """! Get installation directory."""
        return EventGenerator.get_install_dir(self) + "/egs5"

    def setup(self):
        """! Setup of egs5 event generator."""
        EventGenerator.setup(self)

        if self.egs5_dir is None:
            self.egs5_dir = self.get_install_dir()
            logger.debug("Using EGS5 from install dir: " + self.egs5_dir)

        ## data directory
        self.egs5_data_dir = os.path.join(self.egs5_dir, "data")
        ## config directory
        self.egs5_config_dir = os.path.join(self.egs5_dir, "config")

        logger.debug("egs5_data_dir=%s" % self.egs5_data_dir)
        logger.debug("egs5_config_dir=%s" % self.egs5_config_dir)

        if os.path.exists("data"):
            os.unlink("data")
        os.symlink(self.egs5_data_dir, "data")

        if os.path.exists("pgs5job.pegs5inp"):
            os.unlink("pgs5job.pegs5inp")
        os.symlink(self.egs5_config_dir + "/src/esa.inp", "pgs5job.pegs5inp")

        logger.debug("Reading run parameters: {}".format(self.run_params))  # run_params here 3pt74, 1pt1, etc -> called run_params_key in following comments
        # run parameters
        self.run_param_data = RunParameters(self.run_params)  # initializing run params

        # Set target thickness from job parameter or use the default from run parameters
        if self.target_thickness is not None:
            self.target_z = self.target_thickness
            logger.debug("Target thickness set from job param: {}".format(self.target_z))
        else:
            self.target_z = self.run_param_data.get("target_z")  # gets target thickness: run_params["target_z"][run_params_key] (run_params here the params in run_params.py)
            logger.debug("Target thickness set from run_params: {}".format(self.target_z))

        ebeam = self.run_param_data.get("beam_energy")
        electrons = self.run_param_data.get("num_electrons") * self.bunches

        seed_data = "%d %f %f %d" % (self.seed, self.target_z, ebeam, electrons)
        logger.debug("Seed data (seed, target_z, ebeam, electrons): {}".format(seed_data))
        seed_file = open("seed.dat", 'w')
        seed_file.write(seed_data)
        seed_file.close()

    def output_files(self):
        """! Generate output file name.
        @return moller.stdhep if name of generator contains moller; beam.stdhep else"""
        # Output file for Moller generation
        if 'moller' in self.name:
            return ['moller.stdhep']
        # Output file for beam generation
        return ['beam.stdhep']

    def execute(self, log_out, log_err):
        """! Execute event generator.
        @param log_out  name of log file for output
        @param log_err  name of log file for error
        """
        EventGenerator.execute(self, log_out, log_err)
        if 'moller' not in self.name:
            src = os.path.join(self.rundir, 'brems.stdhep')
            dest = os.path.join(self.rundir, self.output_files()[0])
            logger.debug("Copying '%s' to '%s'" % (src, dest))
            shutil.copy(src, dest)

    def required_parameters(self):
        """!
        Return required parameters.

        Required parameters are **seed**, **run_parameters**
        """
        return ['seed', 'run_params']

    def optional_parameters(self):
        """!
        Return optional parameters.

        Optional parameters are: **bunches**, **target_thickness**
        """
        return ['bunches', 'target_thickness']

    # def required_config(self):
    #    return ['egs5_dir']


class StdHepConverter(EGS5):
    """! Convert LHE files to StdHep using EGS5."""

    def __init__(self, name="lhe_v1", **kwargs):
        EGS5.__init__(self, name, **kwargs)

        if self.name not in ["lhe_v1", "lhe_rad", "lhe_prompt", "lhe_uniform", "lhe_exponential"]:
            raise Exception("The name '%s' is not valid for StdHepConverter tools." % self.name)

    def config(self, parser):
        EGS5.config(self, parser)

    def setup(self):
        """! Setup egs5 generator.
        Throws exception if input LHE file is missing."""
        EGS5.setup(self)
        if not len(self.inputs):
            raise Exception("Missing required input LHE file.")

    def execute(self, log_out, log_err):
        """! Execute converter.
        Calls egs5 generator.

        @param log_out  name of log file for output
        @param log_err  name of log file for error
        @return egs5 error code
        """
        input_file = self.inputs[0]
        base, ext = os.path.splitext(input_file)
        if ext == ".lhe":
            os.symlink(input_file, "egs5job.inp")
        elif ext == ".gz":
            with open("egs5job.inp", 'wb') as outfile:
                with gzip.open(self.inputs[0], 'rb') as infile:
                    outfile.write(infile.read())
        else:
            raise Exception('Input file has an unknown extension: %s' % input_file)
        return EGS5.execute(self, log_out, log_err)

    def output_files(self):
        """! Converts *.lhe.gz and *.lhe to *.stdhep files."""
        return [self.input_files()[0].replace(".lhe.gz", ".stdhep").replace(".lhe", ".stdhep")]


class MG(EventGenerator):
    """!
    Abstract class for MadGraph generators.

    Required parameters are: **nevents**, **run_params** \n
    Optional parameters are: **seed**, **param_card**, **apmass**, **map**, **mpid**, **mrhod**
    """

    def __init__(self, name, **kwargs):

        ## Install dir or user config will be used for this.
        self.madgraph_dir = None

        ## Default name of param card
        self.param_card = "param_card.dat"

        # Extra parameters for param card:
        # map = mass of the A-prime
        # mpid = mass of the dark pion
        # mrhod = mass of the dark rho
        ## A-prime mass? \todo apmass or map is A-prime mass?
        self.apmass = None
        ## A-prime mass? \todo apmass or map is A-prime mass?
        self.map = None
        ## dark pion mass
        self.mpid = None
        ## dark rho mass
        self.mrhod = None

        if 'event_types' in kwargs:
            ## event types: weighted or unweighted
            self.event_types = kwargs['event_types']
        else:
            self.event_types = ['unweighted', 'weighted']

        EventGenerator.__init__(self, name, **kwargs)

    def output_files(self):
        """! Generate output file name."""
        o = []
        if 'unweighted' in self.event_types:
            o.append(self.name + "_unweighted_events.lhe.gz")
        if 'weighted' in self.event_types:
            o.append(self.name + "_events.lhe.gz")
        return o

    def set_parameters(self, params):
        """! Set parameters."""
        Component.set_parameters(self, params)
        self.run_card = "run_card_" + self.run_params + ".dat"
        logger.debug("Set run card to '%s'" % self.run_card)

    def required_parameters(self):
        """!
        Return required parameters.

        Required parameters are: **nevents**, **run_params**
        """
        return ['nevents', 'run_params']

    def optional_parameters(self):
        """!
        Return optional parameters.

        Optional parameters are: **seed**, **param_card**, **apmass**, **map**, **mpid**, **mrhod**
        """
        return ['seed', 'param_card', 'apmass', 'map', 'mpid', 'mrhod']

    # def required_config(self):
    #    return ['madgraph_dir']

    def make_run_card(self, run_card):
        """! Make run card."""
        ## \todo explain what run card is
        logger.info("Making run card '%s' with nevents=%d, seed=%d" % (run_card, self.nevents, self.seed))

        with open(run_card, 'r') as cardin:
            data = cardin.readlines()

        for i in range(0, len(data)):
            if "= nevents" in data[i]:
                logger.debug("Setting nevents=%d in run card" % self.nevents)
                data[i] = " " + str(self.nevents) + " = nevents ! Number of unweighted events requested" + '\n'
            if "= iseed" in data[i]:
                logger.debug("Setting seed=%d in run card" % self.seed)
                data[i] = " " + str(self.seed) + " = iseed   ! rnd seed (0=assigned automatically=default))" + '\n'

        with open(run_card, 'w') as cardout:
            cardout.writelines(data)

    def make_param_card(self, param_card):
        """! Make parameter card."""
        ## \todo explain what param card is
        logger.debug("Making param card '%s'" % param_card)

        with open(param_card, 'r') as paramin:
            data = paramin.readlines()

        for i in range(0, len(data)):
            if "APMASS" in data[i] and self.apmass is not None:
                data[i] = "       622     %.7fe-03   # APMASS" % (self.apmass) + '\n'
                logger.debug("APMASS in param card set to %d" % self.apmass)
            if "map" in data[i] and self.map is not None:
                data[i] = "      622 %.7fe-03 # map" % (self.map) + '\n'
            if "mpid" in data[i] and self.mpid is not None:
                data[i] = "      624 %.7fe-03 # mpid" % (self.mpid) + '\n'
            if "mrhod" in data[i] and self.mrhod is not None:
                data[i] = "      625 %.7fe-03 # mrhod" % (self.mrhod) + '\n'

        with open(param_card, 'w') as paramout:
            paramout.writelines(data)

    def cmd_args(self):
        """! Return command arguments."""
        return ["0", self.name]

    def execute(self, log_out, log_err):
        """! Execute MadGraph generator.
        @param log_out  name of log file for output
        @param log_err  name of log file for error
        @return  error code
        """
        os.chdir(os.path.dirname(self.command))
        logger.debug("Executing '%s' from '%s'" % (self.name, os.getcwd()))
        return Component.execute(self, log_out, log_err)

    def setup(self):
        """! Setup event generator."""
        EventGenerator.setup(self)

        if self.madgraph_dir is None:
            self.madgraph_dir = self.get_install_dir()
            logger.debug("Using Madgraph from install dir: " + self.madgraph_dir)

        if self.name == 'ap' and self.apmass is None:
            raise Exception("Missing apmass param for AP generation.")


class MG4(MG):
    """! Run the MadGraph 4 event generator."""

    ## \todo Put this information into a method in the MG superclass.
    dir_map = {"BH": "BH/MG_mini_BH/apBH",
               "RAD": "RAD/MG_mini_Rad/apRad",
               "TM": "TM/MG_mini/ap",
               "ap": "ap/MG_mini/ap",
               "trigg": "trigg/MG_mini_Trigg/apTri",
               "tritrig": "tritrig/MG_mini_Tri_W/apTri",
               "wab": "wab/MG_mini_WAB/AP_6W_XSec2_HallB"}

    def __init__(self, name='ap', **kwargs):

        MG.__init__(self, name, **kwargs)

        if self.name not in MG4.dir_map:
            raise Exception("The name '%s' is not valid for MG4." % self.name)

    def get_install_dir(self):
        """! Get installation directory of MadGraph4."""
        return EventGenerator.get_install_dir(self) + "/madgraph4/src"

    def setup(self):
        """! Setup MadGraph4 generator."""
        MG.setup(self)

        proc_dirs = MG4.dir_map[self.name].split(os.sep)
        src = os.path.join(self.madgraph_dir, proc_dirs[0], proc_dirs[1])
        dest = proc_dirs[1]
        logger.debug("Copying '%s' to '%s'" % (src, dest))
        shutil.copytree(src, dest, symlinks=True)

        self.event_dir = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Events")
        if not os.path.isdir(self.event_dir):
            os.makedirs(self.event_dir)

        self.command = os.path.join(os.getcwd(), proc_dirs[1], proc_dirs[2], "bin", "generate_events")
        logger.debug("Command set to '%s'" % self.command)

        run_card_src = os.path.join(self.madgraph_dir, proc_dirs[0], self.run_card)
        run_card_dest = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Cards", "run_card.dat")
        logger.debug("Copying run card from '%s' to '%s'" % (run_card_src, run_card_dest))
        shutil.copyfile(run_card_src, run_card_dest)

        self.make_run_card(run_card_dest)

        param_card_src = os.path.join(self.madgraph_dir, proc_dirs[0], self.param_card)
        param_card_dest = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Cards", "param_card.dat")
        logger.debug("Copying param card from '%s' to '%s'" % (param_card_src, param_card_dest))
        shutil.copyfile(param_card_src, param_card_dest)

        self.make_param_card(param_card_dest)

    def execute(self, log_out, log_err):
        """! Execute MadGraph4 generator.
        @param log_out  name of log file for output
        @param log_err  name of log file for error
        @return  error code
        """
        returncode = MG.execute(self, log_out, log_err)
        lhe_files = glob.glob(os.path.join(self.event_dir, "*.lhe.gz"))
        for f in lhe_files:
            dest = os.path.join(self.rundir, os.path.basename(f))
            logger.debug("Copying '%s' to '%s'" % (f, dest))
            shutil.copy(f, dest)
        os.chdir(self.rundir)
        return returncode


class MG5(MG):
    """! Run the MadGraph 5 event generator."""

    ## \todo: Put this information into a method in the MG superclass.
    dir_map = {"BH": "BH",
               "RAD": "RAD",
               "tritrig": "tritrig",
               "simp": "simp"}

    def __init__(self, name='tritrig', **kwargs):

        MG.__init__(self, name, **kwargs)

        if self.name not in MG5.dir_map:
            raise Exception("The name '%s' is not valid for MG5." % self.name)

    def get_install_dir(self):
        """! Get installation directory of MadGraph5."""
        return EventGenerator.get_install_dir(self) + "/madgraph5/src"

    def setup(self):
        """! Setup MadGraph5 generator."""
        MG.setup(self)

        self.proc_dir = MG5.dir_map[self.name]
        self.event_dir = os.path.join(self.rundir, self.proc_dir, "Events", self.proc_dir)

        src = os.path.join(self.madgraph_dir, self.proc_dir)
        dest = os.path.join(self.rundir, self.proc_dir)
        logger.debug("Copying '%s' to '%s'" % (src, dest))
        shutil.copytree(src, dest, symlinks=True)

        ## \todo FIXME: This doesn't seem to work as generate_events doesn't read the input config.
        """
        input_dir = os.path.join(self.madgraph_dir, "input")
        dest_input_dir = os.path.join(self.rundir, self.proc_dir, "input")
        logger.info("Copying '%s' to '%s'" % (input_dir, dest_input_dir))
        shutil.copytree(input_dir, dest_input_dir)
        """

        self.command = os.path.join(dest, "bin", "generate_events")
        logger.debug("Command set to '%s'" % self.command)

        run_card_src = os.path.join(src, "Cards", self.run_card)
        run_card_dest = os.path.join(dest, "Cards", "run_card.dat")
        logger.debug("Copying run card from '%s' to '%s'" % (run_card_src, run_card_dest))
        shutil.copyfile(run_card_src, run_card_dest)

        self.make_run_card(run_card_dest)

        param_card_src = os.path.join(src, "Cards", self.param_card)
        param_card_dest = os.path.join(dest, "Cards", "param_card.dat")
        logger.debug("Copying param card from '%s' to '%s'" % (param_card_src, param_card_dest))
        shutil.copyfile(param_card_src, param_card_dest)

        self.make_param_card(param_card_dest)

    def execute(self, log_out, log_err):
        """! Execute MadGraph5 generator.
        @param log_out  name of log file for output
        @param log_err  name of log file for error
        @return  error code
        """
        returncode = MG.execute(self, log_out, log_err)
        lhe_files = glob.glob(os.path.join(self.event_dir, "*.lhe.gz"))
        for f in lhe_files:
            dest = os.path.join(self.rundir, '%s_%s' % (self.name, os.path.basename(f)))
            logger.debug("Copying '%s' to '%s'" % (f, dest))
            shutil.copy(f, dest)
        os.chdir(self.rundir)
        return returncode
