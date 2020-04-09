import os, subprocess, shutil, random, glob, gzip, logging

from component import Component

from run_params import RunParameters

logger = logging.getLogger("hpsmc.generators")

class EventGenerator(Component):

    def __init__(self, name, command=None, **kwargs):
        Component.__init__(self, name, command=command, **kwargs)
        
    def required_parameters(self):
        return ['nevents']
                
class EGS5(EventGenerator):

    def __init__(self, name, **kwargs):
        self.bunches = 5e5
        EventGenerator.__init__(self, name, "egs5_" + name, **kwargs)
                
    def setup(self):
        EventGenerator.setup(self)
              
        self.egs5_data_dir = os.path.join(self.egs5_dir, "data")
        self.egs5_config_dir = os.path.join(self.egs5_dir, "config")
       
        logger.info("egs5_data_dir=%s" % self.egs5_data_dir)
        logger.info("egs5_config_dir=%s" % self.egs5_config_dir)
                           
        if os.path.exists("data"):
            os.unlink("data")
        os.symlink(self.egs5_data_dir, "data")
        
        if os.path.exists("pgs5job.pegs5inp"):
            os.unlink("pgs5job.pegs5inp")
        os.symlink(self.egs5_config_dir + "/src/esa.inp",  "pgs5job.pegs5inp")
        
        self.run_param_data = RunParameters(self.run_params)        
        target_z = self.run_param_data.get("target_z") 
        ebeam = self.run_param_data.get("beam_energy")
        electrons = self.run_param_data.get("num_electrons") * self.bunches
        
        logger.info("Read target_z=%d, ebeam=%d, electrons=%d from '%s' run params"
                    % (target_z, ebeam, electrons, self.run_params))
                
        seed_data = "%d %f %f %d" % (self.seed, target_z, ebeam, electrons)
        seed_file = open("seed.dat", 'w')
        seed_file.write(seed_data)
        seed_file.close()
        
    def output_files(self):
        return ['beam.stdhep']
    
    def execute(self, log_out, log_err):
        EventGenerator.execute(self, log_out, log_err)
        src = os.path.join(self.rundir, 'brems.stdhep')
        dest = os.path.join(self.rundir, self.output_files()[0])
        logger.info("Copying '%s' to '%s'" % (src, dest))
        shutil.copy(src, dest)
                                   
    def required_parameters(self):
        return ['run_params']
    
    def optional_parameters(self):
        return ['bunches']
    
    def required_config(self):
        return ['egs5_dir']

class StdHepConverter(EGS5):

    def __init__(self, **kwargs):
        EGS5.__init__(self, "lhe_v1", **kwargs)
  
    def config(self):
        EGS5.config(self)

    def setup(self):
        EGS5.setup(self)
        if not len(self.inputs):
            raise Exception("Missing required input LHE file.")                    

    def execute(self, log_out, log_err):
        input_file = self.inputs[0]
        base,ext = os.path.splitext(input_file)
        if ext == ".lhe":
            os.symlink(input_file, "egs5job.inp")
        elif ext == ".gz":
            infile = open("egs5job.inp", 'w')
            f = gzip.open(self.inputs[0], 'r')
            for line in f:
                infile.write(line)
            f.close()
            infile.close()
        else:
            raise Exception("Input file '%s' has an unknown extension." % input_file)
        return EGS5.execute(self, log_out, log_err)
    
    def output_files(self):
        return [self.input_files()[0].replace(".lhe.gz", ".stdhep").replace(".lhe", ".stdhep")]
    
class MG(EventGenerator):
    """
    Abstract class for MadGraph generators.
    """    
    def __init__(self, name, **kwargs):
        
        # Default name of param card
        self.param_card = "param_card.dat"
        
        # Extra parameters for param card:
        # map = mass of the A-prime
        # mpid = mass of the dark pion
        # mrhod = mass of the dark rho
        self.apmass = None
        self.map = None
        self.mpid = None
        self.mrhod = None
        
        EventGenerator.__init__(self, name, **kwargs)
    
    def output_files(self):
        return [self.name + "_unweighted_events.lhe.gz",
                self.name + "_events.lhe.gz"]
    
    def set_parameters(self, params):
        Component.set_parameters(self, params)
        self.run_card = "run_card_" + self.run_params + ".dat"
        logger.info("Set run card to '%s'" % self.run_card)     
        
    def required_parameters(self):
        return ['nevents', 'run_params']
    
    def optional_parameters(self):
        return ['seed', 'param_card', 'apmass']
    
    def required_config(self):
        return ['madgraph_dir']
        
    def make_run_card(self, run_card):
        
        logger.info("Making run card '%s' with nevents=%d, seed=%d" % (run_card, self.nevents, self.seed))
            
        with open(run_card, 'r') as file:
            data = file.readlines()
        
        for i in range(0, len(data)):
            if "= nevents" in data[i]:
                logger.info("Setting nevents=%d in run card" % self.nevents)
                data[i] = " " + str(self.nevents) + " = nevents ! Number of unweighted events requested" + '\n'
            if "= iseed" in data[i]:
                logger.info("Setting seed=%d in run card" % self.seed)
                data[i] = " " + str(self.seed) + " = iseed   ! rnd seed (0=assigned automatically=default))" + '\n'
                
        with open(run_card, 'w') as file:
            file.writelines(data)
                
    def make_param_card(self, param_card):
                        
        logger.info("Making param card '%s'" % param_card)
                        
        with open(param_card, 'r') as file:
            data = file.readlines()

        for i in range(0, len(data)):
            if "APMASS" in data[i] and self.apmass is not None:
                data[i] = "       622     %.7fe-03   # APMASS" % (self.apmass) + '\n'
                logger.info("APMASS in param card set to %d" % self.apmass)
            if "map" in data[i] and self.map is not None:
                data[i] = "      622 %.7fe-03 # map" % (params["map"]) + '\n'
            if "mpid" in data[i] and self.mpid is not None:
                data[i] = "      624 %.7fe-03 # mpid" % (params["mpid"]) + '\n'
            if "mrhod" in data[i] and self.mrhod is not None:
                data[i] = "      625 %.7fe-03 # mrhod" % (params["mrhod"]) + '\n'
                                
        with open(param_card, 'w') as file:
            file.writelines(data)
            
    def cmd_args(self):
        return ["0", self.name]
            
    def execute(self, log_out, log_err):
        os.chdir(os.path.dirname(self.command))
        logger.info("Executing '%s' from '%s'" % (self.name, os.getcwd()))
        return Component.execute(self, log_out, log_err)
                
    def setup(self):
        
        EventGenerator.setup(self)
        
        if self.name is 'ap' and self.apmass is None:
            raise Exception("Missing apmass param for AP generation.")
            
class MG4(MG):

    # TODO: Put this information into a method in the MG superclass.
    dir_map = {"BH"      : "BH/MG_mini_BH/apBH",
               "RAD"     : "RAD/MG_mini_Rad/apRad",
               "TM"      : "TM/MG_mini/ap",
               "ap"      : "ap/MG_mini/ap",
               "trigg"   : "trigg/MG_mini_Trigg/apTri",
               "tritrig" : "tritrig/MG_mini_Tri_W/apTri",
               "wab"     : "wab/MG_mini_WAB/AP_6W_XSec2_HallB" }
        
    def __init__(self, name, **kwargs):
        
        MG.__init__(self, name, **kwargs)
        
        if self.name not in MG4.dir_map:
            raise Exception("The name '%s' is not valid for MG4." % self.name)
                                                                   
    def setup(self):
        
        MG.setup(self)
        
        proc_dirs = MG4.dir_map[self.name].split(os.sep)
        src = os.path.join(self.madgraph_dir, proc_dirs[0], proc_dirs[1])
        dest = proc_dirs[1]
        logger.info("Copying '%s' to '%s'" % (src, dest))
        shutil.copytree(src, dest, symlinks=True)
        
        self.event_dir = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Events")
        if not os.path.isdir(self.event_dir): 
            os.makedirs(self.event_dir)

        self.command = os.path.join(os.getcwd(), proc_dirs[1], proc_dirs[2], "bin", "generate_events")
        logger.info("Command set to '%s'"  % self.command)

        run_card_src = os.path.join(self.madgraph_dir, proc_dirs[0], self.run_card)
        run_card_dest = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Cards", "run_card.dat")
        logger.info("Copying run card from '%s' to '%s'" % (run_card_src, run_card_dest))
        shutil.copyfile(run_card_src, run_card_dest)
        
        self.make_run_card(run_card_dest)
        
        param_card_src = os.path.join(self.madgraph_dir, proc_dirs[0], self.param_card)
        param_card_dest = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Cards", "param_card.dat")
        logger.info("Copying param card from '%s' to '%s'" % (param_card_src, param_card_dest))
        shutil.copyfile(param_card_src, param_card_dest)
                
        self.make_param_card(param_card_dest)
                
    def execute(self, log_out, log_err):
        returncode = MG.execute(self, log_out, log_err)
        lhe_files = glob.glob(os.path.join(self.event_dir, "*.lhe.gz"))
        for f in lhe_files:
            dest = os.path.join(self.rundir, os.path.basename(f))
            logger.info("Copying '%s' to '%s'" % (f, dest))
            shutil.copy(f, dest)
        os.chdir(self.rundir) 
        return returncode
                                      
class MG5(MG):
    
    # TODO: Put this information into a method in the MG superclass.
    dir_map = {"BH"      : "BH",
               "RAD"     : "RAD",
               "tritrig" : "tritrig",
               "simp"    : "simp"}

    def __init__(self, name, **kwargs):
                
        MG.__init__(self, name, **kwargs)
        
        if self.name not in MG5.dir_map:
            raise Exception("The name '%s' is not valid for MG5." % self.name)

    def setup(self):

        MG.setup(self)
                                  
        self.proc_dir = MG5.dir_map[self.name]
        self.event_dir = os.path.join(self.rundir, self.proc_dir, "Events", self.proc_dir)
               
        src = os.path.join(self.madgraph_dir, self.proc_dir)
        dest = os.path.join(self.rundir, self.proc_dir)
        logger.info("Copying '%s' to '%s'" % (src, dest))
        shutil.copytree(src, dest, symlinks=True)

        # FIXME: This doesn't seem to work as generate_events doesn't read the input config.
        """
        input_dir = os.path.join(self.madgraph_dir, "input")
        dest_input_dir = os.path.join(self.rundir, self.proc_dir, "input")
        logger.info("Copying '%s' to '%s'" % (input_dir, dest_input_dir))
        shutil.copytree(input_dir, dest_input_dir)
        """
        
        self.command = os.path.join(dest, "bin", "generate_events")
        logger.info("Command set to '%s'" % self.command)
        
        run_card_src = os.path.join(src, "Cards", self.run_card)
        run_card_dest = os.path.join(dest, "Cards", "run_card.dat")        
        logger.info("Copying run card from '%s' to '%s'" % (run_card_src, run_card_dest))        
        shutil.copyfile(run_card_src, run_card_dest)
        
        self.make_run_card(run_card_dest)

        param_card_src = os.path.join(src, "Cards", self.param_card)
        param_card_dest = os.path.join(dest, "Cards", "param_card.dat")
        logger.info("Copying param card from '%s' to '%s'" % (param_card_src, param_card_dest))
        shutil.copyfile(param_card_src, param_card_dest)

        self.make_param_card(param_card_dest)

    def execute(self, log_out, log_err):
        returncode = MG.execute(self, log_out, log_err)
        lhe_files = glob.glob(os.path.join(self.event_dir, "*.lhe.gz"))
        for f in lhe_files:
            dest = os.path.join(self.rundir, '%s_%s' % (self.name, os.path.basename(f)))
            logger.info("Copying '%s' to '%s'" % (f, dest))
            shutil.copy(f, dest)
        os.chdir(self.rundir) 
        return returncode
            