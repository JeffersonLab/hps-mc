import os, subprocess, shutil, random, glob, gzip

from hpsmc.base import Component

class EventGenerator(Component):

    def __init__(self, **kwargs):
        Component.__init__(self, **kwargs)
    
class EGS5(EventGenerator):

    def __init__(self, **kwargs): 
        EventGenerator.__init__(self, **kwargs)
        self.egs5_data_dir = os.environ["EGS5_DATA_DIR"]
        self.egs5_config_dir = os.environ["EGS5_CONFIG_DIR"]
        if "bunches" not in kwargs:
            self.bunches = 5e5
        else:
            self.bunches = kwargs["bunches"]                
        if "run_params" in kwargs:
            self.run_params = kwargs["run_params"]
        else:
            raise Exception("Missing required run_params for EGS5.")
        self.command = "egs5_" + self.name
        
    def setup(self):
        EventGenerator.setup(self)
       
        if self.run_params is None:
            raise Exception("The run_params were never set for EGS5.")
        
        if not len(self.outputs):
            raise Exception("The outputs were never set for EGS5.")
 
        if os.path.exists("data"):
            os.unlink("data")
        os.symlink(self.egs5_data_dir, "data")
        
        if os.path.exists("pgs5job.pegs5inp"):
            os.unlink("pgs5job.pegs5inp")
        os.symlink(self.egs5_config_dir + "/src/esa.inp",  "pgs5job.pegs5inp")
                
        target_z = self.run_params.get("target_z") 
        ebeam = self.run_params.get("beam_energy")
        electrons = self.run_params.get("num_electrons") * self.bunches
                
        seed_data = "%d %f %f %d" % (self.seed, target_z, ebeam, electrons)
        seed_file = open("seed.dat", "w")
        seed_file.write(seed_data)
        seed_file.close()
    
    def execute(self):
        EventGenerator.execute(self)
        stdhep_files = glob.glob(os.path.join(self.rundir, "*.stdhep"))
        if len(stdhep_files) > 1:
            raise Exception("Multiple stdhep files present after running EGS5.")
        stdhep_output_path = os.path.join(self.rundir, self.outputs[0])
        print "EGS5: moving '%s' to '%s'" % (stdhep_files[0], stdhep_output_path)
        shutil.move(stdhep_files[0], stdhep_output_path)

class StdHepConverter(EGS5):

    def __init__(self, **kwargs):
        self.name = "lhe_v1"
        EGS5.__init__(self, **kwargs)

    def setup(self):
        EGS5.setup(self)
        if not len(self.inputs):
            raise Exception("Missing required input LHE file.")                    

    def execute(self):
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
            raise Exception("Input file '%s' has an unknown extension." % self.inputs[0])
        return EGS5.execute(self)

class MG4(EventGenerator):

    dir_map = {"BH"      : "BH/MG_mini_BH/apBH",
               "RAD"     : "RAD/MG_mini_Rad/apRad",
               "TM"      : "TM/MG_mini/ap",
               "ap"      : "ap/MG_mini/ap",
               "trigg"   : "trigg/MG_mini_Trigg/apTri",
               "tritrig" : "tritrig/MG_mini_Tri_W/apTri",
               "wab"     : "wab/MG_mini_WAB/AP_6W_XSec2_HallB" }
        
    def __init__(self, **kwargs):
        EventGenerator.__init__(self, **kwargs)
        if self.name not in MG4.dir_map:
            raise Exception("The name '" + self.name + " is not valid for MG4.")
        self.mg4_dir = os.environ["MG4_DIR"]
        if not len(self.outputs):
            self.outputs.append(self.name + "_events")
        if "run_card" in kwargs:
            self.run_card = kwargs["run_card"]
        else:
            raise Exception("Missing required run_card argument to MG4.")
        if "params" in kwargs:
            self.params = kwargs["params"]
        else:
            self.params = {}

    @staticmethod
    def set_run_card_params(run_card, nevents, seed):
        
        print "MG4: setting run card params on '%s' with nevents '%d' and rand seed '%d'" % (run_card, nevents, seed)
            
        with open(run_card, 'r') as file:
            data = file.readlines()
        
        for i in range(0, len(data)):
            if "= nevents" in data[i]:
                data[i] = " " + str(nevents) + " = nevents ! Number of unweighted events requested" + '\n'
            if "= iseed" in data[i]:
                data[i] = " " + str(seed) + " = iseed   ! rnd seed (0=assigned automatically=default))" + '\n'
                
        with open(run_card, 'w') as file:
            file.writelines(data)
                
    @staticmethod
    def set_params(param_card, params):
        with open(param_card, 'r') as file:
            data = file.readlines()

        for i in range(0, len(data)):                        
            if "APMASS" in params and "APMASS" in data[i]:
                data[i] = "       622     %.7fe-03   # APMASS" % (params["APMASS"]) + '\n'
                                
        with open(param_card, 'w') as file:
            file.writelines(data)
          
    def setup(self):
        
        EventGenerator.setup(self)
        
        if not len(self.outputs):
            raise Exception("The ouputs were not set for MG4.")
    
        self.args = ["0", self.outputs[0]]

        proc_dirs = MG4.dir_map[self.name].split(os.sep)
        src = os.path.join(self.mg4_dir, proc_dirs[0], proc_dirs[1])
        dest = proc_dirs[1]
        print "MG4: copying '%s' to '%s'" % (src, dest)
        shutil.copytree(src, dest, symlinks=True)
        
        self.event_dir = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Events")
        if not os.path.isdir(self.event_dir): 
            os.makedirs(self.event_dir)

        self.command = os.path.join(os.getcwd(), proc_dirs[1], proc_dirs[2], "bin", "generate_events")
        print "MG4: command set to '%s'"  % self.command

        run_card_src = os.path.join(self.mg4_dir, proc_dirs[0], self.run_card)
        run_card_dest = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Cards", "run_card.dat")
        print "MG4: copying run card from '%s' to '%s'" % (run_card_src, run_card_dest)
        shutil.copyfile(run_card_src, run_card_dest)
        
        MG4.set_run_card_params(run_card_dest, self.nevents, self.seed)
        
        param_card_src = os.path.join(self.mg4_dir, proc_dirs[0], "param_card.dat")
        param_card_dest = os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Cards", "param_card.dat")
        print "MG4: copying params from '%s' to '%s'" % (param_card_src, param_card_dest)
        shutil.copyfile(param_card_src, param_card_dest)
        if len(self.params):
            print "MG4: setting params %s on '%s'" % (repr(self.params), param_card_dest)
            MG4.set_params(param_card_dest, self.params)
        else:
            print "MG4: no user params set on param card"
                
    def execute(self):
        os.chdir(os.path.dirname(self.command))
        print "MG4: executing '%s' from '%s'" % (self.name, os.getcwd())
        Component.execute(self)
        lhe_files = glob.glob(os.path.join(self.event_dir, "*.lhe.gz"))
        for f in lhe_files:
            print "MG4: copying '%s' to '%s'" % (f, self.rundir)
            shutil.copy(f, self.rundir)
        os.chdir(self.rundir)
                
class MG5(EventGenerator):
    
    dir_map = {"BH"      : "BH",
               "RAD"     : "RAD",
               "tritrig" : "tritrig"}

    def __init__(self, **kwargs):
        EventGenerator.__init__(self, **kwargs)
        if self.name not in MG5.dir_map:
            raise Exception("The name '" + self.name + " is not valid for MG4.")
        self.mg5_dir = os.environ["MG5_DIR"]            
        if "run_card" in kwargs:
            self.run_card = kwargs["run_card"]
        else:
            raise Exception("Missing required run_card argument to MG4.")
        
    def setup(self):

        if not len(self.outputs):
            raise Exception("The ouputs were not set for MG4.")
        
        EventGenerator.setup(self)
        
        self.args = ["0", self.name]
        
        self.proc_dir = MG5.dir_map[self.name]
        src = os.path.join(os.environ["MG5_DIR"], self.proc_dir)
        dest = os.path.join(self.rundir, self.proc_dir)        
        shutil.copytree(src, dest, symlinks=True)
        
        self.command = os.path.join(dest, "bin", "generate_events")
        print "MG5: command set to '%s'" % self.command
        
        run_card_src = os.path.join(src, "Cards", self.run_card)
        run_card_dest = os.path.join(dest, "Cards", "run_card.dat")
        
        print "MG5: copying run card from '%s' to '%s'" % (run_card_src, run_card_dest)
        
        shutil.copyfile(run_card_src, run_card_dest)
        
        MG4.set_run_card_params(run_card_dest, self.nevents, self.seed)
        
    def execute(self):
        os.chdir(os.path.dirname(self.command))
        print "MG5: executing '%s' from '%s'" % (self.name, os.getcwd())
        Component.execute(self)
        
        lhe_files = glob.glob(os.path.join(self.rundir, self.proc_dir, "Events", self.name, "*.lhe.gz"))
        for f in lhe_files:            
            print "MG5: copying '%s' to '%s'" % (f, self.rundir)
            shutil.copyfile(f, os.path.join(self.rundir, self.name + "_" + os.path.basename(f)))
        
        os.chdir(self.rundir)
        
