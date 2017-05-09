import os, subprocess, shutil, random, glob

from hpsmc.base import Component

class EventGenerator(Component):

    def __init__(self, **kwargs):
        Component.__init__(self, **kwargs)
        self.run_params = None

class EGS5(EventGenerator):

    def __init__(self, **kwargs): 
        EventGenerator.__init__(self, **kwargs)
        self.egs5_data_dir = os.environ["EGS5_DATA_DIR"]
        self.egs5_config_dir = os.environ["EGS5_CONFIG_DIR"]
        if "bunches" not in kwargs:
            self.bunches = 5e5
        else:
            self.bunches = kwargs["bunches"]        
        if "rand_seed" in kwargs:
            rand_seed = kwargs["rand_seed"]
        else:
            self.rand_seed = random.randint(1, 1000000)
        if "run_params" in kwargs:
            self.run_params = kwargs["run_params"]
            print "EGS5: set run_params %s" % repr(self.run_params)
        else:
            print "EGS5: run_params not set!!!"
            self.run_params = None
        self.command = "egs5_" + self.name
        
    def setup(self):
        EventGenerator.setup(self)
       
        if self.run_params is None:
            raise Exception("The run_params were never set for EGS5.")
 
        if os.path.exists("data"):
            os.unlink("data")
        os.symlink(self.egs5_data_dir, "data")
        
        if os.path.exists("pgs5job.pegs5inp"):
            os.unlink("pgs5job.pegs5inp")
        os.symlink(self.egs5_config_dir + "/src/esa.inp",  "pgs5job.pegs5inp")
                
        target_z = self.run_params.get("target_z") 
        ebeam = self.run_params.get("beam_energy")
        electrons = self.run_params.get("num_electrons") * self.bunches
                
        seed_data = "%d %f %f %d" % (self.rand_seed, target_z, ebeam, electrons)
        seed_file = open("seed.dat", "w")
        seed_file.write(seed_data)
        seed_file.close()

"""
    def execute(self):
        Component.execute(self)
        stdhep_files = glob.glob(os.path.join(self.rundir, "*.stdhep"))
        if not len(stdhep_files):
            raise Exception("No stdhep files produced by EGS5 execution.")
        if len(stdhep_files) > 1:
            raise Exception("More than one stdhep file present in run dir after EGS5 execution.")
        self.outputs.append(stdhep_files[0])
"""        

class StdHepConverter(EGS5):

    def __init__(self, **kwargs):
        self.name = "lhe_v1"
        EGS5.__init__(self, **kwargs)

    def setup(self):
        EGS5.setup(self)
        if not len(self.inputs):
            raise Exception("Missing required input LHE file.")
        os.symlink(self.inputs[0], "egs5job.inp")

    def execute(self):
        EGS5.execute(self)

class MG4(EventGenerator):

    dir_map = {
        "BH"      : "BH/MG_mini_BH/apBH",
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
        print kwargs
        if "run_card" in kwargs:
            self.run_card = kwargs["run_card"]
        else:
            raise Exception("Missing required run_card argument to MG4.")
          
    def setup(self):
        
        EventGenerator.setup(self)
    
        self.args = ["0", self.outputs[0]]

        proc_dirs = MG4.dir_map[self.name].split(os.sep)
        src = os.path.join(self.mg4_dir, proc_dirs[0], proc_dirs[1])
        dest = proc_dirs[1]
        print "MG4: copying '%s' to '%s'" % (src, dest)
        shutil.copytree(src, dest)

        self.command = os.path.join(os.getcwd(), proc_dirs[1], proc_dirs[2], "bin", "generate_events")
        print "MG4: command set to '%s'"  % self.command

        shutil.copyfile(os.path.join(self.mg4_dir, proc_dirs[0], self.run_card),
            os.path.join(self.rundir, proc_dirs[1], proc_dirs[2], "Cards", "run_card.dat"))
            
    def execute(self):
        os.chdir(os.path.dirname(self.command))
        print "MG4: executing '%s' from '%s'" % (self.name, os.getcwd()) 
        Component.execute(self)
        os.chdir(self.rundir)
