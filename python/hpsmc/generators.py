import os, subprocess, shutil

class EventGenerator:

    def __init__(self, **kwargs):
        if "executable" in kwargs:
            self.executable = kwargs["executable"]
        if "args" in kwargs:
            self.args = kwargs["args"]
        else:
            self.args = []
        if "rundir" in kwargs:
            self.rundir = kwargs["rundir"]
        else:        
            self.rundir = os.getcwd()
        if "output" in kwargs:
            self.output = kwargs["output"]
        else:
            self.output = None
        self.run_params = None
        self.job_num = 1

    def run(self):
        print "EventGenerator: running '" + self.executable + "' with args " + str(self.args)
        command = [self.executable]
        command.extend(self.args)
        proc = subprocess.Popen(command, shell=False)
        proc.communicate()

    def setup(self):
        os.chdir(self.rundir)

    def cleanup(self):
        pass

class EGS5(EventGenerator):

    def __init__(self, program_name, **kwargs): 
        EventGenerator.__init__(self, **kwargs)
        self.egs5_data_dir = os.environ["EGS5_DATA_DIR"]
        self.egs5_config_dir = os.environ["EGS5_CONFIG_DIR"]
        if "bunches" not in kwargs:
            self.bunches = 5e5
        else:
            self.bunches = kwargs["bunches"]        
        self.executable = os.path.join(os.environ["EGS5_BIN_DIR"], "egs5_" + program_name)
        
    def setup(self):
        EventGenerator.setup(self)
        
        if os.path.exists("data"):
            os.unlink("data")
        os.symlink(self.egs5_data_dir, "data")
        
        if os.path.exists("pgs5job.pegs5inp"):
            os.unlink("pgs5job.pegs5inp")
        os.symlink(self.egs5_config_dir + "/src/esa.inp",  "pgs5job.pegs5inp")
                
        target_z = self.run_params.get("target_z") 
        ebeam = self.run_params.get("beam_energy")
        electrons = self.run_params.get("num_electrons") * self.bunches
                
        seed_data = "%d %d %d %d" % (self.job_num, target_z, ebeam, electrons)
        seed_file = open("seed.dat", "w")
        seed_file.write(seed_data)
        seed_file.close()

class MG4(EventGenerator):

    dir_map = {
        "BH"      : "BH/MG_mini_BH/apBH",
        "RAD"     : "RAD/MG_mini_Rad/apRad",
        "TM"      : "TM/MG_mini/ap",
        "ap"      : "ap/MG_mini/ap",
        "trigg"   : "trigg/MG_mini_Trigg/apTri",
        "tritrig" : "tritrig/MG_mini_Tri_W/apTri",
        "wab"     : "wab/MG_mini_WAB/AP_6W_XSec2_HallB" }
        
    def __init__(self, gen_process, run_card=None, **kwargs):
        EventGenerator.__init__(self, **kwargs)
        if gen_process not in MG4.dir_map:
            raise Exception("The gen process '" + gen_process + " is not valid.")
        self.mg4_dir = os.environ["MG4_DIR"]
        self.gen_process = gen_process
        if self.output is None:
            self.output = gen_process + "_events"
        self.args = ["0", self.output]
        self.run_card = run_card      
          
    def setup(self):
        
        EventGenerator.setup(self)
    
        if not os.path.exists("mg4"):
            print "copying MG4 source tree from '" + self.mg4_dir + "' to work dir"
            shutil.copytree(self.mg4_dir, "mg4", symlinks=True)
        else:
            print "WARNING: Skipping copy of MG4 source tree.  It already exists here!"
        
        self.executable = os.path.join(os.getcwd(), "mg4", MG4.dir_map[self.gen_process], "bin", "generate_events")
        print "MG4 executable is set to '" + self.executable + "'"

        if self.run_card is not None:       
            shutil.copyfile(os.path.join(self.mg4_dir, self.gen_process, self.run_card), 
                os.path.join(self.rundir, "mg4", MG4.dir_map[self.gen_process], "Cards", "run_card.dat")) 

        os.chdir(os.path.dirname(self.executable))
        
