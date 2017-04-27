import os, subprocess

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
        self.run_params = None
        self.job_num = 1

    def run(self):
        print "EventGenerator: running '" + self.executable + "' with args " + str(self.args)
        command = [self.executable]
        command.extend(self.args)
        #process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
        #process.wait()
        process = subprocess.Popen(command, shell=False)
        proc.communicate()
        #print "return code: " + str(process.returncode)

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
        self.executable = os.environ["EGS5_BIN_DIR"] + "/egs5_" + program_name
        
    def setup(self):
        EventGenerator.setup(self)
        
        try:
            os.symlink(self.egs5_data_dir, "data")
        except:
            pass
        
        try:
            os.symlink(self.egs5_config_dir + "/src/esa.inp",  "pgs5job.pegs5inp")
        except:
            pass
                
        target_z = self.run_params.get("target_z") 
        ebeam = self.run_params.get("beam_energy")
        electrons = self.run_params.get("num_electrons") * self.bunches
                
        seed_data = "%d %d %d %d" % (self.job_num, target_z, ebeam, electrons)
        seed_file = open("seed.dat", "w")
        seed_file.write(seed_data)
        seed_file.close()
