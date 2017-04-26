import subprocess

class EventGenerator:

    def __init__(self, **kwargs):
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
        print "EventGenerator: running " + self.executable
        command = [self.executable]
        command.extend(self.args)
        process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
        process.wait()
        print "return code: " + str(process.returncode)

    def setup(self):
        os.chdir(self.rundir)

    def cleanup(self):
        pass

class EGS5(EventGenerator):

    def __init__(self, executable, **kwargs): 
        self.egs5_data_dir = os.environ["EGS5_DATA_DIR"]
        self.egs5_config_dir = os.environ["EGS5_DATA_DIR"]
        if "bunches" not in kwargs:
            self.bunches = 5e5
        else:
            self.bunches = kwargs["bunches"]
        
    def setup(self):
        EventGenerator.setup(self)
        
        os.symlink(self.egs5_data_dir, "data")
        os.symlink(self.egs5_config_dir + "/src/esa.inp",  "pgs5job.pegs5inp")
                
        target_z = rp.get("target_z") 
        ebeam = rp.get("beam_energy")
        electrons = rp.get("num_electrons") * beam_bunches
                
        seed_data = "%d %d %d %d" % (job_num, target_z, ebeam, electrons)
        seed_file = open("seed.dat", "w")
        seed_file.write(seed_data)
        seed_file.close()
