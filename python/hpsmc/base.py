import os, subprocess, sys, shutil, argparse, getpass

class Component:

    def __init__(self, **kwargs):
        if "name" in kwargs:
            self.name = kwargs["name"]
        elif self.name is None:
            raise Exception("The name of a Component is required.")
        if "args" in kwargs:
            self.args = kwargs["args"]
        else:
            self.args = []
        if "outputs" in kwargs:
            self.outputs = kwargs["outputs"]
        else:
            self.outputs = []
        if "inputs" in kwargs:
            self.inputs = kwargs["inputs"]
        else:
            self.inputs = []
        if "nevents" in kwargs:
            self.nevents = kwargs["nevents"]
        else:
            self.nevents = -1
        if "description" in kwargs:
            self.description = kwargs["description"]
        else:
            self.description = ""
        if "rand_seed" in kwargs:
            self.rand_seed = kwargs["rand_seed"]
        else:
            self.rand_seed = 1

    def execute(self):
        
        cl = [self.command]
        cl.extend(self.cmd_args())
                                            
        proc = subprocess.Popen(cl, shell=False)
        proc.communicate()
                            
        return proc.returncode

    def cmd_exists(self):
        return subprocess.call("type " + self.command, shell=True, 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

    def cmd_args(self):
        return self.args
    
    def setup(self):
        pass

    def cleanup(self):
        pass

class Job:

    def __init__(self, **kwargs):
        if "name" in kwargs:
            self.name = kwargs["name"] 
        else:
            self.name = "HPS MC Job"
        self.delete_rundir = False
        if "rundir" in kwargs:
            self.rundir = kwargs["rundir"]
        else:
            if "LSB_JOBID" in os.environ:
                self.rundir = os.path.join("/scratch", getpass.getuser(), os.environ["LSB_JOBID"])
                self.delete_rundir = True
            else:
                self.rundir = os.getcwd()
        if "components" in kwargs:
            self.components = kwargs["components"]
        else:
            self.components = []
        if "job_num" in kwargs:
            self.job_num = kwargs["job_num"]
        else:
            self.job_num = 1
        if "output_files" in kwargs:
            self.output_files = kwargs["output_files"]
        else:
            self.output_files = {}
        if "output_dir" in kwargs:
            self.output_dir = kwargs["output_dir"]
            if self.output_dir:
                if not os.path.isabs(self.output_dir):
                    raise Exception("The output_dir should be absolute.")
        else:
            self.output_dir = None
        if "append_job_num" in kwargs:
            self.append_job_num = kwargs["append_job_num"]
        else:
            self.append_job_num = False
        if "job_num_pad" in kwargs:
            self.job_num_pad = kwargs["job_num_pad"]
        else:
            self.job_num_pad = 4
        if "ignore_return_codes" in kwargs:
            self.ignore_return_codes = kwargs["ignore_return_codes"]
        else:
            self.ignore_return_codes = False
        if "delete_existing" in kwargs:
            self.delete_existing = kwargs["delete_existing"]
        else:
            self.delete_existing = False

    def run(self):
        print "Job: running '%s'" % self.name
        if not len(self.components):
            raise Exception("Job has no components to run.")
        for i in range(0, len(self.components)):
            c = self.components[i]
            print "Job: executing '%s' with description '%s'" % (str(c.name), str(c.description))
            print "Job: command '%s' with inputs %s and outputs %s" % (c.command, str(c.inputs), str(c.outputs))
            retcode = c.execute()
            if not self.ignore_return_codes and retcode:
                raise Exception("Job: error code '%d' returned by '%s'" % (retcode, str(c.name)))

    def setup(self):
        if not os.path.exists(self.rundir):
            os.makedirs(self.rundir)
        os.chdir(self.rundir)
        for c in self.components:
            print "Job: setting up '%s'" % (c.name)
            c.rundir = self.rundir
            c.setup()
            if not c.cmd_exists():
                raise Exception("Command '%s' does not exist for '%s'." % (c.command, c.name))

    def cleanup(self):
        for c in self.components:
            print "Job: running cleanup for '%s'" % str(c.name)
            c.cleanup()
        if self.delete_rundir:
            print "Job: deleting run dir '%s'" % self.rundir
            shutil.rmtree(self.rundir)
    
    def copy_output_files(self):
        
        if self.output_dir:
        
            if not os.path.exists(self.output_dir):
                print "Job: creating output dir '%s'" % self.output_dir
                os.makedirs(self.output_dir, 0755)
               
            for output_file in self.output_files:
                if isinstance(output_file, basestring):
                    src_file = os.path.join(self.rundir, output_file)
                    dest_file = os.path.join(self.output_dir, output_file)
                elif isinstance(output_file, dict):
                    src, dest = output_file.iteritems().next()
                    src_file = os.path.join(self.rundir, src)
                    if not os.path.isabs(dest):
                        dest_file = os.path.join(self.output_dir, dest)
                    else:
                        dest_file = dest
                if self.append_job_num:
                    base,ext = os.path.splitext(dest_file)
                    dest_file = base + "_" + (("%0" + str(self.job_num_pad) + "d") % self.job_num) + ext
                if os.path.isfile(dest_file):
                    if self.delete_existing:
                        print "Job: deleting existing file at '%s'" % dest_file
                        os.remove(dest_file)
                    else:
                        raise Exception("Output file '%s' already exists." % dest_file)
                
                print "Job: copying '%s' to '%s'" % (src_file, dest_file)
                shutil.copyfile(src_file, dest_file)      
        else:
            
            print "Job: No output_dir was set so files will not be copied."
            
            
class JobStandardArgs:
    
    def __init__(self, job_name):
        self.job_name = job_name

    def create_parser(self):
        parser = argparse.ArgumentParser(description="Run a " + self.job_name + " job")
        parser.add_argument("-p", "--params", help="Run parameter key e.g. '1pt05'", required=False)
        parser.add_argument("-d", "--detector", help="Detector name", required=True)
        parser.add_argument("-n", "--nevents", help="Number of events", required=True)
        parser.add_argument("-j", "--job",  help="Job number", required=False)
        parser.add_argument("-s", "--seed", help="Random seed for all components", required=False)
        parser.add_argument("-f", "--filename", help="Base file name (do not include an extension!)", required=False)
        parser.add_argument("-o", "--output-dir", help="Job output dir", required=False)
        parser.add_argument("-r", "--run", help="Run number for conditions system", required=True)
        parser.add_argument("-R", "--recon-steering", help="Recon lcsim steering resource", required=False)
        parser.add_argument("-O", "--readout-steering", help="Readout lcsim steering resource", required=False)
        return parser
        
    def parse_args(self):
        
        parser = self.create_parser()
        cl = parser.parse_args()

        if cl.job:
            self.job_num = int(cl.job)
        else:
            self.job_num = 1
    
        self.nevents = int(cl.nevents)

        if cl.seed:
            self.seed = int(cl.seed)
        else:
            self.seed = self.job_num
    
        if cl.filename:
            self.filename = cl.filename
        else:
            self.filename = self.job_name + "_events"
    
        self.run_param_key = cl.params

        if cl.output_dir:
            self.output_dir = cl.output_dir
        else:
            output_dir = None
            
        if cl.recon_steering:
            self.recon_steering = cl.recon_steering
        else:
            self.recon_steering = None
    
        if cl.readout_steering:
            self.readout_steering = cl.readout_steering
        else:
            self.readout_steering = None
    
        self.cond_run = int(cl.run)
        self.cond_detector = cl.detector
        
    def print_args(self):    
        print "---- Job Args ----"
        print "job_num = %d" % self.job_num
        print "nevents = %d" % self.nevents
        print "seed = %d" % self.seed
        print "filename = %s" % self.filename
        print "run_param_key = %s" % self.run_param_key
        print "output_dir = %s" % self.output_dir
        print "cond_run = %d" % self.cond_run
        print "cond_detector = %s" % self.cond_detector
        print "readout_steering = %s" % self.readout_steering
        print "recon_steering = %s" % self.recon_steering
        print
                    
