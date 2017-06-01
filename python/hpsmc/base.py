import os, subprocess, sys, shutil, argparse, getpass, json

class Component:

    def __init__(self, **kwargs):
        
        if "name" in kwargs:
            self.name = kwargs["name"]
        elif self.name is None:
            raise Exception("The name of a Component is required.")
        
        if "args" in kwargs:
            if not isinstance(kwargs["args"], list):
                raise Exception("The args are not a list.")
            self.args = kwargs["args"]
        else:
            self.args = []
            
        if "outputs" in kwargs:
            if not isinstance(kwargs["outputs"], list):
                raise Exception("The outputs arg is not a list.")
            self.outputs = kwargs["outputs"]
        else:
            self.outputs = []
            
        if "inputs" in kwargs:
            if not isinstance(kwargs["inputs"], list):
                raise Exception("The inputs arg is not a list.")
            self.inputs = kwargs["inputs"]
        else:
            self.inputs = []
            
        if "nevents" in kwargs:
            self.nevents = kwargs["nevents"]
        else:
            self.nevents = -1
                        
        if "seed" in kwargs:
            self.seed = kwargs["seed"]
        else:
            self.seed = 1
            
        if "ignore_returncode" in kwargs:
            self.ignore_returncode = kwargs["ignore_returncode"]
        else:
            self.ignore_returncode = False

    def execute(self):
        
        cl = [self.command]
        cl.extend(self.cmd_args())
                                  
        print "Component: executing '%s' with command %s" % (self.name, cl)
        proc = subprocess.Popen(cl, shell=False)
        proc.communicate()

        if not self.ignore_returncode and proc.returncode:
            raise Exception("Component: error code '%d' returned by '%s'" % (proc.returncode, c.name))

    def cmd_exists(self):
        return subprocess.call("type " + self.command, shell=True, 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

    def cmd_args(self):
        return self.args
    
    def setup(self):
        pass

    def cleanup(self):
        pass
    
class DummyComponent(Component):
    
    def __init__(self, **kwargs):
        print "DummyComponent: init"  
        Component.__init__(self, **kwargs)
        self.command = "dummy"
        
    def execute(self):
        print "DummyComponent: execute"
        
    def cmd_exists(self):
        return True
    
    def setup(self):
        print "DummyComponent: setup"
        
    def cleanup(self):
        print "DummyComponent: cleanup"
    

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
            
        if "job_num_pad" in kwargs:
            self.job_num_pad = kwargs["job_num_pad"]
        else:
            self.job_num_pad = 4
                        
        if "delete_existing" in kwargs:
            self.delete_existing = kwargs["delete_existing"]
        else:
            self.delete_existing = False
            
        if "set_component_seeds" in kwargs:
            self.set_component_seeds = kwargs["set_component_seeds"]
        else:
            self.set_component_seeds = True
            
        self.params = None
            
    def parse_args(self):
        
        parser = argparse.ArgumentParser(description=self.name)
        parser.add_argument("params", nargs=1, help="Job params in JSON format")
        cl = parser.parse_args()
        
        if len(cl.params):
            print "Job: Loading job params from '%s'" % cl.params[0]
            self.params = JobParameters(cl.params[0])
            print json.dumps(self.params.json_dict, indent=4, sort_keys=False)
        else:
            raise Exception("Missing required JSON file with job params.")
            
        if hasattr(self.params, "input_files"):
            self.input_files = self.params.input_files
        else:
            self.input_files = {}
            
        if hasattr(self.params, "output_files"):
            self.output_files = self.params.output_files
        else:
            self.output_files = {}
            
        if hasattr(self.params, "seed"):
            self.seed = self.params.seed
        else:
            print "Job: random seed is set to default value '%d'" % (self.seed)
            self.seed = 1
            
        if hasattr(self.params, "output_dir"):
            self.output_dir = self.params.output_dir
            if not os.path.isabs(self.output_dir):
                self.output_dir= os.path.abspath(self.output_dir)
                print "Job: changed output dir to abs path '%s'" % self.output_dir
        else:
            self.output_dir = os.getcwd()

        if hasattr(self.params, "job_num"):
            self.job_num = self.params.job_num
        else:
            self.job_num = 1        
            
    def run(self): 
        if not len(self.components):
            raise Exception("Job has no components.")
        if not self.params:
            raise Exception("Job params were never parsed.")
        self.copy_input_files()
        self.setup()
        self.execute()
        self.copy_output_files()
        self.cleanup()
                      
    def execute(self):
        print "Job: running '%s'" % self.name
        if not len(self.components):
            raise Exception("Job has no components to execute.")
        for i in range(0, len(self.components)):
            c = self.components[i]
            print "Job: executing '%s' with inputs %s and outputs %s" % (c.name, str(c.inputs), str(c.outputs))
            c.execute()

    def setup(self):
        if not os.path.exists(self.rundir):
            os.makedirs(self.rundir)
        os.chdir(self.rundir)
        for c in self.components:
            print "Job: setting up '%s'" % (c.name)
            c.rundir = self.rundir
            if self.set_component_seeds:
                print "Job: setting seed on '%s' to '%d'" % (c.name, self.seed)
                c.seed = self.seed
            c.setup()
            if not c.cmd_exists():
                raise Exception("Command '%s' does not exist for '%s'." % (c.command, c.name))

    def cleanup(self):
        for c in self.components:
            print "Job: running cleanup for '%s'" % str(c.name)
            c.cleanup()
        if self.delete_rundir:
            print "Job: deleting execute dir '%s'" % self.rundir
            shutil.rmtree(self.rundir)
    
    def copy_output_files(self):
                
        if not os.path.exists(self.output_dir):
            print "Job: creating output dir '%s'" % self.output_dir
            os.makedirs(self.output_dir, 0755)
               
        for src,dest in self.output_files.iteritems():
            src_file = os.path.join(self.rundir, src)
            dest_file = os.path.join(self.output_dir, dest)
            if os.path.isfile(dest_file):
                if self.delete_existing:
                    print "Job: deleting existing file at '%s'" % dest_file
                    os.remove(dest_file)
                else:
                    raise Exception("Output file '%s' already exists." % dest_file)
            print "Job: copying '%s' to '%s'" % (src_file, dest_file)
            shutil.copyfile(src_file, dest_file)
            
    def copy_input_files(self):
        for dest,src in self.input_files.iteritems():
            if not os.path.isabs(src):
                raise Exception("The input source file '%s' is not an absolute path." % src)
            if os.path.dirname(dest):
                raise Exception("The input file destination '%s' is not valid." % dest)
            print "Job: copying input '%s' to '%s'" % (src, os.path.join(self.rundir, dest))
            shutil.copyfile(src, os.path.join(self.rundir, dest))
            
class JobParameters:
    
    def __init__(self, filename = None):
        if filename:
            self.load(filename)
    
    def load(self, filename):
        rawdata = open(filename, 'r').read()
        self.json_dict = json.loads(rawdata)        

    def __getattr__(self, attr):
        if attr in self.json_dict:
            return self.json_dict[attr]
        else:
            raise AttributeError("%r has no attribute '%s'" %
                                 (self.__class__, attr))
    
    def __str__(self):
        return str(self.json_dict)
     