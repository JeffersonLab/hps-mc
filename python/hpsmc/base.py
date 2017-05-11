import os, subprocess, sys, shutil

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
        if "rundir" in kwargs:
            self.rundir = kwargs["rundir"]
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
            if not os.path.isabs(self.output_dir):
                raise Exception("The output_dir should be absolute.")
        else:
            self.output_dir = None
        if "append_job_num" in kwargs:
            self.append_job_num = kwargs["append_job_num"]
        else:
            self.append_job_num = False

    def run(self):
        print "Job: running '%s'" % self.name
        if not len(self.components):
            raise Exception("Job has no components to run.")
        for i in range(0, len(self.components)):
            c = self.components[i]
            print "Job: executing '%s' with description '%s'" % (str(c.name), str(c.description))
            print "Job: command '%s' with inputs %s and outputs %s" % (c.command, str(c.inputs), str(c.outputs))
            retcode = c.execute()
            if retcode:
                raise Exception("Job: error code '%d' returned by '%s'" % (retcode, str(c.name)))

    def setup(self):
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
    
    def copy_output_files(self):
        if not os.path.exists(self.output_dir):
            print "Job: creating output dir '%s'" % self.output_dir
            os.makedirs(self.output_dir, 0755)
           
        for output_file in self.output_files:
            if isinstance(output_file, basestring):
                src_file = os.path.join(self.rundir, output_file)
                dest_file = os.path.join(self.output_dir, output_file)
            elif isinstance(output_file, dict):
                src, dest = output_file.itervalues().next()
                src_file = os.path.join(self.rundir, src)
                if not os.path.isabs(dest):
                    dest_file = os.path.join(self.output_dir, dest)
                else:
                    dest_file = dest
            if self.append_job_num:
                base,ext = os.path.splitext(dest_file)
                dest_file = base + "_" + str(self.job_num) + ext
            print "Job: copying '%s' to '%s'" % (src_file, dest_file)    
            shutil.copyfile(src_file, dest_file)            