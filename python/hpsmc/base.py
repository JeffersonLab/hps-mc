import os, subprocess

class Component:

    def __init__(self, **kwargs):
        if "name" in kwargs:
            self.name = kwargs["name"]
        elif self.name is None:
            raise Exception("The name argument of Component is required.")
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

    def execute(self):
        command = [self.command]
        command.extend(self.cmd_args())
        print "Component: running '%s' with command %s" % (self.name, command)
        proc = subprocess.Popen(command, shell=False)
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

    def run(self):
        print "Job: running '%s'" % self.name
        if not len(self.components):
            raise Exception("Job has no components to run.")
        for i in range(0, len(self.components)):
            c = self.components[i]
            print "Job: executing '%s' with inputs %s and outputs %s" % (c.command, str(c.inputs), str(c.outputs))
            c.execute()

    def setup(self):
        print "Job: switching to run dir '%s'" % self.rundir
        os.chdir(self.rundir)
        for c in self.components:
            print "Job: setting up '%s'" % (c.name) 
            c.rundir = self.rundir
            c.setup()
            if not c.cmd_exists():
                raise Exception("Command '%s' does not exist for component '%s'." % (c.command, c.name))

    def cleanup(self):
        for c in self.components:
            print "Job: running cleanup for '%s'" % c.name
            c.cleanup()

