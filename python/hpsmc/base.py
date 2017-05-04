import os, subprocess

class Component:

    def __init__(self, **kwargs):
        if "executable" in kwargs:
            self.executable = kwargs["executable"]
        else:
            self.executable = None
        if "name" in kwargs:
            self.name = kwargs["name"]
        else:
            self.name = None
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
        print "Component: running executable '%s' with args %s" % (self.executable, self.args)
        command = [self.executable]
        command.extend(self.args)
        proc = subprocess.Popen(command, shell=False)
        proc.communicate()
        return proc.returncode
    
    def setup(self):
        pass 

    def cleanup(self):
        pass


class Job:

    def __init__(self, **kwargs):
        if "name" in kwargs:
            self.name = kwargs["name"] 
        else:
            self.name = "MC Job"
        if "rundir" in kwargs:
            self.rundir = kwargs["rundir"]
        else:
            self.rundir = os.getcwd()
        if "components" in kwargs:
            self.components = kwargs["components"]
        else:
            self.components = []

    def run(self):
        for i in range(0, len(self.components)):
            c = self.components[i]
            #if i and not len(c.inputs) and len(self.components[i - 1].outputs):
            #    c.inputs.extend(self.components[i - 1].outputs)
            print "Job: executing '%s' with inputs %s and outputs %s" % (c.executable, str(c.inputs), str(c.outputs))
            c.execute()

    def setup(self):
        print "Job: switching to run dir '%s'" % self.rundir
        os.chdir(self.rundir)
        for i in range(0, len(self.components)):
            c = self.components[i]
            print "Job: setting up '%s'" % (c.name) 
            c.rundir = self.rundir
            c.setup()

    def cleanup(self):
        for c in self.components:
            print "Job: running cleanup for '%s'" % c.name
            c.cleanup()

