import os, subprocess

class Component:

    def __init__(self, **kwargs):
        if "executable" in kwargs:
            self.executable = kwargs["executable"]
        else:
            self.executable = None
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
        if "job_num" in kwargs:
            self.job_num = kwargs["job_num"]
        else:
            self.job_num = 1

    def execute(self):
        print "Component: running executable '%s'" % self.executable
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
            self.name = ""
        if "rundir" in kwargs:
            self.rundir = kwargs["rundir"]
        else:
            self.rundir = os.getcwd()
        if "components" in kwargs:
            self.components = kwargs["components"]
        else:
            self.components = []

    def run(self):
        for c in self.components:
            print "Job: Running '%s'" % c.executable 
            c.execute()

    def setup(self):
        print "Job: switching to run dir '%s'" % self.rundir
        os.chdir(self.rundir)
        for c in self.components:
            print "Job: setting up component '%s'" % c.executable
            c.setup()

    def cleanup(self):
        for c in self.components:
            print "Job: running cleanup for '%s'" % c.name
            c.cleanup()

