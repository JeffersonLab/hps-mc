import os

from hpsmc.base import Component

class StdHepTool(Component):

    def __init__(self, **kwargs):
        Component.__init__(self, **kwargs)        
        self.executable = "stdhep_" + self.name

    def execute(self):
        if len(self.outputs):
            self.args.insert(0, self.outputs[0])
        if len(self.inputs):
            self.args.insert(0, self.inputs[0])
        Component.execute(self)

class SLIC(Component):

    def __init__(self, **kwargs):
        Component.__init__(self, **kwargs)
        self.name = "slic"
        self.executable = self.name

class HPSJava(Component):

    def __init__(self, **kwargs):
        Component.__init__(self, **kwargs)
        self.name = "hps-java"
        self.executable = "java"
        self.args.insert(0, os.environ["HPSJAVA_JAR"])
        self.args.insert(0, "-jar")
