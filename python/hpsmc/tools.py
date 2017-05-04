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
