import os

from hpsmc.base import Component

class StdHepTool(Component):

    def __init__(self, **kwargs):
        Component.__init__(self, **kwargs)        
        self.command = "stdhep_" + self.name

    def cmd_args(self):
        if len(self.outputs):
            self.args.insert(0, self.outputs[0])
        if len(self.inputs):
            self.args.insert(0, self.inputs[0])
        return self.args

class SLIC(Component):

    def __init__(self, **kwargs):
        self.name = "slic"
        Component.__init__(self, **kwargs)
        self.name = "slic"
        self.command = self.name
        if "detector" in kwargs:
            self.detector = kwargs["detector"]
        else:
            raise Exception("Missing detector argument for SLIC.")
        if "nevents" in kwargs:
            self.nevents = kwargs["nevents"]
        else:
            self.nevents = 999999999
 
    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("No inputs given for SLIC.")
        detector_file = os.path.join(os.environ["HPSMC_DETECTOR_DIR"], self.detector, self.detector + ".lcdd")
        if not len(self.outputs):
            outputs.append("slic_events.slcio")
        self.args = ["-g", detector_file, "-i", self.inputs[0], "-o", self.outputs[0], "-r", str(self.nevents)]
        return self.args

    def setup(self):
        if not os.path.exists("./fieldmap"):
            os.symlink(os.environ["HPSMC_FIELDMAPS_DIR"], "fieldmap")

class HPSJava(Component):

    def __init__(self, **kwargs):
        self.name = "hps-java"
        Component.__init__(self, **kwargs)
        self.command = "java"
        if "steering_resource" in kwargs:
            self.steering_resource = kwargs["steering_resource"]
        elif "steering_file" in kwargs:
            self.steering_file = kwargs["steering_file"]
        else:
            raise Exception("A steering resource or file was not provided to hps-java.")

    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("No inputs provided to hps-java.")
        self.args = ["-Xmx2g", "-jar", os.environ["HPSJAVA_JAR"]]
        self.args.append("-i")
        self.args.append(self.inputs[0])
        if self.steering_resource is not None:
            self.args.append("-r")
            self.args.append(self.steering_resource)
        elif self.steering_file is not None:
            self.args.append(self.steering_file)
        return self.args
