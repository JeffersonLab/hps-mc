import os, sys, socket, gzip, shutil, logging, subprocess, tarfile, sys, tempfile
from subprocess import PIPE
from component import Component

logger = logging.getLogger("hpsmc.tools")

class StdHepTool(Component):

    seed_names = ["beam_coords", 
                  "beam_coords_old", 
                  "lhe_tridents", 
                  "lhe_tridents_displacetime", 
                  "merge_poisson",
                  "mix_signal",
                  "random_sample"]

    def __init__(self, **kwargs):
        Component.__init__(self, **kwargs)        
        self.command = "stdhep_" + self.name

    def cmd_args(self):        
        if self.name in StdHepTool.seed_names:
            self.args.extend(["-s", str(self.seed)])
        if len(self.outputs):
            self.args.insert(0, self.outputs[0])
        elif len(self.outputs) > 1:
            raise Exception("Too many outputs specified for StdHepTool.")
        if len(self.inputs):
            for i in self.inputs:
                self.args.insert(0, i)
        else:
            raise Exception("Not enough inputs specified for StdHepTool.")
        return self.args

class SLIC(Component):

    def __init__(self, **kwargs):
        self.name = "slic"
        Component.__init__(self, **kwargs)
        self.command = self.name
        if "detector" in kwargs:
            self.detector = kwargs["detector"]
        else:
            raise Exception("Missing detector argument for SLIC.")
        if self.nevents == -1:
            self.nevents = 999999999
 
    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("No inputs given for SLIC.")
        detector_file = os.path.join(os.environ["HPSMC_DETECTOR_DIR"], self.detector, self.detector + ".lcdd")
        if not len(self.outputs):
            outputs.append("slic_events.slcio")
        self.args = ["-g", detector_file,
                     "-i", self.inputs[0],
                     "-o", self.outputs[0],
                     "-r", str(self.nevents),
                     "-d%s" % str(self.seed)]
        tbl = os.path.join(os.environ["HPSMC_DATA_DIR"], "particle.tbl")
        if os.path.exists(tbl):
            self.args.extend(["-P", tbl])
        else:
            logger.warn("SLIC - The particle.tbl location is not being set automatically!")
        return self.args

    def setup(self):
        if not os.path.exists("./fieldmap"):
            os.symlink(os.environ["HPSMC_FIELDMAPS_DIR"], "fieldmap")

class JobManager(Component):

    def __init__(self, **kwargs):
        self.name = "HPS Java Job Manager"
        Component.__init__(self, **kwargs)
        self.command = "java"
        if "steering_resource" in kwargs:
            self.steering_resource = kwargs["steering_resource"]
        elif "steering_file" in kwargs:
            self.steering_file = kwargs["steering_file"]
        else:
            raise Exception("A steering resource or file was not provided to hps-java.")
        if "run" in kwargs:
            self.run_number = kwargs["run"]
        else:
            self.run_number = None
        if "detector" in kwargs:
            self.detector = kwargs["detector"]
        else:
            self.detector = None
        if "nevents" in kwargs:
            self.nevents = kwargs["nevents"]
        else:
            self.nevents = -1
        if "defs" in kwargs:
            self.defs = kwargs["defs"]
        else:
            self.defs = {}
        if "java_args" in kwargs:
            self.java_args = kwargs["java_args"]
        else:
            self.java_args = ["-Xmx500m", "-XX:+UseSerialGC"]
        #if "slac.stanford.edu" in socket.getfqdn():
        #    self.java_args.append("-Dorg.hps.conditions.connection.resource=/org/hps/conditions/config/slac_connection.prop")

    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("No inputs provided to hps-java.")
        self.args.extend(self.java_args)
        self.args.append("-jar")
        self.args.append(os.environ["HPSJAVA_JAR"])
        self.args.append("-e")
        self.args.append("1000")
        if self.run_number is not None:
            self.args.append("-R")
            self.args.append(str(self.run_number))
        if self.detector is not None:
            self.args.append("-d")
            self.args.append(self.detector)
        if len(self.outputs):
            self.args.append("-D")
            self.args.append("outputFile="+self.outputs[0])
        for k,v in self.defs.iteritems():
            self.args.append("-D")
            self.args.append(k+"="+str(v))
        if self.steering_resource is not None:
            self.args.append("-r")
            self.args.append(self.steering_resource)
        elif self.steering_file is not None:
            self.args.append(self.steering_file)
        if self.nevents != -1:
            self.args.append("-n")
            self.args.append(str(self.nevents))
        for input_file in self.inputs:
            self.args.append("-i")
            self.args.append(input_file)
        
        return self.args
    
class JavaTool(Component):
    
    def __init__(self, **kwargs):
        self.name = "HPS Java Tool"
        Component.__init__(self, **kwargs)
        self.command = "java"
        if "java_args" in kwargs:
            self.java_args = kwargs["java_args"]
        else:
            self.java_args = ["-Xmx1g", "-XX:+UseSerialGC"]
        if "java_class" in kwargs:
            self.java_class = kwargs["java_class"]
        elif self.java_class is None:
            raise Exception("Missing java_class argument for JavaTool.")
            
    def cmd_args(self):
        orig_args = self.args
        self.args = []
        self.args.extend(self.java_args)
        self.args.append("-cp")
        self.args.append(os.environ["HPSJAVA_JAR"])
        self.args.append(self.java_class)
        self.args.extend(orig_args)
        return self.args
    
class FilterMCBunches(JavaTool):
    
    def __init__(self, **kwargs):
        self.name = "Filter MC Bunches"
        self.java_class = "org.hps.util.FilterMCBunches"
        JavaTool.__init__(self, **kwargs)
        if "ecal_hit_ecut" in kwargs:
            self.ecal_hit_ecut = kwargs["ecal_hit_ecut"]
        else:
            self.ecal_hit_ecut = None
        if "event_interval" in kwargs:
            self.event_interval = kwargs["event_interval"]
        else:
            raise Exception("Missing required event_interval arg for FilterMCBunches.")
        if "enable_ecal_energy_filter" in kwargs:
            self.enable_ecal_energy_filter = kwargs["enable_ecal_energy_filter"]
        else:
            self.enable_ecal_energy_filter = False 
                    
    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("Missing required inputs for FilterMCBunches.")
        if not len(self.outputs):
            raise Exception("Missing required outputs for FilterMCBunches.")
        orig_args = self.args
        self.args = JavaTool.cmd_args(self)
        self.args.append("-e")
        self.args.append(str(self.event_interval))
        for i in self.inputs:
            self.args.append(i)
        self.args.append(self.outputs[0])
        if self.enable_ecal_energy_filter:
            self.args.append("-d")
        if self.ecal_hit_ecut is not None:
            self.args.append("-E")
            self.args.append(str(self.ecal_hit_ecut))
        if self.nevents > 0:
            self.args.append("-w")
            self.args.append(str(self.nevents))
        return self.args

class DST(Component):
 
    def __init__(self, **kwargs):
        self.name = "HPS DST Maker"
        self.command = "dst_maker"
        Component.__init__(self, **kwargs)
        
    def cmd_args(self):
        if not len(self.outputs):
            raise Exception("Missing required outputs for DST.")
        if not len(self.inputs):
            raise Exception("Missing required inputs for DST.")
        self.args = []
        self.args.append("-o")
        self.args.append(self.outputs[0])
        if self.nevents != -1:
            self.args.append("-n")
            self.args.append(str(self.nevents))
        for i in self.inputs:
            self.args.append(i)
        return self.args
                
class LCIODumpEvent(Component):

    def __init__(self, **kwargs):
        self.name = "LCIO dump event"
        self.command = "lcio_dumpevent"
        Component.__init__(self, **kwargs)
        if "event_num" in kwargs:
            self.event_num = kwargs["event_num"]
        else:
            self.event_num = 1

    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("Missing required inputs for LCIODumpEvent.")
        self.args = []
        self.args.append(self.inputs[0])
        self.args.append(str(self.event_num))
        return self.args

class LCIOTool(Component):

    def __init__(self, **kwargs):
        self.command = "java"
        Component.__init__(self, **kwargs)

    def cmd_args(self):
        orig_args = self.args
        self.args = ["-jar", os.environ["LCIO_JAR"]]
        self.args.append(self.name)
        self.args.extend(orig_args)
        return self.args
    
class LCIOConcat(LCIOTool):
    
    def __init__(self, **kwargs):
        self.name = "concat"
        LCIOTool.__init__(self, **kwargs)
        
    def cmd_args(self):
        args = LCIOTool.cmd_args(self)
        if not len(self.inputs):
            raise Exception("Missing at least one input file.")
        if not len(self.outputs):
            raise Exception("Missing output file.")
        for i in self.inputs:
            args.extend(["-f", i])
        args.extend(["-o", self.outputs[0]])
        self.args = args
        return self.args

class LCIOCount(LCIOTool):

    def __init__(self, minevents=0, **kwargs):
        self.name = "count"
        self.minevents = minevents
        LCIOTool.__init__(self, **kwargs)
        
    def cmd_args(self):
        args = LCIOTool.cmd_args(self)
        if not len(self.inputs):
            raise Exception("Missing an input file.")
        args.extend(["-f", self.inputs[0]])
        self.args = args
        return self.args

    def execute(self, log_out, log_err):
        
        cl = [self.command]
        cl.extend(self.cmd_args())
                                          
        proc = subprocess.Popen(cl, stdout=PIPE)
        (output, err) = proc.communicate()
                
        nevents = int(output.split()[1])
        
        logger.info("LCIO file '%s' has %d events." % (self.inputs[0], nevents)) 
        
        if self.minevents:
            if nevents < self.minevents:
                raise Exception("LHE file '%s' does not contain the minimum %d events." % (self.inputs[0], nevents))

        if not self.ignore_returncode and proc.returncode:
            raise Exception("Component: error code %d returned by '%s'" % (proc.returncode, self.name))        
    
class Unzip(Component):

    def __init__(self, **kwargs):
        self.command = "gunzip"
        self.name = "gunzip"
        Component.__init__(self, **kwargs)
        
    def setup(self):
        if not len(self.inputs):
            raise Exception("Missing inputs.")
        
    def cmd_exists(self):
        return True
        
    def execute(self, log_out, log_err):
        zip_path = self.inputs[0]
        with gzip.open(zip_path, 'rb') as in_file, open(os.path.splitext(zip_path)[0], 'wb') as out_file:
            shutil.copyfileobj(in_file, out_file)
            
class LHECount(Component):
    
    def __init__(self, minevents=0, **kwargs):
        self.name = "lhe_count"
        self.minevents = minevents
        Component.__init__(self, **kwargs)
        
    def setup(self):
        if not len(self.inputs):
            raise Exception("Missing inputs.")
        
    def cmd_exists(self):
        return True
    
    def execute(self, log_out, log_err):
        for i in self.inputs:
            with gzip.open(i, 'rb') as in_file:
                lines = in_file.readlines()
        
            nevents = 0
            for l in lines:
                if "<event>" in l:
                    nevents += 1
            
            logger.info("LHE file '%s' has %d events." % (i, nevents))
            
            if self.minevents:
                if nevents < self.minevents:
                    raise Exception("LHE file '%s' does not contain the minimum %d events." % (i, nevents))
                
class TarFiles(Component):
    
    def __init__(self, **kwargs):
        self.name = "Tar files"
        self.command = "python"
        Component.__init__(self, **kwargs)

    def cmd_exists(self):
        return True
        
    def execute(self, log_out, log_err):
        logger.info("Opening '%s' for writing ..." % self.outputs[0])  
        tar = tarfile.open(self.outputs[0], "w")
        for i in self.inputs:
            logger.info("Adding '%s' to archive" % i)
            tar.add(i)
        tar.close()
        logger.info("Wrote archive '%s'" % self.outputs[0])

class MakeTree(Component):
    
    def __init__(self, **kwargs):
        self.name = "Make ROOT tree"
        Component.__init__(self, **kwargs)
        
    def cmd_exists(self):
        return True
    
    def execute(self, log_out, log_err):        

        # Use local ROOT imports so ROOT env isn't necessary just to load the module.
        from ROOT import gROOT, TFile, TTree
        
        output_file = self.outputs[0]
        input_files = self.inputs
        
        logger.info("Creating output ROOT tuple '%s'" % output_file)
        logger.info("Input text files: %s" % str(input_files))
            
        treeFile = TFile(output_file, "RECREATE")
        tree = TTree("ntuple", "data from text tuple " + input_files[0])
        
        if len(input_files) > 1:
            inputfile = tempfile.NamedTemporaryFile(delete=False)
            print inputfile.name
            firstfile = True
            for filename in input_files:
                if os.path.isfile(filename):
                    f = open(filename, 'r')
                    firstline = True
                    for i in f:
                        if firstline:
                            if firstfile:
                                branchdescriptor = i
                                inputfile.write(i)
                            else:
                                if branchdescriptor != i:
                                    print "branch descriptor doesn't match"
                                    sys.exit(-1)
                        else:
                            inputfile.write(i)
                        firstline = False
                    f.close()
                    firstfile = False
                else:
                    logger.warn("Ignoring non-existant input file '%s'" % filename)
            inputfile.close()
            print tree.ReadFile(inputfile.name)
            os.remove(inputfile.name)
        else:
            print tree.ReadFile(input_files[0])
        
        tree.Write()
