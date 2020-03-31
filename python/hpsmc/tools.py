import os, sys, time, socket, gzip, shutil, logging, subprocess, tarfile, sys, tempfile
from subprocess import PIPE
from component import Component

import hpsmc.config as config

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
        
        self.nevents = 999999999
 
    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("No inputs given for SLIC.")
        
        try:
            self.detector_dir
            detector_file = os.path.join(self.detector_dir, self.detector, self.detector + ".lcdd")
        except:
            raise Exception("Required SLIC config detector_dir was not set.")
                     
        self.args = ["-g", detector_file,
                     "-i", self.input_files()[0],
                     "-o", self.output_files()[0],
                     "-r", str(self.nevents),
                     "-d%s" % str(self.seed)]
        tbl = os.path.join(self.slic_dir, "share", "particle.tbl")
        if os.path.exists(tbl):
            self.args.extend(["-P", tbl])
        else:
            raise Exception("SLIC particle.tbl does not exist at '%s'. (Did you install SLIC?)" % tbl)
        return self.args

    def setup(self):
        
        try:
            self.slic_dir
        except:
            raise Exception("Missing required SLIC config slic_dir")
        if not os.path.exists(self.slic_dir):
            raise Exception("slic_dir does not exist at '%s'" % self.slic_dir)
        self.env_script = self.slic_dir + os.sep + "bin" + os.sep + "slic-env.sh"
        if not os.path.exists(self.env_script):
            raise Exception("slic setup script does not exist at '%s'" % self.name)
        logger.info("slic command set to '%s'" % self.name)

        try:
            self.hps_fieldmaps_dir
        except:
            raise Exception("Missing required SLIC config hps_fieldmaps_dir")
        logger.info("setting link to %s" % self.hps_fieldmaps_dir)
        if not os.path.islink(os.getcwd() + os.path.sep + "fieldmap"):
            os.symlink(self.hps_fieldmaps_dir, "fieldmap")
        else:
            logger.warning("No symlink to fieldmap dir created (already exists)")
    
    def required_parameters(self):
        return ['detector']
        
    def execute(self, log_out, log_err):
               
        args = self.cmd_args()
        
        # SLIC needs to be run inside bash as the Geant4 setup script is a piece of #@$@#$.
        cl = 'bash -c ". %s && slic %s"' % (self.env_script, ' '.join(self.cmd_args()))
                                          
        logger.info("Executing '%s' with command: %s" % (self.name, cl))
        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()
        
        return proc.returncode
    
    def output_files(self):
        if not len(self.outputs):
          for infile in self.input_files():
              filename,ext = os.path.splitext(infile)
              self.outputs.append("%s.slcio" % os.path.basename(filename))
        return self.outputs
            
class JobManager(Component):

    def __init__(self, **kwargs):
        
        self.name = "HPS Java Job Manager"
        Component.__init__(self, **kwargs)
        self.command = "java"
 
        self.run_number = None
        self.detector = None
        self.nevents = -1
        
        # FIXME: Figure out if this looks like a file or resource.
        #        If it begins with '/org/lcsim' then it is a resource.
        if "steering_resource" in kwargs:
            self.steering_resource = kwargs["steering_resource"]
            self.steering_file = None
        elif "steering_file" in kwargs:
            self.steering_file = kwargs["steering_file"]
            self.steering_resource = None
        else:
            raise Exception("A steering resource or file was not provided to hps-java.")
        
        #if "run" in kwargs:
        #    self.run_number = kwargs["run"]
        #else:
        #    self.run_number = None
        
        #if "detector" in kwargs:
        #    self.detector = kwargs["detector"]
        #else:
        #    self.detector = None
            
        #if "nevents" in kwargs:
        #    self.nevents = kwargs["nevents"]
        #else:
        #    self.nevents = -1
            
        if "defs" in kwargs:
            self.defs = kwargs["defs"]
        else:
            self.defs = {}
            
    def cmd_args(self):
        if not len(self.inputs):
            raise Exception("No inputs provided to hps-java.")
        if hasattr(self, "java_args"):
            logger.info("setting java_args from config: %s" + self.java_args)
            self.args.append(self.java_args)
        if hasattr(self, "logging_config_file"):
            logger.info("setting logging_config_file from config: %s" % self.logging_config_file)
            self.args.append("-Djava.util.logging.config.file=%s" % self.logging_config_file)
        if hasattr(self, "lcsim_cache_dir"):
            logger.info("setting lcsim_cache_dir from config: %s" % self.lcsim_cache_dir)
            self.args.append("-Dorg.lcsim.cacheDir=%s" % self.lcsim_cache_dir)
        if hasattr(self, "conditions_user"):
            logger.info("setting conditions_user from config: %s" % self.conditions_user)
            self.args.append("-Dorg.hps.conditions.user=%s" % self.conditions_user)
        if hasattr(self, "conditions_password"):
            logger.info("setting conditions_password from config (not shown)")
            self.args.append("-Dorg.hps.conditions.password=%s" % self.conditions_password)
        if hasattr(self, "conditions_url"):
            logger.info("setting conditions_url from config: %s" % self.conditions_url)
            self.args.append("-Dorg.hps.conditions.url=%s" % self.conditions_url)
        self.args.append("-jar")
        self.args.append(self.hps_java_bin_jar)
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
    
    def required_parameters(self):
        return ['steering_file']

    def optional_parameters(self):
        return ['detector', 'run']    
        
class JavaTool(Component):
    
    def __init__(self, **kwargs):
        self.name = "HPS Java Tool"
        Component.__init__(self, **kwargs)
        self.command = "java"
        if "java_class" in kwargs:
            self.java_class = kwargs["java_class"]
        elif self.java_class is None:
            raise Exception("Missing java_class argument for JavaTool.")
            
    def cmd_args(self):
        orig_args = self.args
        self.args = []
        # copied from JobManager
        if hasattr(self, "java_args"):
            logger.info("setting java_args from config: %s" + self.java_args)
            self.args.append(self.java_args)
        self.args.append("-cp")
        self.args.append(self.hps_java_bin_jar)
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

# Deprecated
"""
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
"""
                
class LCIODumpEvent(Component):

    def __init__(self, **kwargs):
        self.name = "LCIO dump event"
        self.command = "dumpevent"
        Component.__init__(self, **kwargs)
        if "event_num" in kwargs:
            self.event_num = kwargs["event_num"]
        else:
            self.event_num = 1
            
    def setup(self):
        if not hasattr(self, "lcio_dir"):
            raise Exception("Missing required config lcio_dir")
        self.command = self.lcio_dir + os.path.sep + "/bin/dumpevent"

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
        self.args = ["-jar", self.lcio_bin_jar]
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

class MoveFiles(Component):

    def __init__(self, **kwargs):
        self.command = "mv"
        self.name = "mv"
        Component.__init__(self, **kwargs)

    def cmd_exists(self):
        return True

    def execute(self, log_out, log_err):
        if len(self.inputs) != len(self.outputs):
            raise Exception("Input and output lists are not the same length!")
        for io in zip(self.inputs, self.outputs):
            print io
            src = io[0]
            dest = io[1] 
            logger("Moving '%s' to '%s'" % (src, dest))
            shutil.move(src, dest)
            
class HPSTR(Component):

    def __init__(self, **kwargs):
        
        self.name = "hpstr"
        self.command = self.name
        
        Component.__init__(self, **kwargs)
        
        if "year" in kwargs:
            self.year = kwargs["year"]
            
        if "run_mode" in kwargs:
            self.run_mode = kwargs["run_mode"]
        else:
            self.run_mode = 0
            logger.info("Using default run_mode: %d" % self.run_mode)
            
        if "cfg" in kwargs:
            self.cfg = kwargs["cfg"]
        else:
            raise Exception("Missing required argument cfg pointing to Python config file.")
        
    def setup(self):     
        try:
            self.hpstr_install_dir
        except:
            raise Exception("Missing required hpstr config hpstr_install_dir")

        try:
            self.hpstr_base
        except:
            raise Exception("Missing required hpstr config hpstr_base")
        
        if not os.path.exists(self.hpstr_install_dir):
            raise Exception("hpstr_install_dir does not exist at '%s'" % self.hpstr_install_dir)
        self.env_script = self.hpstr_install_dir + os.sep + "bin" + os.sep + "setup.sh"
        
    def cmd_args(self):

        if not len(self.inputs):
            raise Exception("No inputs given for HPSTR.")
        
        if not len(self.outputs):
            raise Exception("No inputs given for HPSTR.")
            
        self.args = ["-t", str(self.run_mode),
                     "-i", self.inputs[0],
                     "-o", self.outputs[0]]
        if self.nevents != -1:
            self.args.extend(["-n", str(self.nevents)])
        if hasattr(self, "year"):
            self.args.extend(["-y", str(self.year)])
        
        return self.args
                
    def execute(self, log_out, log_err):
               
        args = self.cmd_args()
        
        cfg_path = os.path.join(self.hpstr_base, "processors",  "config", self.cfg)
                
        cl = 'bash -c ". %s && %s %s %s"' % (self.env_script, self.command, cfg_path,
                                             ' '.join(self.cmd_args()))
                                                  
        logger.info("Executing '%s' with command: %s" % (self.name, cl))
        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()
        
        return proc.returncode
        