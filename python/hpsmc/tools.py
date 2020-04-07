import os, sys, time, socket, gzip, shutil, logging, subprocess, tarfile, sys, tempfile
from subprocess import PIPE
from component import Component

import hpsmc.config as config

logger = logging.getLogger("hpsmc.tools")

def _replace(r, f):
    nf = f
    for k,v in r.iteritems():
        if nf.endswith(k):
            nf = nf.replace(k, v)
            break
    return nf

def _filtered(exclude, seq):
    r = []
    for s in seq:
        include = True
        for e in exclude:
            if e in s:
                include = False
                logger.info("Excluded '%s' which contains '%s'" % (s, e))
                break
        if include:
            r.append(s)
    return r

class SLIC(Component):

    def __init__(self, **kwargs):
        self.macros = []
        self.run_number = None
        Component.__init__(self, "slic", **kwargs)
                 
    def cmd_args(self):
        
        if not len(self.input_files()):
            raise Exception("No inputs given for SLIC.")  
            
        args = ["-g", self.__detector_file(),
                "-i", self.input_files()[0],
                "-o", self.output_files()[0],
                "-d%s" % str(self.seed)]
        
        if len(self.macros):
            for macro in self.macros:
                if macro == "run_number.mac":
                    raise Exception("Macro name '%s' is not allowed." % macro)
                args.extend(["-m", macro])
                        
        if self.nevents is not None:
            args.extend(["-r", str(self.nevents)])
            
        if self.run_number is not None:
            args.extend(["-m", "run_number.mac"])
        
        tbl = self.__particle_tbl()
        if os.path.exists(tbl):
            args.extend(["-P", tbl])
        else:
            raise Exception("SLIC particle.tbl does not exist at '%s'. (Did you install SLIC properly?)" % tbl)

        return args

    def __detector_file(self):
        return os.path.join(self.detector_dir, self.detector, self.detector + ".lcdd")
    
    def __particle_tbl(self):
        return os.path.join(self.slic_dir, "share", "particle.tbl")

    def setup(self):
        
        if not os.path.exists(self.slic_dir):
            raise Exception("slic_dir does not exist at '%s'" % self.slic_dir)
        
        self.env_script = self.slic_dir + os.sep + "bin" + os.sep + "slic-env.sh"
        if not os.path.exists(self.env_script):
            raise Exception("SLIC setup script does not exist at '%s'" % self.name)
    
        logger.info("Setting fieldmap link to '%s'" % self.hps_fieldmaps_dir)
        if not os.path.islink(os.getcwd() + os.path.sep + "fieldmap"):
            os.symlink(self.hps_fieldmaps_dir, "fieldmap")
        else:
            logger.warning("Link to fieldmap dir already exists!")
    
        if self.run_number is not None:
            run_number_cmd = "/lcio/runNumber %d" % self.run_number
            run_number_mac = open("run_number.mac", 'w')
            run_number_mac.write(run_number_cmd)
            run_number_mac.close()
    
    def optional_parameters(self):
        return ['nevents', 'macros', 'run_number']
    
    def required_parameters(self):
        return ['detector']
    
    def required_config(self):
        return ['slic_dir', 'hps_fieldmaps_dir', 'detector_dir']
        
    def execute(self, log_out, log_err):
               
        args = self.cmd_args()
        
        # SLIC needs to be run inside bash as the Geant4 setup script is a piece of #@$@#$.
        cl = 'bash -c ". %s && %s %s"' % (self.env_script, self.command, ' '.join(self.cmd_args()))
                                          
        logger.info("Executing '%s' with command: %s" % (self.name, cl))
        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()
        
        return proc.returncode
    
    def output_files(self):
        self.outputs = []
        for infile in self.input_files():
            filename,ext = os.path.splitext(infile)
            self.outputs.append("%s.slcio" % os.path.basename(filename))
        return self.outputs 

class JobManager(Component):
    """
    Run the hps-java JobManager class.
    
    This component can take multiple inputs but will only write one output.
    """

    def __init__(self, **kwargs):                
        self.run_number = None
        self.detector = None
        self.event_print_interval = 1000
        self.defs = None
        Component.__init__(self, "job_manager", "java", **kwargs)
                            
    def required_config(self):
        return ['hps_java_bin_jar']
    
    def output_files(self):
        # Return one output file name with appropriate string replacement depending on if readout
        # or recon is being run.
        return [_replace(self.replace, os.path.splitext(self.input_files()[0])[0]) + ".slcio"]
            
    def cmd_args(self):
               
        args = []
                                
        if not len(self.input_files()):
            raise Exception("No inputs provided to hps-java.")
        
        if hasattr(self, "java_args"):
            logger.info("Setting java_args from config: %s" % self.java_args)
            args.append(self.java_args)
        
        if hasattr(self, "logging_config_file"):
            logger.info("Setting logging_config_file from config: %s" % self.logging_config_file)
            args.append("-Djava.util.logging.config.file=%s" % self.logging_config_file)
        
        if hasattr(self, "lcsim_cache_dir"):
            logger.info("setting lcsim_cache_dir from config: %s" % self.lcsim_cache_dir)
            args.append("-Dorg.lcsim.cacheDir=%s" % self.lcsim_cache_dir)
        
        if hasattr(self, "conditions_user"):
            logger.info("Setting conditions_user from config: %s" % self.conditions_user)
            args.append("-Dorg.hps.conditions.user=%s" % self.conditions_user)
        if hasattr(self, "conditions_password"):
            logger.info("Setting conditions_password from config (not shown)")
            args.append("-Dorg.hps.conditions.password=%s" % self.conditions_password)
        if hasattr(self, "conditions_url"):
            logger.info("Setting conditions_url from config: %s" % self.conditions_url)
            args.append("-Dorg.hps.conditions.url=%s" % self.conditions_url)
        
        args.append("-jar")
        args.append(self.hps_java_bin_jar)
        
        args.append("-e")
        args.append(str(self.event_print_interval))
        
        if self.run_number is not None:
            args.append("-R")
            args.append(str(self.run_number))
            
        if self.detector is not None:
            args.append("-d")
            args.append(self.detector)
            
        if len(self.output_files()):
            args.append("-D")
            args.append("outputFile=" + os.path.splitext(self.output_files()[0])[0])
        
        if self.defs:
            for k,v in self.defs.iteritems():
                args.append("-D")
                args.append(k+"="+str(v))
                
        if not os.path.isfile(self.steering):
            # If steering isn't a valid file, assume it is a resource in the jar.
            args.append("-r")
        args.append(self.steering)
            
        if self.nevents is not None:
            args.append("-n")
            args.append(str(self.nevents))
            
        for input_file in self.input_files():
            args.append("-i")
            args.append(input_file)
            
        return args
    
    def required_parameters(self):
        return ['steering']

    def optional_parameters(self):
        return ['detector', 'run_number', 'defs']
    
class StdHepTool(Component):

    # TODO: Need list of valid names rather than just those 
    #       that accept a seed argument.
    seed_names = ["beam_coords",
                  "beam_coords_old",
                  "lhe_tridents",
                  "lhe_tridents_displacetime",
                  "merge_poisson",
                  "mix_signal",
                  "random_sample"]

    def __init__(self, name, **kwargs):
        Component.__init__(self, name, "stdhep_" + name, **kwargs)
        
    def cmd_args(self):
        
        args = []
 
        if self.name is "lhe_tridents_displacetime" and hasattr(self, "ctau"):
            args.extend(["-l", str(self.ctau)])
        elif self.name is "beam_coords" and hasattr(self, "z"):        
            args.extend(["-z", str(self.z)])
        
        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])
        
        if len(self.output_files()):
            args.insert(0, self.output_files()[0])
        elif len(self.outputs) > 1:
            raise Exception("Too many outputs specified for StdHepTool.")
        
        if len(self.input_files()):
            for i in self.inputs:
                args.insert(0, i)
        else:
            raise Exception("No inputs specified for StdHepTool.")
        
        return args
    
    def output_files(self):
        f = os.path.basename(os.path.splitext(self.input_files()[0])[0])
        return ["%s%s.stdhep" % (_replace(self.replace, f), self.append)]
    
    def optional_parameters(self):
        return ['ctau', 'z']

class Unzip(Component):
    """
    Unzip the input files to outputs.
    
    A list of exclude strings can be provided to filter out unwanted files from the inputs.
    """

    def __init__(self, **kwargs):
        Component.__init__(self, "unzip", **kwargs)
               
    def output_files(self):
        self.outputs = []
        for inputfile in self.input_files():
            self.outputs.append(os.path.basename(os.path.splitext(inputfile)[0]))
        return self.outputs
                        
    def execute(self, log_out, log_err):
        for inputfile in self.input_files():
            outputfile = os.path.splitext(inputfile)[0]
            with gzip.open(inputfile, 'rb') as in_file, open(outputfile, 'wb') as out_file:
                shutil.copyfileobj(in_file, out_file)
                logger.info("Unzipped '%s' to '%s'" % (inputfile, outputfile))
                    
class FileFilter(Component):
    """
    Filter input to output files based on a list of strings.
    """
    
    def __init__(self, **kwargs):
        self.excludes = []
        Component.__init__(self, "file_filter", **kwargs)

    def execute(self, log_out, log_err):
        return 0
        
    def output_files(self):
        return _filtered(self.excludes, self.input_files())
                        
class JavaTool(Component):
    
    def __init__(self, name="java", command=None, **kwargs):
        Component.__init__(self, name, command, **kwargs)
        if "java_class" in kwargs:
            self.java_class = kwargs["java_class"]
        elif self.java_class is None:
            raise Exception("Missing java_class argument for JavaTool.")
            
    def cmd_args(self):
        #orig_args = self.args
        args = []
        # copied from JobManager
        if hasattr(self, "java_args"):
            logger.info("setting java_args from config: %s" + self.java_args)
            args.append(self.java_args)
        args.append("-cp")
        args.append(self.hps_java_bin_jar)
        args.append(self.java_class)
#        args.extend(orig_args)
        return args
    
class FilterBunches(JavaTool):
    
    def __init__(self, **kwargs):
        self.java_class = "org.hps.util.FilterMCBunches"
        JavaTool.__init__(self, "filter_bunches", "java", **kwargs)
                    
    def cmd_args(self):
        orig_args = self.args
        self.args = JavaTool.cmd_args(self)
        self.args.append("-e")
        self.args.append(str(self.event_interval))
        for i in self.input_files():
            self.args.append(i)
        self.args.append(self.output_files()[0])
        if self.enable_ecal_energy_filter:
            self.args.append("-d")
        if self.ecal_hit_ecut is not None:
            self.args.append("-E")
            self.args.append(str(self.ecal_hit_ecut))
        if self.nevents > 0:
            self.args.append("-w")
            self.args.append(str(self.nevents))
        return self.args
    
    def output_files(self):
        self.outputs = []
        for infile in self.input_files():
            self.outputs.append(os.path.splitext(infile)[0] + self.append + ".slcio")
        return self.outputs
    
    def required_parameters(self):
        return ['event_interval']
    
    def optional_parameters(self):
        return ['ecal_hit_ecut', 'enable_ecal_energy_filter']
                
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
            
class LHECount(Component):
    
    def __init__(self, minevents=0, **kwargs):
        self.minevents = minevents
        Component.__init__(self, name="lhe_count", **kwargs)
        
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
        Component.__init__(self, 'tar_files', **kwargs)
        
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
        Component.__init__(self, "make_tree", **kwargs)
        
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
        Component.__init__(self, "move_files", **kwargs)

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
                
        Component.__init__(self, 'hpstr', **kwargs)
        
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
        
