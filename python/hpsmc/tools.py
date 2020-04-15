import os, sys, time, socket, gzip, shutil, logging, subprocess, tarfile, sys, tempfile
from subprocess import PIPE

from component import Component
from .run_params import RunParameters
import hpsmc.func as func
import hpsmc.config as config
from audioop import mul

logger = logging.getLogger("hpsmc.tools")

class SLIC(Component):

    def __init__(self):
        
        # List of macros to run (optional)
        self.macros = []
        
        # Run number to set on output file (optional)
        self.run_number = None                
        
        Component.__init__(self, 
                           'slic',
                           'slic',
                           replacements={'rot': ''},
                           output_ext='.slcio')
                               
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
                if not os.path.isabs(macro):
                    raise Exception("Macro '%s' is not an absolute path." % macro)
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
    
class JobManager(Component):
    """
    Run the hps-java JobManager class.
    """

    def __init__(self, steering):
        
        self.run_number = None
        self.detector = None
        self.event_print_interval = None
        self.defs = None
        self.readout_steering = None
        self.recon_steering = None
        self.java_args = None
        self.logging_config_file = None
        self.lcsim_cache_dir = None
        self.conditions_user = None
        self.conditions_password = None
        self.conditions_url = None        
        self.steering = steering
        
        Component.__init__(self, 
                           'job_manager', 
                           'java', 
                           replacements={'filt': 'readout', 'readout': 'recon'},
                           output_ext='.slcio')
        
    def required_config(self):
        return ['hps_java_bin_jar']
    
    def setup(self):
        if not len(self.input_files()):
            raise Exception("No inputs provided to hps-java.")
        
        if self.steering not in self.steering_files:
            raise Exception("Steering '%s' not found in %s" % (self.steering, self.steering_files))        
        self.steering_file = self.steering_files[self.steering]     
             
    def cmd_args(self):
               
        args = []
                
        if self.java_args is not None:
            logger.debug("Setting java_args from config: %s" % self.java_args)
            args.append(self.java_args)
        
        if self.logging_config_file is not None:
            logger.debug("Setting logging_config_file from config: %s" % self.logging_config_file)
            args.append("-Djava.util.logging.config.file=%s" % self.logging_config_file)
        
        if self.lcsim_cache_dir is not None:
            logger.debug("setting lcsim_cache_dir from config: %s" % self.lcsim_cache_dir)
            args.append("-Dorg.lcsim.cacheDir=%s" % self.lcsim_cache_dir)
        
        if self.conditions_user is not None:
            logger.debug("Setting conditions_user from config: %s" % self.conditions_user)
            args.append("-Dorg.hps.conditions.user=%s" % self.conditions_user)
        if self.conditions_password is not None:
            logger.debug("Setting conditions_password from config (not shown)")
            args.append("-Dorg.hps.conditions.password=%s" % self.conditions_password)
        if self.conditions_url is not None:
            logger.debug("Setting conditions_url from config: %s" % self.conditions_url)
            args.append("-Dorg.hps.conditions.url=%s" % self.conditions_url)
        
        args.append("-jar")
        args.append(self.hps_java_bin_jar)
        
        if self.event_print_interval is not None:
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
    
        if not os.path.isfile(self.steering_file):
            args.append("-r")
            logger.debug("Steering does not exist at '%s' so assuming it is a resource." % self.steering_file)
        else:
            if not os.path.isabs(self.steering_file):
                raise Exception("Steering '%s' looks like a file but is not an abs path." % self.steering_file)
        args.append(self.steering_file)
                            
        if self.nevents is not None:
            args.append("-n")
            args.append(str(self.nevents))
            
        for input_file in self.input_files():
            args.append("-i")
            args.append(input_file)
            
        return args
    
    def required_parameters(self):
        return ['steering_files']
    
    def optional_parameters(self):
        return ['detector', 'run_number', 'defs']

class HPSTR(Component):
    """
    Component for the hpstr analysis tool.
    """

    def __init__(self, cfg, run_mode=0, year=None, **kwargs):
        
        self.cfg = cfg                
        self.run_mode = run_mode
        self.year = year

        Component.__init__(self, 
                           name='hpstr', 
                           command='hpstr',
                           **kwargs)
                    
    def setup(self):        
        if not os.path.exists(self.hpstr_install_dir):
            raise Exception("hpstr_install_dir does not exist at '%s'" % self.hpstr_install_dir)
        self.env_script = self.hpstr_install_dir + os.sep + "bin" + os.sep + "setup.sh"
        
        if self.cfg not in self.config_files:
            raise Exception("Config '%s' was not found in %s" % (self.cfg, self.config_files))
        config_file = self.config_files[self.cfg]
        if len(os.path.dirname(config_file)):
            if os.path.isabs(config_file):
                self.cfg_path = config_file
            else:
                raise Exception("The config '%s' has a directory but is not an abs path." % self.cfg)
        else:
            self.cfg_path = os.path.join(self.hpstr_base, "processors",  "config", config_file)
        
        if os.path.splitext(self.input_files()[0])[1] is '.root':
            self.append_tok = self.cfg
            
        logger.info("Set config path to '%s'" % self.cfg_path)
    
    def required_parameters(self):
        return ['config_files']
    
    def optional_parameters(self):
        return ['year', 'run_mode', 'nevents']

    def required_config(self):
        return ['hpstr_install_dir', 'hpstr_base']
        
    def cmd_args(self):
        args = [self.cfg_path,
                "-t", str(self.run_mode),
                "-i", self.input_files()[0],
                "-o", self.output_files()[0]]
        if self.nevents is not None:
            args.extend(["-n", str(self.nevents)])
        if self.year is not None:
            args.extend(["-y", str(self.year)])
        return args

    # FIXME: Make the generic Component method usable with this class.
    def output_files(self):
        f,ext = os.path.splitext(self.input_files()[0])
        print(f)
        print(ext)
        if '.slcio' in ext:
            return ['%s.root' % f]
        else:
            return ['%s_%s.root' % (f, self.append_tok)]
                
    def execute(self, log_out, log_err):               
        args = self.cmd_args()
        cl = 'bash -c ". %s && %s %s"' % (self.env_script, self.command, 
                                          ' '.join(self.cmd_args()))

        logger.info("Executing '%s' with command: %s" % (self.name, cl))
        proc = subprocess.Popen(cl, shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()
        
        return proc.returncode
        
class StdHepTool(Component):

    # TODO: Need list of valid program names rather than just those
    #       that accept a seed argument.
    seed_names = ["beam_coords",
                  "beam_coords_old",
                  "lhe_tridents",
                  "lhe_tridents_displacetime",
                  "merge_poisson",
                  "mix_signal",
                  "random_sample"]

    def __init__(self, name, **kwargs):
                       
        Component.__init__(self, 
                           name,
                           "stdhep_" + name,                           
                           **kwargs)
        
    def cmd_args(self):
        
        args = []
         
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
        
    def optional_parameters(self):
        return ['ctau', 'z']

class BeamCoords(StdHepTool):
    """
    Transform StdHep events into beam coordinates.
    """

    def __init__(self):
        
        self.beam_sigma_x = None
        self.beam_sigma_y = None
        self.target_z = None
        self.beam_rotation = None
        self.beam_skew = None
   
        StdHepTool.__init__(self, 
                            'beam_coords',
                            replacements={'mom': ''},
                            append_tok='rot')

    def cmd_args(self):
        args = StdHepTool.cmd_args(self)
        if self.beam_sigma_x is not None:
            args.extend(['-x', str(self.beam_sigma_x)])
        if self.beam_sigma_y is not None:
            args.extend(['-y', str(self.beam_sigma_y)])
        if self.beam_skew is not None:
            args.extend(['-q', str(self.beam_skew)])
        if self.beam_rotation is not None:
            args.extend(['-r', str(self.beam_rotation)])
        if self.target_z is not None:
            args.extend(['-z', str(self.target_z)])
        return args
    
    def optional_parameters(self):
        return['beam_sigma_x', 'beam_sigma_y', 'target_z', 'beam_rotation', 'beam_skew']
        
class RandomSample(StdHepTool):
    
    def __init__(self):
        StdHepTool.__init__(self, name='beam_coords', replacements={'rot': 'sampled'})
    
    def output_files(self):
        return [f.replace('_1.stdhep', '') for f in Component.output_files(self)]
        
class DisplaceTime(StdHepTool):
    """
    Convert LHE files to StdHep, displacing the time by given ctau.
    """
    
    def __init__(self):
        self.ctau = None
        StdHepTool.__init__(self, name='lhe_tridents_displacetime', output_ext='.stdhep')

    def cmd_args(self):
        args = StdHepTool.cmd_args(self) 
        if self.ctau is not None:
            args.extend(["-l", str(self.ctau)])
        return args

    def optional_parameters(self):
        return ['ctau']   
        
class AddMother(StdHepTool):
    
    def __init__(self):
        StdHepTool.__init__(self, 'add_mother', append_tok='mom')
        
class MergePoisson(StdHepTool):
        
    def __init__(self, **kwargs):
        if 'lhe_file' in kwargs:
            self.lhe_file = kwargs['lhe_file']
        else:
            raise Exception("Missing required init argument 'lhe_file' to compute mu")
        StdHepTool.__init__(self, 'merge_poisson', replacements={'rot': ''}, append_tok='sampled', **kwargs)
    
    def setup(self):
        self.run_param_data = RunParameters(self.run_params)
        self.mu = func.mu(self.lhe_file, self.run_param_data)       
    
    def required_parameters(self):
        return ['run_params']
    
    def optional_parameters(self):
        return ['lhe_file']
    
    def output_files(self):
        return ["%s_1.stdhep" % os.path.splitext(f)[0] for f in Component.output_files(self)]
            
    def cmd_args(self):
        args = StdHepTool.cmd_args(self)
        return args
    
    def cmd_args(self):
        
        args = []
         
        if self.name in StdHepTool.seed_names:
            args.extend(["-s", str(self.seed)])
        
        args.extend(["-m", str(self.mu), "-N", str(1), "-n", str(self.nevents)])
        
        if len(self.output_files()):
            args.insert(0, '_'.join(os.path.splitext(self.output_files()[0])[0].split('_')[:-1]))
        elif len(self.outputs) > 1:
            raise Exception("Too many outputs specified for StdHepTool.")       
        
        if len(self.input_files()):
            for i in self.inputs:
                args.insert(0, i)
        else:
            raise Exception("No inputs specified for StdHepTool.")
        
        return args
        
class MergeFiles(StdHepTool):
    
    def __init__(self, output_name):
        StdHepTool.__init__(self, 'merge_files')

    def required_parameters(self):
        return ['output_name']
        
    def output_files(self):
        return ['%s.stdhep' % self.output_name]
                                    
class JavaTool(Component):
    
    def __init__(self, name, java_class, **kwargs):
        self.java_class = java_class
        self.java_args = None
        Component.__init__(self, 
                           name, 
                           "java", 
                           **kwargs)

    def required_config(self):
        return ['hps_java_bin_jar']
            
    def cmd_args(self):
        args = []
        if self.java_args is not None:
            logger.info("setting java_args from config: %s" + self.java_args)
            args.append(self.java_args)
        args.append("-cp")
        args.append(self.hps_java_bin_jar)
        args.append(self.java_class)
        return args
    
class FilterBunches(JavaTool):
    """
    Space MC events and apply energy filters to process before readout.
    
    The nevents parameter is not settable from JSON in this class. It should
    be supplied as an init argument in the job script if it needs to be
    customized (the default nevents and event_interval used to apply spacing 
    should usually not need to be changed by the user).
    """
    
    def __init__(self, nevents=2000000, event_interval=250):

        # Default max output events
        self.nevents = nevents
        
        # True to enable filtering on min ecal energy dep
        self.enable_ecal_energy_filter = False
        
        self.ecal_hit_ecut = None
        
        # Default event spacing interval
        self.event_interval = event_interval
                
        JavaTool.__init__(self, 
                          "filter_bunches",
                          "org.hps.util.FilterMCBunches",
                          replacements={'rot': ''},
                          append_tok='filt')
                            
    def cmd_args(self):
        
        args = JavaTool.cmd_args(self)
        args.append("-e")
        args.append(str(self.event_interval))
        for i in self.input_files():
            args.append(i)
        args.append(self.output_files()[0])
        if self.enable_ecal_energy_filter:
            args.append("-d")
        if self.ecal_hit_ecut is not None:
            args.append("-E")
            args.append(str(self.ecal_hit_ecut))
        if self.nevents > 0:
            args.append("-w")
            args.append(str(self.nevents))
        return args

    def optional_parameters(self):
        return ['ecal_hit_ecut', 'enable_ecal_energy_filter', 'event_interval']
                   
class Unzip(Component):
    """
    Unzip the input files to outputs.
    
    A list of exclude strings can be provided to filter out unwanted files from the inputs.
    """

    def __init__(self, **kwargs):
        Component.__init__(self, "unzip", "unzip", **kwargs)
               
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
    
    def __init__(self, excludes):
        Component.__init__(self, "file_filter", "filter_filter", excludes=excludes)

    def execute(self, log_out, log_err):
        return 0
                        
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
        if not len(self.input_files()):
            raise Exception("Missing required inputs for LCIODumpEvent.")
        args = []
        args.append(self.input_files()[0])
        args.append(str(self.event_num))
        return args

# FIXME: Everything below here is broken!

class LCIOTool(Component):

    def __init__(self, **kwargs):
        self.command = "java"
        Component.__init__(self, **kwargs)

    def cmd_args(self):
        args = []
        args = ["-jar", self.lcio_bin_jar]
        args.append(self.name)
        return args
    
class LCIOConcat(LCIOTool):
    
    def __init__(self, **kwargs):
        self.name = "concat"
        LCIOTool.__init__(self, **kwargs)
        
    def cmd_args(self):
        args = LCIOTool.cmd_args(self)
        if not len(self.input_files()):
            raise Exception("Missing at least one input file.")
        if not len(self.output_files()):
            raise Exception("Missing an output file.")
        for i in self.input_files():
            args.extend(["-f", i])
        args.extend(["-o", self.outputs[0]])
        return args

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
        return args

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

class MoveFiles(Component):

    def __init__(self, **kwargs):
        Component.__init__(self, "move_files", "", **kwargs)

    def execute(self, log_out, log_err):
        if len(self.inputs) != len(self.outputs):
            raise Exception("Input and output lists are not the same length!")
        for io in zip(self.inputs, self.outputs):
            print io
            src = io[0]
            dest = io[1] 
            logger.info("Moving '%s' to '%s'" % (src, dest))
            shutil.move(src, dest)            
