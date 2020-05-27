import os, subprocess, sys, shutil, argparse, getpass, json, logging, time, re

logger = logging.getLogger("hpsmc.component")

import hpsmc.config as config

def _convert_config_value(val):
    if val == 'True' or val == 'true': 
        return True
    elif val == 'False' or val == 'false':
        return False
    try:
        if val.contains('.'):
            floatval = float(value)
            return floatval
    except:
        pass
    try:
        intval = int(val)
        return intval
    except:
        pass
    return val

class Component(object):
    """
    Base class for components in a job.
    """

    def __init__(self, 
                 name,
                 command=None,
                 **kwargs):
                                  
        self.name = name
        self.command = command
                                           
        if 'description' in kwargs:
            self.description = kwargs['description']
        else:
            self.description = ''

        if 'nevents' in kwargs:
            self.nevents = kwargs['nevents']
        else:
            self.nevents = None
            
        if 'seed' in kwargs:
            self.seed = kwargs['seed']
        else:
            self.seed = 1
        
        if 'inputs' in kwargs:
            self.inputs = kwargs['inputs']
        else:
            self.inputs = []
            
        if 'outputs' in kwargs:
            self.outputs = kwargs['outputs']
        else:
            self.outputs = None
        
        if 'replacements' in kwargs:
            self.replacements = kwargs['replacements']
        else:
            self.replacements = {}
        
        if 'excludes' in kwargs:
            self.excludes = kwargs['excludes']
        else:
            self.excludes = []
            
        if 'append_tok' in kwargs:
            self.append_tok = kwargs['append_tok']
        else:
            self.append_tok = None
            
        if 'output_ext' in kwargs:
            self.output_ext = kwargs['output_ext']
        else:
            self.output_ext = None
            
        if 'input_filter' in kwargs:
            self.input_filter = kwargs['input_filter']
        else:
            self.input_filter = None
 
        if 'ignore_job_params' in kwargs:
            self.ignore_job_params = kwargs['ignore_job_params']
        else:
            self.ignore_job_params = []
        
#        logger.debug("Initialized component '%s'" % self.name)
#        logger.debug(vars(self))
                                            
    def execute(self, log_out, log_err):
        """
        Generic component execution method. 
        
        Individual components may override this if specific behavior is required.
        """
        
        cl = [self.command]
        cl.extend(self.cmd_args())
        logger.info("Executing '%s' with command '%s'" % (self.name, ' '.join(cl)))
        proc = subprocess.Popen(' '.join(cl), shell=True, stdout=log_out, stderr=log_err)
        proc.communicate()
        proc.wait()
        
        return proc.returncode
        
    def cmd_exists(self):
        """
        Check if the component's assigned command exists.
        """
        return subprocess.call("type " + self.command, shell=True,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

    def cmd_args(self):
        """
        Return the command arguments of this component.
        """
        return []
    
    def cmd_args_str(self):
        """
        Return list of arguments, making sure they are all converted to strings.
        """
        return [str(c) for c in self.cmd_args()]
    
    def setup(self):
        """
        Perform any necessary setup for this component to run such as making symlinks
        to required directories.
        """
        pass

    def cleanup(self):
        """
        Perform post-job cleanup such as deleting temporary files.
        """
        pass    

    """
    def config(self, parser, section_name):
        if parser.has_section(section_name):
            for name, value in parser.items(section_name):
                setattr(self, name, convert_config_value(value))
                logger.debug("%s:%s:%s=%s" % (self.name, 
                                              name, 
                                              getattr(self, name).__class__.__name__, 
                                              getattr(self, name)))
    """

    def config(self, parser):
        """
        Automatically load attributes from config by reading in values from 
        the section with the same name as the class in the config file and 
        assigning them to class attributes with the same name.
        
        Components should not need to override this method.
        """
        logger.debug("Configuring '%s'" % self.name)
        section_name = self.__class__.__name__
        if parser.has_section(section_name):
            for name, value in parser.items(section_name):
                setattr(self, name, _convert_config_value(value))
                logger.debug("%s:%s:%s=%s" % (self.name, 
                                              name, 
                                              getattr(self, name).__class__.__name__, 
                                              getattr(self, name)))
                
    def set_parameters(self, params):
        """
        Set class attributes for the component based on JSON parameters.
        
        Components should not need to override this method.
        """
        
        # Set required parameters.
        for p in self.required_parameters():
            if p not in params:
                raise Exception("Required parameter '%s' is missing for component '%s'" 
                                % (p, self.name))        
            else:
                if p not in self.ignore_job_params:
                    setattr(self, p, params[p])
                    logger.info("%s:%s=%s [required]" % (self.name, p, params[p]))
                else:
                    logger.info("Ignored job param '%s'" % p)
                
        # Set optional parameters.
        for p in self.optional_parameters():
            if p in params:
                if p not in self.ignore_job_params:
                    setattr(self, p, params[p])
                    logger.info("%s:%s=%s [optional]"
                                % (self.name, p, params[p]))
                else:
                    logger.info("Ignored job param '%s'" % p)
    
    def required_parameters(self):
        """
        Return a list of required parameters.
        
        The job will fail if these are not present in the JSON file.
        """
        return []
    
    def optional_parameters(self):
        """
        Return a list of optional parameters.
        """
        return ['nevents', 'seed']
    
    def required_config(self):
        """
        Return a list of required configuration settings.
        There are none by default.
        """
        return []
    
    def check_config(self):
        """
        Raise an exception on the first missing config setting for this component.
        """
        for c in self.required_config():
            if not hasattr(self, c):
                raise Exception("Missing required config '%s' for '%s'" % (c, self.name))
    
    def _filtered_inputs(self):
        """
        Return a list of filtered input files.
        """
        return [inputfile for inputfile in self.inputs
                if re.search(self.input_filter, inputfile) is not None]
        
    def input_files(self):
        """
        Return a list of input files for this component, using an input filter if there is one.
        """
        if self.input_filter is not None:
            return self._filtered_inputs()
        else:
            return self.inputs
    
    def output_files(self):
        """
        Return a list of output files created by this component.
        
        By default, a series of transformations will be performed on intputs to
        transform them into outputs.
        """
        if self.outputs is not None and len(self.outputs):
            return self.outputs
        else:
            return self._inputs_to_outputs()

    def __exclude_input(self, i):
        for e in self.excludes:
            if e in i:
                return True
        return False

    def _inputs_to_outputs(self):
        """
        This is the default method for automatically transforming input file names
        to outputs. It applies a series of string transformations based on class attributes
        for replacement, appending, file extensions, and exclusions. 
        
        User components may override the output_files() method to customize this default 
        behavior.
        """        
        outputs = []
        for infile in self.input_files():
            f,ext = os.path.splitext(infile)
            infile_split = f.split('_')
            if self.__exclude_input(infile):
                continue
            if len(self.replacements):
                for k,v in self.replacements.iteritems():
                    if infile_split[-1] == k:
                        infile_split[-1] = v
                        if not len(infile_split[-1]):
                            infile_split = infile_split[:-1]
                        break
            if self.append_tok is not None:
                infile_split.append(self.append_tok)
            if self.output_ext is not None:
                ext = self.output_ext
            outfile = '_'.join(infile_split) + ext
            outputs.append(outfile)
        return outputs
    
class DummyComponent(Component):
    
    def __init__(self, **kwargs):
        Component.__init__(self, 'dummy', 'dummy', **kwargs)
    
    def execute(self, log_out, log_err):
        logger.info("DummComponent.execute - inputs %s and outputs %s"
                    % (str(self.input_files()), str(self.output_files())))
                       
