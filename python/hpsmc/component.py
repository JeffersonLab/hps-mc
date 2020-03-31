import os, subprocess, sys, shutil, argparse, getpass, json, logging, time

logger = logging.getLogger("hpsmc.component")

import hpsmc.config as config

class Component:
    """
    Base class for components in a job.
    """

    def __init__(self, **kwargs):
        
        # TODO: This should not be set here. Put into sub-class init method.
        if "name" in kwargs:
            self.name = kwargs["name"]
        elif self.name is None:
            raise Exception("The name of a Component is required.")
        
        # FIXME: Sort out this mess!
        #        Parameters passed to tools should not be handled this way.
        if "args" in kwargs:
            if not isinstance(kwargs["args"], list):
                raise Exception("The args are not a list.")
            self.args = kwargs["args"]
        else:
            self.args = []
            
        self.outputs = []
        self.inputs = []
            
        self.nevents = -1
                
        self.seed = 1
            
        # FIXME: This should be a global job setting instead.
        if "ignore_returncode" in kwargs:
            self.ignore_returncode = kwargs["ignore_returncode"]
        else:
            self.ignore_returncode = False

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
        return self.args
    
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

    def config(self, section_name):
        """
        Configure this component from configuration file using specific, named section.
        """
        if config.parser.has_section(section_name):
            for name, value in config.parser.items(section_name):
                setattr(self, name, value)
                logger.info("%s=%s" % (name, value))

    def config(self):
        """
        Automatically load attributes from config by reading in values from 
        the section with the same name as the class in the config file and 
        assigning them to class attributes with the same name.
        
        Components should not need to override this method.
        """
        logger.info("Configuring '%s'" % self.name)
        section_name = self.__class__.__name__
        logger.info("Loading config for '%s'" % section_name)
        if config.parser.has_section(section_name):
#            section = config.parser[section_name]
            for name, value in config.parser.items(section_name):
                setattr(self, name, value)
                logger.info("%s=%s" % (name, value))
                
    def set_parameters(self, params):
        """
        Set class attributes for the component based on JSON parameters.
        
        Components should not need to override this method.
        """       
        
        # Set required parameters. 
        for p in self.required_parameters():
            if p not in params:
                raise Exception("Required parameter '%s' is missing for component '%s'." 
                                % (p, self.name))
            setattr(self, p, params[p])
            logger.info("Set required parameter '%s' to '%s' for component '%s'."
                        % (p, params[p], self.name))
            
        # Set optional parameters.
        for p in self.optional_parameters():
            if p in params:
                setattr(self, p, params[p])
                logger.info("Set optional parameter '%s' to '%s' for component '%s'."
                            % (p, params[p], self.name))
    
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
    
    def input_files(self):
        """
        Return a list of input files for this component.
        """
        return self.inputs
    
    def output_files(self):
        """
        Return a list of output files generated by this component.
        """
        return self.outputs
                   