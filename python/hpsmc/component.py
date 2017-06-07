import os, subprocess, sys, shutil, argparse, getpass, json, logging, time

logger = logging.getLogger("component")

class Component:

    def __init__(self, **kwargs):
        
        if "name" in kwargs:
            self.name = kwargs["name"]
        elif self.name is None:
            raise Exception("The name of a Component is required.")
        
        if "args" in kwargs:
            if not isinstance(kwargs["args"], list):
                raise Exception("The args are not a list.")
            self.args = kwargs["args"]
        else:
            self.args = []
            
        if "outputs" in kwargs:
            if not isinstance(kwargs["outputs"], list):
                raise Exception("The outputs arg is not a list.")
            self.outputs = kwargs["outputs"]
        else:
            self.outputs = []
            
        if "inputs" in kwargs:
            if not isinstance(kwargs["inputs"], list):
                raise Exception("The inputs arg is not a list.")
            self.inputs = kwargs["inputs"]
        else:
            self.inputs = []
            
        if "nevents" in kwargs:
            self.nevents = kwargs["nevents"]
        else:
            self.nevents = -1
                        
        if "seed" in kwargs:
            self.seed = kwargs["seed"]
        else:
            self.seed = 1
            
        if "ignore_returncode" in kwargs:
            self.ignore_returncode = kwargs["ignore_returncode"]
        else:
            self.ignore_returncode = False

    def execute(self, log_out, log_err):
        
        cl = [self.command]
        cl.extend(self.cmd_args())
                                  
        logger.info("executing '%s' with command '%s'" % (self.name, ' '.join(cl)))
        start = time.time()
        proc = subprocess.Popen(cl, shell=False, stdout=log_out, stderr=log_err)
        proc.communicate()
        end = time.time()
        elapsed = end - start
        logger.info("execution of '%s' took %d second(s)" % (self.name, elapsed))

        if not self.ignore_returncode and proc.returncode:
            raise Exception("Component: error code %d returned by '%s'" % (proc.returncode, self.name))

    def cmd_exists(self):
        return subprocess.call("type " + self.command, shell=True, 
            stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

    def cmd_args(self):
        return self.args
    
    def setup(self):
        pass

    def cleanup(self):
        pass
         
