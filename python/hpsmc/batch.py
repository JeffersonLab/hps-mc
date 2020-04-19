import argparse, json, os, subprocess, getpass, sys, time, logging, signal, multiprocessing
import xml.etree.ElementTree as ET
from xml.dom import minidom
from xml.sax.saxutils import unescape
from distutils.spawn import find_executable

from job_store import JobStore

logger = logging.getLogger("hpsmc.batch")

class Batch:
    """Generic interface to a batch system."""
    
    def parse_args(self):
        """Parse command line arguments and perform setup."""
        
        parser = argparse.ArgumentParser("Batch system command line interface")
        parser.add_argument("-e", "--email", nargs='?', help="Your email address", required=False)
        parser.add_argument("-D", "--debug", action='store_true', help="Enable debug settings", required=False) 
        parser.add_argument("-l", "--log-dir", nargs=1, help="Log file output dir", required=False)
        parser.add_argument("-o", "--check-output", action='store_true', required=False)
        parser.add_argument("-s", "--job-steps", type=int, default=-1, required=False)
        parser.add_argument("-r", "--job-range", help="Submit jobs numbers within range (e.g. '1:100')", required=False)
        parser.add_argument("-q", "--queue", nargs=1, help="Job queue for submission (e.g. 'long' or 'medium' at SLAC)", required=False)
        parser.add_argument("-W", "--job-length", nargs=1, help="Max job length in hours", required=False)
        parser.add_argument("-p", "--pool-size", nargs=1, help="Job pool size (only applicable to local job pool)", required=False)
        parser.add_argument("-c", "--config-file", nargs=1, help="Config file", required=False)
        parser.add_argument("-d", "--run-dir", nargs=1, help="Base run dir for the jobs")
        parser.add_argument("script", nargs=1, help="Full path to job script")
        parser.add_argument("jobstore", nargs=1, help="Job store in JSON format")
        parser.add_argument("jobids", nargs="*", type=int, help="List of individual job IDs to submit (optional)")
        cl = parser.parse_args()

        if not cl.jobstore: 
            raise Exception('The job store is a required argument.')
        if not os.path.isfile(cl.jobstore[0]):
            raise Exception("The job store '%s' does not exist." % cl.jobstore[0])
        self.jobstore = JobStore(cl.jobstore[0])

        if cl.script:
            self.script = cl.script[0]
            if not os.path.isfile(self.script):
                raise Exception("The job script '%s' does not exist." % self.script)       
        else:
            raise Exception('The job script is a required argument.')
         
        if cl.email:
            self.email = cl.email
        else:
            self.email = None
            
        self.debug = cl.debug
        
        if cl.log_dir:
            self.log_dir = cl.log_dir[0]
        else:
            self.log_dir = None
        #else:
        #    self.log_dir = os.getcwd()        
        #try:
        #    os.stat(self.log_dir)
        #except:
        #    os.makedirs(self.log_dir)
            
        self.check_output = cl.check_output
                    
        if cl.jobids:                    
            self.job_ids = map(int, cl.jobids)
        else:
            self.job_ids = []
        
        self.job_steps = cl.job_steps
        
        if cl.job_range:
            toks = cl.job_range.split(':')
            if len(toks) != 2:
                raise ValueError("Bad format for job range: " + cl.job_range)
            self.start_job_num = int(toks[0])
            self.end_job_num = int(toks[1])
            if self.start_job_num > self.end_job_num:
                raise ValueError("The start job number must be >= the end job num when using a range.")
            if self.start_job_num < 0 or self.end_job_num < 0:
                raise ValueError("The job range numbers must be > 0.")
        else:
            self.start_job_num = None
            self.end_job_num = None
                    
        if cl.queue:
            self.queue = cl.queue[0]
        else:
            self.queue = "long"

        if cl.job_length:
            self.job_length = int(cl.job_length[0])
        else:
            self.job_length = 48
            
        if cl.pool_size:
            self.pool_size = int(cl.pool_size[0])
        else:
            self.pool_size = 8 
            
        if cl.config_file:
            self.config_file = cl.config_file[0]
        else:
            self.config_file = None
        
        if cl.run_dir:
            self.run_dir = cl.run_dir[0]
        else:
            self.run_dir = os.getcwd()
                
        return cl
        
    def submit_cmd(job_id, job_data):
        """
        Submit a single batch job and return the batch ID.
        Must be implemented by derived classes for a specific batch system.
        """
        return NotImplementedError
    
    @staticmethod
    def _outputs_exist(job):
        """Check if job outputs exist.  Return False when first missing output is found."""
        for src,dest in job["output_files"].iteritems():
            if not os.path.isfile(os.path.join(job["output_dir"], dest)):
                print "Job output '%s' does not exist." % (dest)
                return False
        return True
    
    def get_filtered_job_ids(self):
        """
        Get a list of job IDs to submit based on parsed command line options.
        """        
        submit_ids = self.jobstore.get_job_ids()
        if self.start_job_num:            
            submit_ids = [id for id in all_job_ids 
                          if int(id) >= self.start_job_num and int(id) <= self.end_job_num]
        elif len(self.job_ids):
            submit_ids = self.job_ids
        return submit_ids
        
    def submit(self):
        """
        Primary public method for submitting jobs after args are parsed.
        """
        job_ids = self.get_filtered_job_ids()
        logger.info('Submitting jobs: %s' % str(job_ids))
        self._submit_jobs(job_ids)
        
    def _submit_job(self, job_id, job):
        """Submit a single job to the batch system."""
        batch_id = None
        if self.check_output and Batch._outputs_exist(job):
            logger.warning("Skipping submission for job %s because outputs already exist!" % job_id)
        else:
            batch_id = self.submit_cmd(job_id, job)
            logger.info("Submitted job %s with batch ID %s" % (job_id, str(batch_id)))
        return batch_id
    
    def _submit_jobs(self, job_ids):
        """Submit the jobs with the specified job IDs to the batch system."""
        for job_id in job_ids:
            if not self.jobstore.has_job_id(job_id):
                raise Exception('Job ID %s was not found in job store' % job_id)
            job_data = self.jobstore.get_job(job_id)
            self._submit_job(job_id, job_data)
                    
    def build_cmd(self, job_id, job_params):
        """
        Create the command to run a single job.
        
        This generically creates the command to run the job locally but subclasses
        may override if necessary.
        """
        job_dir = os.path.join(self.run_dir, str(job_id))
        if self.log_dir is not None:
            # User specified log dir
            log_dir = self.log_dir
        else:
            # Log files go into the job dir
            log_dir = job_dir
        cmd = ['python', self.script,
               '-o', os.path.join(log_dir, 'job.%d.out' % job_id),
               '-e', os.path.join(log_dir, 'job.%d.err' % job_id)]
        cmd.extend(['-d', job_dir])
        if self.config_file:
            cmd.extend(['-c', self.config_file])
        if self.job_steps > 0:
            cmd.extend(['--job-steps', str(self.job_steps)])
        cmd.extend(['-i', str(job_id)])
        cmd.append(os.path.abspath(self.jobstore.path))
        logger.info("Job command: %s" % " ".join(cmd))
        return cmd  
            
class LSF(Batch):
    """Submit LSF batch jobs."""
    
    def __init__(self):
        os.environ["LSB_JOB_REPORT_MAIL"] = "N"
        
    def parse_args(self):
        Batch.parse_args(self)
        if self.email:
            os.environ["LSB_JOB_REPORT_MAIL"] = "Y"

    def build_cmd(self, name, job_params):
        log_file = os.path.abspath(os.path.join(self.log_dir, name+".log"))
        cmd = ["bsub", "-W", str(self.job_length) + ":0", "-q", self.queue, "-o", log_file, "-e", log_file]
        cmd.extend(Batch.build_cmd(self, name, job_params))        
        return cmd

    def submit_cmd(self, name, job_params): 
        cmd = self.build_cmd(name, job_params)
        logger.info('Submitting job %s to LSF with command: %s' % (name, ' '.join(cmd)))
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        out,err = proc.communicate()
        if err != None and len(err):
            raise Exception("Submit error: %s" % err)
        tokens = out.split(' ')
        if tokens[0] != 'Job':
            raise Exception('Unexpected output from bsub command: %s' % out)
        batch_id = int(tokens[1].replace('<', '').replace('>', ''))
        return batch_id

# FIXME: Probably this class is completely broken.
class Auger(Batch):
    """Manage Auger batch jobs."""

    def __init__(self):
        # TODO: Get this from config info
        # FIXME: Can this use the bash script instead?
        self.setup_script = find_executable('hps-mc-env.csh') 
        if not self.setup_script:
            raise Exception("Failed to find 'hps-mc-env.csh' in environment.")
   
    def build_job_files(self, name, job_params):

        param_file = os.path.join(self.workdir, name + ".json")

        req = ET.Element("Request")
        req_name = ET.SubElement(req, "Name")
        req_name.set("name", name)
        prj = ET.SubElement(req, "Project")
        prj.set("name", "hps")
        trk = ET.SubElement(req, "Track")
        if self.debug:
            trk.set("name", "debug")
        else:
            trk.set("name", "simulation")
        if self.email:
            email = ET.SubElement(req, "Email")
            email.set("email", self.email)
            email.set("request", "true")
            email.set("job", "true")
        mem = ET.SubElement(req, "Memory")
        mem.set("space", "2000")
        mem.set("unit", "MB")
        limit = ET.SubElement(req, "TimeLimit")
        if self.debug:
            limit.set("time", "4")
        else:
            limit.set("time", "24")
        limit.set("unit", "hours")
        os_elem = ET.SubElement(req, "OS")
        os_elem.set("name", "centos7")

        job = ET.SubElement(req, "Job")
        inputfiles = job_params["input_files"]
        for dest,src in inputfiles.iteritems():
            input_elem = ET.SubElement(job, "Input")
            input_elem.set("dest", dest)
            if src.startswith("/mss"):
                src_file = "mss:%s" % src
            else:
                src_file = src
            input_elem.set("src", src_file)
        outputfiles = job_params["output_files"]
        outputdir = job_params["output_dir"]
        outputdir = os.path.realpath(outputdir)
        for src,dest in outputfiles.iteritems():
            output_elem = ET.SubElement(job, "Output")
            output_elem.set("src", src)
            dest_file = os.path.join(outputdir, dest)
            if dest_file.startswith("/mss"):
                dest_file = "mss:%s" % dest_file
            output_elem.set("dest", dest_file)
        job_err = ET.SubElement(job, "Stderr")
        log_file = os.path.abspath(os.path.join(self.log_dir, name+".log"))
        job_err.set("dest", log_file)
        job_out = ET.SubElement(job, "Stdout")
        job_out.set("dest", log_file)
                
        cmd = ET.SubElement(job, "Command")
        cmd_lines = []
        cmd_lines.append("<![CDATA[")
        cmd_lines.append("source %s" % os.path.realpath(self.setup_script))
        
        job_cmd = self.build_cmd(name, job_params)
        
        if self.job_steps > 0:
            job_cmd = job_cmd + " --job-steps " + str(self.job_steps)
        cmd_lines.append(job_cmd)
        cmd_lines.append("]]>")
        cmd.text = '\n'.join(cmd_lines)

        with open(param_file, "w") as jobfile:
             json.dump(job_params, jobfile, indent=2, sort_keys=True)
        print "Wrote job param file '%s'" % (param_file)

        pretty = unescape(minidom.parseString(ET.tostring(req)).toprettyxml(indent = "  "))
        xml_file = os.path.join(self.workdir, name+".xml")
        with open(xml_file, "w") as f:
            f.write(pretty)
        print "Wrote Auger XML '%s'" % xml_file

        return param_file, xml_file

    def submit_cmd(self, name, job_params):
        param_file,xml_file = self.build_job_files(name, job_params)
        cmd = ['jsub', '-xml', xml_file]
        #print(' '.join(cmd))
        if not self.dryrun:
            proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
            out, err = proc.communicate()
            print out
            jobid = None
            if "<jobIndex>" in out:
                jobid = int(out[out.find("<jobIndex>")+10:out.find("</jobIndex>")].strip())
            return jobid
        else:
            print "Job %s was not submitted." % name
            return None
        print        

class Local(Batch):
    """Run a local batch job on the current system."""
            
    def parse_args(self):
        Batch.parse_args(self)

    def submit_cmd(self, name, job_params):
        """Run a single job locally."""
#        log_out = file(os.path.abspath(os.path.join(self.log_dir, name+".log")), 'w')
        cmd = self.build_cmd(name, job_params)
        if self.submit:
            logger.info("Executing local job %s" % name)
            proc = subprocess.Popen(cmd, shell=False)
            proc.communicate()
#            log_out.close()
            if proc.returncode:
               logger.warn("Local execution of '%s' returned error code %d" % (name, proc.returncode))

def run_job_pool(cmd):
    sys.stdout.flush()
    returncode = subprocess.call(cmd)
    try:
        sys.stdout.flush()
        returncode = subprocess.call(cmd, stdout=log,stderr=log)
    except subprocess.CalledProcessError as e:
        print(str(e))
        sys.stdout.flush()
        pass
    return returncode
                
class Pool(Batch):
    """
    Run a set of jobs in a local pool using Python's multiprocessing module.    
    """
                        
    def submit(self):
                
        cmds = []
        for job_id in self.get_filtered_job_ids():
            job_data = self.jobstore.get_job(job_id)
            cmd = self.build_cmd(job_id, job_data)
            cmds.append(cmd)
        print(cmds)
        
        if not len(cmds):
            raise Exception('No job IDs found to submit')
                            
        original_sigint_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
        pool = multiprocessing.Pool(self.pool_size)
        signal.signal(signal.SIGINT, original_sigint_handler)      
        try:
            res = pool.map_async(run_job_pool, cmds)
            # timeout must be properly set, otherwise tasks will crash
            logger.info("Pool results: " + str(res.get(sys.maxint)))
            logger.info("Normal termination")
            pool.close()
            pool.join()
        except KeyboardInterrupt:
            logger.fatal("Caught KeyboardInterrupt, terminating workers")
            pool.terminate()
        except Exception as e:
            logger.fatal("Caught Exception '%s', terminating workers" % (str(e)))
            pool.terminate()
        except: # catch *all* exceptions
            e = sys.exc_info()[0]
            logger.fatal("Caught non-Python Exception '%s'" % (e))
            pool.terminate()
