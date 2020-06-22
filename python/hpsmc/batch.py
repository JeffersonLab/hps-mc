"""Provides a set of scripts for submitting batch jobs including support for
local running, a multiprocessing pool, LSF, and Auger."""

import argparse, json, os, subprocess, getpass, sys, time, logging, signal, multiprocessing
import psutil
import xml.etree.ElementTree as ET
from xml.dom import minidom
from xml.sax.saxutils import unescape
from distutils.spawn import find_executable
from multiprocessing import Queue

from job_store import JobStore
from script_db import JobScriptDatabase

logger = logging.getLogger("hpsmc.batch")
logger.setLevel(logging.DEBUG)

run_script = os.path.join(os.path.dirname(os.path.realpath(__file__)), 'job.py')

class Batch:
    """Generic interface to a batch system."""
    
    def parse_args(self, args):
        """Parse command line arguments and perform setup."""
        
        parser = argparse.ArgumentParser("batch.py",
                                         epilog='Available scripts: %s' % ', '.join(JobScriptDatabase().get_script_names()))
        parser.add_argument("-c", "--config-file", nargs='?', help="Config file", action='append')
        parser.add_argument("-l", "--log-dir", nargs='?', help="Log file output dir", required=False, default=os.getcwd())
        parser.add_argument("-d", "--run-dir", nargs='?', help="Base run dir for the jobs (ignored for Auger and LSF)")
        parser.add_argument("-q", "--queue", nargs='?', help="Job queue for submission (e.g. 'long' or 'medium' at SLAC or 'simulation' at JLAB)", required=False)
        parser.add_argument("-W", "--job-length", type=int, help="Max job length in hours", required=False, default=48)
        parser.add_argument("-p", "--pool-size", type=int, help="Job pool size (only applicable when running pool)", required=False, default=multiprocessing.cpu_count())
        parser.add_argument("-m", "--memory", type=int, help="Max job memory allocation in MB (Auger)", default=1000)
        parser.add_argument("-e", "--email", nargs='?', help="Your email address if you want to get job system emails (default is off)", required=False)
        parser.add_argument("-D", "--debug", action='store_true', help="Enable debug settings", required=False)   
        parser.add_argument("-o", "--check-output", action='store_true', required=False, help="Do not submit jobs where output files already exist")
        parser.add_argument("-s", "--job-steps", type=int, default=-1, required=False)
        parser.add_argument("-r", "--job-range", nargs='?', help="Submit jobs numbers within range (e.g. '1:100')", required=False)
        parser.add_argument("-O", "--os", nargs='?', help="Operating system of batch nodes (Auger and LSF)")
        parser.add_argument("script", nargs='?', help="Name of job script")
        parser.add_argument("jobstore", nargs='?', help="Job store in JSON format")
        parser.add_argument("jobids", nargs="*", type=int, help="List of individual job IDs to submit (optional)")
            
        cl = parser.parse_args(args)
 
        if cl.script is None:
            raise Exception('The script is a required argument.')                
        self.script_name = cl.script # Name of script    
        script_db = JobScriptDatabase()
        if not script_db.exists(self.script_name):
            raise Exception('The script name is not valid: %s' % self.script_name)
        self.script = script_db.get_script_path(self.script_name) # Path to script
        if not os.path.isfile(self.script):
            raise Exception('The job script does not exist: %s' % self.script)       

        if cl.jobstore is None:
            raise Exception('The job store file is a required argument.')
        if not os.path.isfile(cl.jobstore):
            raise Exception('The job store does not exist: %s' % cl.jobstore)
        self.jobstore = JobStore(cl.jobstore)
         
        self.email = cl.email
             
        self.debug = cl.debug
        
        self.log_dir = cl.log_dir
        if not os.path.isabs(self.log_dir):
            raise Exception('The log dir is not an abs path: %s' % self.log_dir)
        # FIXME: This probably shouldn't happen here.
        if not os.path.exists(self.log_dir):
            logger.info('Creating log dir: %s' % self.log_dir)
            os.makedirs(self.log_dir)
                   
        self.check_output = cl.check_output
                    
        if cl.jobids:                    
            self.job_ids = map(int, cl.jobids)
        else:
            self.job_ids = []
        
        self.job_steps = cl.job_steps
        
        if cl.job_range:
            toks = cl.job_range.split(':')
            if len(toks) != 2:
                raise ValueError('Bad format for job range: ' + cl.job_range)
            self.start_job_num = int(toks[0])
            self.end_job_num = int(toks[1])
            if self.start_job_num > self.end_job_num:
                raise ValueError("The start job number must be >= the end job num when using a range.")
            if self.start_job_num < 0 or self.end_job_num < 0:
                raise ValueError("The job range numbers must be > 0.")
        else:
            self.start_job_num = None
            self.end_job_num = None
                    
        self.queue = cl.queue
        self.os = cl.os

        self.job_length = cl.job_length         
        self.memory = cl.memory
                    
        self.pool_size = int(cl.pool_size)
        
        if cl.config_file:
            self.config_files = map(os.path.abspath, cl.config_file)
        else:
            self.config_files = []
                 
        self.run_dir = cl.run_dir
                
        return cl
        
    def submit_cmd(self, job_id, job_data):
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
                logger.warning('Job output does not exist: %s' % (dest))
                return False
        return True
    
    def get_filtered_job_ids(self):
        """
        Get a list of job IDs to submit based on parsed command line options.
        """        
        submit_ids = self.jobstore.get_job_ids()
        if self.start_job_num:
            submit_ids = [id for id in submit_ids 
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
            logger.warning('Skipping submission for job because outputs already exist: %s' % job_id)
        else:
            batch_id = self.submit_cmd(job_id, job)
            logger.info("Submitted job %s with batch ID %s" % (job_id, str(batch_id)))
        return batch_id
    
    def _submit_jobs(self, job_ids):
        """Submit the jobs with the specified job IDs to the batch system."""
        for job_id in job_ids:
            if not self.jobstore.has_job_id(job_id):
                raise Exception('Job ID was not found in job store: %s' % job_id)
            job_data = self.jobstore.get_job(job_id)
            self._submit_job(job_id, job_data)
                    
    def build_cmd(self, job_id, job_params, set_job_dir=True):
        """
        Create the command to run a single job.
        
        This generically creates the command to run the job locally but subclasses
        may override if necessary.
        """
        cmd = ['python', run_script, 'run']
        cmd.extend(['-o', os.path.join(self.log_dir, 'job.%d.out' % job_id),
                    '-e', os.path.join(self.log_dir, 'job.%d.err' % job_id),
                    '-l', os.path.join(self.log_dir, 'job.%d.log' % job_id)
                    ])
        if set_job_dir:
            job_dir = os.path.join(self.run_dir, str(job_id))
            cmd.extend(['-d', job_dir])
        if len(self.config_files):
            for cfg in self.config_files:
                cmd.extend(['-c', cfg])
        if self.job_steps > 0:
            cmd.extend(['--job-steps', str(self.job_steps)])
        cmd.extend(['-i', str(job_id)])
        cmd.append(self.script)
        cmd.append(os.path.abspath(self.jobstore.path))
        logger.debug("Job command: %s" % " ".join(cmd))
        return cmd  
            
class LSF(Batch):
    """Submit LSF batch jobs."""
    
    def __init__(self):
        os.environ["LSB_JOB_REPORT_MAIL"] = "N"
        
    def parse_args(self, args):
        Batch.parse_args(self, args)
        if self.email:
            os.environ["LSB_JOB_REPORT_MAIL"] = "Y"

    def build_cmd(self, name, job_params):
        log_file = os.path.abspath(os.path.join(self.log_dir, 'job.%s.log' % str(name)))
        
        queue = self.queue
        if queue is None:
            queue = 'long'
        
        if self.os is not None:
            lsf_os = self.os
        else:
            lsf_os = 'centos77'
        
        cmd = ['bsub', 
               '-W', str(self.job_length) + ':0', 
               '-R', lsf_os, 
               '-q', queue,
               '-o', log_file,
               '-e', log_file]
        cmd.extend(Batch.build_cmd(self, name, job_params, set_job_dir=False))
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

class Auger(Batch):
    """Submit Auger batch jobs."""

    def __init__(self):
        self.setup_script = find_executable('hps-mc-env.csh') 
        if not self.setup_script:
            raise Exception("Failed to find 'hps-mc-env.csh' in environment.")

    def submit(self):
        """Primary batch submission method for Auger."""

        job_ids = self.get_filtered_job_ids()
        logger.info('Submitting jobs: %s' % str(job_ids))
        req = self._create_req(self.script_name) # create request XML header
        for job_id in job_ids:
            if not self.jobstore.has_job_id(job_id):
                raise Exception('Job ID was not found in job store: %s' % job_id)
            job_params = self.jobstore.get_job(job_id)
            if self.check_output and Batch._outputs_exist(job_params):
                logger.warning("Skipping Auger submission for job " 
                               "because outputs already exist: %d" % job_id)
            else:
                self._add_job(req, job_params) # add job to request
        xml_filename = self._write_req(req)    # write request to file
        auger_ids = self._jsub(xml_filename)   # execute jsub to submit jobs
        logger.info("Submitted Auger jobs: %s" % str(auger_ids))

    def _jsub(self, xml_filename):
        cmd = ['jsub', '-xml', xml_filename]
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
        out, err = proc.communicate()
        auger_ids = self._get_auger_ids(out)
        return auger_ids 

    def _get_auger_ids(self, out):
        auger_ids = []
        for line in out.splitlines():
            if line.strip().startswith('<jsub>'):
                j = ET.fromstring(line)
                for req in j.getchildren():
                    for child in req.getchildren():
                        if child.tag == 'jobIndex':
                            auger_id = int(child.text)
                            auger_ids.append(auger_id)
                        elif child.tag == 'error':
                            # Submission failed so raise an exception with the error msg
                            raise Exception(child.text)
                break
        return auger_ids

    def _write_req(self, req, filename='temp.xml'):
        pretty = unescape(minidom.parseString(ET.tostring(req)).toprettyxml(indent = "  "))
        with open(filename, 'w') as f:
            f.write(pretty)
            return f.name

    def _create_req(self, req_name):
        req = ET.Element("Request")
        name_elem = ET.SubElement(req, "Name")
        name_elem.set("name", req_name)
        prj = ET.SubElement(req, "Project")
        prj.set("name", "hps")
        trk = ET.SubElement(req, "Track")
        if self.debug:
            # Queue arg is not used when debug flag is active.
            trk.set("name", "debug")
        else:
            # Queue name is used to set job track.
            queue = 'simulation'
            if self.queue is not None:
                queue = self.queue
            trk.set("name", queue)
        if self.email:
            email = ET.SubElement(req, "Email")
            email.set("email", self.email)
            email.set("request", "true")
            email.set("job", "true")
        mem = ET.SubElement(req, "Memory")
        mem.set("space", str(self.memory))
        mem.set("unit", "MB")
        limit = ET.SubElement(req, "TimeLimit")        
        limit.set("time", str(self.job_length))
        limit.set("unit", "hours")
        os_elem = ET.SubElement(req, "OS")
        if self.os is not None:
            auger_os = self.os
        else:
            auger_os = 'general'
        os_elem.set("name", os) 
        return req

    def build_cmd(self, job_id, job_params):
        cmd = ['python', run_script, 'run']
        #cmd.extend(['-d', '/scratch/%d' % job_id])
        if len(self.config_files):
            for cfg in self.config_files:
                cmd.extend(['-c', cfg])
        if self.job_steps > 0:
            cmd.extend(['--job-steps', str(self.job_steps)])
        cmd.extend(['-i', str(job_id)])
        cmd.append(self.script)
        cmd.append(os.path.abspath(self.jobstore.path))
        logger.debug("Job command: %s" % " ".join(cmd))
        return cmd  
  
    def _add_job(self, req, job_params):
        job = ET.SubElement(req, "Job")
        job_id = job_params['job_id']
        inputfiles = job_params["input_files"]
        for src,dest in inputfiles.iteritems():
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
        stdout_file = os.path.abspath(os.path.join(self.log_dir, "job.%d.out" % job_id))
        stderr_file = os.path.abspath(os.path.join(self.log_dir, "job.%d.err" % job_id))
        job_err.set("dest", stderr_file)
        job_out = ET.SubElement(job, "Stdout")
        job_out.set("dest", stdout_file)
                
        cmd = ET.SubElement(job, "Command")
        cmd_lines = []
        cmd_lines.append("<![CDATA[")
        cmd_lines.append('pwd')
        cmd_lines.append('\n')
        cmd_lines.append("source %s" % os.path.realpath(self.setup_script))
        cmd_lines.append('\n')

        job_cmd = self.build_cmd(job_id, job_params)
        
        if self.job_steps > 0:
            job_cmd = job_cmd + " --job-steps " + str(self.job_steps)
        cmd_lines.extend(job_cmd)
       
        cmd_lines.append('\n')
        cmd_lines.append('ls -lah .')

        cmd_lines.append("]]>")
        #print(cmd_lines)
        cmd.text = ' '.join(cmd_lines)

class Local(Batch):
    """Run a local batch jobs sequentially."""
            
    def parse_args(self, args):
        Batch.parse_args(self, args)

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
                logger.error("Local execution of '%s' returned error code: %d" % (name, proc.returncode))

# Queue used to keep track of processes created by batch pool.
mp_queue = Queue()

def run_job_pool(cmd):
    """Run the command in a new process which is added to a global MP queue."""
    try:
        sys.stdout.flush()
        proc = subprocess.Popen(cmd, preexec_fn=os.setsid)
        mp_queue.put(proc)
        proc.wait()
        returncode = proc.returncode
    except subprocess.CalledProcessError as e:
        logger.error(str(e))
        sys.stdout.flush()
        pass
    return returncode

class KillProcessQueue():
    """Kills process objects from an MP queue after exit from a with statement."""
    
    def __init__(self, mp_queue):
        self.mp_queue = mp_queue
    
    def __enter__(self):
        return self
    
    def __exit__(self, type, val, tb):
        logger.debug('Killing %d processes in queue ...' % mp_queue.qsize())
        while True:
            proc = mp_queue.get()
            logger.debug('Checking process %d' % proc.pid)
            if proc.returncode is None:
                try:
                    parent = psutil.Process(proc.pid)
                    for child in parent.children(recursive=True):
                        logger.debug('Killing child process %d' % child.pid)
                        child.kill()
                    logger.debug('Killing parent process %s' % parent.pid)
                    parent.kill()
                except:
                    pass
            else:
                logger.debug('Process %d already finished with returncode %d' % (proc.pid, proc.returncode))
                                
            if mp_queue.empty():
                break
            
        logger.debug('Done killing processes!')
                
class Pool(Batch):
    """
    Run a set of jobs in a local pool using Python's multiprocessing module.    
    """
                            
    def submit(self):
        """Submit jobs to a local processing pool.
        
        This method will not return until all jobs are finished or execution
        is interrupted.
        """
                
        cmds = []
        for job_id in self.get_filtered_job_ids():
            job_data = self.jobstore.get_job(job_id)
            cmd = self.build_cmd(job_id, job_data)            
            cmds.append(cmd)
        logger.debug('Running job commands in pool ...')
        logger.debug('\n'.join([' '.join(cmd) for cmd in cmds]))
     
        if not len(cmds):
            raise Exception('No job IDs found to submit')

        # Run jobs in an MP pool and cleanup child processes on exit        
        with KillProcessQueue(mp_queue):
            original_sigint_handler = signal.signal(signal.SIGINT, signal.SIG_IGN)
            pool = multiprocessing.Pool(self.pool_size)
            signal.signal(signal.SIGINT, original_sigint_handler)      
            try:
                logger.info("Running %d jobs in pool ..." % len(cmds))
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

if __name__ == '__main__':
    system_dict = {
        "lsf": LSF,
        "auger": Auger,
        "local": Local,
        "pool": Pool
    }
    if len(sys.argv) > 1:
         system = sys.argv[1].lower()
         if system not in system_dict.keys():
             raise Exception("The batch system '%s' is not valid." % system)
         batch = system_dict[system]()
         args = sys.argv[2:]
         batch.parse_args(args)
         batch.submit()
    else:
        print("Usage: batch.py [system] [args]")
        print("    available systems: %s" % ', '.join(system_dict.keys()))
           
