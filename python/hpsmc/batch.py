import argparse, json, os, subprocess, getpass, sys, time
import xml.etree.ElementTree as ET
from xml.dom import minidom
from xml.sax.saxutils import unescape
from distutils.spawn import find_executable

from workflow import Workflow

class Batch:
    """Generic interface to a batch system."""
    
    def __init__(self):
        self.sys = 'local'
    
    def parse_args(self):
        """Parse command line arguments and perform setup."""
        
        parser = argparse.ArgumentParser("Batch system command line interface")
        parser.add_argument("-e", "--email", nargs='?', help="Your email address", required=False)
        parser.add_argument("-D", "--debug", action='store_true', help="Enable debug settings", required=False)
        parser.add_argument("-x", "--dryrun", action='store_true', help="Process the workflow but do not actually submit the jobs", required=False)        
        parser.add_argument("-l", "--log-dir", nargs=1, help="Log file output dir", required=False)
        parser.add_argument("-c", "--check-output", action='store_true', required=False)
        parser.add_argument("-s", "--job-steps", type=int, default=-1, required=False)
        parser.add_argument("-r", "--job-range", help="Submit jobs numbers within range (e.g. '1:100')", required=False)
        parser.add_argument("-n", "--nsub", type=int, default=-1, help="Number of jobs to submit before waiting when using staggered submission", required=False)
        parser.add_argument("-t", "--time", type=int, default=0, help="Number of seconds to wait before submitting next job set when using staggered submission", required=False)
        parser.add_argument("-q", "--queue", nargs=1, help="Job queue for submission (e.g. 'long' or 'medium' at SLAC)", required=False)
        parser.add_argument("-W", "--job-length", nargs=1, help="Max job length in hours", required=False)
        parser.add_argument("jobstore", nargs=1, help="Job store in JSON format")
        parser.add_argument("jobids", nargs="*", type=int, help="List of individual job IDs to submit (optional)")
        cl = parser.parse_args()
                
        if not os.path.isfile(cl.jobstore[0]):
            raise Exception("The job store file '%s' does not exist." % cl.jobstore[0])
        self.workflow = Workflow(cl.jobstore[0])
        
        self.script = self.workflow.job_script
        if not os.path.isfile(self.script):
            raise Exception("The script '%s' does not exist." % self.script)
        
        if cl.email:
            self.email = cl.email
        else:
            self.email = None
            
        self.debug = cl.debug
        
        self.dryrun = cl.dryrun        

        self.workdir = self.workflow.workdir

        try:
            os.stat(self.workdir)
        except:
            raise Exception("The work dir does not exist: " + self.workdir)

        if cl.log_dir:            
            self.log_dir = cl.log_dir[0]
        else:
            self.log_dir = os.getcwd()        
        try:
            os.stat(self.log_dir)
        except:
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
            
        self.jobs = self.workflow.get_jobs()          
        
        self.nsub = cl.nsub
        self.waittime = cl.time
        if self.nsub > 0 and self.waittime > 0:
            print "Job submission staggering is enabled."
            self.enable_staggered = True
        else:
            print "Job submission staggering is disabled."
            self.enable_staggered = False

        if cl.queue:
            self.queue = cl.queue[0]
        else:
            self.queue = "long"

        if cl.job_length:
            self.job_length = int(cl.job_length[0])
        else:
            self.job_length = 48
        
    @staticmethod
    def outputs_exist(job):
        """Check if job outputs exist.  Returns False when first missing output is found."""
        for src,dest in job["output_files"].iteritems():
            if not os.path.isfile(os.path.join(job["output_dir"], dest)):
                print "Job %d output '%s' does not exist." % (job["job_id"], dest)
                return False
        return True
        
    def submit(self):
        """
        Primary method for submitting jobs based on current command line arguments.
        """
        if self.start_job_num:
            if self.enable_staggered:
                # Submit a specified range of job IDs, waiting between job sets.
                self.submit_staggered(self.start_job_num, self.end_job_num)
            else:
                # Submit from a specified range of job IDs.
                self.submit_job_range(self.start_job_num, self.end_job_num)
        elif len(self.job_ids):
            # Submit jobs from a set of specified job IDs.  Submissions staggering not usable.
            self.submit_jobs(self.job_ids)
        else:
            if self.enable_staggered:
                # Submit all jobs, waiting between job sets.
                self.submit_staggered()
            else:
                # Submit all jobs in the workflow.
                self.submit_all()

    def submit_all(self):
        """Submit all jobs in the workflow to the batch system."""        
        for k in sorted(self.jobs):
            self.submit_job(k, self.jobs[k])
            
    def submit_staggered(self, job_start = 0, job_end = sys.maxint):
        """Submit range of jobs from the workflow to the batch system with a wait time between job sets."""
        print "Submitting %d jobs at a time and waiting %d seconds between submissions ..." % (self.nsub, self.waittime)
        submitted = 0
        for k in sorted(self.jobs):            
            job = self.jobs[k]
            id = job["job_id"]
            if id >= job_start and id <= job_end:
                self.submit_job(k, self.jobs[k])
                submitted += 1
            if submitted == self.nsub:
                print "Waiting for %d seconds before submitting next set of jobs ..." % self.waittime
                time.sleep(self.waittime)
                print "Done waiting!"
                submitted = 0
                    
    def submit_job(self, name, job):
        """Submit a single job to the batch system."""
        job_id = job["job_id"]
        res = None
        if not len(self.job_ids) or job_id in self.job_ids:
            if not self.check_output or not Batch.outputs_exist(job):
                batch_id = self.submit_cmd(name, job)                
                print "Submitted <%s> with batch ID <%d>" % (name, batch_id)
            else:
                print "Outputs for <%s> already exist so submit is skipped." % name
    
    def submit_jobs(self, job_ids):
        """Submit the jobs with the specified job IDs to the batch system."""
        for k in self.jobs:        
            if self.jobs[k]["job_id"] in self.job_ids:
                j = self.jobs[k]
                self.submit_job(k, j)
                
    def submit_job_range(self, start_job_num, end_job_num):
        """Submit jobs using a range of job IDs."""
        for k in sorted(self.jobs):            
            job = self.jobs[k]
            id = job["job_id"]
            if id >= self.start_job_num and id <= self.end_job_num:
                self.submit_job(k, job)
    
    def submit_cmd(name, jobparams):
        """
        Submit a single batch job and return the batch ID.
        Must be implemented by derived classes.
        """
        raise NotImplementedError
     
    def check_outputs(self):
        """Checks if job outputs exist and prints the results."""
        for k in sorted(self.jobs):
            j = self.jobs[k]
            job_id = j["job_id"]
            if not outputs_exist(j):
                print "Job <%d> outputs do not exist." % job_id
    
class LSF(Batch):
    """Manage LSF batch jobs."""
    
    def __init__(self):
        os.environ["LSB_JOB_REPORT_MAIL"] = "N"
        self.sys = 'LSF'
        
    def parse_args(self):
        Batch.parse_args(self)
        if self.email:
            os.environ["LSB_JOB_REPORT_MAIL"] = "Y"

    def build_cmd(self, name, job_params):
        param_file = os.path.join(self.workdir, name + ".json")
        log_file = os.path.abspath(os.path.join(self.log_dir, name+".log"))
        cmd = ["bsub", "-W", str(self.job_length) + ":0", "-q", self.queue, "-o",  log_file, "-e",  log_file]
        #cmd.extend(["python", self.script, "-o", "job.out", "-e", "job.err", os.path.abspath(param_file)])
        cmd.extend(["python", self.script])        
        if self.job_steps > 0:
            cmd.extend(["--job-steps", str(self.job_steps)])
        cmd.append(os.path.abspath(param_file))
        #job_params["output_files"]["job.out"] = name+".out"
        #job_params["output_files"]["job.err"] = name+".err"
        with open(param_file, "w") as jobfile:
            json.dump(job_params, jobfile, indent=2, sort_keys=True)
        return cmd

    def submit_cmd(self, name, job_params): 
        cmd = self.build_cmd(name, job_params)
        print ' '.join(cmd)
        if not self.dryrun:
            proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            out,err = proc.communicate()
            if err != None and len(err):
                raise Exception("Submit error '%s'." % err)
            tokens = out.split(' ')
            if tokens[0] != 'Job':
                raise Exception("Unexpected output '%s' from bsub command." % out)
            batch_id = int(tokens[1].replace('<', '').replace('>', ''))
            return batch_id
        else:
            return None
    
class Auger(Batch):
    """Manage Auger batch jobs."""

    def __init__(self):
        self.setup_script = find_executable('hps-mc-env.csh') 
        if not self.setup_script:
            raise Exception("Failed to find 'hps-mc-env.csh' in environment.")
        self.sys = 'Auger'
   
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
        cmd_lines.append("python %s %s" % (os.path.realpath(self.script), os.path.join(os.getcwd(), param_file)))
        cmd_lines.append("]]>")
        cmd.text = '\n'.join(cmd_lines)

        with open(param_file, "w") as jobfile:
             json.dump(job_params, jobfile, indent=2, sort_keys=True)
        print "Wrote job param file <%s>" % (param_file)

        pretty = unescape(minidom.parseString(ET.tostring(req)).toprettyxml(indent = "  "))
        xml_file = os.path.join(self.workdir, name+".xml")
        with open(xml_file, "w") as f:
            f.write(pretty)
        print "Wrote Auger XML <%s>" % xml_file

        return param_file, xml_file

    def submit_cmd(self, name, job_params):
        param_file,xml_file = self.build_job_files(name, job_params)
        cmd = ['jsub', '-xml', xml_file]
        print ' '.join(cmd)
        if not self.dryrun:
            proc = subprocess.Popen(cmd, shell=False)
            proc.communicate()
            # TODO: get ID of submitted job here
            return -1
        else:
            print "Job <%s> was not submitted." % name
            return None
        print        

class Local(Batch):
    """Run a local batch job on the current system."""
    
    def __init__(self):
        self.sys = 'local'
        
    def parse_args(self):
        Batch.parse_args(self)

    def build_cmd(self, name, job_params):
        param_file = os.path.join(self.workdir, name + ".json")
        with open(param_file, "w") as jobfile:
            json.dump(job_params, jobfile, indent=2, sort_keys=True)
        cmd = ["python", self.script, os.path.abspath(param_file)]
        if self.job_steps > 0:
            cmd.extend(["--job-steps", str(self.job_steps)])
        return cmd

    def submit_cmd(self, name, job_params):
        """Run a single job locally."""
        log_out = file(os.path.abspath(os.path.join(self.log_dir, name+".log")), 'w')
        cmd = self.build_cmd(name, job_params)
        if self.submit:
            print "Executing local job <%s>" % name
            proc = subprocess.Popen(cmd, shell=False, stdout=log_out, stderr=log_out)
            proc.communicate()
            log_out.close()
            if proc.returncode:
               print "ERROR: Local execution of <%s> returned error code: %d" + (name, proc.returncode)
            raise Exception("Local job execution failed.")
        else:
            return None

if __name__ == "__main__":
    batch = LSF()
    batch.parse_args()
    batch.submit() 
