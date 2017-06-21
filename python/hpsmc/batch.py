import argparse, json, os, subprocess, getpass
import xml.etree.ElementTree as ET
from xml.dom import minidom
from xml.sax.saxutils import unescape
from distutils.spawn import find_executable

from workflow import Workflow
from db import Database

class Batch:
    
    def parse_args(self):
        parser = argparse.ArgumentParser("Submit batch jobs")
        parser.add_argument("--email", nargs='?', help="Your email address", default=getpass.getuser()+"@jlab.org")
        parser.add_argument("--debug", action='store_true', help="Enable debug settings")
        parser.add_argument("--no-submit", action='store_true', help="Do not actually submit the jobs")
        parser.add_argument("--job-ids", nargs="*", default=[], help="List of job IDs to submit")
        parser.add_argument("--work-dir", nargs=1, help="Work dir where JSON and XML files will be saved")
        parser.add_argument("--log-dir", nargs=1, help="Log file output dir")
        parser.add_argument("--check-output", action='store_true')
        parser.add_argument("--job-steps", type=int, default=-1)
        parser.add_argument("--database", help="Name of SQLite db file")
        parser.add_argument("script", nargs=1, help="Python job script")
        parser.add_argument("jobstore", nargs=1, help="Job store in JSON format")
        cl = parser.parse_args()

        if not os.path.isfile(cl.jobstore[0]):
            raise Exception("The job store file '%s' does not exist." % cl.jobstore[0])
        self.workflow = Workflow(cl.jobstore[0])
        self.script = cl.script[0]
        if not os.path.isfile(self.script):
            raise Exception("The script '%s' does not exist." % self.script)
        self.email = cl.email
        self.debug = cl.debug
        if cl.no_submit:
            self.submit = False
        else:
            self.submit = True

        if cl.work_dir:
            self.work_dir = cl.work_dir[0]
        else:
            self.work_dir = os.getcwd()
        try:
            os.stat(self.work_dir)
        except:
            os.makedirs(self.work_dir)

        if cl.log_dir:            
            self.log_dir = cl.log_dir[0]
        else:
            self.log_dir = os.getcwd()        
        try:
            os.stat(self.log_dir)
        except:
            os.makedirs(self.log_dir)
            
        self.check_output = cl.check_output
                    
        self.job_ids = map(int, cl.job_ids)
        
        self.job_steps = cl.job_steps
        
        if cl.database:
            self.db = Database(cl.database)
            self.db.connect()
        else:
            self.db = None
            
        self.sys = 'local'
        
    @staticmethod
    def outputs_exist(job):
        for o in job["output_files"]:
            if not os.path.exists(o):
                return False
        return True

    def submit_all(self):
        if self.db:
            prod_id = self.db.productions.select(self.workflow.name)[0]
        for k in sorted(self.workflow.jobs):
            job = self.workflow.jobs[k]
            job_id = job["job_id"]
            if not len(self.job_ids) or job_id in self.job_ids:
                if self.db:
                    job_rec = self.db.jobs.select(prod_id, job_id)
                if not self.check_output or not outputs_exist(job):
                    batch_id = self.submit_single(k, self.workflow.jobs[k])
                    if batch_id and self.db:
                        self.db.batch_jobs.insert(batch_id, job_id, prod_id, self.sys)
                else:
                    print "The output files for job %d already exist so submission is skipped." % job["job_id"]
                    
            self.db.commit()
    
    def submit_single(self, name, job_params):
        return None
    
    # TODO: get list of records from 'batch_jobs' table for the workflow
    def batch_jobs(self):
        raise Exception("Method not implemented.")

    # TODO: find active batch jobs that are in the current workflow, e.g. using 'bjobs -a' for LSF, and return their batch IDs and statuses
    def active_jobs(self):
        raise Exception("Method not implemented.")

    # TODO: kill all active batch jobs in the workflow and delete their db records
    def kill(self):
        raise Exception("Method not implemented.")

    # TODO: update the status fields in 'batch_jobs' e.g. using info from 'bjobs -a' for LSF
    def update(self):
        raise Exception("Method not implemented.")

    # TODO: check if the output files exist for every job in the workflow and print results
    def check_outputs(self):     
        raise Exception("Method not implemented.")

    # TODO: submitting jobs using a range of job IDs
    def submit_range(self, start, end):
        raise Exception("Method not implemented.")
    
class LSF(Batch):
    
    def __init__(self):
        os.environ["LSB_JOB_REPORT_MAIL"] = "N"
        self.sys = 'LSF'

    def build_cmd(self, name, job_params):
        param_file = os.path.join(self.work_dir, name + ".json")
        log_file = os.path.abspath(os.path.join(self.log_dir, name+".log"))
        cmd = ["bsub", "-W", "24:0", "-q", "long", "-o",  log_file, "-e",  log_file]
        #cmd.extend(["python", self.script, "-o", "job.out", "-e", "job.err", os.path.abspath(param_file)])
        cmd.extend(["python", self.script])        
        if self.job_steps > 0:
            cmd.extend(["--job-steps", str(self.job_steps)])
        cmd.append(os.path.abspath(param_file))
        #job_params["output_files"]["job.out"] = name+".out"
        #job_params["output_files"]["job.err"] = name+".err"
        with open(param_file, "w") as jobfile:
            json.dump(job_params, jobfile, indent=4, sort_keys=True)
        return cmd

    def submit_single(self, name, job_params):
        cmd = self.build_cmd(name, job_params)
        print ' '.join(cmd)
        if self.submit:            
            proc = subprocess.Popen(cmd, stdout=subprocess.PIPE)
            out,err = proc.communicate()
            if err != None:
                raise Exception("Submit error '%s'." % err)
            tokens = out.split(' ')
            if tokens[0] != 'Job':
                raise Exception("Unexpected output '%s' from bsub command." % out)
            batch_id = int(tokens[1].replace('<', '').replace('>', ''))
            return batch_id
        else:
            print "Job was not submitted."
            return None
    
class Auger(Batch):

    def __init__(self):
        self.setup_script = find_executable('hps-mc-env.csh') 
        if not self.setup_script:
            raise Exception("Failed to find 'hps-mc-env.csh' in environment.")
        self.sys = 'Auger'
   
    def build_job_files(self, name, job_params):

        param_file = os.path.join(self.work_dir, name + ".json")

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
             json.dump(job_params, jobfile, indent=4, sort_keys=True)
        print "wrote job param file '%s'" % (param_file)

        pretty = unescape(minidom.parseString(ET.tostring(req)).toprettyxml(indent = "    "))
        xml_file = os.path.join(self.work_dir, name+".xml")
        with open(xml_file, "w") as f:
            f.write(pretty)
        print "wrote Auger XML '%s'" % xml_file

        return param_file, xml_file

    # TODO: should return the Auger ID of the submitted job
    def submit_single(self, name, job_params):
        param_file,xml_file = self.build_job_files(name, job_params)

        cmd = ['jsub', '-xml', xml_file]
        print ' '.join(cmd)
        if self.submit:
            proc = subprocess.Popen(cmd, shell=False)
            proc.communicate()
        else:
            print "Job was not submitted."
        print
