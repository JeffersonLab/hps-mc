import argparse, json, os, subprocess
from workflow import Workflow

class Batch:
    
    def parse_args(self):
        parser = argparse.ArgumentParser("Submit batch jobs")
        parser.add_argument("script", nargs=1, help="Python job script")
        parser.add_argument("jobstore", nargs=1, help="Job store in JSON format")
        cl = parser.parse_args()

        self.workflow = Workflow(cl.jobstore[0])
        self.script = os.path.abspath(cl.script[0])

    def submit_all(self):
        for k in sorted(self.workflow.jobs):
            self.submit_single(k, self.workflow.jobs[k])
    
    def submit_single(self, name, job_params):
        pass
    
class LSF(Batch):
    
    def __init__(self):
        os.environ["LSB_JOB_REPORT_MAIL"] = "N"

    def build_cmd(self, name, job_params):
        param_file = name+".json"
        cmd = ["bsub", "-W", "24:0", "-q", "long", "-o",  os.path.abspath(name+".log"), "-e",  os.path.abspath(name+".log")]
        cmd.extend(["python", self.script, "-o", "job.out", "-e", "job.err", os.path.abspath(param_file)])
        job_params["output_files"]["job.out"] = name+".out"
        job_params["output_files"]["job.err"] = name+".err"
        with open(param_file, "w") as jobfile:
            json.dump(job_params, jobfile, indent=4, sort_keys=True)
        return cmd

    def submit_single(self, name, job_params):
        cmd = self.build_cmd(name, job_params)
        print ' '.join(cmd)
        proc = subprocess.Popen(cmd, shell=False)
        proc.communicate()
    
class Auger(Batch):
    
    def __init__(self):
        pass
