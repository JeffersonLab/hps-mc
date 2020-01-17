from hpsmc.job import Job
from hpsmc.tools import JobManager

job = Job(name="Job Manager Test")
job.initialize()
params = job.params

mgr = JobManager(steering_file=params.steering_file,
                 inputs=params.input_files)
job.components = [mgr]
job.run()
