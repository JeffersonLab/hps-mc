"""
"""

from hpsmc.tools import JobManager

job.description = 'LCIO to Hit Residuals'

recon = JobManager(steering='hitres')

job.add([recon])
