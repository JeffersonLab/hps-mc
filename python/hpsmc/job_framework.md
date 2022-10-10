Job framework {#jobframework}
=============

In hps-mc, tasks are carried out using jobs and components. Here, a job is a framework to which the components that carry the actual functionality are added. The main job class is defined in `job.py`, whereas `tools.py` and `generators.py` implement the different components.
Please refer to the class documentation for more specific information.

Jobs are set up in job scripts which are stored under `python/jobs`. These job scripts are responsible for creating, configuring, and adding the components to the job. For more information, please see @ref jobscripts and @ref examples.