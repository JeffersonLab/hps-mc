This note explains how to submit batch jobs using alspellms installation of hps-m on sdf. 

### Guide to Submitting Batch Jobs ###

*** A LARGE VARIETY OF JOB EXAMPLES AVAILABLE IN HPS-MC INSTALL: '/sdf/group/hps/users/alspellm/src/hps-mc/examples' ***
*** THIS GUIDE WILL REFERENCE ONE OF MY PERSONAL JOB DIRECTORIES: '/sdf/group/hps/users/alspellm/projects/collab_2021/ntuples/' *** 

To use my installation of hps-mc, add make a copy of this script '/sdf/group/hps/users/alspellm/src/hps-mc/install/bin/hps-mc-env.sh' and source it
inside of your '~/.bashrc'. 

1. The first step is to setting up the sytem to run jobs using hps-mc is producing the '~/.hpsmc' file, which sets all of the *system level* configuration parameters for all types of jobs. 
    1a. Copy this file '/sdf/group/hps/users/alspellm/hps_mc_config/.hpsmc_system_config' to '~/.hpsmc'.
    1b. This file tells hps-mc where to find the software involved in various types of jobs. You will want to edit the relevent paths to use your
        own installations of the software. For example, to use your version of hps-java, modify 'hps_java_bin_jar' under '[JobManager]' to point to your
        installation.

2. Create a job directory to prepare and submit your jobs, and cd to it.  

3. Inside of your job directory, create the **job level** configuration file '<job_dir>/.hpsmc'. 
   Do not confuse this with the system level '~/.hpsmc'. This **job level** config file adds additional configurations to the system level '~./hpsmc'.
   You must create this file inside of your job directory. 
    3a. Copy the example job level configuration file '/sdf/group/hps/users/alspellm/hps_mc_config/.hpsmc_local_config' to your job directory as '.hpsmc'.

4. Build your 'jobs.json' file with all of the relevant job configurations (Step 5 explains how to build these jobs automatically). This file contains a list of job configurations that will be submitted to the batch system, where each job has a unique job_id. 
    4a. Inspect the jobs.json file '/sdf/group/hps/users/alspellm/projects/collab_2021/ntuples/jobs.json'
    4b. All of the required parameters to run a job must be configured here. Use the many examples found in 
    '/sdf/group/hps/users/alspellm/src/hps-mc/examples' to determine what configuration parameters are required for your particular job. 
    4c. When hps-mc jobs are submitted to the batch system, each job runs inside of its own scratch directory (specified later). hps-mc renames the input
        and output files to some locally defined generic name (configured here). The locally named output file is saved inside the scratch directory. 
        hps-mc then copies this local output file to the specified output directory, with the specified output name, both configured in 'jobs.json'. 
    4d. Inside of "input_files": {}, specify the input file name on the left. When this job is submitted, hps-mc will locally rename this file to the
        "data_events.slcio" file-name on the right. 
    4e. Inside of "output_files":{}, the left side contains the hps-mc local output file-name "data_events.root". Define the actual output file-name on 
        the right side of the ':', such as "data_events_0.root". 
    4f. You must have an "output_files" entry for every output file you want to save for your job. An example is found
    '/sdf/group/hps/users/alspellm/src/hps-mc/examples/data_cnv/job.json'
    4g. Be careful about formatting this file. Pay attention to where you your commas go, and where they dont go. 
    4h. Notice that each job must have its own unique "job_id" to seed the batch jobs. 

5. Automatically build thousands of job configurations in your 'jobs.json' file using 'mkjobs.sh'!
    5a. The mkjobs script (for example:'/sdf/group/hps/users/alspellm/projects/collab_2021/ntuples/mkjobs.sh') takes a text file of input files, and 
        a job configuration template 'job.json.tmpl', to automatically create a job configuration for every input file. Each generated job configuration 
        will be assigned a unique "job_id" variable. 

        **There are other forms of 'mkjobs.sh' that generate n job configurations based on the script, such as 
        '/sdf/group/hps/users/alspellm/src/hps-mc/examples/tritrig/mkjobs.sh'**

    5b. The 'job.json.tmpl' file is a template that defines the job parameters for a particular job. 
        Open the example '/sdf/group/hps/users/alspellm/projects/collab_2021/ntuples/job.json.tmpl'. 
        Items appearing inside of double curly braces are variables that are assigned values by 'mkjobs.sh'
        The template variables are processed using jinja2.
            5b1. Any variable parameters you might want to iterate over, such as the target position, tracking algorithm, "{run_number}}" etc. 
                 are defined as lists inside of the 'vars.json' file, which is passed argument '-a vars.json' inside of 'mkjobs.sh'/ 
            5b2. The "{{input_files['data'][0]}}" variable in 'job.json.tmpl' is set by '-i data hps_14522_infiles.txt 1' in 'mkjobs.sh'.
            5b3. Under output_files, the output file name is set by the variable "job_id", which is generate automatically by 'mkjobs.sh'.

    5c. Run the 'mkjobs.sh' script to generate your 'jobs.json' file. The next step is to submit these jobs to the batch system using hps-mc.
    Inspect your jobs.json file to make sure your input and output file names, etc, look correct. 

6. Create the Scratch Directories to run your batch jobs!
    6a. Each hps-mc job submitted to the batch system must run inside of its own scratch directory. You have to create the parent scratch directory,
        and the individual job scratch directories will be automatically created within that parent dir. 
    6b. Do something along the lines of (for SDF), 'mkdir -p /scratch/alspellm/<job_name>/logs'.
        The 'logs' directory is required to store job logs.

7. Create the 'sh/' directory inside of your job directory. This step is required. It stores the run commands generated by hps-mc. 

8. Use 'hps-mc-batch' to submit your jobs to the batch system via a submission script, such as '/sdf/group/hps/users/alspellm/projects/collab_2021/ntuples   /run_slurm.sh'. hps-mc prepares a single run command for each job using 'hps-mc-job', which the batch tool knows how to prepare and call properly,
    given the configuration parameters of the job

    8a. Open the 'run_slurm.sh' submission script.
    
    8b. Specify the batch system. The beginning of the command says 'hps-mc-batch slurm', which tells hps-mc to submit these jobs using
        slurm. To use something other than slurm, you must change 'slurm' to what you want to use, assuming it is supported by hps-mc. 
        You can learn more by reading '/sdf/group/hps/users/alspellm/src/hps-mc/'.

    8c. Specify the type of hps-mc job you want to run.
        In the 'run_slurm.sh' example, find where it says 'hpstr'. That is the hps-mc job being run using the job configurations inside 'jobs.json'. 
        The 'hpstr' job is defined here: '/sdf/group/hps/users/alspellm/src/hps-mc/python/jobs/hpstr_job.py'.
        To change the type of job, replace 'hpstr' with any of the jobs defined in '/sdf/group/hps/users/alspellm/src/hps-mc/python/jobs/'.
        Notice than when selecting a job, drop '_job.py' from the name you put inside of 'run_slurm.sh'. 

    8d. Provide the path to the 'jobs.json' file.

    8e. Specify the scratch directory with flag '-d' and the scratch logs directory with flag '-l'

    8f. Specify the local '.hpsmc' file with flag '-c'

    8g. Specify the hps-mc environment setup using my script '/sdf/group/hps/users/alspellm/src/hps-mc/install/bin/hps-mc-env.sh' with flag '-E'

    8h. Other batch submission configurations
        8h1. '-r' specifies which job numbers from the 'jobs.json' file to submit to the batch system
        8h2. '-q' specifies the partition to run on. *I always use 'shared', but im not sure the right way to choose these things*
        8h3. '-m' allocates memory for each job. If your job fails, check the log inside '/scratch/<user>/<job_name>/logs/'. If you see an error
             about memory, there wasnt enough memory allocated. This happens to me alot. Just increase the number.
        8h4. Other configurations can be found in '/sdf/group/hps/users/alspellm/src/hps-mc/python/hpsmc/batch.py'

    
    8i. Submit your jobs with 'source run_slurm.sh' 
    8j. The logs located in the scratch directory will provide information about the status of your jobs, any errors, the commands used, etc.
    8k. To check the run status of your jobs, in the command line type 'squeue | grep <username>'
    8l. To kill all jobs, type 'scancel -u <username>'








    
    
