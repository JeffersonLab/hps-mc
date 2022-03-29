1. HPS-MC INSTALL   
    You will use my installed copy of hps-mc on sdf. You just need to run the setup script, and you will need to create the hpsmc config script
    1a. Source this script in your ~/.bashrc: '/sdf/group/hps/users/alspellm/src/hps-mc/install/bin/hps-mc-env.sh'
            1aa. This just sets the hps-mc path requirements
    1b. You need a '~/.hpsmc' script to configure the various hps-mc job software components. Paste the contents below into a file and save as '~/.hpsmc'.
        Each header in brackets (such as [JobManager]) defines a type of hps-mc job category. Under the bracket is where you tell hps-mc to 
        find the software used for that type of job. So [JobManager] should point to your hps-java snapshot, etc.
        *** start file below ***
            [MG4]
            madgraph_dir = /sdf/group/hps/users/bravo/src/hps-mc/generators/madgraph4/src

            [MG5]
            madgraph_dir = /sdf/group/hps/users/bravo/src/hps-mc/generators/madgraph5/src

            [EGS5]
            egs5_dir = /sdf/group/hps/users/bravo/src/hps-mc/generators/egs5

            [StdHepConverter]
            egs5_dir = /sdf/group/hps/users/bravo/src/hps-mc/generators/egs5

            [SLIC]
            slic_dir = /sdf/group/hps/users/bravo/src/slic/install
            hps_fieldmaps_dir = /sdf/group/hps/users/bravo/src/hps-fieldmaps
            detector_dir = /sdf/group/hps/users/alspellm/src/hps-java/detector-data/detectors

            [JobManager]
            hps_java_bin_jar = /sdf/home/a/alspellm/.m2/repository/org/hps/hps-distribution/5.1-SNAPSHOT/hps-distribution-5.1-SNAPSHOT-bin.jar
            java_args = -DdisableSvtAlignmentConstants -Djna.library.path="/sdf/group/hps/users/bravo/src/GeneralBrokenLines/cpp/install/lib/" 
            -XX:+UseSerialGC -Xmx6500m

            [FilterBunches]
            hps_java_bin_jar = /sdf/group/hps/users/alspellm/src/hps-java/distribution/target/hps-distribution-5.1-SNAPSHOT-bin.jar

            [ExtractEventsWithHitAtHodoEcal]
            hps_java_bin_jar = /sdf/group/hps/users/bravo/src/hps-java/distribution/target/hps-distribution-5.1-SNAPSHOT-bin.jar

            [HPSTR]
            hpstr_install_dir = /sdf/group/hps/users/alspellm/src/hpstr/install
            hpstr_base = /sdf/group/hps/users/alspellm/src/hpstr

            [LCIOCount]
            lcio_bin_jar = /sdf/group/hps/users/bravo/src/LCIO/target/lcio-2.7.4-SNAPSHOT-bin.jar

            [LCIOMerge]
            lcio_bin_jar = /sdf/group/hps/users/bravo/src/LCIO/target/lcio-2.7.4-SNAPSHOT-bin.jar

            [EvioToLcio]
            hps_java_bin_jar = /sdf/group/hps/users/alspellm/src/hps-java/distribution/target/hps-distribution-5.1-SNAPSHOT-bin.jar
            java_args = -Xmx4g -XX:+UseSerialGC
        *** end file ***

2. TEST USING EXAMPLE
    HPS-MC submits batch jobs that are ran inside of a specified scratch directory. All input and output files are renamed to some hps-mc 
    locally defined name. Once the job is done, the locally named output files are saved in the scratch directory. HPS-MC then copies those
    scratch output files to your specified output directory, with your specified output file names. 

    2a. Navigate to an example directory: 'cd /sdf/group/hps/users/alspellm/src/hps-mc/examples/data_cnv'

        data_cnv first converts Evio to Lcio using hps-java, where you must specify the steering-file in your job configuration.
        It then takes the slcio output and runs HPSTR, where you must specify the hpstr processor in your job configuration.
        data_cnv_job is defined here: '/sdf/group/hps/users/alspellm/src/hps-mc/python/jobs/data_cnv_job.py'.

    2b. Inside the data_cnv directory you will find "job.json" and "run.sh". 
        
        2b1. 'job.json' configures a single hps-mc job. Lets look at one of the configuration parameters.

             Looking at the line 
                "input_files": {
                        "/sdf/group/hps/data/physrun2019/hps_10030/hps_010030.evio.00120": "data_events.evio"
                },
             The left side of the colon is the input file. The right side is what hps-mc renames that file locally before running.

             Looking at the line 
                 "output_files": {
                    "data_events.slcio": "data_events_1.slcio",
                    "data_events.root": "data_events_1.root"
                  },
             There are two output files that are saved, the slcio output from the EvioToLCIO process, and the root output from running HPSTR 
             on the slcio file. Any output file you want to save must be specified here. 
             The left side of the colon defines the local output-file name hps-mc saves in the scratch directory.
             The right side of the colon is the actual output file-name. 
             If you dont want to save one of the output files, such as the slcio output, just delete that line.

        2b2. 'run.sh' here is not a batch job, but a simple run script.
             The important thing to note is that in the run script, you specify the hps-mc job name "data_cnv", which tells hps-mc to run
             the 'data_cnv_job.py' script. 
             Every job is defined in '/sdf/group/hps/users/alspellm/src/hps-mc/python/jobs/'. When specifying the job name in the run script,
             leave out the '_job.py' part. 

    2c. Try to get this to work first. Just try running this single job and make sure things work. 

3. Configuring batch jobs
    
    We will use one of my job directories as an example: '/sdf/group/hps/users/alspellm/projects/collab_2021/ntuples'

    3a. Inside this directory you will need the '.hpsmc' file, a 'job.json.tmpl' file, a 'mkjobs.sh' script, an 'sh/' directory, an 'input_files.txt'
        a 'vars.json' file, an a 'run_slurm.sh' script. 

        3a1. The '.hpsmc' file:
             The directory you sumbit jobs from must have a local '.hpsmc' file that looks like this:
                **start file**
                    [Job]
                    dry_run = False
                    enable_file_chaining = True
                    check_output_files = False
                    enable_copy_output_files = True
                    delete_rundir = False
                **end file**

             You can also configure more things in this file, such as the conditions_url for all hps-java jobs, by adding a line such as:
                [EvioToLcio]
                conditions_url = jdbc:sqlite:/sdf/group/hps/users/alspellm/projects/fit_shape_params/2019/jlab_24_ns/database/
                hps_conditions_220201_newfitparams.db

        3a2. The 'input_files.txt' should just be a list of the full path to all of your input files. 
             You will end up with 1 job for each input file. 

        3a3. The 'job.json.tmpl' file:
             This file defines the configuration template for all of the jobs you will eventually submit to the batch system. 
             'mkjobs.sh' reads the input_files.txt, and this template, and generates 1 job configuration, with a unique job_id, for each input file.
             These jobs are saved in together in the 'jobs.json' file.
             
             hps-mc uses jinja2 to parse the text in the template. You can use this to be as clever as you wish with naming things.
             The example 'job.json.tmpl' here contains
                "input_files": {
                    "{{input_files['data'][0]}}" : "data_events.slcio"
                },
                "output_files": {
                    "data_events.root" : "data_events_{{ job_id - 1}}.root"
                },                                                                 " 

             The input_file names are generated based on how 'mkjobs.sh' reads in the 'input_files.txt' file. Take a look. 
             The output_file names are generated based on the job_id that is assigned automatically to each job when 'mkjobs.sh' is run.


        3a4. The 'mkjobs.sh' script reads in the input_files.txt, the job.json.tmpl, and saves the output in 'jobs.json'.
             Notice the flag '-i data'. This assigns a variable name to the input files, which is referred to in the 'job.json.tmpl'.              

        3a5. The 'vars.json' file is used to define variables which are then used with 'mkjobs.sh' and 'job.json.tmpl' when generating jobs.
             This particular example directory does not use the vars.json file. For an example, check '/sdf/group/hps/users/alspellm/src/hps-mc/
             examples/beam_gen'

        3a6. The 'run_slurm.sh' script submits all of the jobs defined in the 'jobs.json' file. 
             The next section covers this script.

4.  SUBMITTING BATCH JOBS
    
    4a. Scratch Directories
        To submit 'run_slurm.sh' youve got to create and define the scratch directories that your jobs will run in.
        On sdf I always do 'mkdir -p /scratch/alspellm/<job_name>/logs'. 
        Specify both the '<job_name>' dir, AND the '<job_name>/logs' dir in the 'run_slurm.sh' script. 

    4b. Give the 'jobs.json' file path in 'run_slurm.sh'.

    4c. Specify the hps-mc job script!
        In this 'run_slurm.sh' example, find where it says 'hpstr'. That is the hps-mc job we are running in this example.
        It tells hps-mc to run the job defined here: '/sdf/group/hps/users/alspellm/src/hps-mc/python/jobs/hpstr_job.py'

        If you wanted to run hps-java reconstruction on evio data, you would replace 'hpstr' with 'data_cnv', which tells hps-mc to run
        the job defined here: '/sdf/group/hps/users/alspellm/src/hps-mc/python/jobs/data_cnv_job.py'. 

    4d. Specify the locally defined '.hpsmc' file inside of 'run_slurm.sh'

    4e. Flag '-E' specifies the hps-mc environment script from step 1a.

    4f. Flag '-r' specifies which job numbers from the 'jobs.json' file to submit to the batch system
    
    4g. Flag '-q' specifies which partition to run on. I always use 'shared', and dont fully understand when to use what partition.

    4h. Flag '-m' allocates memory for each job. If your job fails, there will be a log inside '/scratch/<user>/<job_name>/logs/<job.<n>.err' 
        that will say something about running out of memory. This happens to me alot. Just increase the memory size.

    4i. Inside of 'run_slurm.sh' at the beginning find 'hps-mc-batch slurm'. This specifies the batch system. To use something else, you have to change
        'slurm' to whatever youre trying to use, assuming its defined in hps-mc. You can learn more by reading '/sdf/group/hps/users/alspellm/src/hps-mc/
        python/hpsmc/batch.py'

             

            

             

             



        

        


                
                
                









