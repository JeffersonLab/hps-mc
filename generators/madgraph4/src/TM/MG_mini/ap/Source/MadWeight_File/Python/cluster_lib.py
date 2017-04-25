#!/usr/bin/env python
##########################################################################
##                                                                      ##
##                               MadWeight                              ##
##                               ---------                              ##
##########################################################################
##                                                                      ##
##   author: Mattelaer Olivier (CP3)                                    ##
##       email:  olivier.mattelaer@uclouvain.be                         ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   license: GNU                                                       ##
##   last-modif:16/10/08                                                ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   Content                                                            ##
##   -------                                                            ##
##                                                                      ##
##                         *** CLUSTER CLASS ***                        ##
##                                                                      ##
##      +cluster                                                        ##
##      |    +   init                                                   ##
##      |    C   driver                                                 ##
##      |    +   +    init                                              ##
##      |    +   +    call                                              ##
##      |    +   +    all                                               ##
##      |    +   +    create_all_dir                                    ##
##      |    +   +    compile_all                                       ##
##      |    +   +    schedule                                          ##
##      |    +   +    launch                                            ##
##      |    +   +    control                                           ##
##      |    C   create_file                                            ##
##      |    |   +    init                                              ##
##      |    |   +    call                                              ##
##      |    |   +    all                                               ##
##      |    |   +    all_MadWeight                                     ##
##      |    |   +    all_MadEvent                                      ##
##      |    |   +    all_dir                                           ##
##      |    |   +    Event                                             ##
##      |    |   +    Card                                              ##
##      |    |   +    one                                               ##
##      |    C   launch                                                 ##
##      |    |   +    init                                              ##
##      |    |   +    call                                              ##
##      |    |   +    all                                               ##
##      |    |   +    all_MadWeight                                     ##
##      |    |   +    all_MadEvent                                      ##
##      |    |   +    directory                                         ##
##      |    |   +    one                                               ##
##      |    C   control                                                ##
##      |    |   +    init                                              ##
##      |    |   +    call                                              ##
##      |    |   +    all                                               ##
##      |    |   +    all_MadWeight                                     ##
##      |    |   +    all_MadEvent                                      ##
##      |    |   +    directory                                         ##
##      |    |   +    one                                               ##
##      |    |   +    str                                               ##
##                                                                      ##
##                    *** SUBMISSION FILE CLASS ***                     ##
##                                                                      ##
##      +submission_file                                                ##
##      |     +  init                                                   ##
##      |     +  clear                                                  ##
##      |     +  write                                                  ##
##      |     +  listing                                                ##
##      +submission_file_directory                                      ##
##      |     +  init                                                   ##
##      |     +  clear                                                  ##
##      |     +  write                                                  ##
##      |     +  find                                                   ##
##      |     +  listing                                                ##
##                                                                      ##
##                    *** Sub-CLUSTER ***                               ##
##      +    def_cluster                                                ##
##########################################################################
##
## BEGIN INCLUDE
##
import os
import time
import sys
import progressbar
##
## TAG
##
__author__="Mattelaer Olivier, the MadTeam"
__version__="1.0.1"
__date__="11 Jan 2009"
__copyright__= "Copyright (c) 2009 MadGraph/MadEvent team"
__license__= "GNU"
##
## BEGIN CODE
##


#################################################################################################
##                                     DEFAULT CLUSTER  CLASS                                  ##
#################################################################################################



## ERROR CLASS
class Clustererror(Exception): pass
class DirectoryError(Clustererror): pass

#1 ##############################################################################################
class cluster:
    """ default frame for cluster class """

    ##
    ## TAG VALUE
    ##
    tag=-1  #value for use this cluster type during the run.

    ##
    ## content
    ##
    ##   class driver
    ##   class create_file
    ##   class launch
    ##   class control



    #2 ##############################################################################################
    def __init__(self,MWparam):
        """ initialisation of the object """

        self.MWparam=MWparam  #object containing all run option
        self.submission_file=submission_file(MWparam)   #create class for treating schedulling file
        self.create_file=eval('cluster.'+self.__class__.__name__+'.create_file(self)')# link between object
        self.launch=eval('cluster.'+self.__class__.__name__+'.launch(self)') # link between object
        self.control=eval('cluster.'+self.__class__.__name__+'.control(self)') # link between object
        self.driver=eval('cluster.'+self.__class__.__name__+'.driver(self)') # link between object

    #2 ##############################################################################################
    def __str__(self):
        return "Cluster type "+str(self.tag)
        
    #2 ##############################################################################################
    #2 CLASS:  DRIVER
    #2 ##############################################################################################
    class driver:
        """ organize all the operation potentialy cluster dependant
            - creation of directory for each job
            - launch/relaunch
            - control
            - ...
        """

        #3 ##############################################################################################
        def __init__(self,mother):
            self.mother=mother
            self.MWparam=mother.MWparam

        #3 ##############################################################################################
        def __call__(self):
            #print 'pass in call'
            self.all()

        #3 ##############################################################################################
        def all(self):
            """ Launch all subroutine """

            #print 'control for all cluster related routine'
            self.create_all_dir()
            self.compile_all()
            self.schedule()
            self.launch()
            self.control()
            
            
        #3 ##############################################################################################
        def create_all_dir(self):
            """ Launch all subroutine """

            import create_run as Create

            print 'schedullar'
            Create.create_all_schedular(self.MWparam)

            if self.MWparam.run_opt['dir']:
                create_dir_obj=Create.create_dir(self.MWparam)
                create_dir_obj.all()
            return


            if self.MWparam.run_opt['dir']:
                if not self.MWparam.info['mw_run']['22']:
                    print 'creating all directories'
                    if self.MWparam.norm_with_cross:
                        for dir in self.MWparam.P_listdir:
                            Create.create_all_Pdir(dir,self.MWparam)
                else:
                    print 'creating new directories for additional events'
                    
                for dir in self.MWparam.MW_listdir:
                    Create.create_all_MWdir(dir,self.MWparam)
                print 'done'
                
        #3 ##############################################################################################
        def compile_all(self):
            """ Launch all subroutine """

            if self.MWparam.run_opt['launch'] or self.MWparam.run_opt['relaunch']:
                print 'compiling'
                #verify if everything is compiled and if include file are well defined        
                if self.MWparam.norm_with_cross:
                    dirlist=self.MWparam.P_listdir+self.MWparam.MW_listdir
                else:
                    dirlist=self.MWparam.MW_listdir

                for dir in dirlist:
                    os.chdir('./SubProcesses/'+dir)
                    os.system('make &>/dev/null')
                    os.chdir('../../')        
                print 'done'

        #3 ##############################################################################################
        def schedule(self):
            """ create all the scheduling file """


            if self.MWparam.run_opt['launch']:
                print """launch: create scheduling file"""
                self.mother.create_file()
                                            
            if self.MWparam.run_opt['relaunch']:
                print """relaunch: create scheduling file"""
                #clear sumision file
                self.mother.submission_file.clear()
                #collect failed directory
                self.failed={}
                for dir in self.MWparam.P_listdir+self.MWparam.MW_listdir:
                    failed_job=self.return_failed(dir)
                    if failed_job:
                        self.failed[dir]=failed_job
                        self.mother.create_file.for_failed(dir,failed_job)
                #clean all failed
                self.mother.control.clean(self.failed)
            print 'done'
                   
        #4 ##############################################################################################
        def return_failed(self,dir):
            """read the failed process and return them in a list """

            out=[]
            try:
                for line in open('./SubProcesses/'+dir+'/'+self.MWparam.name+'/failed_job.dat'):
                    out.append([int(value) for value in line.split()])
            except IOError:
                return []

            return out
        #3 ##############################################################################################
        def launch(self):
            """ launch all the scheduling file """

            from clean import Clean_event
            Clean_event(self.MWparam.name)
            if self.MWparam.run_opt['launch'] or self.MWparam.run_opt['relaunch'] :
                #check the compilation
                for directory in self.MWparam.MW_listdir+self.MWparam.P_listdir:
                    os.chdir('./SubProcesses/'+directory)
                    os.system('make')
                    os.chdir('../../')
            
                if self.MWparam.run_opt['launch']:
                    self.mother.launch()
                elif self.MWparam.run_opt['relaunch']:
                    self.mother.launch.relaunch(self.failed)


        #3 ##############################################################################################
        def control(self):
            """ control all the process """
            if self.MWparam.run_opt['control']:
                if hasattr(self,'failed'):
                    self.mother.control.all_list(self.failed)
                else:
                    self.mother.control()
                print 'all job done'



    #2 ##############################################################################################
    #2 CLASS:  CREATE_FILE
    #2 ##############################################################################################
    class create_file:
        """ all the possible routine to create submission file """

        #3 ##############################################################################################
        def __init__(self,mother,clear=0):
            self.mother=mother
            self.MWparam=mother.MWparam
            self.file_number={}
            if clear:
                mother.submission_file.clear() #supress ALL old submission file
        
        #3 ##############################################################################################
        def __call__(self):
            self.all()

        #3 ##############################################################################################
        def all(self):
            """ creates all the submition routines: Default launches MW and ME routines """

            self.mother.submission_file.clear() #supress ALL old submission file
            self.all_MadWeight()
            if self.MWparam.norm_with_cross:
                self.all_MadEvent()
                
        #3 ##############################################################################################
        def all_MadWeight(self,main=0):
            """ creates all the submission for MadWeight """
            
            if main:
                self.mother.submission_file.clear() #supress ALL old submission file
            for directory in self.MWparam.MW_listdir:
                for i in self.MWparam.actif_param:
                    self.Card(directory,i,'M')
        #3 ##############################################################################################
        def all_MadEvent(self,main=0):
            """ creates all the submission for MadEvent """
            if main:
                self.mother.submission_file.clear() #supress ALL old submission file
            for directory in self.MWparam.P_listdir:
                    self.Event(directory,1,'P')

        #3 ##############################################################################################
        def all_dir(self,directory,dir_type='auto'):
            """ creates all the submission for the directory """
            """ dir: directory
                dir_type: must belongs to ['MW','P','auto']
            """

            if dir_type not in ['M','P']:
                dir_type=dir[0]

            if dir_type == 'M':
                for i in self.MWparam.actif_param:
                    self.Card(directory,i,'M')
            elif(dir_type == 'P'):
                    self.Event(directory,1,'P')
            else:
                raise DirectoryError, "directory must start with MW or P :"+directory

        #3 ##############################################################################################
        def Event(self,directory,nb_event,dir_type):
            """ creates the submission for all the card for a given event in a specific directory """
            
            if dir_type=='M':
            	n_job=self.MWparam.nb_card
            	self.packet([directory]*n_job,               #directory
            				self.MWparam.actif_param, #first number
            				[nb_event]*n_job,                #second number
            				['M']*n_job)					 # type of job
            
            elif dir_type=='P':
                n_job=self.MWparam.nb_card	
            	self.packet([directory]*n_job,               #directory
            				[1]*n_job,                       #first number  
            				[num-1 for num in self.MWparam.actif_param], #second number
            				['P']*n_job)					 # type of job
                        
        #3 ##############################################################################################            
        def Card(self,directory,nb_card,dir_type):
            """ creates the submission for all the event for a given card in a specific directory """	
                            
            n_job=self.MWparam.nb_event	
            self.packet([directory]*n_job,               #directory
            			[nb_card]*n_job,                 #first number  
            			range(0,self.MWparam.nb_event),  #second number
            			[dir_type]*n_job)			     # type of job
            
                
        #3 ##############################################################################################
        def packet(self,directory,nb1,nb2,dir_type):
            """ create the submission file for a list of event/card/directory """
			
			
            if type(nb1)!= list:
                return self.one(directory,nb1,nb2,dir_type)
				
	    #default use the standard submission routine 
            for i in range(0,len(nb1)):
                self.one(directory[i],nb1[i],nb2[i],dir_type[i])	
				
        #3 ##############################################################################################
        def one(self,directory,nb1,nb2,dir_type):
            """ create the submission file for one event/card/directory """
            
            raise Clustererror , 'no way to schedule only one job in this cluster'
            #template
            #text=...
            #self.mother.submission_file.write(directory,file)  #create the submission file            
            
        #3 ##############################################################################################
        def for_failed(self,directory,failed):
            """ create the submission file for the failed job"""

            if failed:
            	nb1=[input[0] for input in failed]
            	nb2=[input[1] for input in failed]
            	self.packet([directory]*len(nb1),nb1,nb2,[directory[0]]*len(nb1))
            

    #2 ##############################################################################################
    #2 CLASS LAUNCH
    #2 ##############################################################################################
    class launch:
        """ all the routine to launch all the routine """

        #3 ##############################################################################################
        def __init__(self,mother):
            """store/link the usefull object """
            
            self.mother=mother
            self.MWparam=mother.MWparam
            self.pbar=0 #no progressbar
            
        #3 ##############################################################################################            
        def __call__(self):
            """ launch everything """
            self.all()
            
        #3 ##############################################################################################
        def all(self):
            """ control all the submition routines: Default launches MW and ME routines """
            
            if self.mother.submission_file.nbfile:
                self.pbar=progressbar.progbar('Submission',self.mother.submission_file.nbfile)
                
            self.all_MadWeight()
            if self.MWparam.norm_with_cross:
                self.all_MadEvent()
            if self.pbar:
                self.pbar.finish()
                
        #3 ##############################################################################################
        def all_MadWeight(self):
            """ creates all the submission for MadWeight """

            for directory in self.MWparam.MW_listdir:
                self.directory(directory,'M')
                
        #3 ##############################################################################################            
        def all_MadEvent(self):
            """ creates all the submission for MadEvent """

            for directory in self.MWparam.P_listdir:
                self.directory(directory,'P')

        #3 ##############################################################################################
        def directory(self,directory,dir_type='auto'):
            """ creates all the submission for the directory """
            """ dir: directory
                dir_type: must belongs to ['MW','P','auto']
            """

            # use the submision file if they are at least one submision file
            if self.mother.submission_file.nbfile:
                self.launch_submission_file(directory)
                return
            
            # continue -pass to each directory- if no submision file defined!
            
            if dir_type=='auto':
                dir_type=directory[0]
                
            if dir_type=='P':
                list_nb1=[1]
                list_nb2=[num-1 for num in self.MWparam.actif_param]
            elif dir_type=='M':
                list_nb1=self.MWparam.actif_param
                list_nb2=range(0,self.MWparam.nb_event)
            else:
                raise DirectoryError, "directory must start with MW or P : "+directory

            for event in list_nb2:
                for card in list_nb1:
                    self.one(directory,card,event,dir_type)
                    
        #3 ##############################################################################################
        def one(self,directory,nb1,nb2,dir_type='auto'):
            """ launch all the submission for the directory """
            """ dir: directory
                dir_type: must belongs to ['M','P','auto']
                nb1: first number: 1 in P_, card number in MW_
                nb2: second number: event number in P_; card number(-1) in P_
            """
			#the use of submission_file class is not compatible with the use of this routine
            raise Clustererror , 'no way to launch a single job on this cluster'
        
        #3 ##############################################################################################
        def relaunch(self,failed={}):
            """ relaunch all the failed job for the directory """

            # use the submision file if they are at least one submision file
            if self.mother.submission_file.nbfile:
                self.pbar=progressbar.progbar('Submission',self.mother.submission_file.nbfile)                
                self.launch_submission_file()
                self.pbar.finish()
                return
            
            #else use the self.one method
            for directory in failed.keys():
                dir_type=directory[0]
                for nb1,nb2 in failed[directory]:
                    self.one(directory,nb1,nb2,dir_type)
                                                                                                           
        #3 ##############################################################################################
        def launch_submission_file(self,directorylist=''):
            """ launch all the submission file for all the directories """

            if not directorylist:
                directorylist=self.MWparam.MW_listdir+self.MWparam.P_listdir
            elif type(directorylist)!=list:
                directorylist=[directorylist]

            for directory in directorylist:
                os.chdir('./SubProcesses/'+directory+'/schedular/')
                for file in self.mother.submission_file.listing(directory):
                    try:
                        os.system(self.submit_word+' '+file+' &>log_'+file+'.log')
                    except:
                        sys.exit('you need to specify how to launch scedular file in your cluster:\
                        variable submit_word')
                    if self.pbar:
                        #a progress bar is defined -> update this one
                        self.pbar.update()
                os.chdir('../../../')
                                                                                                                                                        
            
            
    #2 ##############################################################################################
    #2 CLASS CONTROL
    #2 ##############################################################################################
    class control:
        """control the run: by default look for output event in directory"""

        #3 ##############################################################################################
        def __init__(self,mother,clear=1):
            self.mother=mother
            self.MWparam=mother.MWparam
            self.idle=0
            self.running=0
            self.finish=0

        #3 ##############################################################################################
        def __call__(self):
            print 'start cluster control'
            self.all()

        #3 ##############################################################################################
        def all(self):
            """ control all the submission routines: Default launches MW and ME routines """

            while 1:
                self.idle,self.running,self.finish = 0,0,0
                self.all_MadWeight(main=0)
                if self.MWparam.norm_with_cross:
                    self.all_MadEvent(main=0)
                print self
                if self.idle+self.running==0:
                    break
                time.sleep(30)
                
        #3 ##############################################################################################
        def all_MadWeight(self,main=1):
            """ control all the submission for MadWeight """

            if main:
                while 1:
                    self.idle,self.running,self.finish = 0,0,0
                    for directory in self.MWparam.MW_listdir:
                        self.directory(directory,'M')
                    print self
                    if self.idle+self.running==0:
                        break
                    time.sleep(30)                
            else:
                for directory in self.MWparam.MW_listdir:
                    self.directory(directory,'M')

        #3 ##############################################################################################            
        def all_MadEvent(self,main=1):
            """ control all the submission for MadEvent """

            if main:
                while 1:
                    self.idle,self.running,self.finish = 0,0,0
                    for directory in self.MWparam.P_listdir:
                        self.directory(directory,'P')
                    print self
                    if self.idle+self.running==0:
                        break
                    time.sleep(30)                
            else:
                for directory in self.MWparam.P_listdir:
                    self.directory(directory,'P')

        #3 ##############################################################################################
        def all_list(self,listtocontrol):
            """ control all the submission for the list """

            while 1:
                self.idle,self.running,self.finish = 0,0,0
                for directory in listtocontrol.keys():
                    for nb1,nb2 in listtocontrol[directory]:
                        self.one(directory,nb1,nb2,directory[0])
                print self
                if self.idle+self.running==0:
                    break
                time.sleep(30)


        #3 ##############################################################################################
        def directory(self,directory,dir_type):
            """ Control all the submission in a given SubProcesses directory """

            #A ###################################
            #A STEP A: DEFINE DEPENDENT VARIABLE
            #A ###################################
            
            if dir_type not in ['M','P']:
                dir_type=dir[0]

            if dir_type == 'M':
                list_nb1=self.MWparam.actif_param
                list_nb2=range(0,self.MWparam.nb_event)
            elif(dir_type == 'P'):
                list_nb1=[1]                     #typical call is _1_Card (from zero)
                list_nb2=[num-1 for num in self.MWparam.actif_param]
            else:
                raise DirectoryError, "directory must start with MW or P"

            #B ###################################
            #B STEP B: CHECK STATUS FOR EACH DIR
            #B ###################################
            name=self.MWparam.name
            for nb1 in list_nb1:
                for nb2 in list_nb2:
                    self.one(directory,nb1,nb2,dir_type)

        #3 ##############################################################################################
        def one(self,directory,nb1,nb2,dir_type):
            """ control the status for a single job """
            
            #A ###################################
            #A STEP A: DEFINE DEPENDENT VARIABLE
            #A ###################################
            
            if dir_type not in ['M','P']:
                dir_type=dir[0]

            if dir_type == 'M':
                coherent_file='verif.lhco'
                start_file='start'
                output_file='weights.out'
            elif(dir_type == 'P'):
                coherent_file='param.dat'
                start_file='ftn25'
                output_file='results.dat'
            else:
                raise DirectoryError, "directory must start with MW or P"

            #B ###################################
            #B STEP B: CHECK STATUS FOR EACH DIR
            #B ###################################
            name=self.MWparam.name
            pos='./SubProcesses/'+directory+'/'+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/'
            file1=pos+coherent_file
            file2=pos+start_file
            file3=pos+output_file

            #UPDATE THE GLOBAL VALUE
            if os.path.isfile(file3):
                self.finish+=1  
            elif os.path.isfile(file2):
                self.running+=1
            elif os.path.isfile(file1):
                self.idle+=1
#            else:
#                print 'unknow position',pos,'from',os.getcwd()

        #3 ##############################################################################################
        def clean(self,failed):
            """ supress start file/output file for failed job """

            for directory,nb in failed.items():
                for nb1,nb2 in nb:
                    pos='./SubProcesses/'+directory+'/'+self.MWparam.name+'/'+self.MWparam.name+'_'+str(nb1)+'_'+str(nb2)+'/'
                    os.system('rm '+pos+'start &>/dev/null')
                    os.system('rm '+pos+'ftn25 &>/dev/null')
                    os.system('rm '+pos+'weights.out &>/dev/null')
                    os.system('rm '+pos+'results.dat &>/dev/null')
                                                                
            
        #3 ##############################################################################################
        def __str__(self):
            """ return string """
            return str(self.idle)+'  '+str(self.running)+'  '+str(self.finish)




##########################################################################
##                           SUBMISSION FILE                             ##
##########################################################################

#1 #######################################################################
class submission_file:
    """ class containing all the information to create/find/... all the
        submission file """

    #2 #######################################################################
    def __init__(self,MWparam):
        """ create schedullinhg for each directory """

        self.MWparam=MWparam
        self.object={}
        self.nbfile=0
        for directory in MWparam.MW_listdir+MWparam.P_listdir:
            self.object[directory]=submission_file_directory(directory)
            self.nbfile+=self.object[directory].number

    #2 #######################################################################
    def clear(self):
        """ remove all submission file """
        [submission.clear() for submission in self.object.values()]
        self.nbfile=0
        
    #2 #######################################################################
    def write(self,directory,text):
        """ write a new submission file"""

        self.object[directory].write(text)
        self.nbfile+=1
        
    #2 #######################################################################
    def listing(self,directory):
        """ return the listing of all submission file in directory """
        
        return self.object[directory].listing()


#1 #######################################################################
class submission_file_directory:
    """ special routine for submission file in a specific directory"""

    #2 #######################################################################
    def __init__(self,directory):
        """update the status of the submission file"""
        
        self.directory='./SubProcesses/'+directory+'/schedular'
        try:
            os.mkdir(self.directory)
        except:
            pass
        self.number=self.find()

    #2 #######################################################################
    def clear(self):
        """ supress all the submission file """

        for file in os.listdir(self.directory):
            if 'log' in file or 'submission_file_' in file:
                os.remove(self.directory+'/'+file)
    
        self.number=0

    #2 #######################################################################
    def write(self,text):
        """ write a new submission file """

        ff=open(self.directory+'/submission_file_'+str(self.number)+'.txt','w')
        ff.write(text)
        ff.close()
        self.number+=1

    #2 #######################################################################        
    def find(self):
        """ find the number of existing submission file """

        return max([0]+[int(listdir[16:-4]) for listdir in os.listdir(self.directory) if (listdir[0:16]=='submission_file_' and listdir[-4:]=='.txt')])

    #2 #######################################################################
    def listing(self):
        """ find all the submission file """

        if os.path.basename(os.path.realpath('.'))=="schedular":
            directory='./'
        else:
            directory=self.directory
        return [listdir for listdir in os.listdir(directory) if listdir[0:16]=='submission_file_']


            

            
#####################################################################################################
##                                GESTION DERIVATIVE CLUSTER  CLASS                                ##
#####################################################################################################

import cluster


def def_cluster(MWparam):
    """ create the instance of an object in the class where:
        - the class derivates from class "cluster" (at first level)
        - the class.tag is MWparam.cluster
        - the input parameter are 'input'
    """
    mother_class="cluster"

    for object in dir(cluster):
        try:
            if mother_class==getattr(cluster,object).__bases__[0].__name__: #this gives the name of the first parent class (if defined)
                if getattr(cluster,object).tag==MWparam.cluster:
                    return getattr(cluster,object)(MWparam)
        except (AttributeError,IndexError):
            pass

    raise Clustererror, 'No cluster implemented for tag '+str(MWparam.cluster)



