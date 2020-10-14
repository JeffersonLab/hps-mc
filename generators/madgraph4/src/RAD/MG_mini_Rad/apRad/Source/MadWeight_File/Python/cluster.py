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
##                       *** CLUSTER ***                                ##
##      + single_machine (tag=0)                                        ##
##      + condor         (tag=1)	             			##
##      + SGE            (tag=2)					##
##        + SGE2 -for run in packet- (tag=21)                           ##
##########################################################################
##
## BEGIN INCLUDE
##
import os
import time
from cluster_lib import *
##
## TAG
##
__author__="Mattelaer Olivier, the MadTeam"
__version__="1.0.0"
__date__="Oct 2008"
__copyright__= "Copyright (c) 2008 MadGraph/MadEvent team"
__license__= "GNU"
##
## BEGIN CODE
##
#########################################################################
#   SINGLE MACHINE														#
#########################################################################
class single_machine(cluster):
    """ all the routine linked to the gestion of a single machine job """
    tag=0
    
    #2 ##################################################################
    class create_file(cluster.create_file):
        #3 ##################################################################    
        def one(self,directory,nb_event,nb_card,dir_type):
            pass #no file creation needed
            
    #2 ##################################################################        
    class launch(cluster.launch):
        #3 ##################################################################
        def one(self,directory,nb1,nb2,dir_type):
            name=self.MWparam.name
            if dir_type=='M':
                prog='../../gfortran_madweight'
            elif dir_type=='P':
                prog='../../gfortran_madevent'
                
            os.chdir('./SubProcesses/'+directory+'/'+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/')
            os.system(prog)
            os.chdir('../../../../')
            
#########################################################################
#   CONDOR CLUSTER														#
#########################################################################
class condor(cluster):
    """specific routine for condor cluster """ 
	
    tag=1 # value for use condor in the MadWeight_card.dat

    #2 ##################################################################		
    class create_file(cluster.create_file):
        """ create the condor submission file """
	
        #3 ##################################################################		
        def condor_submission(self,directory,nb1,nb_submit,dir_type):
            """ common part of all condor submission file """
            """ no standard for this functions """
			
            pos=os.path.abspath('./SubProcesses/'+directory) #to check
            if dir_type=='P':
                prog='gfortran_madevent'
            elif dir_type=='M':
                prog='gfortran_madweight'
            dir_name=self.MWparam.name
		
            text= 'Executable   = '+pos+'/'+prog+'\n'
            text+='output       = '+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_$(PROCESS)/out\n'
            text+='error        = '+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_$(PROCESS)/err\n'
            text+='log          = '+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_$(PROCESS)/log\n'
            text+='Universe     = vanilla\n'
            text+='notification = Error\n'
            text+='Initialdir='+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_$(PROCESS)\n'

            condor_req=eval(self.MWparam.condor_req) #supress the ''
    	    if(condor_req!='' and type(condor_req)==str):
                text+='requirements ='+condor_req+'\n'         
            text+='Queue '+str(nb_submit)+'\n'     
   
            self.mother.submission_file.write(directory,text)
			
        #3 ##################################################################	
        def Card(self,directory,nb_card,dir_type):
            """ creates the submission for all the event for a given card in a specific directory """
			
            if dir_type=='M':
                self.condor_submission(directory,nb_card,self.MWparam.nb_event,dir_type)
            else:
                raise ClusterError, 'launch by card is foreseen only for MadWeight Job'
				
        #3 ##################################################################				
        #def Event(self,directory,nb_event,dir_type):
        #    """ creates the submission for all the card for a given event in a specific directory """
        # supress routine in order to be able to include the desactivation of param_card
        # use default and then self.one to submit
        #
        #if dir_type=='P':
        #        self.condor_submission(directory,nb_event,self.MWparam.nb_card,dir_type)
        #    else:
        #        raise ClusterError, 'launch by card is foreseen only for MadEvent Job'
				
        #3 ##################################################################				
        def one(self,directory,nb1,nb2,dir_type):
            """ """

            if dir_type=='P' and not  self.MWparam.param_is_actif[nb2+1]:
                return
            elif dir_type=='M' and not self.MWparam.param_is_actif[nb1]:
                return
                                            			
            pos=os.path.abspath('./SubProcesses/'+directory) #to check
            if dir_type=='P':
                prog='gfortran_madevent'
            elif dir_type=='M':
                prog='gfortran_madweight'
            dir_name=self.MWparam.name
		
            text= 'Executable   = '+pos+'/'+prog+'\n'
            text+='output       = '+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_'+str(nb2)+'/out\n'
            text+='error        = '+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_'+str(nb2)+'/err\n'
            text+='log          = '+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_'+str(nb2)+'/log\n'
            text+='Universe     = vanilla\n'
            text+='notification = Error\n'
            text+='Initialdir='+pos+'/'+dir_name+'/'+dir_name+'_'+str(nb1)+'_'+str(nb2)+'\n'

            condor_req=eval(self.MWparam.condor_req) #supress the '
    	    if(condor_req!='' and type(condor_req)==str):
                text+='requirements ='+condor_req+'\n'         
            text+='Queue 1\n'     
  
            self.mother.submission_file.write(directory,text)
			
    #2 ##################################################################			
    class launch(cluster.launch):
        """ launch all the job condor 
            use the submission file class
        """
        submit_word='condor_submit' #submit a file 'test' by "condor_submit test"


    #2 ##############################################################################################
    #2 CLASS CONTROL for multiple run
    #2 ##############################################################################################
    #class control(cluster.control):
    #    """control the run: by default look for output event in directory"""
    #                    
    #    def all(self):
    #        """ control all the submission routines: Default launches MW and ME routines """
    #        
    #        while 1:
    #            self.idle,self.running,self.finish = 0,0,0
    #            self.all_MadWeight(main=0)
    #            if self.MWparam.norm_with_cross:
    #                self.all_MadEvent(main=0)
    #            print self
    #            if self.idle<300:
    #                print 'stop waiting for this run: go to next run'
    #                break
    #            time.sleep(30)

	     		
#########################################################################
#   SGE CLUSTER  					  		#
#########################################################################
class SGE(cluster):

    tag=2 # value to call this class from MadWeight_card.dat

    #2 ##################################################################		
    class create_file(cluster.create_file):
        """ create the condor submission file """

        standard_text=open('./Source/MadWeight_File/Tools/sge_schedular','r').read()
        #3 ##################################################################	
        #def Card(self,directory,nb_card,dir_type):
        #    """ creates the submission for all the event for a given card in a specific directory """
        #    pass	
        #3 ##############################################################################################

       #3 ##################################################################				
        def one(self,directory,nb1,nb2,dir_type):
            """SGE in pure local for the moment  """
            pos=os.path.realpath(os.getcwd())
            name=self.MWparam.name
            
            if dir_type=='P' and not  self.MWparam.param_is_actif[nb2+1]:
                return
            elif dir_type=='M' and not self.MWparam.param_is_actif[nb1]:
                return
            
            text=self.standard_text
            text+="#$ -wd "+pos+"/SubProcesses/"+directory+'/'+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/\n'
            text+="#$ -e "+pos+"/SubProcesses/"+directory+"/"+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/err\n'
            text+="#$ -o "+pos+"/SubProcesses/"+directory+"/"+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/out\n'
            text+="\n"
            text+="date\n"
            if dir_type=='P':
                text+="../../gfortran_madevent\n"
            elif dir_type=='M':
                text+="../../gfortran_madweight\n"
            text+="date\n"

            self.mother.submission_file.write(directory,text)
            
    #2 ##################################################################			
    class launch(cluster.launch):
        """ launch all the SGE job  
            use the submission file class
        """
        submit_word='qsub' #submit a file 'test' by "qsub test"

#########################################################################
#   SGE CLUSTER  -submission by bigger packet-					  		#
#########################################################################
class SGE2(cluster,SGE):

    tag=21 # value to call this class from MadWeight_card.dat

    #2 ##################################################################		
    class create_file(cluster.create_file):
        """ create the condor submission file """

        standard_text=open('./Source/MadWeight_File/Tools/sge_schedular','r').read()
        #3 ##################################################################	
        #def Card(self,directory,nb_card,dir_type):
        #    """ creates the submission for all the event for a given card in a specific directory """
        #    pass	
        #3 ##############################################################################################
        
        def all_MadWeight(self,main=0):
            """ creates all the submission for MadWeight """
            if main:
                self.mother.submission_file.clear() #supress ALL old submission file
            for directory in self.MWparam.MW_listdir:
                for i in range(0,self.MWparam.nb_event):
                    self.Event(directory,i,'M')                                                                        
        
        #3 ##################################################################				
        def Event(self,directory,nb_event,dir_type):
            """ creates the submission for all the card for a given event in a specific directory """
            pos=os.path.realpath(os.getcwd())
            name=self.MWparam.name
            
            text=self.standard_text
            text+="#$ -wd "+pos+"/SubProcesses/"+directory+'/'+name+'/'+name+'_'+str(1)+'_'+str(nb_event)+'/\n'
            text+="#$ -e "+pos+"/SubProcesses/"+directory+"/"+name+'/'+name+'_'+str(1)+'_'+str(nb_event)+'/err\n'
            text+="#$ -o "+pos+"/SubProcesses/"+directory+"/"+name+'/'+name+'_'+str(1)+'_'+str(nb_event)+'/out\n'
            text+="\n"
            text+="date\n"
            
            if dir_type=='P':
                return cluster.create_file.Event(self,directory,nb_event,dir_type)
            elif dir_type=='M':
                text+="cd ../..\n"
                text+="make\n"
                for i in range(1,self.MWparam.actif_param):
                    text+="cd "+name+"/"+name+"_"+str(i)+"_"+str(nb_event)+'\n'
                    text+="../../madweight\n"
                    text+="cd ../..\n"
            text+="date\n"
        
            self.mother.submission_file.write(directory,text)
        
        #3 ##############################################################################################
        def packet(self,directory,nb1,nb2,dir_type):
            """ create the submission file for a list of event/card/directory """

            if type(nb1)!=list:
            	return self.one(directory,nb1,nb2,dir_type)

            pos=0
            while pos<len(nb1):
                if dir_type[pos]=='P' and not  self.MWparam.param_is_actif[nb2[pos]+1]:
                    del nb1[pos]
                    del nb2[pos]
                    del dir_type[pos]
                elif dir_type[i]=='M' and not self.MWparam.param_is_actif[nb1[i]]:
                    del nb1[pos]
                    del nb2[pos]
                    del dir_type[pos]
                else:
                    pos+=1
                
            n_join=5 # change the value if you want to launch less job on your cluster
            if len(nb1)>n_join: 
            	self.packet(directory[0:n_join],nb1[0:n_join],nb2[0:n_join],dir_type[0:n_join])
                self.packet(directory[n_join:],nb1[n_join:],nb2[n_join:],dir_type[n_join:])
               	return


            
            pos=os.path.realpath(os.getcwd())
            name=self.MWparam.name
            
            text=self.standard_text
            text+="#$ -wd "+pos+"/SubProcesses/"+directory[0]+'/'+name+'/'+name+'_'+str(nb1[0])+'_'+str(nb2[0])+'/\n'
            text+="#$ -e "+pos+"/SubProcesses/"+directory[0]+"/"+name+'/'+name+'_'+str(nb1[0])+'_'+str(nb2[0])+'/err\n'
            text+="#$ -o "+pos+"/SubProcesses/"+directory[0]+"/"+name+'/'+name+'_'+str(nb1[0])+'_'+str(nb2[0])+'/out\n'
            text+="\n"
            text+="date\n"
            text+="cd ../..\n"			

            for i in range(0,len(nb1)):
                text+="cd ../"+directory[0]+'\n'
                text+="cd "+name+"/"+name+"_"+str(nb1[i])+"_"+str(nb2[i])+'\n'
                if dir_type[i]=='P':
                	text+="../../madevent\n"
                else:
                	text+="../../madweight\n"
                text+="cd ../..\n"
                
            text+="date\n"            	
            
            self.mother.submission_file.write(directory[0],text)
            
    class launch(SGE.launch): pass        


	
#########################################################################
#   BSUB CLUSTER 					  		#
#########################################################################
class Bsub(cluster):

    tag=3 # value to call this class from MadWeight_card.dat

    #2 ##################################################################		
    class create_file(SGE.create_file):
        """ create the condor submission file """

        standard_text=open('./Source/MadWeight_File/Tools/bsub_schedular','r').read()

       #3 ##################################################################				
        def one(self,directory,nb1,nb2,dir_type):
            """BSUB in pure local for the moment  """
            pos=os.path.realpath(os.getcwd())
            name=self.MWparam.name

            if dir_type=='P' and not  self.MWparam.param_is_actif[nb2+1]:
                return
            elif dir_type=='M' and not self.MWparam.param_is_actif[nb1]:
                return
            
            text=self.standard_text
            text+="#BSUB -e "+pos+"/SubProcesses/"+directory+"/"+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/err\n'
            text+="#BSUB -o "+pos+"/SubProcesses/"+directory+"/"+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/out\n'
            text+="cd "+pos+"/SubProcesses/"+directory+'/'+name+'/'+name+'_'+str(nb1)+'_'+str(nb2)+'/\n'
            text+="\n"
            text+="date\n"
            if dir_type=='P':
                text+="../../gfortran_madevent\n"
            elif dir_type=='M':
                text+="../../gfortran_madweight\n"
            text+="date\n"

            self.mother.submission_file.write(directory,text)
    #2 ##################################################################			
    class launch(cluster.launch):
        """ launch all the SGE job  
            use the submission file class
        """
        submit_word='bsub <' #submit a bash file 'test' by "bsub < test" (the < is important in order to define some run variable)

