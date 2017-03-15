#!/usr/bin/env python

#Extension
import string
import os
import sys
import re
import popen2
import time
import stat
from collect_result import Collect_dir
import mod_file
import filecmp
import progressbar
###########################################################################
##                                CONTENT                                ##
##                                -------                                ##
##                                                                       ##
##    + update_cuts_status                                               ##
##         activate/desactivate the cuts in ME/MW                        ##
##     + cut_is_active (send cut status)                                 ##
##     + check_Subprocesses_update (check if Subprocces/(MW_?)P files    ##
##         follows the last version of the file -protection against cp-) ##
##    + del_old_dir                                                      ##
##    + create_dir                                                       ##
##    + copy_file                                                        ##
##    + put_data                                                         ##
##    + restore_last_info_for_control (not use anymore)                  ##
##    + create_all_MWdir                                                 ##
##    + create_all_Pdir                                                  ##
##    + create_all_schedular                                             ##
###########################################################################


###########################################################################
###################           VARIABLE GLOBALE           ##################
###########################################################################
opt=sys.argv
write = sys.stdout.write

###########################################################################
###################     GESTION OF CUT (des)ACTIVATE     ##################
###########################################################################

def update_cuts_status(MW_param):
    """ remove the call to the cuts if asked in MadWeight_Card.dat """

    if MW_param.info['mw_run'].has_key('9'):
        if  MW_param.info['mw_run']['9'] in ['F','.false.','0','f']:
            use_cut=0
        else:
            use_cut=1

        cut_status=cut_is_active() #chek if we use the cut for the moment!!

        if (use_cut+cut_status)%2:
            #pass if not coherent between asked and actual status
            print 'update the cut status'
            file_to_mod=['./SubProcesses/cuts.f','./SubProcesses/cuts_MW.f']
            rule=['./Source/MadWeight_File/mod_file/suppress_cuts_MG','./Source/MadWeight_File/mod_file/suppress_cuts_MW']
            mod_file.mod_file(file_to_mod,rule,opt={'nowarning':"""['PASSCUTS','TO_SPECISA']"""})
            filename=[file.split('/')[-1] for file in file_to_mod]
            if MW_param.info['mw_run'].has_key('91'):
                mode= MW_param.info['mw_run']['91']
            else:
                mode=1
                
            check_Subprocesses_update('cuts.f',MW_param.P_listdir,'./Source/MadWeight_File/mod_file/suppress_cuts',mode)
            check_Subprocesses_update('cuts_MW.f',MW_param.MW_listdir,'./Source/MadWeight_File/mod_file/suppress_cuts',mode)


def cut_is_active():
    """ check is the cut is active or not """

    for line in file('./SubProcesses/cuts.f'):
        if line.count('DESACTIVATE_CUT'):
            return 1
        elif line.count('ACTIVATE_CUT'):
            return 0


class check_Subprocesses_update:
    """ check if Subprocces/'listdir' files follows the last version of
    ./SubProcesses/'filename'. This is a protection against copy files
    -normally they are just linked-

    different mode are available
    warning:
      0: no warning apply modification
      1: warning and apply modifiaction
      2: warning and user choice
      3: warning but no modification
      4: no warning and no modification
      5: warning and raising error
    modifrule:
      '', copy the original file
      else: apply the modification asked in modifrule

    """

    class ERROR_DifferenceInFile(Exception):pass


 
    def __init__(self,filelist,listdir,modifrule='',warning=1,run=1):
        
        #chek type input:
        if type(filelist)==str:
            self.filelist=[filelist]
        else:
            self.filelist=filelist
        if type(listdir)==str:
            self.listdir=[listdir]
        else: self.listdir=listdir

        self.modifrule=modifrule

    
        #assign tag mode
        usewarning=0
        if int(warning) in [1,2,3,5]:
            self.usewarning=1
        
        self.usemodif=0
        self.intmode=0
        self.raiseerror=0
        if int(warning) in [0,1]:
            self.usemodif=1 #apply
        elif int(warning)==2:
            self.intmode=1 #interactive mode
        elif int(warning)==5:
                self.raiseerror==1 #raising error
        
        if run:
            self.compare()
    
    
    def compare(self):
        #main loop
        for dirname in self.listdir:
            for filename in self.filelist:
                status=filecmp.cmp('./SubProcesses/'+filename,'./SubProcesses/'+dirname+'/'+filename)

            if status:
                continue #continue check if ok

            #warning
            if self.usewarning:
                self.printwarning('./SubProcesses/'+filename,'./SubProcesses/'+dirname+'/'+filename)
                
            #error   
            if self.raiseerror:
                raise self.ERROR_DifferenceInFile

            #interactive mode
            if self.intmode:
                self.usemodif=self.printintmode('./SubProcesses/'+dirname+'/'+filename)

            #modif file
            if self.usemodif:
                self.modiffile('./SubProcesses/'+dirname+'/'+filename,self.modifrule)


    def printwarning(self,filename1,filename2):
        
                print """ WARNING: those file are supposed to be identical (symbolic link):\n\
                          ./SubProcesses/"""+filename1+"""\n./SubProcesses/"""+filename2
                print """ define the tag MW_run/901 to change the rule for modification """
                if self.usemodif==0:
                    print """ no modification to the file are done """
                elif self.usemodif==1:
                    print """ modify the file """

    def printintmode(self,file):

        a=raw_input('modify file '+file+'with rule'+self.modifrule+'? (y/n)')
        if a=='y':
            return 1
        elif a=='n':
            return 0
        else:
            a=self.printintmode(file)
            return a
        
    def modiffile(self,file,rule):
        mod_file.mod_file(file,rule)
        

###########################################################################
#########          CREATION DES DOSSIER/FICHIERS DE SORTIE      ###########
###########################################################################

#1 ##################################################################################
class create_dir:
    """create all the directory for the run """

    #2 ##############################################################################
    def __init__(self,MWparam):

        self.MWparam=MWparam
        self.dir_name=MWparam.name

        self.ref_card=self.MWparam.actif_param[0] #card where the file verif.lhco are written
        self.created=0

    #2 ##############################################################################
    def all(self):
        
        print 'creating all directories'
        if self.MWparam.norm_with_cross:
            for dir in self.MWparam.P_listdir:
                self.all_dir(dir)
            else:
                print 'creating new directories for additional events'
                        
        for dir in self.MWparam.MW_listdir:
            self.all_dir(dir)

    #2 ##############################################################################
    def all_dir(self,dir):
        """creates the directory for standard run """

        self.dir_type=dir[0]
        self.Sdir_pos='./SubProcesses/'+dir
        
        if self.dir_type=='M':
            if self.MWparam['mw_run']['22']:
                self.add_events()
            else:
                self.create_M_dir()
        else:
            if self.MWparam['mw_run']['22']:
                return
            self.create_P_dir()
            
        print 'created',self.created,'directories'
        self.created=0

    #2 ##############################################################################
    def update_card_status(self,cardref=-1):
        """creates the directory for standard run """

        #check if all card have a directory in P_
        list_card=[num-1 for num in self.MWparam.actif_param]
        #progress bar
        pbar = progressbar.progbar('update Pdir',len(list_card)*len(self.MWparam.P_listdir))
        for directory in self.MWparam.P_listdir:
            self.dir_type=directory[0]
            self.Sdir_pos='./SubProcesses/'+directory
            for card in list_card:
                self.create_one_P_dir(card,remove_old=0)
                pbar.update()
        pbar.finish()

        #update events
        #progress bar
        list_card=self.MWparam.actif_param
        list_event=range(0,self.MWparam.nb_event)
        pbar = progressbar.progbar('update MWdir',(len(list_card))*(len(list_event))*len(self.MWparam.P_listdir))
        for dir in self.MWparam.MW_listdir:
            self.dir_type=dir[0]
            self.Sdir_pos='./SubProcesses/'+dir

            #find an uptodate card
            if cardref==-1:
                ref_num=self.MWparam.nb_event-1 #-1 due to the starting at zero
                self.ref_card=-1
                for card in self.MWparam.actif_param:
                    print self.find_exist_event(card)
                    if self.find_exist_event(card)>=ref_num:
                        self.ref_card=card
                        break
            else:
                self.card_ref=cardref
                
            if self.ref_card==-1:
                break
            #then pass in update mode for all the card
            for card in list_card:
                for event in list_event:
                    self.create_one_M_dir(card,event,remove_old=0)
                    pbar.update()
        pbar.finish()
                                                                                                
                                                                                                                                    
    #2 ##############################################################################
    def add_events(self):
        
        self.file_event=open(self.Sdir_pos+'/verif.lhco')
        self.line_event=self.file_event.readline()

        nb_exist_event=self.find_exist_event()
        list_card=self.MWparam.actif_param
        print nb_exist_event,self.MWparam.nb_event
        list_event=range(nb_exist_event+1,nb_exist_event+self.MWparam.nb_event+1)
        pbar = progressbar.progbar('create_dir',(len(list_card))*(len(list_event)))
        for card in list_card:
            for event in list_event:
                self.create_one_M_dir(card,event)
                pbar.update()
        pbar.finish()
                                                                
        

    #2 ###############################################################################
    def find_exist_event(self,card_nb=-1):

        print self.Sdir_pos
        if card_nb==-1:
            card_nb=self.ref_card

        number=[int(directory.split('_')[-1]) for directory in os.listdir(self.Sdir_pos+'/'+self.MWparam.name+'/') if len(directory.split('_'))==3 and\
                          directory.count('.')==0 and int(directory.split('_')[-2])==card_nb]

        if len(number):
            return max(number)
        else:
            
            return 0
                
    #2 ##############################################################################
    def create_M_dir(self):

        self.file_event=open(self.Sdir_pos+'/verif.lhco')
        self.line_event=self.file_event.readline()

        self.del_old_dir()
        list_card=self.MWparam.actif_param
        list_event=range(0,self.MWparam.nb_event)
        #progress bar
        pbar = progressbar.progbar('create_dir',(len(list_card))*(len(list_event)))
        for card in list_card:
            for event in list_event:
                self.create_one_M_dir(card,event)
                pbar.update()
        pbar.finish()

    #2 ##############################################################################
    def create_P_dir(self):

        self.del_old_dir()
        list_card=[num-1 for num in self.MWparam.actif_param]
        #progress bar
        pbar = progressbar.progbar('create_dir',len(list_card))
        for card in list_card:
            self.create_one_P_dir(card)
            pbar.update()
        pbar.finish()

    #2 ##############################################################################
    def create_one_M_dir(self,card,event,remove_old=1):
        """ create the directory for the event \"event\" and the card nb \"card\"
        """
        dir_name=self.MWparam.name
        pos=self.Sdir_pos+'/'+dir_name+'/'+dir_name+'_'+str(card)+'_'+str(event)+'/'
        try:
            os.mkdir(pos)
        except:
            if remove_old:
                os.system('rm '+pos+'/* >/dev/null')
            else:
                return

        ff=open(pos+'/param.dat','w')
        ff.writelines('param_card_'+str(card)+'.dat\n')
        ff.writelines(str(self.MWparam['mw_run']['6'])+'\n')
        ff.close()

        if card==self.ref_card:
            data=self.give_new_exp_point()
            hh=open(pos+'/verif.lhco','w')
            hh.writelines(data)
            hh.close()
        else:
            try:
                os.symlink('../'+dir_name+'_'+str(self.ref_card)+'_'+str(event)+'/verif.lhco',pos+'/verif.lhco')
            except OSError:
                pass
        self.created+=1


    #2 ##############################################################################
    def create_one_P_dir(self,card,remove_old=1):
        """ create the directory for the event \"event\" and the card nb \"card\"
        """
        dir_name=self.MWparam.name
        pos=self.Sdir_pos+'/'+dir_name+'/'+dir_name+'_1_'+str(card)
        try:
            os.mkdir(pos)
        except:
            if remove_old:
                os.system('rm '+pos+'/* >/dev/null')
            else:
                return
        ff=open(pos+'/param.dat','w')
        ff.writelines('param_card_'+str(card+1)+'.dat\n')
        ff.writelines(str(self.MWparam['mw_run']['6'])+'\n')
        ff.close()
        self.created+=1
        
    Pattern=re.compile(r'''^\s*0\s+\d+\s+\d+\s+$''',re.I)
    #2 ##############################################################################
    def give_new_exp_point(self):

        data=self.line_event
        while 1:
            line=self.file_event.readline()
            if line=='':
                self.file_event.close()
                return data
            if self.Pattern.search(line):
                self.line_event=line
                return data
            else:
                data+=line
            


    #2 ##############################################################################
    def del_old_dir(self):
        ''' delete old event directory '''
        
        output=1
        #verification du format des dossiers
        list_dir=os.listdir(os.pardir)
        if self.MWparam.info['mw_run']['22']:
            print self.Sdir_pos.split('/')[-1],': no deleting'
            return output
        
        print self.Sdir_pos.split('/')[-1],': deleting old run directory'
        try:
            os.system('rm '+self.Sdir_pos+'/'+self.MWparam.name+'/ -r')
        except:
            print "WARNING: this directory ", os.getcwd()+'/'+self.Sdir_pos+'/'+self.MWparam.name," are not deleted"
            output=0
        os.system('mkdir '+self.Sdir_pos+'/'+self.MWparam.name+'/')
             
        return output
                                                                                                                                                                                                                                                                                         
###### copy file
def copy_file(list,i):
    pos=os.getcwd()
    init_dir=os.sep+pos.split(os.sep)[-1]
    
    title=[]
    content=[]

    for file in list:
        title.append(file.split('/')[-1])
        ff=open(file,'r')
        content.append(ff.readlines())
        ff.close()
        
#    for i in range(0,nmax):
    os.chdir('../'+dir_name+str(i))
    for i in range(0,len(title)):
            ff=open('./'+title[i],'w')
            ff.writelines(content[i])
            ff.close()
    os.chdir(os.pardir+init_dir)

def restore_last_info_for_control(MW_param):
    """ restore run info for controlling """
    
    pattern=re.compile(r'''condor_id/\s*(?P<run_list>.+)\s*/job/\s*(?P<job>\d*)''',re.DOTALL)
    launch_job=0
    run_job=[]
    dir_list=[]

    for dir in MW_param.P_listdir+MW_param.MW_listdir:
        ff=open('./SubProcesses/'+dir+'/schedular/condor.log','r')
        text=ff.readline()
        ff.close()
        pat=pattern.search(text)
        if pat:
            launch_job+=int(pat.group('job'))
            prov_job=eval(pat.group('run_list'))
            dir_list+=[dir]*len(prov_job)
            run_job+=prov_job
    return launch_job,run_job,dir_list


#def return_failed(dir,MW_param):
#    """read the failed process and return them in a list """
#
#    try:
#        ff=open('./SubProcesses/'+dir+'/'+MW_param.name+'/failed_job.dat','r')
#    except:
#        return []
#    out=[]
#    while 1:
#        line=ff.readline()
#        if line=='':
#            break
#        out.append(line.replace('\n',''))
#
#    return out


###########################################################################
###################             MAIN PROGRAM             ##################
###########################################################################
#def create_all_MWdir(dir,MW_param):
#    """n: number of data , name : name_run"""
#    
#    run_name=MW_param.name
#    try:
#        os.chdir('./SubProcesses/'+dir+'/schedular')
#    except:
#        os.mkdir('./SubProcesses/'+dir+'/schedular')
#        os.chdir('./SubProcesses/'+dir+'/schedular')
#
#    os.system('mkdir ../'+run_name+'&>/dev/null')
#    dir_sche=create_dir(MW_param)
#    dir_sche.all()
#    os.chdir("../../../")
#    print 'created',dir_sche.created,'directories'
#    return dir_sche.created

###########################################################################
###################            MAIN PROGRAM 2            ##################
###########################################################################

#def create_all_Pdir(dir,MW_param):
#    """n: number of data , name : name_run"""##
#
#    #re-init global parameter
#    run_name=MW_param.name
##    os.system(' mkdir ./SubProcesses/'+dir+'/schedular') 
#    os.chdir('./SubProcesses/'+dir+'/schedular')
#    os.system(' mkdir ../'+run_name+'&>/dev/null') 
#    dir_sche=create_dir(MW_param)
#    dir_sche.all()
#    os.chdir("../../../")
#    print 'created',dir_sche.created,'directories'
#    return dir_sche.created


def create_all_schedular(MWparam):

    for directory in MWparam.MW_listdir+MWparam.P_listdir:
        try:
            os.mkdir('./SubProcesses/'+directory+'/schedular')
        except:
            pass






##############################################################################################
##                                  Single Launch                                           ##
##############################################################################################
if __name__=='__main__':


#reupdate all the active card to a certain nuber of event
    #import global run opt
    import MW_param
    MW_param.go_to_main_dir()
    MWparam=MW_param.MW_info('MadWeight_card.dat')

    #launch the check
    create_obj=create_dir(MWparam)
    create_obj.update_card_status()




