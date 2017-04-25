#!/usr/bin/env python
##########################################################################
##                                                                      ##
##                               MadWeight                              ##
##                               ---------                              ##
##########################################################################
##                                                                      ##
##   author: Mattelaer Olivier (CP3)                                    ##
##       email:  olivier.mattelaer@uclouvain.be                         ##
##   author: Artoisenet Pierre (CP3)                                    ##
##       email:  pierre.artoisenet@uclouvain.be                         ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   license: GNU                                                       ##
##   last-modif:10/06/08                                                ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   Content                                                            ##
##   -------                                                            ##
##      +go_to_main_dir                                                 ##
##      +read_card                                                      ##
##      +check_for_help                                                 ##
##      +Class MW_param                                                 ##
##      |     +  init                                                   ##
##      |     |    + init_run_opt                                       ##
##      |     +  check_info
##      |     +  check_condor                                           ##
##      |     +  detect_SubProcess                                      ##
##      |     +  update_nb_card                                         ##
##      |     +  number_of_P_run                                        ##
##      |     +  set_run_opt                                            ##
##      |     |    + control_opt                                        ##
##                                                                      ##
##########################################################################
##
## BEGIN INCLUDE
##
import sys
import os
import re
import stat
import Cards
##
## END INCLUDE
## GLOBAL DEFINITION

num_to_tag={1:'param',2:'analyzer',3:'compilation',4:'event',5:'dir',6:'launch',7:'control',8:'collect',9:'plot',-1:'relaunch',-2:'clean',-3:'refine'}
tag_to_num={'param':1,'analyzer':2,'compilation':3,'event':4,'dir':5,'launch':6,'control':7,'collect':8,'plot':9,'relaunch':-1,'clean':-2,'refine':-3}
last_step=9

#1#########################################################################
##                    START CODE
#1#########################################################################
def go_to_main_dir():
    """ move to main position """
    pos=os.getcwd()
    last=pos.split(os.sep)[-1]
    if last=='bin':
        os.chdir(os.pardir)
        return
    if last=='Python':
        os.chdir(os.pardir+os.sep+os.pardir+os.sep+os.pardir)
        return
    
    list_dir=os.listdir('./')
    if 'bin' in list_dir:
        return
    else:
        print 'Error: script must be executed from the main, bin or Python directory'
        
        sys.exit()



#1#########################################################################
def read_card(name_card):
    """put all card information in a dictionary"""

    card=Cards.Card(name_card)
    return card.info

#1#########################################################################
def check_for_help(opt):
    """ check if the user use the -h or -help option or simply invalid option """

    opt_pat=re.compile(r'''-?(?P<opt>\w*)[+-]?''',re.I)
    help=0
    authorized_opt=tag_to_num.keys()+['version']
    for i in range(0,len(num_to_tag)):
        authorized_opt+=[str(i)]
    for i in range(1,len(opt)):
        if opt_pat.search(opt[i]):
            if opt_pat.search(opt[i]).group('opt').lower() not in authorized_opt:
                try:
                    int(opt_pat.search(opt[i]).group('opt').lower())
                except:
                    os.system('cat ./Source/MadWeight_File/MWP_template/Readme.txt')
                    sys.exit()
            if opt_pat.search(opt[i]).group('opt').lower()=='version':
                print 'MadWeight Version'
                os.system('cat ./Source/MadWeight_File/MW_TemplateVersion.txt')
                sys.exit()

#1#########################################################################
class MW_info(dict):
    """ class containing all the option/information from the run """

    #2#########################################################################
    def __init__(self,card_name):
        """ init all the param for the run """
        self.mw_card=Cards.Card(card_name)
        self.info=self.mw_card.info
        for key,value in self.info.items():
            self[key]=value

        dict.__init__(self.info)
        self.check_info()
        #assign special value
        self.nb_event=int(self.info['mw_run']['2'])
        self.nb_card=self.number_of_P_run()
        self.check_condor()
        self.name=self.take_run_name()
        self.P_listdir,self.MW_listdir=self.detect_SubProcess()
        self.init_run_opt()
        self.def_actif_param()


    #3#########################################################################
    def init_run_opt(self,value=1):
        """ init all the run scheduling paramater to value """
        self.run_opt={}
        self.run_opt['param']=value
        self.run_opt['analyzer']=value
        self.run_opt['compilation']=value
        self.run_opt['event']=value
        self.run_opt['dir']=value
        self.run_opt['launch']=value
        self.run_opt['control']=value
        self.run_opt['collect']=value
        self.run_opt['plot']=value 
        self.run_opt['madweight_main']=value
        self.run_opt['relaunch']=0 #only for bugging case... -> desactivate
        self.run_opt['refine']=0 #only for bugging case... -> desactivate
        self.run_opt['clean']=0    #dangerous... -> desactivate
        self.control_opt()

    #2#########################################################################
    def check_info(self):
        """ assign default value if not defined already and check the input type
            those default value and the type are defined in MW_param_default.inc
            structure of this file:
            block tag type value #comment
        """
        #define convertissor
        def pass_in_integer(value):
            return int(value)
        def pass_in_logical(value):
            if value in ['1','t','T','.true.']:
                return 1
            else:
                return 0
        def pass_in_float(value):
            return float(value)

        for line in open('./Source/MadWeight_File/Python/MW_param_default.inc'):
            line=line.split('#')[0] #remove comment
            splitline=line.split()  #split the data
            if len(splitline)!=4:
                continue
            #assign element
            block=splitline[0].lower()
            tag=splitline[1].lower()
            type=splitline[2].lower()
            value=splitline[3]
            #check if exist -> default
            try:
                self[block][tag]
            except:
                try:
                    self[block][tag]=value
                except:
                    self[block]={tag:value}
            #change type
            if type in ['integer','logical','float']:
                self[block][tag]=eval('pass_in_'+type+'(self[block][tag])')
    
        
                
        
    #2#########################################################################
    def check_condor(self):
        """ assign variable cluster and normalisation """

        self.cluster=self.info['mw_run']['1']
        self.norm_with_cross=self.info['mw_run']['4']
        self.condor_req=self.info['mw_run']['11']

        #type is automaticaly updated now
        #self.cluster=int(condor)
        #if norm_with_cross=="F":
        #    self.norm_with_cross=0
        #else:
        #    self.norm_with_cross=1

    #2#########################################################################
    def take_run_name(self):
        """take the run name in run_card"""
        name="run"
        Pattern=re.compile(r'''\'(\S*)\'\s*=\s*run_tag''',re.I)
        card=open("./Cards/run_card.dat")

        while 1:
            line=card.readline()
            if line=='':
                break
            
            if Pattern.search(line):
                name=Pattern.search(line).groups()[0]
                break
        return name


    #2#########################################################################
    def detect_SubProcess(self):

        MW_SubProcess_list=[]
        P_SubProcess_list=[]

        list_dir=os.listdir("./SubProcesses/")
        for name in list_dir:
            try:           
                st = os.lstat(os.path.join("./SubProcesses/", name))
            except os.error:
                continue
            if stat.S_ISDIR(st.st_mode):
                if name[:2]=="MW":
                    MW_SubProcess_list.append(name)
                elif self.norm_with_cross and name[0]=='P':
                    P_SubProcess_list.append(name)                

        return P_SubProcess_list,MW_SubProcess_list

    #2##########################################################################
    def update_nb_card(self):
        "take the info from MW_runcard.dat"
        self.nb_card=self.number_of_P_run()
        self.def_actif_param()
        
    #2##########################################################################
    def number_of_P_run(self):
        "take the info from MW_runcard.dat"

        #check if we use different param_card.dat
#        if self.info["mw_parameter"]["1"]=="1":
        j=1
        while 1:
            if os.path.isfile('Cards/param_card_'+str(j)+'.dat'): j+=1
            elif(j==1): return j
            else: return j-1
            


    #2##########################################################################
    def load_events(self):
        "detect the number of events for P and MW run"

        self.P_nevents=self.info['mw_run']['5']
        self.MW_nevents=self.info['mw_run']['6']

        
    #2##########################################################################
    def give_block_param_info(self):
        """ return the number of modified parameter and the number of different value for each"""

        nb_block=0
        nb_values=[]
        k=0
        while 1:
            k+=1
            try:
                self.info['mw_parameter'][str(10*k+1)]
            except:
                break
            nb_block+=1
            if type(self.info['mw_parameter'][str(10*k+3)])==list:
                nb_values.append(len(self.info['mw_parameter'][str(10*k+3)]))
            else:
                nb_values.append(1)

        return nb_block,nb_values

    #3########################################################################
    def CardNb_to_ParameterTag(self,num_card):
        """ find from th card number, to which value for each block this card belong
            num_card is the number of the card in the last generation. 
            card in previous generation are not accessible by this routine
            (and are not related to this MadWeight card anyway)
         """

        nb_block,nb_data_by_block=self.give_block_param_info()

        if self['mw_parameter']['1']==2:
            return [num_card-1]*len(nb_data_by_block)

        tag=[]
        for k in range(-1,-nb_block-1,-1):
            tag.append((num_card-1)%nb_data_by_block[k])
            num_card=1+(num_card-(num_card-1)%nb_data_by_block[k])/nb_data_by_block[k]
            tag.reverse()
        return tag

    #2##########################################################################
    def set_run_opt(self,option):
        """analyze option for the run"""

        if len(option)>1:
            self.init_run_opt(0)#put everything to false
        else:
            return
        for i in range(1,len(option)):
            if option[i][0]=='-' and option[i][-1]=='+':
                num=int(option[i][1:-1])
                for j in range(num,last_step+1):
                    self.run_opt[num_to_tag[j]]=1
            elif option[i][0]=='-' and option[i][-1]=='-':
                num=int(option[i][1:-1])+1
                for j in range(1,num):
                    self.run_opt[num_to_tag[j]]=1
            elif option[i][0]=='-':
                num=int(option[i][1:])
                for i in option[i][1:]:
                    self.run_opt[num_to_tag[int(i)]]=1
            elif option[i][-1]=='+':
                num=tag_to_num[option[i][:-1]]
                for j in range(num,last_step+1):
                    self.run_opt[num_to_tag[j]]=1
            elif option[i][-1]=='-':
                num=tag_to_num[option[i][:-1]]+1
                for j in range(1,num):
                    self.run_opt[num_to_tag[j]]=1
            elif '=' in option[i]:
                obj=option[i].split('=')
                tag=obj[0]
                value=obj[1]
                self.run_opt[tag]=value
            else:
                self.run_opt[option[i]]=1
                                                
        self.control_opt()

    #3##########################################################################
    def control_opt(self):
        """analyze option for the run to have coherent input"""


        if self.run_opt['refine']:
            self.run_opt['relaunch']=1
        
        #check value for 'madweight_main'
        for i in range(3,9)+[-1,-3]:
            if self.run_opt[num_to_tag[i]]==1:
                self.run_opt['madweight_main']=1
                break

        if self.run_opt['relaunch']==1:
            self.run_opt['control']=1

    #3##########################################################################
    def def_actif_param(self):
        """ find which card are still actif """

        self.param_is_actif={}
        try:
            ff=open('Cards/mapping_card.dat')
        except:
            for i in range(1,self.nb_card+1):
                self.param_is_actif[i]=1 #if no file defined all card are supose to be used
            self.actif_param=range(1,self.nb_card+1)
            return

        self.actif_param=[]
        for line in ff:
            split=line.split()
            nb=int(split[0])
            actif=int(split[-1])
            self.param_is_actif[nb]=actif
            if actif:
                self.actif_param.append(nb)

        if len(self.param_is_actif)!=self.nb_card:
            print 'WARNING: wrong mapping file'
