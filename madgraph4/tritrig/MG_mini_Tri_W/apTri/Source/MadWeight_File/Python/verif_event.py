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
##   last-modif:13/08/08                                                ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   Content                                                            ##
##   -------                                                            ##
##      +verif_event                                                    ##
##      +Lhco_filter                                                    ##
##      |    + init                                                     ##
##      |    + load_particule_number                                    ##
##      |    + extract_file_info                                        ##
##      |    |   +    define_particule_number                           ##
##      |    + init_hlt_cut                                             ##
##      |    + update_hlt_cut                                           ##
##      |    + verif_event                                              ##
##      |    |   +    check_data                                        ##
##                                                                      ##
##########################################################################
#Extension
import os
import popen2
import re
import sys
import time
import diagram_class
import substructure_class
from MW_param import go_to_main_dir


#1 ##############################################
def verif_event(MWparam):

    # 0 ##############
    ####  go to main directory and copy file
    go_to_main_dir()
    try:
        os.mkdir('./Events/'+MWparam.name)
    except:
        pass
    os.system('cp ./Events/input.lhco ./Events/'+MWparam.name+'/')
    # 1 ##############
    ####   take run information
    for MW_dir in MWparam.MW_listdir:
        select=Lhco_filter(MW_dir,'input.lhco',MWparam)

#1 ###############################################################################################################
class Lhco_filter:

    class Lepton_Without_Charge_Error(Exception): pass
    
    #2 ###############################################################################################################
    def __init__(self,directory,lhco_file='',MWparam=''):
        """ input is either a file containing particule number info or a SubProcesses directory """


        
        if MWparam:
            self.MWparam=MWparam
        else:
            import MW_param
            self.MWparam=MW_param.MW_info('MadWeight_card.dat')

        #treat directory info
        if directory.count('SubProcesses'):
            self.directory=directory
        else:
            self.directory='./SubProcesses/'+directory

        # find number of particle of each type
        if os.path.isfile(self.directory+'info_part.dat'): 
            self.extract_file_info(self.directory)
        else:
            self.load_particle_number(self.directory)

        self.init_hlt_cut()
		
        if self.MWparam.info.has_key('hltcut'):
            self.update_hlt_cut()
        	
        if lhco_file:
            self.verif_event(lhco_file)
        	
    #2 ###############################################################################################################        
    def load_particle_number(self,directory):
        """ extract the number of particule from the iconfigs """

        print os.getcwd()
        diag=diagram_class.MG_diagram(directory,1)

        list=['jet','bjet','el-','el+','mu-','mu+','ta-','ta+','inv']
        content=diag.output_type_info()
        
        total=0
        data={}
        for i in range(0,len(list)):
            data[list[i]]=content[i]
            total+=content[i]
        data['n_out']=total
        
        #check status of the b-jet
        if self.MWparam.info['mw_perm']['2']=='F':
            self.use_bjet=1
        elif  self.MWparam.info['mw_perm'].has_key('21') and self.MWparam.info['mw_perm']['21']=='T' :
            self.use_bjet=1
        else:
            self.use_bjet=0
            data['jet']+=data['bjet']
            data['bjet']=0

        self.part_nb=data
	return data

    #2 ###############################################################################################################
    def extract_file_info(self,dir):

        os.chdir('./SubProcesses/'+dir)

        ff=open('./info_part.dat','r')
        split=re.compile(r'''\s''')
        text=ff.readline()
        ff.close()
        list=['jet','bjet','el-','el+','mu-','mu+','ta-','ta+','inv']
        info=split.split(text)[1:]
        data={}
        total=0
        for i in range(0,len(list)):
            data[list[i]]=int(info[i])
            total+=int(info[i])
        data['n_out']=total
        os.chdir('../..')

        #check status of the b-jet
        if self.MWparam.info['mw_perm']['2']=='F':
            self.use_bjet=1
        elif  self.MWparam.info['mw_perm'].has_key('21') and self.MWparam.info['mw_perm']['21']=='T' :
            self.use_bjet=1
        else:
            self.use_bjet=0
            data['jet']+=data['bjet']
            data['bjet']=0



        self.part_nb=data
        return data

    #2 ###############################################################################################################
    def define_particle_number(self,particle,number):
        
        list=['jet','bjet','el-','el+','mu-','mu+','ta+','ta-','inv']
        if particle not in list:
            print 'unknown type of particle'
            return
        else:
            self.part_nb[particle]=int(number)

        return

    #2 ###############################################################################################################
    def init_hlt_cut(self):
        """init hlt cut"""

        # HLT cut
        cutvalue={}
    	no='n'
    	nocut=[[no,no],[no,no],[no,no]]
    	cutvalue[0]=list(nocut)#[[no,no],[no,no],[no,no]]#[PTmin,PTmax],[etamin,etamax],[phimin,phimax]    # PHOTON	
    	cutvalue[1]=list(nocut)#[[7,no],[-2.4,2.4],[no,no]]#[PTmin,PTmax],[etamin,etamax],[phimin,phimax]  # elec
    	cutvalue[2]=list(nocut)#[[7,no],[-2.4,2.4],[no,no]]#[PTmin,PTmax],[etamin,etamax],[phimin,phimax]  # muon
    	cutvalue[3]=list(nocut)#[[no,no],[no,no],[no,no]]#[PTmin,PTmax],[etamin,etamax],[phimin,phimax]    # tau
    	cutvalue[4]=list(nocut)#[[40,no],[-2.4,2.4],[no,no]]#[PTmin,PTmax],[etamin,etamax],[phimin,phimax] # jet
    	cutvalue[5]=list(nocut)#[[40,no],[-2.4,2.4],[no,no]]#[PTmin,PTmax],[etamin,etamax],[phimin,phimax] # bjet	 
    	cutvalue[6]=list(nocut)#[[no,no],[-2.4,2.4],[no,no]]#[PTmin,PTmax],[etamin,etamax],[phimin,phimax] # missing ET	        
    	cutvalue[7]=list(nocut)#                                                                           # initial energy
        
        self.hltcut=cutvalue
        


    #2 ###############################################################################################################
    def update_hlt_cut(self):
        """ take the hlt cut from the Madweight card """

        for key in self.MWparam.info['hltcut']:
        
            value2=int(key)%10
            value1=(int(key)-value2)/10
            if type(self.MWparam.info['hltcut'][key])==list:
                self.hltcut[value1][value2]=[float(self.MWparam.info['hltcut'][key][0]),float(self.MWparam.info['hltcut'][key][1])]
            else:
                self.hltcut[value1][value2]=[float(self.MWparam.info['hltcut'][key]),'n']                

    #2 ###############################################################################################################
    def verif_event(self,file):
        """ use the cuts to select event in file """ 

        selected_events=0
        try:
            f_in=open('./Events/'+file,'r')
        except:
            print 'FATAL ERROR: No experimental file \"'+file+'\" in directory Events.'
            sys.exit()
            
        #supress first X events:
        if self.MWparam.info['mw_run'].has_key('21'):
            start=int(self.MWparam.info['mw_run']['21'])
        else:
            start=0
            
        f_out=open(self.directory+'/verif.lhco','w')
        p_new_block=re.compile('''^\s*0''')
        p_good=re.compile('''^\s*\d''')

        data_num=self.part_nb["n_out"]+2-self.part_nb['inv'] #+2 (init line and met line)
        status=0
        block=[]
        while 1:
            line=f_in.readline()
            if line=="" and status:
                if len(block) in [data_num,data_num+1]:
                    out=self.check_data(block)
                    if out!='':
                        selected_events+=1
                        if selected_events>start:
                            f_out.writelines(out)
                break
            elif line=="":
                break
            
            if p_new_block.search(line) and status:
                if len(block)in [data_num,data_num+1]:
                    out=self.check_data(block)
                    if out!='':
                        selected_events+=1
                        if selected_events>start:
                            f_out.writelines(out)
                block=[line]
            elif p_new_block.search(line):
                status=1
                block.append(line)
            elif status:
                if p_good.search(line):
                    block.append(line)

        print selected_events-start,'selected  events for ',self.directory,' subprocess'
        return selected_events-start


    #####################
    def check_data(self,block):

        #  3 step
        # A) order block in type order
        #    A.1) need to recognize which type it is
        #    A.2) control type by type
        #    A.3) put cut on PT,eta,phi
        # B) order block in MG order
        #    B.1) put in type order
        #    B.2) define matching functions
        # C) put in string format

        ### Step 0 parametrization
        ####### A.1 : recognize type
        pattern=re.compile(r'''^\s*(?P<card>\d+)\s+               #cardinal
                               (?P<typ>\d+)                # type: 0 = photon,1 = elec,2 = muon,3 = hadronic tau,4 = jet,6 =MTE
                               \s+(?P<eta>[+-\.\de]+)             # pseudorapidity
                               \s+(?P<phi>[+-\.\de]+)\s+       # phi
                               (?P<pt>[+-\.\de]*)\s+       #pt
                               (?P<jmass>[+-\.\de]+)\s+    #invariant mass of the object
                               (?P<ntrk>[+-\.\de]+)\s+     #number of tracks associated( muliplied by charge for lepton)
                               (?P<btag>[+-\.\de]+)\s+     #jet: !=0 taggued b//muon: closest jet
                               (?P<had_em>[+-\.\de]+)\s+  #hadronic versus electromagnetic energy deposited
                               (?P<dummy1>[+-\.\de]+)\s+   # user free at this stage
                               (?P<dummy2>[+-\.\de]+)\s+$  # user free at this stage
                        ''',80) # 80= VERBOSE +ignore case
        ### Step A
        ####### A.3 :input for cut	    
        cutvalue=self.hltcut
        data=self.part_nb
        no='n'
        nocut=[[no,no],[no,no],[no,no]]
        type=[0]

    
        for j in range(1,len(block)):
            line=block[j]
            obj=pattern.search(line)
            type.append(int(obj.group('typ')))

            if type[j]==4:
                btag=int(float(obj.group('btag')))
                if btag and self.use_bjet:
                    type[j]=5
            elif(type[j] in [1,2,3]):
                charge=int(float(obj.group('ntrk')))
                if charge==0:
                    raise self.Lepton_Without_Charge_Error
                type[j]*=charge/abs(charge)
		
	    ### Step A
	    ####### A.3 control of cut

            if cutvalue[abs(type[j])] not in [nocut,no]:
                    if cutvalue[abs(type[j])][0][0]!=no:
			if float(obj.group('pt'))<cutvalue[abs(type[j])][0][0]:
                            return ''
                    if cutvalue[abs(type[j])][0][1]!=no:
                        if float(obj.group('pt'))>cutvalue[abs(type[j])][0][1]:
                            return ''
                    if cutvalue[abs(type[j])][1][0]!=no:
			if float(obj.group('eta'))<cutvalue[abs(type[j])][1][0]:
                            return ''
                    if cutvalue[abs(type[j])][1][1]!=no:
                        if float(obj.group('eta'))>cutvalue[abs(type[j])][1][1]:
                            return ''
                    if cutvalue[abs(type[j])][2][0]!=no:
                        if float(obj.group('phi'))>cutvalue[abs(type[j])][2][0]:
                            return ''
                    if cutvalue[abs(type[j])][2][1]!=no:
                        if float(obj.group('phi'))>cutvalue[abs(type[j])][2][1]:
                            return ''
	
        ### Step A
        ####### A.2 :control type by type
        el_p=0
        el_m=0
        mu_p=0
        mu_m=0
        ta_p=0
        ta_m=0
        jet=0
        bjet=0
        for j in range(1,len(type)):
            if type[j]==1:
                el_p+=1
            elif type[j]==2:
                mu_p+=1
            elif type[j]==3:
                ta_p+=1
            elif type[j]==4:
                jet+=1
            elif type[j]==5:
                bjet+=1
            #antiparticle
            elif type[j]==-1:
                el_m+=1
            elif type[j]==-2:
                mu_m+=1
            elif type[j]==-3:
                ta_m+=1
                
        if (data['el-']!=el_m or data['mu-']!=mu_m or data['jet']!=jet or data['bjet']!=bjet or data['ta-']!=ta_m):
            return ''
        if (data['el+']!=el_p or data['mu+']!=mu_p or data['ta+']!=ta_p):
            return ''
	    
        ### Step B
        ####### B.1 :assign in type order
        type_pos=[]
        for j in range(1,len(type)):
            if type[j]==4:
                type_pos.append(j)
    
        for j in range(1,len(type)):
            if type[j]==5:
                type_pos.append(j)        
                        
        for j in range(1,len(type)):
            if type[j]==-1:
                type_pos.append(j)

        for j in range(1,len(type)):
            if type[j]==1:
                type_pos.append(j)

        for j in range(1,len(type)):
            if type[j]==-2:
                type_pos.append(j)
            
        for j in range(1,len(type)):
            if type[j]==2:
                type_pos.append(j)

        for j in range(1,len(type)):
            if type[j]==-3:
                type_pos.append(j)
            
        for j in range(1,len(type)):
            if type[j]==3:
                type_pos.append(j)
            
        ### Step C
        ####### C.1 : put in string format
        out=block[0]
        for j in range(0,len(type_pos)):
            out+=block[type_pos[j]]
        pat6=re.compile(r'''^\s*\d+\s+6\s+''')
        pat7=re.compile(r'''^\s*\d+\s+7\s+''')

        if(pat6.search(block[-2])):
            out+=block[-2]
        if(pat6.search(block[-1]) or pat7.search(block[-1])):
            out+=block[-1]

        return out


     
                
#########################################################################
#########################################################################
if(__name__=="__main__"):
    #import MW_param
    #info=MW_param.MW_info('MadWeight_card.dat')
    #verif_event(info)
    from MW_param import go_to_main_dir
    go_to_main_dir()
    Lhco_filter('proc_card.dat')
