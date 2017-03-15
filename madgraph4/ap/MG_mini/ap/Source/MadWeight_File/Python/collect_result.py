#!/usr/bin/env python
##########################################################################
##                                                                      ##
##                               MadWeight                              ##
##                               ---------                              ##
##########################################################################
##                                                                      ##
##   author: Mattelaer Olivier (CP3)                                    ##
##   email:  olivier.mattelaer@uclouvain.be                             ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   license: GNU                                                       ##
##   last-modif:25/03/08                                                ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   Content                                                            ##
##   -------                                                            ##
##      +collect_schedular                                              ##
##      +Class Collect_dir1                                             ##
##      |     +  init                                                   ##
##      |     +  write_failed                                           ##
##      +Class Collect_P_dir                                            ##
##      |     +  init                                                   ##
##      |     +  collect_cross_data                                     ##
##      +Class Collect_MW_dir                                           ##
##      |     +  init                                                   ##
##      |     +  create_list                                            ##
##      |     +  create_refine_list                                     ##
##      |     +  write_output                                           ##
##      +Class Collect_dir                                              ##
##      |     +  init                                                   ##
##      +Class Collect_All_dir                                          ##
##      |     +  init                                                   ##
##      |     +  collect_all_data                                       ##
##      |     +  normalise_weight                                       ##
##      |_    +  collect_proc_data                                      ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   Note                                                               ##
##   ----                                                               ##
##                                                                      ##
##     1) All local  routines must be launched from schedular           ##
##     2) All global routines must be launched from bin                 ## 
##     3) All routines wich start in a directory must finish in the     ##
##            same directory                                            ## 
##                                                                      ##
##########################################################################
##
## BEGIN INCLUDE
##
import string
import os
import sys
import re
import time
from math import log,exp,sqrt
from MW_param import MW_info,go_to_main_dir
from clean import Clean_event
##
## END INCLUDE
##
#########################################################################
##                    START CODE
#########################################################################
def collect_schedular(MW_param):

    go_to_main_dir()
    run_name=MW_param.name

    if MW_param.norm_with_cross:    #check if we use normalisation by CS
        print 'collecting cross section results'
        for dir in MW_param.P_listdir:
            os.chdir('./SubProcesses/'+dir+'/schedular')
            Collect_P_dir(dir,MW_param)
            os.chdir('../../../')
        # suppress lhe file and vegas-grid
        Clean_event(run_name)

    print 'collecting output  weight results'
    for dir in MW_param.MW_listdir:
        os.chdir('./SubProcesses/'+dir+'/schedular')
        Collect_MW_dir(dir,MW_param)
        os.chdir('../../../')
    All_Col=Collect_All_dir(MW_param)

#1########################################################################
class Collect_dir1:
    " all collect routine in a class "
    
    #2########################################################################
    def __init__(self,dir_name,MW_param):
        """ initialize collect data """

        self.info=MW_param #MW_info object
        self.run_name=self.info.name
        self.dir_name=dir_name
        self.failed=[]

    #2########################################################################
    def write_failed(self):
        """ write in a file the failed subprocess """

        ff=open('../'+self.info.name+'/failed_job.dat','w')
        for i in range(0,len(self.failed)):
            ff.writelines(str(self.failed[i][0])+' '+str(self.failed[i][1])+' \n')
        ff.close()




    
#1########################################################################
class Collect_P_dir(Collect_dir1):

    def __init__(self,dir_name,MW_param):
        
        Collect_dir1.__init__(self,dir_name,MW_param)
        self.collect_cross_data()
        self.write_failed()


    #2#########################################################################
    def  collect_cross_data(self):
        "collect cross section for each param_card.dat"

        num_card=len(self.info.actif_param)
        run_name=self.info.name
        pattern=re.compile(r'''\s*''')
        cross={}
        err={}

        for i in [num-1 for num in self.info.actif_param]:
            try:
                ff=open('../'+run_name+'/'+run_name+'_1_'+str(i)+'/results.dat','r')
            except:
                print 'failed in ', os.getcwd()+'/../'+run_name+'/'+run_name+'_1_'+str(i)
                self.failed.append([1,i])
                continue
            
            line=ff.readline()
            ff.close()

            data=pattern.split(line)
            if data[0]=="":
                cross[i]=data[1]
                err[i]=data[2]
            else:
                 cross[i]=data[0]
                 err[i]=data[1]
        print ' collect suceeded in ',num_card-len(self.failed),'/',num_card,'directories'
        
        ff=open('../'+run_name+'/cross.out','w')
        if len(self.failed):
            print 'Imposible to normalize with cross section'
            time.sleep(5)
            self.info.norm_with_cross=0

        for i in [num-1 for num in self.info.actif_param]:
            if [1,i] in self.failed:
                ff.writelines(str(i+1)+'\t'+str(-1)+'\t'+str(1)+'\n')
            else:
                ff.writelines(str(i+1)+'\t'+str(cross[i])+'\t'+str(err[i])+'\n')
        ff.close()

        
#1########################################################################       
class Collect_MW_dir(Collect_dir1):

    def __init__(self,dir_name,MW_param):

        Collect_dir1.__init__(self,dir_name,MW_param)
        weight,error,num_int=self.create_list()
        if MW_param.run_opt['refine']:
            self.create_refine_list(weight,error,MW_param.run_opt['refine'])
        print 'collect in process ',dir_name,'suceeded in ',num_int,'/',MW_param.nb_event*len(MW_param.actif_param),'directories'
        self.write_output(weight,error)
        self.write_failed()

    #2########################################################################
    def create_list(self):

#        num_card=int(self.info.nb_card)
        num_dir=int(self.info.nb_event)
        dir_name=self.dir_name
        run_name=self.run_name
        
        list_card=[]
        weight={}
        error={}
        failed=[]
        Pattern=re.compile(r'''\s*''')
        num_int=0
        
        for nb_card in self.info.actif_param:
            for nb_events in range(0,num_dir):
                num_int+=1

                #move to directory
                if dir_name!="obj":
                    os.chdir(os.pardir+os.sep+run_name+os.sep+run_name+'_'+str(nb_card)+'_'+str(nb_events))
                else:
                     os.chdir(os.pardir+os.sep+dir_name)


                if 0: #put to one in order the select the best permutation only as the weight
                    input=list(self.choose_bestperm())
                elif 0: #put to one in order to select a particular permutation
                	input=list(self.select_perm(1))
                elif 0: #recreate the weights.out from details.out
                        input=list(self.find_from_details())
                else:
                    try:
                        ff=open('weights.out','r')
                    except:
                        print "WARNING: no result in directory "+dir_name+'/'+run_name+'_'+str(nb_card)+'_'+str(nb_events)
                        self.failed.append([nb_card,nb_events])
                        os.chdir('../../schedular')
                        num_int+=-1
                        continue

                    #read weight
                    has_content=0
                    while 1:
                        line=ff.readline()
                        if line=="":
                            break
                        input=Pattern.split(line)
                        while 1:
                            try:
                                input.remove('')
                            except:
                                break

                    #complete output information    
                if len(input)==2:
                        has_content=1
                        if not float(input[0]):
                            print "WARNING: zero result in directory "+dir_name+'_'+str(nb_card)+'_'+str(nb_events)
                            self.failed.append([nb_card,nb_events])
                        if(not nb_card in list_card):
                            list_card.append(nb_card)
                            weight[nb_card]={nb_events:float(input[0])}
                            error[nb_card]={nb_events:float(input[1])}
                        else:
                            weight[nb_card][nb_events]=float(input[0])
                            error[nb_card][nb_events]=float(input[1])                        
                #elif(has_content==0):
                #        print "WARNING: empty file in directory "+dir_name+'_'+str(nb_card)+'_'+str(nb_events)
                #        self.failed.append([nb_card,nb_events])
                #        num_int+=-1
                #        break
                os.chdir('../../schedular')

        return weight,error,num_int

    def choose_bestperm(self):
        """ select the best permutation for the weight"""

        ff=open('details.out','r')
        pat=re.compile(r'''^\s*\d+\s*\|\|\s*\d+\s*\|\|\s*(?P<value>[\de.+-]+)\s*\|\|\s*(?P<error>[\deE.+-]+)\s*$''',re.I)
    
        max=0
        i=0
        for line in ff:
            if pat.search(line):
                i+=1
                value=float(pat.search(line).group('value'))
                if value>max:
                    max=value
                    error=pat.search(line).group('error')
                
        return max/i,error/i

    def find_from_details(self):
        """ recompute the weights.out from details.out """

        ff=open('details.out','r')
        pat=re.compile(r'''^\s*\d+\s*\|\|\s*\d+\s*\|\|\s*(?P<value>[\de.+-na]+)\s*\|\|\s*(?P<error>[\de.+-na]+)\s*$''',re.I)

        i=0
        total=0
        total_err=0
        for line in ff:
            if pat.search(line):
#                print line
                i+=1
                value=pat.search(line).group('value').lower()
                err=float(pat.search(line).group('error'))
#                print value,err
                if value in ['nan','NAN'] or float(value)==0:
#                    print 'nan',value,value==float('nan')
                    value=0
                    err=0
                        

                total+=float(value)
                total_err+=(err)**2
#                print value,err,total,err
        ff.close()
#        print i,total,sqrt(total_err)
        gg=open('weights.out','w')
        gg.writelines(str(total/i)+'\t'+str(sqrt(total_err)/i))
                    
        return total/i,sqrt(total_err)/i
                
    def select_perm(self,perm):
        """ select the permutation value for the weight"""

        ff=open('details.out','r')
        pat=re.compile(r'''^\s*\d+\s*\|\|\s*\d+\s*\|\|\s*(?P<value>[\de.+-]+)\s*\|\|\s*(?P<error>[\deE.+-]+)\s*$''',re.I)
    
        i=0
        for line in ff:
            if pat.search(line):
            	i+=1
                if i==perm:
                    value=float(pat.search(line).group('value'))
                    error=pat.search(line).group('error')
                
        return max/i,error/i        

    #2########################################################################
    def create_refine_list(self,weight,error,minimal_precision):
        """ update the failed list with the content of job without a sufficient precision (in order to relaunch job)"""

        minimal_precision=float(minimal_precision)

        for card  in weight.keys():
            for event in weight[card].keys():
                if not weight[card][event] or minimal_precision<error[card][event]/weight[card][event]:
                    self.failed.append([card,event])
            

    #2########################################################################
    def write_output(self,weight,error):
        """ write the weight file for this subProcess """
        
        run_name=self.run_name
        ff=open('../'+run_name+'/'+run_name+'_weights.out','w')
        for i in weight.keys():
            for j in weight[i].keys():
                ff.writelines(str(i)+'.'+str(j)+"\t"+str(weight[i][j])+"  "+str(error[i][j])+"\n")

        
#1########################################################################       
class Collect_dir(Collect_dir1,Collect_MW_dir,Collect_P_dir):
    """ determine wich class is the good one """
    
    def __init__(self,pos,MW_param):

        MW_pattern=re.compile(r'''\bMW_P''') #need to split type of data
        P_pattern=re.compile(r'''\bP''') #need to split type of data
        os.chdir(pos+'/schedular/')

        name_candidate=pos.split('/')
        for i in range(-1,-len(name_candidate)-1,-1):
            if  MW_pattern.search(name_candidate[i]):
                dir_name=name_candidate[i]
                run_type='MW'
                break
            elif  P_pattern.search(name_candidate[i]):
                dir_name=name_candidate[i]
                run_type='P'
                break

        if run_type=='MW':
            Collect_MW_dir(dir_name,MW_param)
        else:
            Collect_P_dir(dir_name,MW_param)

        os.chdir('../../../')

##     #2########################################################################       
##     def collect_proc_data(self,name,old_pos,num_cards=-1,num_events=-1):
##         global pos,run_name

##         pattern=re.compile(r'''MW_''') #need to split type of data

##         run_name=name

##         pos=os.getcwd()

##         print "collect data in dir ",old_pos
##         if num_cards<0 or num_events<0:    
##             num_cards,num_events=verif_num_dir(run_name)
##         if num_cards<0 or num_events<0: 
##             print "FATAL ERROR: CANNOT FIND DIRECTORY NUMBER"
##             sys.exit()

##         if not pattern.search(pos):
##             collect_cross_data(num_events)   #remind the shift in position for the two directory
##             os.chdir('../../../')
##             return

##         weight,error,num_int=create_list(num_cards,num_events,run_name)
##         write_output(weight,error)
##         print 'succeed in  ',num_int,' on ',num_cards*num_events,' directory'
##         os.chdir('../../../')
        

                         
#1########################################################################
class Collect_All_dir:

    #2########################################################################
    def __init__(self,MW_param):

        self.info=MW_param
        self.run_name=self.info.name

        self.collect_all_data(self.run_name,self.info.MW_listdir)
        #if self.info.norm_with_cross:
        self.normalise_weight()

    #2########################################################################
    def collect_all_data(self,name,process_list,condor=1):

        pattern=re.compile(r'''(?P<param>\d*).(?P<event>\d*)\s+(?P<weigth>[\d.e+-]+|nan)\s+(?P<error>[\d.e+-]+|nan)''',re.I)
        dico={}

        for dir_name in process_list:
            print os.getcwd()
            ff=open('./SubProcesses/'+dir_name+'/'+name+'/'+name+'_weights.out','r')
            while 1:
                line=ff.readline()
                if line=="":
                    break
                if pattern.search(line):
                    tag=pattern.search(line).group('param')+'.'+pattern.search(line).group('event')
                    weigth=float(pattern.search(line).group('weigth'))
                    error=float(pattern.search(line).group('error'))**2

                    if dico.has_key(tag):
                        dico[tag][0]+=weigth
                        dico[tag][1]+=error
                    else:
                       dico[tag]=[weigth,error]
            ff.close()
        try:
            gg=open('./Events/'+name+'/'+name+'_weights.out','w')
        except:
            sys.exit()
            os.mkdir('./Events/'+name)
            gg=open('./Events/'+name+'/'+name+'_weights.out','w')

            #            try:
            #                prov= dico[str(i)+".0"]
            #            except:
            #                break #end of the
        #the two following line take all the input of the dico -separate param/event number -convert those in int and sort everything
        tag=[[int(data2) for data2 in data.split('.')] for data in dico.keys() ]
        tag.sort()
        for i,j in tag:
            input=dico[str(i)+'.'+str(j)]
            gg.writelines(str(i)+'.'+str(j)+"\t"+str(input[0])+"\t"+str(sqrt(input[1]))+"\n")
                
        gg.close()


    #2########################################################################
    def  normalise_weight(self):

        run_name=self.run_name
        P_proclist=self.info.P_listdir


        conv=1/(389379.66*1000) #conv pb -> Gev^-2


##         if use_condor==0:
##             for name in P_proclist:
##                 os.chdir('./SubProcesses/'+name+'/schedular')
##                 num_dir=verif_num_dir(dir_name)
##                 collect_cross_data(num_dir)
##                 os.chdir('../../../')

        #load 
        MW_pattern=re.compile(r'''(?P<param>\d*).(?P<event>\d*)\s+(?P<weight>[\d.e+-]+|nan)\s+(?P<error>[\d.e+-]+|nan)''',re.I)
        P_pattern=re.compile(r'''\s*(?P<param>\d*)\s+(?P<cross>[\d.e+-]+|nan)\s+(?P<error>[\d.e+-]+|nan)''',re.I)

        total_cross={}
        #a) load norma
        for P_dir in P_proclist:
            P_dico={}
            ff=open('./SubProcesses/'+P_dir+'/'+run_name+'/cross.out')
            while 1:
                line=ff.readline()
                if line=="":
                    break
                if P_pattern.search(line):
                    tag=P_pattern.search(line).group('param')           
                    cross=float(P_pattern.search(line).group('cross'))*conv
                    error=float(P_pattern.search(line).group('error'))*conv         
                    P_dico[tag]=[cross,error**2]
                    if total_cross.has_key(tag):
                        total_cross[tag][0]+=cross
                        total_cross[tag][1]+=error**2
                    else:
                        total_cross[tag]=[cross,error**2]
            ff.close()

            #b) write result in the subprocess
            ff=open('./SubProcesses/MW_'+P_dir+'/'+run_name+'/'+run_name+'_weights.out','r')
            gg=open('./SubProcesses/MW_'+P_dir+'/'+run_name+'/'+run_name+'_weights_norm.out','w')
            while 1:
                line=ff.readline()
                if line=="":
                    break
                pat=MW_pattern.search(line)
                if pat:
                    param=pat.group('param')
                    event=pat.group('event')
                    weight=float(pat.group('weight'))
                    error=float(pat.group('error'))
                    if P_dico.has_key(param):
                        if (P_dico[param][0] and weight):
                            new_error=weight/P_dico[param][0]*sqrt(P_dico[param][1]/P_dico[param][0]**2+error**2/weight**2) #uncorrelated error
                            #new_error=abs(error/P_dico[param][0])+abs(weight*P_dico[param][1]/(P_dico[param][0])**2) #corelated error
                            gg.writelines(param+"."+event+"\t"+str(weight/P_dico[param][0])+"\t"+str(new_error)+"\n")
                        else:
                            gg.writelines(param+"."+event+"\t"+str(0)+"\t"+str(0)+"\n")
            ff.close()
            gg.close()

        #b)normalise final weight
        ff=open('./Events/'+run_name+'/'+run_name+'_weights.out','r')
        gg=open('./Events/'+run_name+'/'+run_name+'_norm_weights.out','w')
        while 1:
            line=ff.readline()
            if line=="":
                break
            pat=MW_pattern.search(line)        
            if pat:
                param=pat.group('param')
                event=pat.group('event')
                weight=float(pat.group('weight'))
                error=float(pat.group('error'))
                if total_cross.has_key(param):
                    if (total_cross[param][0] and weight):
                        new_error=weight/total_cross[param][0]*sqrt(error**2/weight**2+total_cross[param][1]/total_cross[param][0]**2) #uncorrelated error
                        #new_error=abs(error/total_cross[param][0])+abs(weight*total_cross[param][1]/(total_cross[param][0])**2) #correlated error
                        gg.writelines(param+"."+event+"\t"+str(weight/total_cross[param][0])+"\t"+str(new_error)+"\n")
                    else:
                        gg.writelines(param+"."+event+"\t"+str(0)+"\t"+str(0)+"\n")

        ff.close()
        gg.close()
        #c) write the cross section file
        hh=open('./Events/'+run_name+'/'+run_name+'_cross_weights.out','w')
        for param in self.info.actif_param:
            if total_cross[str(param)][0]:
                hh.writelines(str(param)+"\t"+str(total_cross[str(param)][0])+"\t"+str(sqrt(total_cross[str(param)][1]))+"\n")
                                    

        hh.close()
        return


###########################################################################
###################             TEST                      ##################
############################################################################           
if __name__=='__main__':

    print os.getcwd()
    go_to_main_dir()

    MW_content=MW_info('MadWeight_card.dat')
    #collect_schedular(MW_content)
    Collect_All_dir(MW_content)

    
    
        
