#!/usr/bin/env python

#Extension
import string
import sys
import os
import re
import time
import stat
# patch if symbolic directory replace by real file
sys.path.append('./Source/MadWeight_File/Python')
sys.path.append('../Source/MadWeight_File/Python')
#end patch
import write_MadWeight
import create_param
import Cards
from Cards import Card
import cluster_lib as Cluster
from create_run import update_cuts_status
from MW_param import MW_info,go_to_main_dir,check_for_help
from collect_result import Collect_dir,Collect_P_dir,Collect_MW_dir,Collect_All_dir,collect_schedular
from verif_event import verif_event

####################################################
##               PARAMETER
####################################################
rel_pos='../'
####################################################
##               MAIN PROGRAM
####################################################
def Launch_all_SubProcess(MWparam):

    name=MWparam.name
    print 'name :',name
    P_proclist,MW_proclist=MWparam.P_listdir,MWparam.MW_listdir
    #create banner
    if MWparam.run_opt['launch']:
        os.system('mkdir ./Events/'+name)
        os.chmod('./bin/put_banner_MW',0775)
        os.system('./bin/put_banner_MW '+name+'/'+name+'_banner.txt')

    if MWparam.run_opt['compilation']:
        print 'starting program compilation'
        compile_SubProcesses(MW_proclist)
        if MWparam.norm_with_cross:    
            make_symb_link(P_proclist)    
            compile_P_SubProcesses(P_proclist)
            
    if MWparam.run_opt['event']:
        verif_event(MWparam)

    if MWparam.run_opt['refine']:
        print "collecting data to find data with a precision less than",MWparam.run_opt['refine']
        collect_schedular(MWparam)          


    #all cluster operation
    cluster=Cluster.def_cluster(MWparam)
    cluster.driver()

    if MWparam.run_opt['collect']:
        print "collecting data"
        collect_schedular(MWparam)          

#########################################################################
##                      FUNCTIONS
#########################################################################
def     compile_SubProcesses(process_list):
    os.chdir("./Source")
    os.system("make")
    os.chdir("../SubProcesses")
    for name in process_list:
        os.chdir("./"+name)
        #os.system(" rm madweight")
        exit_status=os.system("make > /dev/null")
        if  os.path.isfile("./f77_madweight") and exit_status==0 :
            os.chdir("..")
        else:
            print "fortran compilation error"
            sys.exit()
    os.chdir("..")   
    return

##############################################################################
def     compile_P_SubProcesses(process_list):
    if not os.path.isfile("./Cards/param_card.dat"):
        os.system('cp ./Cards/param_card_1.dat ./Cards/param_card.dat')
    os.chdir("./SubProcesses")
    for name in process_list:
        os.chdir("./"+name)
        #os.system(" rm madevent")
        os.system("make gensym > /dev/null")
        os.system("./gensym > /dev/null")
        exit_status=os.system("make > /dev/null")
        if  os.path.isfile("./f77_madevent") and exit_status==0:
            os.chdir("..")
        else:
            print "fortran compilation error"
            sys.exit()
    os.chdir("..")   
    return  
##############################################################################
def  make_symb_link(P_proclist):
    os.chdir("./SubProcesses")
    for name in P_proclist:
        os.chdir("./"+name)
        #os.system('ln -s ../../Source/give_run_parameter.f')
        #os.system('ln -s ../../Source/update_sm_param.f')
        #os.system('ln -s ../madevent2')
        os.system('ln -s ../madevent_param')
        os.system('ln -s ../../Source/pdf.inc')
        os.chdir('..')
    os.chdir("..")   
    return


#####################################################################################"
##
##               LAUNCH PROGRAM
##
######################################################################################
if(__name__=='__main__'):

    go_to_main_dir()
    check_for_help(sys.argv)
    MWparam=MW_info('MadWeight_card.dat')
    MWparam.set_run_opt(sys.argv)

    if MWparam.run_opt['param']:
        create_param.Param_card(run_name=MWparam)
        MWparam.update_nb_card()
        Cards.create_include_file(MWparam)
        update_cuts_status(MWparam)

    if MWparam.run_opt['analyzer']:
        write_MadWeight.create_all_fortran_code(MWparam)
    if MWparam.run_opt['madweight_main']:    
        Launch_all_SubProcess(MWparam)
    if MWparam.run_opt['plot']:
        import plot
        plot.Likelihood(mw_param=MWparam,auto=1)
        # This routine can/must be added when you use the creation of the histogram
        # By the way she is not fully tested and stable yet
        #plot.Differential_Graph(MWparam,auto=1)        

    if MWparam.run_opt['clean']:
        from clean import Clean_run
        if MWparam.run_opt['clean']==1:
            Clean_run(MWparam.name)
        else:
            Clean_run(MWparam.run_opt['clean'])
