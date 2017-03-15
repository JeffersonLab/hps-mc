#!/usr/bin/env python
# -*- coding: cp1252 -*-

#Extension
import os
import mod_file
import diagram_class
#from time import time
from MW_fct import *


def create_all_fortran_code(MW_info,i=1):
    """goes  in each subprocess and creates the fortran code in each of them"""
    import madweight
#    start=time()    
    # load template for file    
    template=mod_file.Mod_file(rule_file='./Source/MadWeight_File/mod_file/mod_main_code')
    # load MadWeight option
#    MW_info=madweight.read_card('MadWeight_card.dat')
    for MW_dir in MW_info.MW_listdir:
        print 'treating',MW_dir,'directory'
        diag=MG_diagram('./SubProcesses/'+MW_dir,'param_card_1.dat','./Source/MadWeight_File/Transfer_Fct/ordering_file.inc',i,MW_info.info)
        diag.create_all_fortran_code()
        diag.write_code(template)
#    stop=time()
#    print ' topology in ',stop-start,'s'


#################################################################################################################
##
##                                   MG_diagram : write code part
##
#################################################################################################################

class MG_diagram(diagram_class.MG_diagram):
    """ add the write routine for fortran code in this file """

    def __init__(self,dir_file,param_card,tf_file,config,opt='default'):
        """ update object to add write content """
        
        diagram_class.MG_diagram.__init__(self,dir_file,config,opt)
        self.organize_particle_content(param_card,tf_file)
        self.code=[]


    def create_all_fortran_code(self):
        """go  in each subprocesses and create the fortran code in each of them"""

#        start=time()
        Mlist_unaligned=self.detect_non_resonant_area()
        all_pos=Mlist_unaligned.give_combinaison()
        num_sol=0
        if not all_pos: #no BW ambiguity
            self.clear_solution()
            self.define_Constraint_sector()
            self.solve_blob_sector()
            print self
            num_sol=self.load_fortran_code(num_sol)
            
        for unaligned in all_pos:
            self.clear_solution()
            self.tag_unaligned(unaligned)

            self.define_Constraint_sector()
            self.solve_blob_sector()
            print self
            num_sol=self.load_fortran_code(num_sol)
#        stop=time()
#        print 'done in ',stop-start,'s'
        

    def load_fortran_code(self,num_sol=0):
        """ create the code """

##        self.create_pmass2()
        self.create_output_type_info()
        
        for ECS in self.ECS_sol: # ALL ECS SECTOR
            print 'treating Enlarged Contrained Sector ',self.ECS_sol.index(ECS)+1
            full_solution_tag=[ECS,[]]
            full_blob_sol=Multi_list()
            for BLOB in ECS.blob_content:
                full_blob_sol.append(BLOB.solution)
            
            full_blob_sol=full_blob_sol.give_combinaison()#expanded solution
            for i in range(0,len(full_blob_sol)):
                num_sol+=1
                full_solution_tag[1]=full_blob_sol[i]
                code=[self.create_MadWeight_main(full_solution_tag,num_sol)]
                code.append(self.create_MadWeight_data(full_solution_tag,num_sol))
                if self.is_new_sol(code):
                    self.code.append(code)
                else:
#                    print 'no single solution'
                    num_sol-=1
                
#        print 'we have at this stage',num_sol,'solutions'
        return num_sol

    def create_MadWeight_main(self,full_sol_obj,num_sol):
        """ create the main_code_$i.inc for all solution """
        
        ECS=full_sol_obj[0]
        blob_sol_list=full_sol_obj[1]
        self.num_fuse=self.ext_part+3 #+2 for initial particle +1 to be on the next one
        self.fuse_dict={}
        #template=self.template
        write_text=''
        #
        #    INTRODUCTION
        #
        #write_text=template.dico['INTRO_FOR_MAIN']           #tag re-interpreted later to insert intro in file
        if num_sol==1:
            write_text+='''       if (config.eq.1) then '''
        else:
           write_text+='       elseif (config.eq.'+str(num_sol)+') then ' 
        write_text+='\n$B$ S-COMMENT_C $B$\n'
        write_text+=full_sol_obj[0].info()                    # -> write short ECS/BLOB information
        write_text+='\n$E$ S-COMMENT_C $E$\n'        
        #
        #   BLOB
        #
        step=0
        for blob_sol in  blob_sol_list:
            #supress entry for external particle blob
            if len(blob_sol.step)==1:
                if blob_sol.step[0].chgt_var=='0':
                    continue
            for block in blob_sol.step:
                if blob_sol.step.index(block):
                    write_text+='C       ++++++++++++           \n'
                step+=1
                if block.chgt_var in ['1','2','3']:
                    block_name=' call fuse('
                elif block.chgt_var =='0':                  
                    continue #this is already done by MadWeight
                else:
                    block_name=' call block_'+block.chgt_var.lower()+'(x,n_var,'
                line=block_name
                for particle in block.order_content:
                    if type(particle.MG)==int:
                        line+=str(particle.MG)+','
                    elif type(particle.MG)==str:
                        if self.fuse_dict.has_key(particle.MG):
                            line+=str(self.fuse_dict[particle.MG])+','
                            del self.fuse_dict[particle.MG]
                        else:
                            line+=str(self.num_fuse)+','
                            self.fuse_dict[particle.MG]=self.num_fuse
                            self.num_fuse+=1
                line=line[:-1]+')\n' #supress last , and add )
                line=put_in_fortran_format(line)
                write_text+=line
                if(self.opt.use_stat):
                    text=' call block_stat('+str(step)+",\'"+str(block.chgt_var)+'-'+str(block.order_content[0].MG)+"""')\n"""
                    text+=' if (jac.le.0d0) return\n'
                elif(block.chgt_var not in ['1','2','3']):
                    text=' if (jac.le.0d0) return\n'
                else:
                    continue
                write_text+= put_in_fortran_format(text)     
       
        #
        #   ECS 
        #
#        write_text+='\n$B$ S-COMMENT_C $B$\n'
#        write_text+=' ENLARGED CONTRAINT SECTOR \t CLASS '+str(ECS.chgt_var.upper())
#        write_text+='\n$E$ S-COMMENT_C $E$\n'         
        for block in ECS.step:
            step+=1           
            if block.chgt_var=='2':
                line=' call fuse('
            elif block.chgt_var in ['a','c','e','f','g']:
                line=' call class_'+ECS.chgt_var.lower()+'(x,n_var,'
            else:
                line=' call class_'+ECS.chgt_var.lower()+'('
            for particle in block.order_content:
                if type(particle.MG)==int:
                    line+=str(particle.MG)+','
                elif type(particle.MG)==str:
                    if self.fuse_dict.has_key(particle.MG):
                        line+=str(self.fuse_dict[particle.MG])+','
                        del self.fuse_dict[particle.MG]
                    else:
                        line+=str(self.num_fuse)+','
                        self.fuse_dict[particle.MG]=self.num_fuse
                        self.num_fuse+=1
                            
            line=line[:-1]+')\n' #supress last , and add )
            line=put_in_fortran_format(line)
            write_text+=line
            if(self.opt.use_stat):
                text=' call block_stat('+str(step)+",\'"+str(block.chgt_var)+'-'+str(block.order_content[0].MG)+"""')\n"""
                text+=' if (jac.le.0d0) return\n'
            elif block.chgt_var not in ['2']:
                text=' if (jac.le.0d0) return\n'
            else:
                text='\n'
            write_text+= put_in_fortran_format(text)

        self.nb_block=step
        #
        #   INVISIBLE DECAY
        #
        out=self.check_invisible_decay()
        if out:
            write_text+='\n'+out

            
                        
        #ff=open(self.directory+'/MW_main_code_'+str(num_sol)+'.inc','w')
        #ff.writelines(write_text)
        #ff.close()
        return write_text
        
    def create_MadWeight_data(self,full_sol_obj,num_sol):
        """ create the data_$i.inc for all solution """
        #
        # intro write in write_code part, this is a script for one possibility
        #  of generation
        #

        ECS=full_sol_obj[0]
        blob_sol_list=full_sol_obj[1]
#        template=self.template
        blob_sol=[]
        for b_sol in blob_sol_list:
            blob_sol+=b_sol.step
        write_text=''
        #
        #    INTRODUCTION
        #
        write_text='\n$B$ S-COMMENT_C $B$\n'
        write_text+=full_sol_obj[0].info()
        write_text+='\n$E$ S-COMMENT_C $E$\n'        
        
        num_vis=0
        vis_str=''
        vis_list=[]
        for block in ECS.step+blob_sol:
            if block.chgt_var not in ['D','E','a','c']:
                for particle in block.in_part:
                    if particle.external and not particle.neutrino:
                        if particle.MG not in vis_list:
                            num_vis+=1
                            vis_str+=str(particle.MG)+','
                            vis_list.append(particle.MG)
            elif block.chgt_var in ['E','c']:
                particle=block.in_part[2] #take the forward particle
                if particle.external and not particle.neutrino:
                    if particle.MG not in vis_list:
                        num_vis+=1
                        vis_str+=str(particle.MG)+','
                        vis_list.append(particle.MG)
        text=' data num_vis('+str(num_sol)+') /'+str(num_vis)+'/\n'                
        text+=' data (vis_nb(label,'+str(num_sol)+'),label=1,'+str(num_vis)+') /'+vis_str[:-1]+'/\n'
        text+=' data nb_block('+str(num_sol)+') / '+str(self.nb_block)+'/\n\n\n'
        write_text+=put_in_fortran_format(text)
        #
        #   PROPAGATOR CONTENT -> LINKED TO SOLUTION
        #
        # 1) collect all generated propagator (in croissant order)
        # 2) write the code
        list=self.collect_generated_propa(ECS,blob_sol_list)
        #text=' integer num_propa\n'
        text=' data num_propa('+str(num_sol)+') /'+str(len(list))+'/ \n'        
        if list:
            text+=' data (propa_cont(label,'+str(num_sol)+'),label=1,'+str(len(list))+') /'
            for particle in list:
                text+=str(particle.MG)+','
            text=text[:-1]+'/\n'
        else:
            text+='\n$B$ S-COMMENT_C $B$\n No propagator aligned\n$E$ S-COMMENT_C $E$\n'

        for i in range(0,len(list)):
            text+=self.return_propa_generation(list,i,num_sol)
        text=put_in_fortran_format(text)
        write_text+=text
        
        return write_text

##     def create_pmass2(self):
##         """ create the pmass2 for all solution """
##         write_text="" 
##         for particle in self.content.values():
##             text='       pmass('+str(particle.MG)+') = '+str(particle.mass)+'d0\n'
##             if not particle.external:
##                text+='       pwidth('+str(particle.MG)+') = '+str(particle.width)+'d0\n'
##             write_text+=put_in_fortran_format(text)

##         ff=open(self.directory+'/pmass2.inc','w')
##         ff.writelines(write_text)
##         ff.close()

    def is_new_sol(self,code):
        """ check if this code is new or already defined """
        #Step 1: supress identical solution
        for i in range(0,len(self.code)):
            if self.code[i][0]==code[0]:
                if self.code[i][1]==code[1]:
#                    print 'identical solution to sol:',i
                    return 0
        return 1

    def write_code(self,template):
        """ write the data_file and the main_code file """

        write_main=template.dico['INTRO_FOR_MAIN']
        write_main+=template.dico['START_ROUTINE']
        write_data=template.dico['INTRO_FOR_DATA']
        write_data+=self.write_f77_parameter()
        write_data+=template.dico['COMMON_DEF']        
        for i in range(0,len(self.code)):
            write_main+=self.code[i][0]
            write_data+=self.code[i][1]
        write_main+='        endif\n'
        write_main+='        return\n'
        write_main+='        end\n'
                
        mod_file.mod_text(write_main,template.dico,self.directory+'/main_code.f')
        mod_file.mod_text(write_data,template.dico,self.directory+'/data.inc')        
        


    def write_f77_parameter(self):
        """ define the f77 parameter for the data file """
        
#        text=' integer nb_inv_part\n'                    
#        text+=' parameter (nb_inv_part='+str(self.num_neut)+')\n'
        text=' integer nb_vis_part\n'
        text+=' parameter (nb_vis_part='+str(len(self.ext_content)-self.num_neut)+')\n'        
        text+=' integer nb_sol_config\n'                    
        text+=' parameter (nb_sol_config='+str(len(self.code))+')\n'
#        text+=' integer max_branch\n'                    
#        text+=' parameter (max_branch='+str(len(self.ext_content))+')\n'        
        text=put_in_fortran_format(text)
        return text
        

    def collect_generated_propa(self,ECS,blob_sol_list):
        """ return all the propagator that must be generated following BW distibution """


        list=[]
        #print 'ECS',
        for particle in ECS.step[-1].order_content:
            #print particle.MG,
            if not particle.external and type(particle.MG)==int and particle not in list:
                list.append(particle)
        #print
        for blob_sol in blob_sol_list:
            for block in blob_sol.step:
                if block.chgt_var in ['A','B','C','D','E']:
                    for particle in block.order_content:
                        if not particle.external and type(particle.MG)==int and particle not in list:
                            list.append(particle)
#        list.reverse()

        list2=[]
        list3=[]
        while list:
            propa=list.pop()
            if propa.channel=='S':
                list2.append(propa)
            else:
                list3.append(propa)
                
##                 gen=1
##                 for i in range(0,len(propa.des)):
##                     if propa.des[i] in list:
##                         list.insert(propa,i+1)
##                         gen=0
##                         break
##                 if gen:
##                    list2.append(propa) 
                    
        return list2+list3

    def return_propa_generation(self,list,pos,num_sol):
        """return the line for the definition of how to generate the mass
           typical output are:
           data (propa_???($,label),label=1,$) /$,$,$,$,$,0/ 
        """



        particle=list[pos]
        line1=' data (propa_max('+str(pos+1)+',label,'+str(num_sol)+'),label=1,'
        line2=' data (propa_min('+str(pos+1)+',label,'+str(num_sol)+'),label=1,'
        generated_mother=[]
        generated_twin=[]
        generated_son=[]
        already_gen=list[:pos]
        
        motherX=list[pos]
        #look for minimal value
        generated_son+=self.already_generated_in_decay(motherX,already_gen)
        generated_son.append(0)
        while 1:
            motherXbut1=motherX
            motherX=motherX.mother
            if motherX==0:
                break
            #look for maximal value
            generated_twin+=self.already_generated_in_decay(motherXbut1.twin,already_gen)                
            if motherX in already_gen:
                generated_mother=[motherX.MG]
                generated_twin.append(0)
                break                               
        if not generated_mother:
             generated_mother=[0]
             generated_twin=[]

        gen=generated_mother+generated_twin
        line1+=str(len(gen))+') / '
        line2+=str(len(generated_son))+') / '
        
        for MG_num in gen:
            line1+=str(MG_num)+','
        line1=line1[:-1]+'/\n'
        
        for MG_num in generated_son:
            line2+=str(MG_num)+','
        line2=line2[:-1]+'/\n'
        
        return line1+line2
        
            
        
    def already_generated_in_decay(self,particle,generated_propa):
        """give (recurently) all the first particle already generated in the branchs of desintegration"""

        if particle.external:
            return [particle.MG]
        elif particle in generated_propa:
            return [particle.MG]
        else:
            part1=self.already_generated_in_decay(particle.des[0],generated_propa)
            part2=self.already_generated_in_decay(particle.des[1],generated_propa)
            return part1+part2

    def check_invisible_decay(self):
        """ check if one of the invisible particle decay in 2 invisible particle
            return 0 if not
            return a text with the call of the equivalent subroutine
        """
        decay_num=0
        for particle in self.neut_content:
            if particle.external:
                continue
            decay_num+=1
            if not decay_num:
                out_text=self.template.comment_text('\t Invisible Propagator','C')
            text=' decay('+str(particle.MG)+','+str(particle.des[0].MG)+','+str(particle.des[1].MG)+')'

            out_text+=put_in_fortran_format(text)

        if decay_num:
            return out_text
        else:
            return 0

    def create_output_type_info(self):
        """ create output file containing the number of muon/electron/jet/bjet/invisible_part """


        content=self.output_type_info()  
                    
        ff=open(self.directory+'/info_part.dat','w')
        text=""
        for i in range(0,len(content)):
            text+='\t'+str(content[i])
        ff.writelines(text)
        ff.close()

        
if(__name__=="__main__"):
    """ launched the generation """
    import MW_param

    MW_param.go_to_main_dir()
    MW_opt=MW_param.MW_info('MadWeight_card.dat')

    create_all_fortran_code(MW_opt)
    



