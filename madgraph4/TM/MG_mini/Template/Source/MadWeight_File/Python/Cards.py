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
##   last-modif:04/08/08                                                ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   Content                                                            ##
##   -------                                                            ##
##                                                                      ##
##      + create_include_file                                           ##
##      + Card                                                          ##
##      |    + init                                                     ##
##      |    |    +   detect_type                                       ##
##      |    + read_card                                                ##
##      |    + read_ident                                               ##
##      |    + read_particles                                           ##
##      |    |    +   give_pid_dict                                     ##
##      |    + create_include_file                                      ##
##      |    |    +   pass_in_type                                      ##
##                                                                      ##
##########################################################################
import re
import os

#1 #########################################################################
def create_include_file(MWparam):
    """ create all the include file and make all the link """

    #create include file corresponding to MW_card and transfer_card
    #charge card
    ident=Card('./Cards/ident_mw_card.dat')
    if MWparam:    madweight=MWparam.mw_card
    else: madweight=Card('./Cards/MadWeight_card.dat')
    transfer=Card('./Cards/transfer_card.dat')

    #create output
    madweight.create_include_file(ident,'./Source/madweight_card.inc')
    transfer.create_include_file(ident,'./Source/MadWeight_File/Transfer_Fct/transfer_card.inc')

    #make all the symbolic link
    for dir in MWparam.MW_listdir:
        os.system('ln -sf ../../Source/madweight_card.inc ./SubProcesses/'+dir+'/madweight_card.inc')
        os.system('ln -sf ../../Source/MadWeight_File/Transfer_Fct/transfer_card.inc ./SubProcesses/'+dir+'/transfer_card.inc')



#1 #########################################################################
class Card:
    """ The routine to read and treat all the card (but the run_card.dat)"""
    
    #2 #########################################################################
    def __init__(self,file,type=''):
        self.file=file

        if not type:
            self.type=self.detect_type()
        else:
            self.type=type.lower()

        if self.type in ['transfer','param','madweight']:
            self.info=self.read_card(self.file)
        elif(self.type in ['ident']):
            self.info=self.read_ident()

    #3 #########################################################################           
    def detect_type(self):
        """detect the type of the card """
        
        return self.file.split('/')[-1].split('_')[0].lower()
        
    #2 #########################################################################
    def read_card(self,name_card):
        """put all card information in a dictionary"""

        p_block=re.compile(r'''^block\s+(?P<block>\w*)\s*(?P<comment>.*)''',re.I)
        p_comment=re.compile('#')
        p_space=re.compile('\s*')

        try:
            card=open(name_card,'r')
        except:
            try:
                card=open("./Cards/"+name_card,'r')
            except:
                card=open("./Events/"+name_card,'r')  #->read banner

        #init output
        info={'comment':{}}
        name_block=''

        #read the card
        while 1:
            line=card.readline()
            if line=='':
                break
            prov=[name_block]

            if p_block.search(line):
                name_block=p_block.search(line).group('block')
                name_block=name_block.lower()
                info[name_block]={}
                info['comment'][name_block]=p_block.search(line).group('comment')
                continue

            line_content=p_space.split(p_comment.split(line)[0])

            while 1:
                try:
                    line_content.remove('')
                except:
                    break

            if not line_content:
                continue

            #treat decay anomaly
            if line_content[0].lower()=='decay':
                name_block='decay'
                line_content=line_content[1:]
                if 'decay' not in info.keys():
                    info['decay']={}
                decay_tag=line_content[0]
            elif name_block in ['decay','br']:
                name_block='br'
                line_content=[decay_tag]+line_content[2:]+[line_content[0]]
                if 'br' not in info.keys():
                    info['br']={}
                    
            #create list of dictionary
            obj=line_content[-1]
            for i in range(-2,-len(line_content)-1,-1):
                obj={line_content[i]:obj}
            #put in final data
            dico=info[name_block]
            if len(line_content)==1:
                dico[' ']=obj
            for i in range(0,len(line_content)-1):
                if line_content[i] not in dico.keys():
                    dico[line_content[i]]=obj[line_content[i]]
                    break
                elif i!=len(line_content)-2:
                    dico=dico[line_content[i]]
                    obj=obj[line_content[i]]
                elif(type(dico[line_content[i]])==list):                         #multiple definition of the same input
                    dico[line_content[i]].append(obj[line_content[i]])
                else:
                    dico[line_content[i]]=[dico[line_content[i]],line_content[i+1]]

        return info

    #2 #########################################################################
    def read_ident(self):
        """ read ident file only four and five column format are supported yet """

        ff=open(self.file,'r')
        line_format=re.compile(r'''^\s*(?P<block>\w+)\s+(?P<tag>\w+)\s+(?P<name>\w+)\s+(?P<type>\w+)\s*(?P<default>[\w.+-]*)\s*$''')
        ident={}
        while 1:
            line=ff.readline()
            if line=='':
                break
            words=line_format.search(line)
            if not words:
                continue
            block=words.group('block').lower()
            tag=words.group('tag').lower()
            name=words.group('name').lower()
            type=words.group('type').lower()
            value=words.group('default').lower()

            if not ident.has_key(block):
                ident[block]={tag:[name,type,value]}
            else:
                ident[block][tag]=[name,type,value]
                
        return ident
    

    #2 #########################################################################
    def read_particles(self):
        """read a particles.dat file (don't read multiple info now)"""

        particle_pattern=re.compile(r'''^\s*
                                        (?P<part>[\w+-~]{1,4})\s+
                                        (?P<partx>[\w+-~]{1,5})\s+
                                        (?P<spin>[SFV])\s+
                                        (?P<LineType>[WSDC])\s+
                                        (?P<mass>\w+)\s+
                                        (?P<width>\w+)\s+
                                        (?P<color>[STO])\s+
                                        (?P<label>[\w+-~]{1,5})\s+
                                        (?P<pid>[\d-]*)\s*$''',re.VERBOSE)
        ff=open(self.file,'r')

        particle=[]
        while 1:
            line=ff.readline()
            if line=='':
                break
            pat_particle=particle_pattern.search(line)
            if pat_particle:
                particle.append(list(pat_particle.groups()))
            
        self.info={'particles':particle}
    
    #3 #########################################################################
    def give_pid_dict(self):
        """ return a list of pid for each tag -d'ont treat multiple tag-"""

        self.read_particles() #not automaticly read for the moment
        pid={}
        for data in self.info['particles']:
            
            if data[0]==data[1]:
                pid.update({data[0]:[int(data[-1])]})
            else:
                pid.update({data[0]:[int(data[-1])],data[1]:[-1*int(data[-1])]})

        return pid
    


    #2 #########################################################################
    def create_include_file(self,card,output):
        """ create an output of type name=value from part common in both card
            be careful of the restriction in the ident file -> 4 column (only one tag) or 5 (one tag+default value)

            Default value are used only if the block is defined in the card but not the entry
        """
        
        
        print 'create file', output
        out=file(output,'w')
        out.writelines('C automatic include file for card '+self.file+' and '+card.file+'\n\n')

        if card.type=='ident':
            ident=card.info
            info=self.info
        elif self.type=='ident':
            info=card.info
            ident=self.info
                                    
        for block in info.keys(): 
            if ident.has_key(block):
                for tag in ident[block].keys():
                    if info[block].has_key(tag):
                        value=self.pass_in_type(info[block][tag],ident[block][tag][1])
                        out.writelines('        '+ident[block][tag][0]+' = '+str(value)+'\n')
                    elif ident[block][tag][2]: #check if default value is defined
                        value=self.pass_in_type(ident[block][tag][2],ident[block][tag][1])
                        out.writelines('        '+ident[block][tag][0]+' = '+str(value)+'\n')

    #3 #########################################################################
    def pass_in_type(self,value,type):
        """ value convertisor """
        if type=='logical':
            if value in ['F','f','0']:
                return '.false.'
            elif value in ['T','t','1']:
                return '.true.'
            else:
                return value
        elif type in ['integer','string']:
            return value
        elif type in ['double','real']:
            value=value.replace('d','e')
            value=value.replace('D','e')
            value=float(value)
            value='%(test)e' % {'test':value}
            value=value.replace('e','d')
            return value
        else:
            print 'error in type for',value,type

# update for the card -transfer_card and madweight_card-
if(__name__=="__main__"):

    import MW_param
    pos=os.getcwd()
    if pos.count('SubProcesses/MW_P') or pos.count('SubProcesses/P'):
        os.chdir('../..')
    MW_param.go_to_main_dir()
    MWparam=MW_param.MW_info('MadWeight_card.dat')
    create_include_file(MWparam)
    from create_run import update_cuts_status
    update_cuts_status(MWparam)


    if pos.count('SubProcesses/MW_P'):
                os.chdir(pos)
                os.system('make')
