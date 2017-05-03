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
##   last-modif: 07/01/09                                               ##
##                                                                      ##
##########################################################################
##                                                                      ##
##   Content                                                            ##
##   -------                                                            ##
##                                                                      ##
##   +Likelihood                                                        ##
##   |   +  init                                                        ##
##   |   +  make_all                                                    ##
##   |   +  charge_weight                                               ##
##   |   +    +   charge_weight_file                                    ##
##   |   +  correct_with_acc                                            ##
##   |   +  define_plots                                                ##
##   |   +  define_section_plots                                        ##
##   |   +    +   search_best_card                                      ##
##   |   +    +   CardNb_to_ParameterTag                                ##
##   |   +  plot                                                        ##
##   |   +  all_values                                                  ##
##########################################################################
##   +Discriminant                                                      ##
##   |   +  init                                                        ##
##   |   +  charge_weight_file                                          ##
##   |   +  compute_discriminant                                        ##
##   |   +  plot                                                        ##
##########################################################################
##   +Differential_Graph                                                ##
##   |   +  init                                                        ##
##   |   +  make_all                                                    ##
##   |   +  charge_plot_info                                            ##
##   |   +    +   read_bin                                              ##
##   |   +    +   read_begin                                            ##
##   |   +  add_information                                             ##
##   |   +    +   find_normalisation                                    ##
##   |   +    +   read_data                                             ##
##########################################################################
##   +Plot                                                              ##
##   |   +  init                                                        ##
##   |   +    +   def_title                                             ##
##   |   +    +   def_binname                                           ##
##   |   +    +   def_xlabel                                            ##
##   |   +  add_value                                                   ##
##   |   +  add_fitting_curve                                           ##    
##   |   +  draw                                                        ##
##   |   +    +   create_gnuplot                                        ##
##   |   +    +   launch_gnuplot                                        ##
##   |   +    +   create_td                                             ##
##   |   +    +   launch_td                                             ##
##########################################################################
##   +Fit                                                               ##
##   |   +  init                                                        ##
##   |   +  create_gnuplot_text                                         ##
##   |   +    +   text_gnuplot_parabol_for_likelihood                   ##
##                                                                      ##
##########################################################################
import MW_param
import re
import math
import os
from MW_fct import BackRead

#1########################################################################
class Likelihood:
    """ class for plotting likelihood """
    
##########################################################################
##   +Likelihood                                                        ##
##   |   +  init                                                        ##
##   |   +  make_all                                                    ##
##   |   +  charge_weight                                               ##
##   |   +    +   charge_weight_file                                    ##
##   |   +  correct_with_acc                                            ##
##   |   +  define_plots                                                ##
##   |   +  define_section_plots                                        ##
##   |   +    +   search_best_card                                      ##
##   |   +    +   CardNb_to_ParameterTag                                ##
##   |   +  plot                                                        ##
##   |   +  all_values                                                  ##    
##########################################################################
    
    #2########################################################################
    def __init__(self,mw_param='',run_name='',auto=0):
        """ initialisation of the object """
        
        #init
        self.y={}
        self.dy={}
        self.event={} #to verify that all bin have the same number of data

        #charge parameter
        if run_name=='' and mw_param=='':
            self.MWparam=MW_param.MW_info('MadWeight_card.dat')
        elif mw_param=='':
            self.MWparam=MW_param.MW_info(run_name+'_banner.txt')
        else:
            self.MWparam=mw_param
            
        self.run_type=self.MWparam.info['mw_parameter']['1']   #how are use/generated param_card
        self.name=self.MWparam.name
            
        #init step 2
        for k in self.MWparam.actif_param: 
            self.y[k]=0
            self.dy[k]=0
            self.event[k]=0

        #launch everything
        if auto:
            self.make_all()

    #2########################################################################
    def make_all(self):
        """ main schedullar """

        self.charge_weight()   #self.info['mw_run']['4'] = normalize with cross
        if self.MWparam.info['mw_run']['4']: self.correct_with_cross()
        self.correct_with_acc()
        try:

            self.define_plots(self.MWparam.info['mw_parameter']['1'])  #option -> gives how param card are generated 
            if self.run_type=='1':
                self.define_section_plots()
            self.plot()
        except: print 'not possible to make all'
        self.all_values()
        return

    #2########################################################################
    def charge_weight(self,filename='',norm=0):
        """charge in abscisse/ordonee the data (use normalize data if norm=1)"""

        #file to read
        opt=''
        if norm:
            opt='_norm'
        if not filename:
            filename='./Events/'+self.name+'/'+self.name+opt+'_weights.out'
        print 'charge file ',filename

        if type(filename)==list:
            for input_file in filename:
                self.charge_weight_file(input_file)
        else:
            self.charge_weight_file(filename)

        self.nb_event=self.event[self.MWparam.actif_param[0]]
        for i in self.MWparam.actif_param:
            if self.nb_event!=self.event[i]:
                print 'Warning: some data are missing/null in '+filename+' only '+str(self.event[i])+' events of '+str(self.nb_event)
                print 'Applying correction factor for the normalization'
                self.y[i]=self.y[i]*self.nb_event/self.event[i]
                self.dy[i]=self.dy[i]*self.nb_event/self.event[i]


        return 
    #3########################################################################
    def charge_weight_file(self,filename):
        """charge in abscisse/ordonee the data (use normalize data if norm=1)"""

        #pattern
        pattern=re.compile(r'''(?P<param>\d*).(?P<event>\d*)\s+(?P<weight>[\d.e+-]+|nan)\s+(?P<error>[\d.e+-]+|nan)''',re.I)

        #start read file    
        ff=open(filename,'r')
        while 1:
            line=ff.readline()
            if line=='':
                break #end of file
            if not pattern.search(line):
                continue
            pat=pattern.search(line)
            weight=float(pat.group('weight'))
            error=float(pat.group('error'))

            if weight and int(pat.group('param')) in self.MWparam.actif_param:
                self.y[int(pat.group('param'))]-=math.log(weight)
                self.dy[int(pat.group('param'))]+=error**2/weight**2
                self.event[int(pat.group('param'))]+=1
            elif weight:
                print 'warning: do not consider not active param_card'
            else:
                print 'Warning: ZERO RESULT: we will not consider this data for bin ',int(pat.group('param'))-1 
        ff.close()
        
        return 

    #2########################################################################
    def correct_with_cross(self,filename=''):
        """correct with acceptance term"""
        
        if filename=='':
            filename='./Events/'+self.name+'/'+self.name+'_cross_weights.out'

           
        cross={}
        pattern=re.compile(r'''(?P<param>[\d.e+-]+)\s+(?P<cross>[\d.e+-]+|nan)\s+(?P<error>[\d.e+-]+|nan)''',re.I)
        try:
            ff=open(filename,'r')
            print 'load cross section term',filename
        except:
            print 'WARNING: no cross section loaded'
            return
        
        while 1:
            line=ff.readline()
            if line=='':
                break
            if not pattern.search(line):
                continue
            pat=pattern.search(line)
            cross[int(pat.group('param'))]=[float(pat.group('cross')),float(pat.group('error'))]
        ff.close()

        if len(cross)==len(self.MWparam.actif_param):
            for i in self.MWparam.actif_param:
                self.y[i]+=self.nb_event*math.log(cross[i][0])
                self.dy[i]+=(self.nb_event*cross[i][1]/cross[i][0])**2
        else:
            print 'wrong number of input in cross_weights.out'
            print 'normalization factor term will be ignored'

        return



    #2########################################################################
    def correct_with_acc(self,filename=''):
        """correct with acceptance term"""
        
        if filename=='':
            os.system('cp Events/accfac.dat Events/'+self.name+'/accfac.dat &>/dev/null')
            filename='./Events/'+self.name+'/accfac.dat'

           
        acc={}
        pattern=re.compile(r'''(?P<param>[\d.e+-]+)\s+(?P<acc>[\d.e+-]+|nan)\s+(?P<error>[\d.e+-]+|nan)''',re.I)
        try:
            ff=open(filename,'r')
            print 'load acceptance term',filename
        except:
            print 'no acceptance term loaded'
            return
        
        while 1:
            line=ff.readline()
            if line=='':
                break
            if not pattern.search(line):
                continue
            pat=pattern.search(line)
            acc[int(pat.group('param'))]=[float(pat.group('acc')),float(pat.group('error'))]
        ff.close()

        for i in self.MWparam.actif_perm:
            if i not in acc.keys():
                print 'wrong accfac.dat'
                print 'acceptance term will be ignored'
                return

        for i in self.MWparam.actif_perm:
            print 'correct with ',self.nb_event,'factor'
            self.y[i]+=self.nb_event*acc[i][0]
            self.dy[i]+=self.nb_event*acc[i][1]**2
                            
        return


    #2########################################################################
    def define_plots(self,case):
        """define name/value for each bin and how to dispatch information in plot"""

        # 3 case depending of how param_card are treated:
        # case 0: name=tag and all info in the plot n=1 (the only one)
        # case 2: name=value of first blog and all info in the plot n=1 (the only one)
        # case 1: multiple plot case
        # in this case we will treat only full projection plot
        self.bin_name=[]
        self.param_to_bin={}

        #initialisation 
        if case in ['1','2']:
            self.nb_block,self.nb_data_by_block=self.MWparam.give_block_param_info()
            nb_data_by_block=self.nb_data_by_block
        else:
            self.nb_block=1
            nb_data_by_block=[len(self.MWparam.actif_param)]
            
        #creation off Plots
        self.plot_list=[]
        for i in range(0,self.nb_block):
           self.plot_list.append(Plot(nb_data_by_block[i],self.MWparam))
            
        #assignement card to Plot_bin
        for i in self.MWparam.actif_param:
            if case in ['0','2']:
                self.param_to_bin[i]=[i] #param card i in bin i
            else: #case 1
                to_plot=self.CardNb_to_ParameterTag(i)
                self.param_to_bin[i]=to_plot

        #assignement of the name of the bin
        if case=='0':
            plot=self.plot_list[0]
            plot.bin_name.append(str(i))
            plot.Xlabel="param_card number"
            plot.def_title("likelihood")
        elif(case=="2"):
            plot=self.plot_list[0]
            plot.def_binname('auto',self.MWparam.info['mw_parameter']['13'])
            plot.Xlabel=self.MWparam.info['mw_parameter']['11']+':'+str(self.MWparam.info['mw_parameter']['12'])
            plot.def_title("likelihood")
        elif(case=="1"):
            for k in range(0,self.nb_block):
                plot=self.plot_list[k]
                plot.def_binname('auto',self.MWparam.info['mw_parameter'][str(10*k+13)])
                plot.Xlabel=self.MWparam.info['mw_parameter'][str(10*k+11)]+':'+\
                             str(self.MWparam.info['mw_parameter'][str(10*k+12)])
                plot.def_title("likelihood: projection for "+plot.Xlabel)

        # assignment of fitting curve for those plot
        for k in range(0,self.nb_block):
            plot=self.plot_list[k]
            plot.add_fitting_curve('parabol_for_likelihood')
          
        return

    #2########################################################################
    def define_section_plots(self):
        """ define section plot """

        best=self.search_best_card()
        best_param=self.CardNb_to_ParameterTag(best)

        #creation of new Plots
        for i in range(0,self.nb_block):
            self.plot_list.append(Plot(self.nb_data_by_block[i],self.MWparam))

        #assignement card to Plot_bin
        for i in self.MWparam.actif_param:
            param=self.CardNb_to_ParameterTag(i)
            to_plot=[]
            for j in range(0,self.nb_block):
                value=param[j]
                for k in range(0,self.nb_block):
                    if k!=j and best_param[k]==param[k]:
                        value=-1
                        break
                to_plot.append(value)
            self.param_to_bin[i]+=to_plot

        #assignement of the name of the bin
        for k in range(0,self.nb_block):
            plot=self.plot_list[self.nb_block+k]
            plot.def_binname('auto',self.MWparam.info['mw_parameter'][str(10*k+13)]) 
            plot.Xlabel=self.MWparam.info['mw_parameter'][str(10*k+11)]+':'+\
                         str(self.MWparam.info['mw_parameter'][str(10*k+12)])
            plot.def_title("likelihood: best section for "+plot.Xlabel)

        #assignement of 
            plot.add_fitting_curve('parabol_for_likelihood')

    #3########################################################################
    def search_best_card(self):
        """ find the minimal value for the likelood """

        best=0
        min_value=self.y[self.y.keys()[0]]
        for key in self.y.keys():
            if self.y[key]<min_value:
                min_value,best=self.y[key],key

        return best

        
    #3########################################################################
    def CardNb_to_ParameterTag(self,num_card):
        """ find from the card number, to which value for each block this card belong """

        #this routine is a simple copy from the standard one from MW_param.py
	return self.MWparam.CardNb_to_ParameterTag(num_card)
        
    #2########################################################################
    def plot(self):
        """ plot the graph define in plot_list """

        #assignement of value for the plot
        for i in range(0,len(self.plot_list)):
            plot=self.plot_list[i]
            for j in self.MWparam.actif_param:
                if self.param_to_bin[j][i]>=0:
                    plot.add_value(self.param_to_bin[j][i],self.y[j],math.sqrt(self.dy[j]))
                            
            #create graph
            plot.draw()

    #2########################################################################
    def all_values(self):
        """ write a file with all the value of the likelihood """

        ff=open('./Events/'+self.MWparam.name+'/likelihood_value.out','w')

        try:
            out=def_legend_lines()
            ff.writelines(out)
        except:
            pass
        
        self.charge_mapping()
        
        for card in self.MWparam.actif_param:
            line=self.mapping_info[card]
            line+='\t'+str(self.y[card])+'\t'+str(self.dy[card])+'\n'
            ff.writelines(line)

    #3########################################################################
    def charge_mapping(self):
        """ create self_mapping_info= list containing the different line from the mapping file """

        self.mapping_info={}
        for line in open('./Cards/mapping_card.dat'):
            splitline=line.split()
            if len(splitline)>2 and splitline[0][0]=='#':
                continue
            self.mapping_info[int(splitline[0])]='\t'.join(splitline[:-1])
        
    #3########################################################################
    def def_legend_lines(self):
    
        #write the legend line
        line='#'
        nb_param=1
        len_list=[]
        while self.MWparam.info['mw_parameter'].has_key(str(nb_param*10+1)):

            line+=self.MWparam.info['mw_parameter'][str(nb_param*10+1)]+':'+str(self.MWparam.info['mw_parameter'][str(nb_param*10+2)])+'\t'
            # store the number of data by parameter
            if type(self.MWparam.info['mw_parameter'][str(nb_param*10+2)])==list:
                len_list.append(len(self.MWparam.info['mw_parameter'][str(nb_param*10+2)]))
            else:
                len_list.append(1)
                self.MWparam.info['mw_parameter'][str(nb_param*10+2)]=[self.MWparam.info['mw_parameter'][str(nb_param*10+2)]] #pass in list to avoid problem
            nb_param+=1
            
        line+='likelihood\terror\n'

        return line

        
            
#1########################################################################
class Discriminant:
    """ Discriminant between two weight file"""

##########################################################################
##   +Discriminant                                                      ##
##   |   +  init                                                        ##
##   |   +  charge_weight_file                                          ##
##   |   +  compute_discriminant                                        ##
##   |   +  plot                                                        ##
##########################################################################

    #2 ########################################################################
    def __init__(self,file1,file2,MWparam):

        self.MWparam=MWparam
            
        #import data
        input1=self.charge_weight_file(file1)
        input2=self.charge_weight_file(file2)

        #create the discriminant
        value=self.compute_discriminant(input1,input2)

        if MWparam:
            self.plot(value)


    #2 ########################################################################
    def charge_weight_file(self,filename):
        """charge the data (use normalize data if norm=1)"""

        #init
        y={}
        dy={}

        #pattern
        pattern=re.compile(r'''(?P<param>\d*).(?P<event>\d*)\s+(?P<weight>[\d.e+-]+|nan)\s+(?P<error>[\d.e+-]+|nan)''',re.I)

        #start read file    
        ff=open(filename,'r')
        while 1:
            line=ff.readline()
            if line=='':
                break #end of file
            if not pattern.search(line):
                continue
            pat=pattern.search(line)
            param=int(pat.group('param'))
            event=int(pat.group('event'))
            weight=float(pat.group('weight'))
            error=float(pat.group('error'))

            if not y.has_key(param):
                y[param]={}
                dy[param]={}

            y[int(pat.group('param'))][int(pat.group('event'))]=weight
            dy[int(pat.group('param'))][int(pat.group('event'))]=error

        ff.close()

        return {'weight':y,'error':dy}


    #2 ########################################################################
    def compute_discriminant(self,input1,input2):
        """charge the data (use normalize data if norm=1)"""

        output={}
        for card in input1['weight'].keys():
            try:
                data1=input1['weight'][card]
                data2=input2['weight'][card]
                output[card]=[]
            except:
                print """don't consider the card """, card
                continue
            for event in data1.keys():
                if data2.has_key(event):
                    den=data1[event]+data2[event]
                    if den:
                        output[card].append((data1[event]/den,0))
                    else: print 'pass'

        return output

    #2 ########################################################################
    def plot(self,data,NBbin=10):
        """ plot the graph define in plot_list """

        NBbin=int(NBbin)
        name_bin=[]
        for i in range(0,NBbin):
            name_bin.append(str(i/float(NBbin)))

        for card,discriminant in data.items():
            
            plot=Plot(NBbin,self.MWparam)
            plot.def_binname('auto',name_bin) 
            plot.Xlabel='discriminant'
            plot.def_title('card '+str(card))

            for point,error in discriminant:
                bin=int(point*NBbin)
                plot.add_value(bin,1,error)
                            
            #create graph
            plot.draw()
    
        

#1########################################################################
class Differential_Graph:
    """ Combine and create plot for diffential weight"""

##########################################################################
##   +Differential_Graph                                                ##
##   |   +  init                                                        ##
##   |   +  make_all                                                    ##
##   |   +  charge_plot_info                                            ##
##   |   +    +   read_bin                                              ##
##   |   +    +   read_begin                                            ##
##   |   +  add_information                                             ##
##   |   +    +   find_normalisation                                    ##
##   |   +    +   read_data                                             ##
##########################################################################

    #2########################################################################
    def __init__(self,MWparam,auto=0):
        """create all Differential graph create point by point by the fortran code"""

        self.num_graph=0
        self.list_plot=[]
        self.MWparam=MWparam

        if auto==1:
            self.make_all()
            
    #2########################################################################
    def make_all(self):
        """launch everything"""

        self.charge_plot_info()
        for num_card in range(1,self.MWparam.nb_card+1):
            for num_event in range(0,self.MWparam.nb_event):
                self.add_information(num_card,num_event)
        for plot in self.list_plot:
            plot.draw('topdrawer')
        
    #2########################################################################
    def charge_plot_info(self):
        """ Charge first info from the first event """
        
        pos='./SubProcesses/'+self.MWparam.MW_listdir[0]+'/'+\
             self.MWparam.name+'/'+self.MWparam.name+'_1_0/plot.top'
        self.file_list=[BackRead(pos)]

        while 1:
            # read all block
            cur_norm=self.find_normalisation()
            if cur_norm=='stop':
                self.file_list[0].close()
#                self.file_list=[]
                break
            local_list=[]
            for i in range(1,self.MWparam.nb_card+1):
                plot=Plot(50,self.MWparam)
                plot.card_tag=i
                self.num_graph+=1
                self.list_plot.append(plot)
                local_list.append(plot)
            
            i=0
            while 1:
                i-=1 #back reading
                bin=self.read_bin()
                if bin=='stop':
                    break
                for plot in local_list:
                    plot.def_binname(i,bin)                
            self.read_begin(local_list)
       
    #3########################################################################
    def read_bin(self):
        """ read bin """

        pat_bin=re.compile(r'''^\s*(?P<bin>[\d.e+-]+|nan)\s+([\d.e+-]+|nan)\s+([\d.e+-]+|nan)''',re.I)
        pat_end=re.compile(r'''SET ORDER''',re.I)
        file=self.file_list[0]
        
        #pass text
        while 1:
            line=file.readline()
            if pat_bin.search(line):
                return pat_bin.search(line).group('bin')
            elif pat_end.search(line):
                self.last_line=line #to pass in read begin
                return 'stop'
                
    #3########################################################################
    def read_begin(self,list_plot):
        """ Read the begin of the file, and use information
            !back read of file"""

        begin_text=self.last_line #from data
        pat_end=re.compile(r'''NEW PLOT''',re.I)
        pat_title=re.compile(r'''TITLE TOP "(?P<title>.+)"''',18)
        file=self.file_list[0]
        
        while 1:
            line=file.readline()
            if line=="":
                break
            if pat_end.search(line):
                break
            if pat_title.search(line):
                for plot in list_plot:
                    plot.def_title(pat_title.search(line).group('title'))
            begin_text=line+begin_text

        for plot in list_plot:
            plot.td_begin=begin_text

        return
    
    #2########################################################################    
    def add_information(self,num_param,num_events):
        """ Update final graph information with local data """

        #open all file for the event/card
        num_file=len(self.MWparam.MW_listdir)
        self.file_list=[]
        for i in range(0,num_file):
            pos='./SubProcesses/'+self.MWparam.MW_listdir[i]+'/'+\
                 self.MWparam.name+'/'+self.MWparam.name+'_'+str(num_param)+'_'+str(num_events)+'/plot.top'
            cur_file=BackRead(pos)
            self.file_list.append(cur_file)

        i=0
        # read all block        
        while 1:
            i+=1
            self.norm=self.find_normalisation()
            if self.norm=='stop':
                break
            plot=self.list_plot[i*num_param-1]
            j=0
            #read all data
            while 1:
                j-=1 #back reading
                value,err=self.read_data() #take care of normalisation and off all subprocess
                if value=='stop':
                    break
                plot.add_value(j,value,err)

        #close all file
        for file in self.file_list:
            file.close()
                 
    #3########################################################################
    def find_normalisation(self):
        """ find normalisation """

        pattern=re.compile(r'''int\s*=\s*(?P<int>[\d.e+-]+|nan)''',re.I)
        norm=[]

        for file in self.file_list:
            while 1:
                line=file.readline()
                if line=='':
                    return 'stop'
                
                if pattern.search(line):
                    norm.append(float(pattern.search(line).group('int')))
                    break
        return norm


            
    #3########################################################################
    def read_data(self):
        """ Search next data ! back read of file"""
        
        pat_data=re.compile(r'''^\s*([\d.e+-]+|nan)\s+(?P<value>[\d.e+-]+|nan)\s+(?P<err>[\d.e+-]+|nan)''',re.I)
        pat_end=re.compile(r'''NEW PLOT''',re.I)
        value=0
        err=0
        norm=0
        
        for i in range(0,len(self.file_list)):
            file=self.file_list[i]
            norm+=self.norm[i]
            pos=0
            while 1: #to remove
                pos-=1
                line=file.readline()
                if line=='':
                    return 'stop',0
                if pat_data.search(line):
                    value+=float(pat_data.search(line).group('value'))
                    err+=float(pat_data.search(line).group('err'))
                    break
            
        return value/norm,err/norm

    
#1########################################################################
class Plot:
    """Plot object """
    
##########################################################################
##   +Plot                                                              ##
##   |   +  init                                                        ##
##   |   +    +   def_title                                             ##
##   |   +    +   def_binname                                           ##
##   |   +    +   def_xlabel                                            ##
##   |   +  add_value                                                   ##
##   |   +  add_fitting_curve                                           ##    
##   |   +  draw                                                        ##
##   |   +    +   create_gnuplot                                        ##
##   |   +    +   launch_gnuplot                                        ##
##   |   +    +   create_td                                             ##
##   |   +    +   launch_td                                             ##
##########################################################################
    
    #2########################################################################
    def __init__(self,nb_bin,MWparam):
        """ initialisation of the plot """
        self.nb_bin=nb_bin
        self.bin_name=[]
        self.value=[]
        self.err=[]
        self.X_name=''
        self.title=''
        self.tag='plot'
        if type(MWparam)==str:
            self.run_name=MWparam
        else:
            self.run_name=MWparam.name
            
        self.fit=0
        
        for i in range(0,nb_bin):
            self.value.append(0)
            self.err.append(0)
            self.bin_name.append('')

     
    #3########################################################################
    def def_title(self,title):
        """ define title and tag"""

        self.title=title
        self.tag=self.title.replace(' ','_')
        self.tag=self.tag.replace(':','')
        self.tag=self.tag.replace('$','p')
        self.tag=self.tag.replace('(','')
        self.tag=self.tag.replace(')','')
        self.tag=self.tag.replace('/','_div_')
        self.tag=self.tag.replace('\'','')
        self.tag=self.tag.replace('\"','')

        try:
            self.tag+='_'+self.card_tag
        except:
            pass
        


    #3########################################################################
    def def_binname(self,bin='auto',label='0'):
        """ define xlabel for bin i"""

        if type(bin)==int:
            self.bin_name[bin]=label
        elif type(bin)==list:
            for i in range(0,len(label)):
                self.bin_name[bin[i]]=label[i]
        elif type(label)==list:
            for i in range(0,len(label)):
                self.bin_name[i]=label[i]
        else:
            self.bin_name[0]=label


        

    #2########################################################################
    def add_value(self,bin,value,err):
        """ add value in bin """

        self.value[bin]+=value
        self.err[bin]+=err
        
    #2########################################################################
    def add_fitting_curve(self,function):

        self.fit=function

    #2########################################################################
    def draw(self,type='gnuplot'):
        """ create gnuplot file and launch them """
        
        if type=='gnuplot':
            self.create_gnuplot()
            self.launch_gnuplot()
        elif type=='topdrawer':
            self.create_td()
            self.launch_td()
        return

    #3########################################################################
    def create_gnuplot(self):
        """ create gnuplot file """
        text="""
        set term post eps color enhanced 12
        set out \""""+self.tag+""".eps\"
        set xlabel "{/=28 """+self.Xlabel+"""}"
        set title "{/=28 """+self.title+"""}" """
        
        if self.fit:
            fit=Fit('parabol_for_likelihood',self)
            text+=fit.give_gnuplot_text()
            fit_function=', '+fit.function+' title \"fit\" '
        else:
            fit_function=''
            
        text+="""plot '"""+self.tag+""".dat' u 1:2:3 with errorbar title \"\" """+fit_function
        try:
            ff=open('./Events/'+self.run_name+'/Graph/'+self.tag+'.plt','w')
        except:
            os.mkdir('./Events/'+self.run_name+'/Graph/')
            ff=open('./Events/'+self.run_name+'/Graph/'+self.tag+'.plt','w')
        ff.writelines(text+'\n')
        ff.close()
        ff=open('./Events/'+self.run_name+'/Graph/'+self.tag+'.dat','w')
        for i in range(0,self.nb_bin):
            ff.writelines(self.bin_name[i]+' '+str(self.value[i])+' '+str(self.err[i])+'\n')
        ff.close()

    #3########################################################################
    def launch_gnuplot(self):
        """ create image """
        os.chdir('./Events/'+self.run_name+'/Graph/')
        os.system('gnuplot '+self.tag+'.plt')
        os.chdir('../../../')


    #3########################################################################
    def create_td(self):
        """ create topdrawer file """
        try:
            ff=open('./Events/'+self.run_name+'/Graph/'+self.tag+'.top','w')
        except:
            os.mkdir('./Events/'+self.run_name+'/Graph/')
            ff=open('./Events/'+self.run_name+'/Graph/'+self.tag+'.top','w')            

        ff.writelines(self.td_begin)
        for i in range(0,self.nb_bin):
            ff.writelines(self.bin_name[i]+' '+str(self.value[i])+' '+str(self.err[i])+'\n')
        ff.close()
        
        
    #3########################################################################
    def launch_td(self):
        """ create image """

        os.chdir('./Events/'+self.run_name+'/Graph')
        os.system('td '+self.tag+'.top')
        os.chdir('../../../')


#1  ########################################################################
class Fit:
    """ class for fitting routine """
    
##########################################################################    
##   +Fit                                                               ##
##   |   +  init                                                        ##
##   |   +  create_gnuplot_text                                         ##
##   |   +    +   text_gnuplot_parabol_for_likelihood                   ##
##########################################################################
    
    #2 ########################################################################
    def __init__(self,type,graph):
        
        self.type=type
        self.graph=graph
        
    #2 ########################################################################
    def give_gnuplot_text(self):

        if hasattr(self, 'text_gnuplot_'+self.type ):
             f=getattr(self, 'text_gnuplot_'+self.type)
        else:
            print 'no such fitting curve'
            return ''
        
        text=f()
        
        return text
    
    #3 ########################################################################        
    def text_gnuplot_parabol_for_likelihood(self):
        
        graph=self.graph
        self.function='1/(2*b**2)*(x-a)**2+c'
        
        mid=graph.nb_bin/2 # integer division
        # initial value for the fitting
        a=float(graph.bin_name[mid]) #minimum of the likelihood: central value
        b=2*(float(graph.bin_name[mid-1])-float(graph.bin_name[mid])) #sigma
        c=float(graph.value[mid])           #value of the center

        text="""
        a="""+str(a)+"""
        b="""+str(b)+"""
        c="""+str(c)+"""
        f(x)="""+self.function+"""
        fit f(x) '"""+graph.tag+""".dat' u 1:2:3 via a,b,c
        """

        return text


################################################################################################
##                                           MAIN                                             ##
################################################################################################
if (__name__=="__main__"):

    from MW_param import go_to_main_dir,MW_info

    go_to_main_dir()
    MWparam=MW_info('MadWeight_card.dat')
    MWparam.name='fermi'

    if 0:
        obj=Likelihood(MWparam,auto=1)
    elif 1:
        obj=Likelihood(MWparam,auto=0)
        file_list=['./Events/fermi2/fermi2_weights.out','./Events/fermi3/fermi3_weights.out']
        obj.charge_weight(filename=file_list)   #self.info['mw_run']['4'] = normalize with cross
        obj.correct_with_cross('./Events/fermi2/fermi2_cross_weights.out')
        obj.correct_with_acc()
        obj.define_plots(MWparam.info['mw_parameter']['1'])  #option -> gives how param card are generated 
        if obj.run_type=='1':
            obj.define_section_plots()
        obj.plot()
    elif 1:
        obj=Discriminant('../Higgsplusmuon/Events/'+MWparam.name+'/'+MWparam.name+'_norm_weights.out',
                         './Events/'+MWparam.name+'/'+MWparam.name+'_norm_weights.out',
                         MWparam)



    
