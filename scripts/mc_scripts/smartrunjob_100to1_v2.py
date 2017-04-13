#############################################################################################
# Use this script to take in 100 input files per <num>, and submit 1 job that creates       #
# 1 output file. This is typically done for readout jobs.                                   #
#                                                                                           #
# This procedure makes sure that all input files exist, and that no output already exists;  #
# it only loads the input files and submits the jobs if these conditions are met.           #
#                                                                                           #
# v2 takes in a variable template (EngRun2015params.xml) in place of just the beam energy.  #
# SET THE VARIABLE TEMPLATE TO THE DESIRED DETECTOR, TRIGGER,etc. BEFORE RUNNING THIS!!!    #
#                                                                                           #
#############################################################################################
import sys
import shutil
import re
import string
import os
from subprocess import Popen, PIPE
vars = dict()

#Helpers....
##########################################################################################
def stripPrefix(line):
    newline=line
    matchMe=re.search("^file:(\S*)",line)
    if matchMe!=None :
        newline=matchMe.group(1)
    matchMe=re.search("^mss:(\S*)",line)
    if matchMe!=None :
        newline=matchMe.group(1)
    return newline


def varrepl(matchobj):
    global vars
    return  str(vars[matchobj.group(1)])

#Main....
##########################################################################################
####
if len(sys.argv) < 5:  #remember, the script name counts as an argument!
   # print 'runjob.py <xml template> <energy>  <firstnum> <lastnum> [Really Run?]'
    print 'runjob_100.py <xml template> <param template> <firstnum> <lastnum>'
    sys.exit()
####

xmlfile=sys.argv[1]
#energy=sys.argv[2]
paramfile=sys.argv[2]
minnum=sys.argv[3]
maxnum=sys.argv[4]
reallyRun = ' ' 
if len(sys.argv) == 6 :
    reallyRun = sys.argv[5]



### parse the xml templates
with open(xmlfile,"r") as tmp:
    lines = tmp.readlines()

with open(paramfile,"r") as tmp2:
    paramlines = tmp2.readlines()


missingInput=''
existingOutput=''
nothingWrong=True

### Check for input and output files associated with each number ######
num=int(sys.argv[3])
while int(num) <= int(sys.argv[4]) :

    tmpfile = 'temp.xml'
    num100 = ( 99 + num ) / 100

    outputExists=True
    inputExists=True
    allInputsExist=True
    anyOutputExists=False

    with open(tmpfile,"w") as tmp2:
            for line in lines:
                alreadyPrintedLine=False

### Reads variables from parameter xml into job template ###
                if re.search("^\s*\<Variable name=", line) != None:
                     matchMe=re.search("name=\"(\S*)\"\s*value=\"(\S*)\"",line)
                     if matchMe!=None :
                         thisname=matchMe.group(1)
                         thisval=stripPrefix(matchMe.group(2))
                         vars.update({thisname:thisval})
                         for paramline in paramlines:
                             if re.search("^\s*\<Variable name=", paramline) != None:
                                 matchMe=re.search("name=\""+thisname+"\"\s*value=\"(\S*)\"",paramline)
                                 if matchMe!=None :
                                     line=paramline

                matchMe=re.search("List name=\"num\"",line)
                if matchMe!=None :
                    line=re.sub(">\s*\d*<\/List>",">"+str(num)+"</List>",line)

                tmp2.write(line)
              

### Read in updated lines to check input/output
    with open(tmpfile,"r") as tmp2:
       lines = tmp2.readlines()


    for line in lines:
        if re.search("^\s*\<Variable name=", line) != None:
            matchMe=re.search("name=\"(\S*)\"\s*value=\"(\S*)\"",line)
            if matchMe!=None :
                thisname=matchMe.group(1)
                thisval=stripPrefix(matchMe.group(2))
                vars.update({thisname:thisval})


### Perform checks ##### 

#get the input files...
    vars.update({'num':num})
    for line in lines :
        for i in range(1,101):

            vars.update({'file1':i+(num-1)*100})
     
            if re.search("^\s*\<Input src=", line) != None:
                matchMe=re.search("src=\"(\S*)\"",line)
                if matchMe!=None :
                    thisval=stripPrefix(matchMe.group(1))
                    infile=re.sub("\$\{(\w*)\}?",varrepl,thisval)                   
                    inputExists=inputExists and os.path.isfile(infile)
                    print "Does this input file exist ? "+infile+" " +str(inputExists)
                    if inputExists == False :
                        missingInput=str(missingInput)+" "+str(i+(num-1)*100)
                    allInputsExist=allInputsExist and inputExists            

#get the output files...
        if re.search("^\s*\<Output src=.*\$\{out_file\}", line) != None :
            matchMe=re.search("dest=\"(\S*)\"",line)
            if matchMe!=None :
                tmpOut=matchMe.group(1)
                outfile=re.sub("\$\{(\w*)\}?",varrepl,tmpOut)
                outputExists=outputExists and os.path.isfile(outfile)
                print "Does this output file exist ? "+outfile+" "+str(outputExists)
                if outputExists == True :
                    existingOutput=str(existingOutput)+"\n"+str(outfile)
                anyOutputExists=anyOutputExists and outputExists
     
##############################################################################################################################
## If ALL inputs exist and NONE of the outputs exist, write input files and variables into temp100to1.xml, then submit 1 job #
##############################################################################################################################
    if allInputsExist==True and anyOutputExists==False :
#    if True:
        with open(tmpfile,"w") as tmp2:
            for line in lines:
                alreadyPrintedLine=False           
                matchMe=re.search("src=\"mss:(\S*)\"",line)
                if matchMe!=None :
                    for i in range(1,101) : 

                        vars.update({'file1':i+(num-1)*100})
                        file=re.sub("\$\{(\w*)\}?",varrepl,thisval)
                        line=re.sub("<Input src=\\S*\"","<Input src=\"mss:"+file+"\"",line)
                        line=re.sub("dest=\"in_\\S*","dest=\"in_"+str(i+(num-1)*100)+".slcio\"",line)

                        tmp2.write(line)

                        #This just keeps the last input file from being written twice...
                        alreadyPrintedLine=True

                if alreadyPrintedLine==False :
                    tmp2.write(line)

##################### Submit jobs if nothing went wrong #################################
        print "\nSubmitting jobs "+str(num-1)+" to "+str(num)+"\n"
        os.system("jsub -xml temp.xml") 
    else :
        nothingWrong=False

    num=int(num)+1

### Print all the problems, if the jobs were not submitted for some reason
if nothingWrong==False :
   print "\nISSUES FOUND:"
   if allInputsExist==False :
      print "\nMissing input #'s:\n"+str(missingInput)
   if anyOutputExists==True :
      print "\nThese output files already exist:\n"+str(existingOutput)
   print "\nIf this is somehow incorrect, you can override these checks using:\n./runjob_100to1 " + xmlfile+" "+paramfile+ " " +str(sys.argv[3])+" "+str(sys.argv[4])+"\n"

