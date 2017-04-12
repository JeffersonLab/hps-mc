############################################################################################
# Use this script to submit jobs which take in 1 of each input file, and then output 100   #
# files using 100 jobs. This is typically done with beam-tri slic jobs.                    #
#                                                                                          #
# This procedure makes sure that all input files exist, and that no output already exists; # 
# It only submits the jobs if these conditions are met.                                    #
#                                                                                          #
# v2 takes in a variable template (EngRun2015params.xml) in place of just the beam energy. #
# SET THE VARIABLE TEMPLATE TO THE DESIRED DETECTOR, TRIGGER,etc. BEFORE RUNNING THIS!!!   #
#                                                                                          #
############################################################################################

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

### parse the xml templates
with open(xmlfile,"r") as tmp:
    lines = tmp.readlines()

with open(paramfile,"r") as tmp2:
    paramlines = tmp2.readlines()
 
              
missingInput=''
existingOutput=''
nothingWrong=True

### Check for input and output files associated with each number, and submit 100 jobs if checks are passed ######
num100=int(sys.argv[3])
while int(num100) <= int(sys.argv[4]) :
    
    nums='' 
    tmpfile = 'temp100.xml'
    numlast = 100*int(num100)
    numfirst = int(numlast) - 99

    outputExists=True
    inputExists=True
    allInputsExist=True
    anyOutputExists=False

### load variables into job template
    tmpfile = 'temp100.xml'
    with open(tmpfile,"w") as tmp3:
          for line in lines:
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
              tmp3.write(line)


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


#check the input files...
    for line in lines:
        vars.update({'num100':num100})
        if re.search("^\s*\<Input src=", line) != None:
            matchMe=re.search("src=\"(\S*)\"",line)
            if matchMe!=None :
                thisval=stripPrefix(matchMe.group(1))
                infile=re.sub("\$\{(\w*)\}?",varrepl,thisval)
                inputExists=True
                inputExists=inputExists and os.path.isfile(infile)
                print "Does this input file exist ? "+infile+" " +str(inputExists)
                if inputExists == False :
                    missingInput=str(missingInput)+"\n"+str(infile)
                allInputsExist=allInputsExist and inputExists            
        
#check the output files...
        for i in range(numfirst,int(numlast)+1):
            vars.update({'num':i})
            if re.search("^\s*\<Output src=.*\$\{out_file\}", line) != None :
                matchMe=re.search("dest=\"(\S*)\"",line)
                if matchMe!=None :
                    tmpOut=matchMe.group(1)
                    outfile=re.sub("\$\{(\w*)\}?",varrepl,tmpOut)
                    outputExists=True
                    outputExists=outputExists and os.path.isfile(outfile)
                    print "Does this output file exist ? "+outfile+" " +str(outputExists)
                    if outputExists == True :
                        existingOutput=str(existingOutput)+" "+str(i)
                    anyOutputExists=anyOutputExists or outputExists
     

# If inputs exist and outputs don't exist, write variables and nums into temp100.xml, then submit 100 jobs
    if allInputsExist==True and anyOutputExists==False :
        nums=''
        for i in range(numfirst,int(numlast)+1):
          nums=str(nums)+" "+str(i)

        with open(tmpfile,"w") as tmp3:
          for line in lines:
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
                  line=re.sub(">\s*\d*<\/List>",">"+nums+"</List>",line)
               
              matchMe=re.search("name=\"num100\"\s*value=\"(\S*)\"", line)
              if matchMe!=None :
                  line=re.sub("value=\"\S*\"","value=\""+str(num100)+"\"",line)

              tmp3.write(line)

        print "Submitting jobs "+str(numfirst)+" to "+str(numlast)
        os.system("jsub -xml temp100.xml") 
    else :
        nothingWrong=False

    num100=int(num100)+1

# Print all the problems that caused job submissions to fail
if nothingWrong==False :
   print "\nISSUES FOUND:"
   if allInputsExist==False :
      print "\nMissing input:\n"+str(missingInput)
   if anyOutputExists==True :
      print "\nThese output file numbers already exist:\n"+str(existingOutput)
   print "\nTo override these checks, run with:\n./runjob_100.sh "+xmlfile+" <energy> "+str(sys.argv[3])+" "+str(sys.argv[4])+"\n"

