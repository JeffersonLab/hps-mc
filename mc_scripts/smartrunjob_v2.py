############################################################################################
# Use this to submit jobs to the farm, those which run 1 job/output for each <num>         #
# This is typical for jobs in the 'stdhep' and 'recon' directories.                        #
#                                                                                          #
# To (dry) run:                                                                            #
# 'python smartrunjob_v2.py <job xml> <param xml> <firstnum> <lastnum>'                    #
# This sets up a job for each <num>, if all input files exist, and no output               #
# files already exist. Re-enter the command with 'yes' included to submit the job.         #
#                                                                                          #
# v2 takes in a variable template (EngRun2015params.xml) in place of just the beam energy. #
# SET THE VARIABLE TEMPLATE TO THE DESIRED DETECTOR, TRIGGER, etc. BEFORE RUNNING THIS!!!  #
#                                                                                          #
############################################################################################
import sys
import shutil
import re
import string
import os
from subprocess import Popen, PIPE
vars = dict()

#Search Helpers
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

#Main
##########################################################################################
####
if len(sys.argv) < 5:  #remember, the script name counts as an argument!
    print 'runjob.py <xml template> <param template> <firstnum> <lastnum> [Really Run?]'
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



#read and store the xml templates

with open(xmlfile,"r") as tmp:
    lines = tmp.readlines()

with open(paramfile,"r") as tmp2:
    paramlines = tmp2.readlines()

#get any variables defined
for line in lines:
    if re.search("^\s*\<Variable name=", line) != None:
        matchMe=re.search("name=\"(\S*)\"\s*value=\"(\S*)\"",line)
        if matchMe!=None :
            thisname=matchMe.group(1)
            thisval=stripPrefix(matchMe.group(2))
            vars.update({thisname:thisval})

#check if input files exist
inum=minnum
nums=''
while int(inum) <= int(maxnum) :
    outputExists=False
    outputExists=True
    inputExists=True
    vars.update({'num':inum})
    for line in lines:
        if re.search("^\s*\<Input src=", line) != None:
            matchMe=re.search("src=\"(\S*)\"",line)
            if matchMe!=None :
                thisval=stripPrefix(matchMe.group(1))
                infile=re.sub("\$\{(\w*)\}?",varrepl,thisval)
                inputExists=inputExists and os.path.isfile(infile)
                print "Does this input file exist ? "+infile+" " +str(inputExists)

#check that out_files don't already exist
        if re.search("^\s*\<Output src=.*\$\{out_file\}", line) != None :
            matchMe=re.search("dest=\"(\S*)\"",line)
            if matchMe!=None :
                tmpOut=matchMe.group(1)
                outfile=re.sub("\$\{(\w*)\}?",varrepl,tmpOut)
                outputExists=outputExists and os.path.isfile(outfile)
                print "Does this output file exist ? "+outfile+" " +str(outputExists)
    if outputExists != True and inputExists == True :
        nums=str(nums)+" "+str(inum)
    inum=int(inum)+1

print nums

if re.search("\d",nums) == None :
    print "No jobs to run!"
    exit


tmpfile = 'temp.xml'
with open(tmpfile,"w") as tmp3:

#load variables from parameter template into job template
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

#submit job if really sure
if reallyRun == "yes" :
    os.system("jsub -xml temp.xml")
else :
    print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    print "ok...dry run over..."
    print "Check temp.xml and make sure everything is correct!"
    print "If temp.xml looks ok, add \"yes\" argument at end  to really submit the jobs...like this:"
    print  "python smartrunjob.py " + xmlfile+" "+paramfile+ " " +minnum+" "+maxnum+" yes"
