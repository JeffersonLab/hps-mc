#########################################################################################
# Use this script to submit jobs which take in 1 of each input file, and then output 100
# files using 100 jobs. This is typically done with beam-tri slic jobs.
#
# This procedure makes sure that all input files exist, and that no output already exists;
# It only submits the jobs if these conditions are met.
########################################################################################
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
    print 'runjob_100.py <xml template> <energy> <firstnum> <lastnum>'
    sys.exit()
####

xmlfile=sys.argv[1]
energy=sys.argv[2]
minnum=sys.argv[3]
maxnum=sys.argv[4]
reallyRun = ' '
if len(sys.argv) == 6 :
    reallyRun = sys.argv[5]



#parse the xml template to get some information
#shutil.copy(xmlfile, tmpfile)

with open(xmlfile,"r") as tmp:
    lines = tmp.readlines()

#get any variables defined
for line in lines:
    if re.search("^\s*\<Variable name=", line) != None:
        matchMe=re.search("name=\"(\S*)\"\s*value=\"(\S*)\"",line)
        if matchMe!=None :
            thisname=matchMe.group(1)
            thisval=stripPrefix(matchMe.group(2))
            vars.update({thisname:thisval})

# assign beam energy
vars.update({'ebeam':energy})
print vars

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
    i=int(numfirst)

#get the input files...
    for line in lines:
        vars.update({'num100':num100})
        if re.search("^\s*\<Input src=", line) != None:
            matchMe=re.search("src=\"(\S*)\"",line)
            if matchMe!=None :
                thisval=stripPrefix(matchMe.group(1))
                infile=re.sub("\$\{(\w*)\}?",varrepl,thisval)
                inputExists=True
                inputExists=os.path.isfile(infile)
                print "Does this input file exist ? "+infile+" " +str(inputExists)
                if inputExists == False :
                    missingInput=str(missingInput)+"\n"+str(infile)
                allInputsExist=allInputsExist and inputExists

#get the output files...
        for i in range(numfirst,int(numlast)+1):
            vars.update({'num':i})
            if re.search("^\s*\<Output src=.*\$\{out_file\}", line) != None :
                matchMe=re.search("dest=\"(\S*)\"",line)
                if matchMe!=None :
                    tmpOut=matchMe.group(1)
                    outfile=re.sub("\$\{(\w*)\}?",varrepl,tmpOut)
                    #outputExists=True
                    outputExists=os.path.isfile(outfile)
                    print "Does this output file exist ? "+outfile+" " +str(outputExists)
                    if outputExists == True :
                        existingOutput=str(existingOutput)+" "+str(i)
                    anyOutputExists=anyOutputExists or outputExists


#    If inputs exist and outputs don't exist, write nums for the current num100 into temp100.xml, then submit 100 jobs
    if inputExists==True and anyOutputExists==False :
       nums=''
       for i in range(numfirst,int(numlast)+1):
          nums=str(nums)+" "+str(i)

       with open(tmpfile,"w") as tmp2:
          for line in lines:
           matchMe=re.search("name=\"ebeam\"\s*value=\"(\S*)\"", line)
           if matchMe!=None :
               line=re.sub("value=\"\S*\"","value=\""+energy+"\"",line)

           matchMe=re.search("List name=\"num\"",line)
           if matchMe!=None :
               line=re.sub(">\s*\d*<\/List>",">"+nums+"</List>",line)

           matchMe=re.search("name=\"num100\"\s*value=\"(\S*)\"", line)
           if matchMe!=None :
               line=re.sub("value=\"\S*\"","value=\""+str(num100)+"\"",line)
           tmp2.write(line)
       print "Submitting jobs "+str(numfirst)+" to "+str(numlast)
       os.system("jsub -xml temp100.xml")
    else :
       nothingWrong=False

    num100=int(num100)+1

# Print all the problems, if the jobs were not submitted
#if nothingWrong==False :
if True:
   print "\nISSUES FOUND:"
   if allInputsExist==False :
      print "\nMissing input:\n"+str(missingInput)
   if anyOutputExists==True :
      print "\nThese output file numbers already exist:\n"+str(existingOutput)
   print "\nIf this is somehow incorrect, you can override these checks using:\n./runjob_100 " + xmlfile+" "+energy+ " " +str(sys.argv[3])+" "+str(sys.argv[4])+"\n"
   print "The template used was: temp100.xml"

