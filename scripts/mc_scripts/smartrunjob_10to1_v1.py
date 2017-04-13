#############################################################################################
# Use this script to take in 100 input files per number, and submit 1 job that creates      #
# 1 output file. This is typically done for readout jobs.                                   #
#                                                                                           #
# This procedure makes sure that all input files exist, and that no output already exists;  #
# It only submits the jobs if these conditions are met.                                     #
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
    print 'runjob_10to1.py <xml template> <energy> <firstnum> <lastnum>'
    sys.exit()
####

xmlfile=sys.argv[1]
energy=sys.argv[2]
minnum=sys.argv[3]
maxnum=sys.argv[4]
#reallyRun = ' '
#if len(sys.argv) == 6 :
#    reallyRun = sys.argv[5]



#parse the xml template to get some information
#shutil.copy(xmlfile, tmpfile)

with open(xmlfile,"r") as tmp:
    lines = tmp.readlines()

#nums=' '

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
### Check for input and output files associated with each number ######
num=int(sys.argv[3])
while int(num) <= int(sys.argv[4]) :

    #nums=''
    tmpfile = 'temp.xml'
    num100 = ( 9 + num ) / 10

    outputExists=True
    inputExists=True
    allInputsExist=True
    anyOutputExists=False
    i=1
#get the input files...
    for line in lines :
        for i in range(1,11):
            vars.update({'file1':i+(num100-1)*10})
            inputExists=True

            if re.search("^\s*\<Input src=", line) != None:
                matchMe=re.search("src=\"(\S*)\"",line)
                if matchMe!=None :
                    thisval=stripPrefix(matchMe.group(1))
                    infile=re.sub("\$\{(\w*)\}?",varrepl,thisval)
                    # inputExists=inputExists and os.path.isfile(infile)
                    inputExists=os.path.isfile(infile)
                    print "Does this input file exist ? "+infile+" " +str(inputExists)
                    if inputExists == False :
                        missingInput=str(missingInput)+" "+str(i+(num100-1)*10)
                        #missingInput=str(missingInput)+" "+str(i)
                        allInputsExist=allInputsExist and inputExists

#get the output files...
        vars.update({'num':num})
        if re.search("^\s*\<Output src=.*\$\{out_file\}", line) != None :
            matchMe=re.search("dest=\"(\S*)\"",line)
            if matchMe!=None :
                tmpOut=matchMe.group(1)
                outfile=re.sub("\$\{(\w*)\}?",varrepl,tmpOut)
                outputExists=True
                outputExists=outputExists and os.path.isfile(outfile)
                print "Does this output file exist ? "+outfile+" " +str(outputExists)
                if outputExists == True :
                    existingOutput=str(existingOutput)+"\n"+str(outfile)
                anyOutputExists=anyOutputExists or outputExists


# If inputs exist and outputs don't exist, write input files and variables into temp.xml, then submit 1 job
    if allInputsExist==True and outputExists==False :
#    if True:
       with open(tmpfile,"w") as tmp2:
          for line in lines:

           alreadyPrintedLine=False
           matchMe=re.search("name=\"ebeam\"\s*value=\"(\S*)\"", line)
           if matchMe!=None :
               line=re.sub("value=\"\S*\"","value=\""+energy+"\"",line)

           matchMe=re.search("List name=\"num\"",line)
           if matchMe!=None :
               line=re.sub(">\s*\d*<\/List>",">"+str(num)+"</List>",line)


###################### Write input files into template #############################

           matchMe=re.search("src=\"mss:(\S*)\"",line)
           if matchMe!=None :
              for i in range(1,11) :

               vars.update({'file1':i+(num-1)*10})
               file=re.sub("\$\{(\w*)\}?",varrepl,thisval)
               line=re.sub("<Input src=\\S*\"","<Input src=\"mss:"+file+"\"",line)
               line=re.sub("dest=\"in_\\S*","dest=\"in_"+str(i+(num-1)*10)+".slcio\"",line)

               tmp2.write(line)

               #This just keeps the last input from being written twice...
               alreadyPrintedLine=True

           if alreadyPrintedLine==False :
            tmp2.write(line)

##################### Submit jobs if nothing went wrong #################################
       print "Submitting jobs "+str(num-1)+" to "+str(num)
       os.system("jsub -xml temp.xml")
    else :
       nothingWrong=False

    num=int(num)+1

# Print all the problems, if the jobs were not submitted
if nothingWrong==False :
#if True:
   print "\nISSUES FOUND:"
   #if allInputsExist==False :
   if True:
      print "\nMissing input numbers:\n"+str(missingInput)
   if anyOutputExists==True :
      print "\nThese output files already exist:\n"+str(existingOutput)
   print "\nIf this is somehow incorrect, you can override these checks by using:\n./runjob_100to1 " + xmlfile+" "+energy+ " " +str(sys.argv[3])+" "+str(sys.argv[4])+"\n"
   print "Check 'temp.xml' to see the submission template"
