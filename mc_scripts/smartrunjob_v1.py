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
    if matchMe!=None :\
        newline=matchMe.group(1)
    return newline


def varrepl(matchobj):
    global vars
    return  str(vars[matchobj.group(1)])

#Main....
##########################################################################################
####
if len(sys.argv) < 5:  #remember, the script name counts as an argument!
    print 'runjob.py <xml template> <energy>  <firstnum> <lastnum> [Really Run?]'
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

nums=' '

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


inum=minnum
nums=''
while int(inum) <= int(maxnum) :
#    outputExists=False
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
#get the output files...only the one with the out_file variable though
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

with open(tmpfile,"w") as tmp2:
    for line in lines:
        matchMe=re.search("name=\"ebeam\"\s*value=\"(\S*)\"", line)
        if matchMe!=None :
            line=re.sub("value=\"\S*\"","value=\""+energy+"\"",line)
        matchMe=re.search("List name=\"num\"",line)
        if matchMe!=None :
            line=re.sub(">\s*\d*<\/List>",">"+nums+"</List>",line)
        tmp2.write(line)


if reallyRun == "yes" :
    os.system("jsub -xml temp.xml")
else :
    print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
    print "ok...dry run over..."
    print "check temp.xml and make sure this is what you want to do!"
    print "if temp.xml looks ok, add \"yes\" argument at end  to really submit the jobs...like this:"
    print  "python smartrunjob.py " + xmlfile+" "+energy+ " " +minnum+" "+maxnum+" yes"
