import ROOT as r
import os
import glob

myT = r.TTree("myT","myT")
myT.ReadFile("hps2019goldRuns.csv")
nPartsList = []
nPartsTot = 0
nPartsPass = 0

outF = open('run2019pass0.txt','w')

for run in myT:
    RN = run.number
    fullpath = f'/mss/hallb/hps/physrun2019/data/hps_0{RN}/hps_0{RN}.evio.*'
    nParts = len(glob.glob(fullpath))
    nPartsList.append(nParts)
    print("")
    print("Run number: ", RN)
    print("N partitions: ", nParts)
    passList = glob.glob(f'/mss/hallb/hps/physrun2019/data/hps_0{RN}/hps_0{RN}.evio.0[1230]03[1234567890]')
    print(passList)
    print(len(passList))
    if nParts == 0: print("hmmmmmmmmmmmmm   ", fullpath)
    nPartsTot += nParts
    nPartsPass += len(passList)
    for fileLine in passList:
        outF.write(fileLine+'\n')
        pass

    pass

print("Total parts total: ", nPartsTot)
print("Total parts in pass: ", nPartsPass)
nPartsList.sort()
print(nPartsList)

outF.close()
