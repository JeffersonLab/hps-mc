import ROOT as r
lheFile = open('ap_unweighted_events.lhe')
histos = {}
histos['ap_pz_h'] = r.TH1D('ap_pz_h', ';A\' p_{z} [GeV];Events / 100 MeV', 50, 0.0, 5.0)
histos['ele_py_h'] = r.TH1D('ele_py_h', ';Electron p_{y} [GeV];Events / 10 MeV', 100, 0.0, 1.0)
histos['ele_pz_h'] = r.TH1D('ele_pz_h', ';Electron p_{z} [GeV];Events / 100 MeV', 50, 0.0, 5.0)
histos['ele_lambda_h'] = r.TH1D('ele_lambda_h', ';Electron p_{y}/p_{z};Events / 0.001', 1000, -1.0, 1.0)
histos['ele_pxy_hh'] = r.TH2D('ele_pxy_hh', ';Electron p_{x};Electron p_{y}', 100, -1.0, 1.0, 100, -1.0, 1.0)
histos['ele_pxyz_hh'] = r.TH2D('ele_pxyz_hh', ';Electron p_{x}/p_{z};Electron p_{y}/p_{z}', 100, -1.0, 1.0, 100, -1.0, 1.0)
histos['pos_py_h'] = r.TH1D('pos_py_h', ';Positron p_{y};Events / 10 MeV', 100, 0.0, 1.0)
histos['pos_pz_h'] = r.TH1D('pos_pz_h', ';Positron p_{z};Events / 100 MeV', 50, 0.0, 5.0)
histos['pos_lambda_h'] = r.TH1D('pos_lambda_h', ';Positron p_{y}/p_{z};Events / 0.001', 1000, -1.0, 1.0)
histos['pos_pxy_hh'] = r.TH2D('pos_pxy_hh', ';Positron p_{x};Positron p_{y}', 100, -1.0, 1.0, 100, -1.0, 1.0)
histos['pos_pxyz_hh'] = r.TH2D('pos_pxyz_hh', ';Positron p_{x}/p_{z};Positron p_{y}/p_{z}', 100, -1.0, 1.0, 100, -1.0, 1.0)
inEv = False
Nev = 0
for fLine in lheFile:
    fLine = fLine.rstrip()
    if fLine == "<event>":
        eleP = []
        posP = []
        inEv = True
        Nev += 1
        continue
    if fLine == "</event>":
        histos['pos_pxy_hh'].Fill(posP[0],posP[1])
        histos['pos_pxyz_hh'].Fill(posP[0]/posP[2],posP[1]/posP[2])
        histos['ele_pxy_hh'].Fill(eleP[0],eleP[1])
        histos['ele_pxyz_hh'].Fill(eleP[0]/eleP[2],eleP[1]/eleP[2])
        inEv = False
        continue
    pass
    if inEv:
        fLine = fLine.split()
        if int(fLine[0]) == 611:
            posP.extend([float(fLine[6]),float(fLine[7]),float(fLine[8])])
            histos['pos_py_h'].Fill(float(fLine[7]))
            histos['pos_pz_h'].Fill(float(fLine[8]))
            histos['pos_lambda_h'].Fill(float(fLine[7])/float(fLine[8]))
        if int(fLine[0]) == -611:
            eleP.extend([float(fLine[6]),float(fLine[7]),float(fLine[8])])
            histos['ele_py_h'].Fill(float(fLine[7]))
            histos['ele_pz_h'].Fill(float(fLine[8]))
            histos['ele_lambda_h'].Fill(float(fLine[7])/float(fLine[8]))
        if int(fLine[0]) == 622:
            histos['ap_pz_h'].Fill(float(fLine[8]))
outFile = r.TFile('lheTest.root','RECREATE')
outFile.cd()
for hist in histos:
    histos[hist].Write()
    pass
outFile.Close()
print("Nev: ", Nev)
