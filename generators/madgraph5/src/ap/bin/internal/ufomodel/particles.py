# This file was automatically created by FeynRules =.2.4
# Mathematica version: 9.0 for Linux x86 (64-bit) (November 20, 2012)
# Date: Wed 20 Jul 2016 20:12:57


from __future__ import division
from object_library import all_particles, Particle
import parameters as Param

import propagators as Prop

ve = Particle(pdg_code = 12,
              name = 've',
              antiname = 've~',
              spin = 2,
              color = 1,
              mass = Param.ZERO,
              width = Param.ZERO,
              texname = 've',
              antitexname = 've~',
              charge = 0,
              GhostNumber = 0,
              LeptonNumber = 1)

ve__tilde__ = ve.anti()

vm = Particle(pdg_code = 14,
              name = 'vm',
              antiname = 'vm~',
              spin = 2,
              color = 1,
              mass = Param.ZERO,
              width = Param.ZERO,
              texname = 'vm',
              antitexname = 'vm~',
              charge = 0,
              GhostNumber = 0,
              LeptonNumber = 1)

vm__tilde__ = vm.anti()

vt = Particle(pdg_code = 16,
              name = 'vt',
              antiname = 'vt~',
              spin = 2,
              color = 1,
              mass = Param.ZERO,
              width = Param.ZERO,
              texname = 'vt',
              antitexname = 'vt~',
              charge = 0,
              GhostNumber = 0,
              LeptonNumber = 1)

vt__tilde__ = vt.anti()

e__minus__ = Particle(pdg_code = 11,
                      name = 'e-',
                      antiname = 'e+',
                      spin = 2,
                      color = 1,
                      mass = Param.Me,
                      width = Param.ZERO,
                      texname = 'e-',
                      antitexname = 'e+',
                      charge = -1,
                      GhostNumber = 0,
                      LeptonNumber = 1)

e__plus__ = e__minus__.anti()

f__minus__ = Particle(pdg_code = 611,
                      name = 'f-',
                      antiname = 'f+',
                      spin = 2,
                      color = 1,
                      mass = Param.Me,
                      width = Param.ZERO,
                      texname = 'f-',
                      antitexname = 'f+',
                      charge = -1,
                      GhostNumber = 0,
                      LeptonNumber = 1)

f__plus__ = f__minus__.anti()

m__minus__ = Particle(pdg_code = 13,
                      name = 'm-',
                      antiname = 'm+',
                      spin = 2,
                      color = 1,
                      mass = Param.MM,
                      width = Param.ZERO,
                      texname = 'm-',
                      antitexname = 'm+',
                      charge = -1,
                      GhostNumber = 0,
                      LeptonNumber = 1)

m__plus__ = m__minus__.anti()

tt__minus__ = Particle(pdg_code = 15,
                       name = 'tt-',
                       antiname = 'tt+',
                       spin = 2,
                       color = 1,
                       mass = Param.MTA,
                       width = Param.ZERO,
                       texname = 'tt-',
                       antitexname = 'tt+',
                       charge = -1,
                       GhostNumber = 0,
                       LeptonNumber = 1)

tt__plus__ = tt__minus__.anti()

u = Particle(pdg_code = 2,
             name = 'u',
             antiname = 'u~',
             spin = 2,
             color = 3,
             mass = Param.MU,
             width = Param.ZERO,
             texname = 'u',
             antitexname = 'u~',
             charge = 2/3,
             GhostNumber = 0,
             LeptonNumber = 0)

u__tilde__ = u.anti()

c = Particle(pdg_code = 4,
             name = 'c',
             antiname = 'c~',
             spin = 2,
             color = 3,
             mass = Param.MC,
             width = Param.ZERO,
             texname = 'c',
             antitexname = 'c~',
             charge = 2/3,
             GhostNumber = 0,
             LeptonNumber = 0)

c__tilde__ = c.anti()

t = Particle(pdg_code = 6,
             name = 't',
             antiname = 't~',
             spin = 2,
             color = 3,
             mass = Param.MT,
             width = Param.WT,
             texname = 't',
             antitexname = 't~',
             charge = 2/3,
             GhostNumber = 0,
             LeptonNumber = 0)

t__tilde__ = t.anti()

d = Particle(pdg_code = 1,
             name = 'd',
             antiname = 'd~',
             spin = 2,
             color = 3,
             mass = Param.MD,
             width = Param.ZERO,
             texname = 'd',
             antitexname = 'd~',
             charge = -1/3,
             GhostNumber = 0,
             LeptonNumber = 0)

d__tilde__ = d.anti()

s = Particle(pdg_code = 3,
             name = 's',
             antiname = 's~',
             spin = 2,
             color = 3,
             mass = Param.MS,
             width = Param.ZERO,
             texname = 's',
             antitexname = 's~',
             charge = -1/3,
             GhostNumber = 0,
             LeptonNumber = 0)

s__tilde__ = s.anti()

b = Particle(pdg_code = 5,
             name = 'b',
             antiname = 'b~',
             spin = 2,
             color = 3,
             mass = Param.MB,
             width = Param.ZERO,
             texname = 'b',
             antitexname = 'b~',
             charge = -1/3,
             GhostNumber = 0,
             LeptonNumber = 0)

b__tilde__ = b.anti()

chi = Particle(pdg_code = 9000001,
               name = 'chi',
               antiname = 'chi~',
               spin = 2,
               color = 1,
               mass = Param.MChi,
               width = Param.ZERO,
               texname = 'chi',
               antitexname = 'chi~',
               charge = 0,
               GhostNumber = 0,
               LeptonNumber = 0)

chi__tilde__ = chi.anti()

N = Particle(pdg_code = 9000002,
             name = 'N',
             antiname = 'N~',
             spin = 2,
             color = 1,
             mass = Param.MNul,
             width = Param.ZERO,
             texname = 'N',
             antitexname = 'N~',
             charge = 0,
             GhostNumber = 0,
             LeptonNumber = 0)

N__tilde__ = N.anti()

ghA = Particle(pdg_code = 9000003,
               name = 'ghA',
               antiname = 'ghA~',
               spin = -1,
               color = 1,
               mass = Param.ZERO,
               width = Param.ZERO,
               texname = 'ghA',
               antitexname = 'ghA~',
               charge = 0,
               GhostNumber = 1,
               LeptonNumber = 0)

ghA__tilde__ = ghA.anti()

ghZ = Particle(pdg_code = 9000004,
               name = 'ghZ',
               antiname = 'ghZ~',
               spin = -1,
               color = 1,
               mass = Param.MZ,
               width = Param.ZERO,
               texname = 'ghZ',
               antitexname = 'ghZ~',
               charge = 0,
               GhostNumber = 1,
               LeptonNumber = 0)

ghZ__tilde__ = ghZ.anti()

ghWp = Particle(pdg_code = 9000005,
                name = 'ghWp',
                antiname = 'ghWp~',
                spin = -1,
                color = 1,
                mass = Param.MW,
                width = Param.ZERO,
                texname = 'ghWp',
                antitexname = 'ghWp~',
                charge = 1,
                GhostNumber = 1,
                LeptonNumber = 0)

ghWp__tilde__ = ghWp.anti()

ghWm = Particle(pdg_code = 9000006,
                name = 'ghWm',
                antiname = 'ghWm~',
                spin = -1,
                color = 1,
                mass = Param.MW,
                width = Param.ZERO,
                texname = 'ghWm',
                antitexname = 'ghWm~',
                charge = -1,
                GhostNumber = 1,
                LeptonNumber = 0)

ghWm__tilde__ = ghWm.anti()

ghG = Particle(pdg_code = 9000007,
               name = 'ghG',
               antiname = 'ghG~',
               spin = -1,
               color = 8,
               mass = Param.ZERO,
               width = Param.ZERO,
               texname = 'ghG',
               antitexname = 'ghG~',
               charge = 0,
               GhostNumber = 1,
               LeptonNumber = 0)

ghG__tilde__ = ghG.anti()

ghAp = Particle(pdg_code = 9000008,
                name = 'ghAp',
                antiname = 'ghAp~',
                spin = -1,
                color = 1,
                mass = Param.ZERO,
                width = Param.ZERO,
                texname = 'ghAp',
                antitexname = 'ghAp~',
                charge = 0,
                GhostNumber = 1,
                LeptonNumber = 0)

ghAp__tilde__ = ghAp.anti()

A = Particle(pdg_code = 22,
             name = 'A',
             antiname = 'A',
             spin = 3,
             color = 1,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 'A',
             antitexname = 'A',
             charge = 0,
             GhostNumber = 0,
             LeptonNumber = 0)

Z = Particle(pdg_code = 23,
             name = 'Z',
             antiname = 'Z',
             spin = 3,
             color = 1,
             mass = Param.MZ,
             width = Param.WZ,
             texname = 'Z',
             antitexname = 'Z',
             charge = 0,
             GhostNumber = 0,
             LeptonNumber = 0)

W__plus__ = Particle(pdg_code = 24,
                     name = 'W+',
                     antiname = 'W-',
                     spin = 3,
                     color = 1,
                     mass = Param.MW,
                     width = Param.WW,
                     texname = 'W+',
                     antitexname = 'W-',
                     charge = 1,
                     GhostNumber = 0,
                     LeptonNumber = 0)

W__minus__ = W__plus__.anti()

G = Particle(pdg_code = 21,
             name = 'G',
             antiname = 'G',
             spin = 3,
             color = 8,
             mass = Param.ZERO,
             width = Param.ZERO,
             texname = 'G',
             antitexname = 'G',
             charge = 0,
             GhostNumber = 0,
             LeptonNumber = 0)

Ap = Particle(pdg_code = 622,
              name = 'Ap',
              antiname = 'Ap',
              spin = 3,
              color = 1,
              mass = Param.MAp,
              width = Param.WAp,
              texname = 'Ap',
              antitexname = 'Ap',
              charge = 0,
              GhostNumber = 0,
              LeptonNumber = 0)

H = Particle(pdg_code = 25,
             name = 'H',
             antiname = 'H',
             spin = 1,
             color = 1,
             mass = Param.MH,
             width = Param.WH,
             texname = '\\phi',
             antitexname = '\\phi',
             charge = 0,
             GhostNumber = 0,
             LeptonNumber = 0)

phi0 = Particle(pdg_code = 250,
                name = 'phi0',
                antiname = 'phi0',
                spin = 1,
                color = 1,
                mass = Param.MZ,
                width = Param.ZERO,
                texname = 'phi0',
                antitexname = 'phi0',
                goldstone = True,
                charge = 0,
                GhostNumber = 0,
                LeptonNumber = 0)

phi__plus__ = Particle(pdg_code = 251,
                       name = 'phi+',
                       antiname = 'phi-',
                       spin = 1,
                       color = 1,
                       mass = Param.MW,
                       width = Param.ZERO,
                       texname = '\\phi^+',
                       antitexname = '\\phi^-',
                       goldstone = True,
                       charge = 1,
                       GhostNumber = 0,
                       LeptonNumber = 0)

phi__minus__ = phi__plus__.anti()

