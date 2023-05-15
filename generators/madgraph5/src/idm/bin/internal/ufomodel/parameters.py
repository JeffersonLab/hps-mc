# This file was automatically created by FeynRules 1.6.9
# Mathematica version: 8.0 for Mac OS X x86 (64-bit) (October 5, 2011)
# Date: Wed 22 Jul 2015 00:41:10



from object_library import all_parameters, Parameter


from function_library import complexconjugate, re, im, csc, sec, acsc, asec

# This is a default parameter object representing 0.
ZERO = Parameter(name = 'ZERO',
                 nature = 'internal',
                 type = 'real',
                 value = '0.0',
                 texname = '0')

# User-defined parameters.
cabi = Parameter(name = 'cabi',
                 nature = 'external',
                 type = 'real',
                 value = 0.488,
                 texname = '\\theta _c',
                 lhablock = 'CKMBLOCK',
                 lhacode = [ 1 ])

Mchi = Parameter(name = 'Mchi',
                 nature = 'external',
                 type = 'real',
                 value = 0.1,
                 texname = 'm_{\\chi}',
                 lhablock = 'DM',
                 lhacode = [ 1 ])

dMchi = Parameter(name = 'dMchi',
                  nature = 'external',
                  type = 'real',
                  value = 0.02,
                  texname = '\\Delta',
                  lhablock = 'DM',
                  lhacode = [ 2 ])


GAN = Parameter(name = 'GAN',
                nature = 'external',
                type = 'real',
                value = 0.3028177,
                texname = '\\text{GAN}',
                lhablock = 'FRBlock',
                lhacode = [ 3 ])

Anuc = Parameter(name = 'Anuc',
        nature = 'external',
        type = 'real',
        value = 184.,
        texname = '\\text{Anuc}',
        lhablock = 'FRBlock',
        lhacode = [ 5 ])

Znuc = Parameter(name = 'Znuc',
        nature = 'external',
        type = 'real',
        value = 74.,
        texname = '\\text{Znuc}',
        lhablock = 'FRBlock',
        lhacode = [ 6 ])

protonmu = Parameter(name = 'protonmu',
        nature = 'internal',
        type = 'real',
        value = '2.79',
        texname = '\\mu_p')

Mproton = Parameter(name = 'Mproton',
        nature = 'internal',
        type = 'real',
        value = '0.938',
        texname = 'm_p')

# inelastic1 is another factor multiplying t
#  in a (1+F*t)^2 factor
inelastic1 = Parameter(name = 'inelastic1',
        nature = 'internal',
        type = 'real',
        value = '(protonmu**2 - 1)/(4*Mproton**2)',
        texname = '\\text{inelastic1}')

# inelastic2 is another factor multiplying t
#  but it is in the (1+F*t)^-8 factor
# I (Tom E) think of this as dpval since it
#  is similar to apval (in some respects)
inelastic2 = Parameter(name = 'inelastic2',
        nature = 'internal',
        type = 'real',
        value = '1/0.71',
        texname = '\\text{inelastic2}')

aval = Parameter(name = 'aval',
        nature = 'internal',
        type = 'real',
        value = '111.0*Znuc**(-1/3)/ymel',
        texname = '\\text{aval}')

dval = Parameter(name = 'dval',
        nature = 'internal',
        type = 'real',
        value = '0.164*Anuc**(-2/3)',
        texname = '\\text{dval}')

apval = Parameter(name = 'apval',
        nature = 'internal',
        type = 'real',
        value = '773.0*Znuc**(-2/3)/ymel',
        texname = '\\text{apval}')

MNul = Parameter(name = 'MNul',
                 nature = 'external',
                 type = 'real',
                 value = 174,
                 texname = 'm_{nucl}',
                 lhablock = 'MASS',
                 lhacode = [ 9000002 ])

MZ = Parameter(name = 'MZ',
               nature = 'external',
               type = 'real',
               value = 91.188,
               texname = 'm_{Z}',
               lhablock = 'GAUGEMASS',
               lhacode = [ 1 ])

MW = Parameter(name = 'MW',
               nature = 'external',
               type = 'real',
               value = 80.419,
               texname = 'm_{W}',
               lhablock = 'GAUGEMASS',
               lhacode = [ 2 ])

Map = Parameter(name = 'Map',
                nature = 'external',
                type = 'real',
                value = 1,
                texname = 'm_{A\'}',
                lhablock = 'HIDDEN',
                lhacode = [ 1 ])

MHS = Parameter(name = 'MHS',
                nature = 'external',
                type = 'real',
                value = 200,
                texname = 'm_{H_S}',
                lhablock = 'HIDDEN',
                lhacode = [ 2 ])

epsilon = Parameter(name = 'epsilon',
                    nature = 'external',
                    type = 'real',
                    value = 0.01,
                    texname = '\\epsilon',
                    lhablock = 'HIDDEN',
                    lhacode = [ 3 ])

kap = Parameter(name = 'kap',
                nature = 'external',
                type = 'real',
                value = 1.e-9,
                texname = '\\text{kap}',
                lhablock = 'HIDDEN',
                lhacode = [ 4 ])

aXM1 = Parameter(name = 'aXM1',
                 nature = 'external',
                 type = 'real',
                 value = 127.9,
                 texname = '\\text{aXM1}',
                 lhablock = 'HIDDEN',
                 lhacode = [ 5 ])

MH = Parameter(name = 'MH',
                    nature = 'external',
                    type = 'real',
                    value = 125,
                    texname = 'm_H',
                    lhablock = 'HIGGS',
                    lhacode = [ 1 ])

swsqSM = Parameter(name = 'swsqSM',
                   nature = 'external',
                   type = 'real',
                   value = 0.225,
                   texname = '\\text{swsqSM}',
                   lhablock = 'SMINPUTS',
                   lhacode = [ 1 ])

aEWM1 = Parameter(name = 'aEWM1',
                  nature = 'external',
                  type = 'real',
                  value = 127.9,
                  texname = '\\text{aEWM1}',
                  lhablock = 'SMINPUTS',
                  lhacode = [ 2 ])

Gf = Parameter(name = 'Gf',
               nature = 'external',
               type = 'real',
               value = 0.000011663900000000002,
               texname = '\\text{Gf}',
               lhablock = 'SMINPUTS',
               lhacode = [ 3 ])

aS = Parameter(name = 'aS',
               nature = 'external',
               type = 'real',
               value = 0.118,
               texname = '\\text{aS}',
               lhablock = 'SMINPUTS',
               lhacode = [ 4 ])

ymc = Parameter(name = 'ymc',
                nature = 'external',
                type = 'real',
                value = 1.42,
                texname = '\\text{ymc}',
                lhablock = 'YUKAWA',
                lhacode = [ 4 ])

ymb = Parameter(name = 'ymb',
                nature = 'external',
                type = 'real',
                value = 4.7,
                texname = '\\text{ymb}',
                lhablock = 'YUKAWA',
                lhacode = [ 5 ])

ymt = Parameter(name = 'ymt',
                nature = 'external',
                type = 'real',
                value = 174.3,
                texname = '\\text{ymt}',
                lhablock = 'YUKAWA',
                lhacode = [ 6 ])

ymel = Parameter(name = 'ymel',
                 nature = 'external',
                 type = 'real',
                 value = 0.000511,
                 texname = '\\text{ymel}',
                 lhablock = 'YUKAWA',
                 lhacode = [ 11 ])

ymmu = Parameter(name = 'ymmu',
                 nature = 'external',
                 type = 'real',
                 value = 0.1057,
                 texname = '\\text{ymmu}',
                 lhablock = 'YUKAWA',
                 lhacode = [ 13 ])

ymtau = Parameter(name = 'ymtau',
                  nature = 'external',
                  type = 'real',
                  value = 1.777,
                  texname = '\\text{ymtau}',
                  lhablock = 'YUKAWA',
                  lhacode = [ 15 ])

ME = Parameter(name = 'ME',
               nature = 'external',
               type = 'real',
               value = 0.000511,
               texname = '\\text{ME}',
               lhablock = 'MASS',
               lhacode = [ 11 ])

MM = Parameter(name = 'MM',
               nature = 'external',
               type = 'real',
               value = 0.1057,
               texname = '\\text{MM}',
               lhablock = 'MASS',
               lhacode = [ 13 ])

MTA = Parameter(name = 'MTA',
                nature = 'external',
                type = 'real',
                value = 1.777,
                texname = '\\text{MTA}',
                lhablock = 'MASS',
                lhacode = [ 15 ])

MC = Parameter(name = 'MC',
               nature = 'external',
               type = 'real',
               value = 1.42,
               texname = '\\text{MC}',
               lhablock = 'MASS',
               lhacode = [ 4 ])

MT = Parameter(name = 'MT',
               nature = 'external',
               type = 'real',
               value = 174.3,
               texname = '\\text{MT}',
               lhablock = 'MASS',
               lhacode = [ 6 ])

MB = Parameter(name = 'MB',
               nature = 'external',
               type = 'real',
               value = 4.7,
               texname = '\\text{MB}',
               lhablock = 'MASS',
               lhacode = [ 5 ])

WT = Parameter(name = 'WT',
               nature = 'external',
               type = 'real',
               value = 1.50833649,
               texname = '\\text{WT}',
               lhablock = 'DECAY',
               lhacode = [ 6 ])

Wchi2 = Parameter(name = 'Wchi2',
                  nature = 'external',
                  type = 'real',
                  value = 0.001,
                  texname = '\\text{Wchi2}',
                  lhablock = 'DECAY',
                  lhacode = [ 1000023 ])

WZ = Parameter(name = 'WZ',
               nature = 'external',
               type = 'real',
               value = 2.44140351,
               texname = '\\text{WZ}',
               lhablock = 'DECAY',
               lhacode = [ 23 ])

WAp = Parameter(name = 'WAp',
                nature = 'external',
                type = 'real',
                value = 0.0008252,
                texname = '\\text{WAp}',
                lhablock = 'DECAY',
                lhacode = [ 1023 ])

WW = Parameter(name = 'WW',
               nature = 'external',
               type = 'real',
               value = 2.04759951,
               texname = '\\text{WW}',
               lhablock = 'DECAY',
               lhacode = [ 24 ])

WH = Parameter(name = 'WH',
               nature = 'external',
               type = 'real',
               value = 0.00282299,
               texname = '\\text{WH}',
               lhablock = 'DECAY',
               lhacode = [ 25 ])

WHS = Parameter(name = 'WHS',
                nature = 'external',
                type = 'real',
                value = 5.23795,
                texname = '\\text{WHS}',
                lhablock = 'DECAY',
                lhacode = [ 35 ])

CKM11 = Parameter(name = 'CKM11',
                  nature = 'internal',
                  type = 'complex',
                  value = 'cmath.cos(cabi)',
                  texname = '\\text{CKM11}')

CKM12 = Parameter(name = 'CKM12',
                  nature = 'internal',
                  type = 'complex',
                  value = 'cmath.sin(cabi)',
                  texname = '\\text{CKM12}')

CKM21 = Parameter(name = 'CKM21',
                  nature = 'internal',
                  type = 'complex',
                  value = '-cmath.sin(cabi)',
                  texname = '\\text{CKM21}')

CKM22 = Parameter(name = 'CKM22',
                  nature = 'internal',
                  type = 'complex',
                  value = 'cmath.cos(cabi)',
                  texname = '\\text{CKM22}')

eta = Parameter(name = 'eta',
                nature = 'internal',
                type = 'real',
                value = 'epsilon/(cmath.sqrt(1 - epsilon**2/(1 - swsqSM))*cmath.sqrt(1 - swsqSM))',
                texname = '\\eta')

MChi1 = Parameter(name = 'MChi1',
                  nature = 'internal',
                  type = 'real',
                  value = '-dMchi/2. + Mchi',
                  texname = '\\text{MChi1}')

MChi2 = Parameter(name = 'MChi2',
                  nature = 'internal',
                  type = 'real',
                  value = 'dMchi/2. + Mchi',
                  texname = '\\text{MChi2}')

aEW = Parameter(name = 'aEW',
                nature = 'internal',
                type = 'real',
                value = '1/aEWM1',
                texname = '\\text{aEW}')

aX = Parameter(name = 'aX',
               nature = 'internal',
               type = 'real',
               value = '1/aXM1',
               texname = '\\text{aX}')

G = Parameter(name = 'G',
              nature = 'internal',
              type = 'real',
              value = '2*cmath.sqrt(aS)*cmath.sqrt(cmath.pi)',
              texname = 'G')

v = Parameter(name = 'v',
              nature = 'internal',
              type = 'real',
              value = '1/(2**0.25*cmath.sqrt(Gf))',
              texname = 'v')

chi = Parameter(name = 'chi',
                nature = 'internal',
                type = 'real',
                value = 'eta/cmath.sqrt(1 + eta**2)',
                texname = '\\chi')

DZ = Parameter(name = 'DZ',
               nature = 'internal',
               type = 'real',
               value = '((1 + eta**2)*(eta**2*MW**2 + Map**2 + MZ**2 + (2*cmath.atan(10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000*(Map - MZ))*cmath.sqrt(-4*(1 + eta**2)*Map**2*MZ**2 + (eta**2*MW**2 + Map**2 + MZ**2)**2))/cmath.pi))/(eta**2*MW**2 + Map**2 + MZ**2 - (2*cmath.atan(10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000*(Map - MZ))*cmath.sqrt(-4*(1 + eta**2)*Map**2*MZ**2 + (eta**2*MW**2 + Map**2 + MZ**2)**2))/cmath.pi)',
               texname = '\\text{DZ}')

MZ0 = Parameter(name = 'MZ0',
                nature = 'internal',
                type = 'real',
                value = 'cmath.sqrt((eta**2*MW**2 + Map**2 + MZ**2 - (2*cmath.atan(10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000*(Map - MZ))*cmath.sqrt(-4*(1 + eta**2)*Map**2*MZ**2 + (eta**2*MW**2 + Map**2 + MZ**2)**2))/cmath.pi)/(1 + eta**2))/cmath.sqrt(2)',
                texname = '\\text{MZ0}')

ee = Parameter(name = 'ee',
               nature = 'internal',
               type = 'real',
               value = '2*cmath.sqrt(aEW)*cmath.sqrt(cmath.pi)',
               texname = 'e')

GH = Parameter(name = 'GH',
               nature = 'internal',
               type = 'real',
               value = '-(G**2*(1 + (13*MH**6)/(16800.*MT**6) + MH**4/(168.*MT**4) + (7*MH**2)/(120.*MT**2)))/(12.*cmath.pi**2*v)',
               texname = 'G_H')

Gphi = Parameter(name = 'Gphi',
                 nature = 'internal',
                 type = 'real',
                 value = '-(G**2*(1 + MH**6/(560.*MT**6) + MH**4/(90.*MT**4) + MH**2/(12.*MT**2)))/(8.*cmath.pi**2*v)',
                 texname = 'G_h')

gX = Parameter(name = 'gX',
               nature = 'internal',
               type = 'real',
               value = '2*cmath.sqrt(aX)*cmath.sqrt(cmath.pi)',
               texname = 'g_X')

yb = Parameter(name = 'yb',
               nature = 'internal',
               type = 'real',
               value = '(ymb*cmath.sqrt(2))/v',
               texname = '\\text{yb}')

yc = Parameter(name = 'yc',
               nature = 'internal',
               type = 'real',
               value = '(ymc*cmath.sqrt(2))/v',
               texname = '\\text{yc}')

ye = Parameter(name = 'ye',
               nature = 'internal',
               type = 'real',
               value = '(ymel*cmath.sqrt(2))/v',
               texname = '\\text{ye}')

ym = Parameter(name = 'ym',
               nature = 'internal',
               type = 'real',
               value = '(ymmu*cmath.sqrt(2))/v',
               texname = '\\text{ym}')

yt = Parameter(name = 'yt',
               nature = 'internal',
               type = 'real',
               value = '(ymt*cmath.sqrt(2))/v',
               texname = '\\text{yt}')

ytau = Parameter(name = 'ytau',
                 nature = 'internal',
                 type = 'real',
                 value = '(ymtau*cmath.sqrt(2))/v',
                 texname = '\\text{ytau}')

cw = Parameter(name = 'cw',
               nature = 'internal',
               type = 'real',
               value = 'MW/MZ0',
               texname = 'c_w')

MX = Parameter(name = 'MX',
               nature = 'internal',
               type = 'real',
               value = 'MZ0*cmath.sqrt(DZ)',
               texname = '\\text{MX}')

AH = Parameter(name = 'AH',
               nature = 'internal',
               type = 'real',
               value = '(47*ee**2*(1 - (2*MH**4)/(987.*MT**4) - (14*MH**2)/(705.*MT**2) + (213*MH**12)/(2.634632e7*MW**12) + (5*MH**10)/(119756.*MW**10) + (41*MH**8)/(180950.*MW**8) + (87*MH**6)/(65800.*MW**6) + (57*MH**4)/(6580.*MW**4) + (33*MH**2)/(470.*MW**2)))/(72.*cmath.pi**2*v)',
               texname = 'A_H')

sw = Parameter(name = 'sw',
               nature = 'internal',
               type = 'real',
               value = 'cmath.sqrt(1 - cw**2)',
               texname = 's_w')

g1 = Parameter(name = 'g1',
               nature = 'internal',
               type = 'real',
               value = 'ee/cw',
               texname = 'g_1')

xi = Parameter(name = 'xi',
               nature = 'internal',
               type = 'real',
               value = '(MX*cmath.sqrt(1 - chi**2))/gX',
               texname = '\\xi')

ta = Parameter(name = 'ta',
               nature = 'internal',
               type = 'real',
               value = '-(-1 + DZ + eta**2*sw**2 - (2*cmath.atan(10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000*(-1 + DZ))*cmath.sqrt(4*eta**2*sw**2 + (-1 + DZ + eta**2*sw**2)**2))/cmath.pi)/(2.*eta*sw)',
               texname = 't_{\\alpha }')

th = Parameter(name = 'th',
               nature = 'internal',
               type = 'real',
               value = '(-MH**2 + MHS**2 + (2*cmath.atan(10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000*(MH - MHS))*cmath.sqrt((MH**2 - MHS**2)**2 - 4*kap**2*v**2*xi**2))/cmath.pi)/(2.*kap*v*xi)',
               texname = 't_h')

gw = Parameter(name = 'gw',
               nature = 'internal',
               type = 'real',
               value = 'ee/sw',
               texname = 'g_w')

lam = Parameter(name = 'lam',
                nature = 'internal',
                type = 'real',
                value = '(MH**2 + MHS**2 + (2*cmath.atan(10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000*(MH - MHS))*cmath.sqrt((MH**2 - MHS**2)**2 - 4*kap**2*v**2*xi**2))/cmath.pi)/(4.*v**2)',
                texname = '\\text{lam}')

rho = Parameter(name = 'rho',
                nature = 'internal',
                type = 'real',
                value = '(MH**2 + MHS**2 - (2*cmath.atan(10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000*(MH - MHS))*cmath.sqrt((MH**2 - MHS**2)**2 - 4*kap**2*v**2*xi**2))/cmath.pi)/(4.*xi**2)',
                texname = '\\rho')

ca = Parameter(name = 'ca',
               nature = 'internal',
               type = 'real',
               value = '1/cmath.sqrt(1 + ta**2)',
               texname = 'c_{\\alpha }')

ch = Parameter(name = 'ch',
               nature = 'internal',
               type = 'real',
               value = '1/cmath.sqrt(1 + th**2)',
               texname = 'c_h')

muH2 = Parameter(name = 'muH2',
                 nature = 'internal',
                 type = 'real',
                 value = '(kap*v**2)/2. + rho*xi**2',
                 texname = '\\text{muH2}')

muSM2 = Parameter(name = 'muSM2',
                  nature = 'internal',
                  type = 'real',
                  value = 'lam*v**2 + (kap*xi**2)/2.',
                  texname = '\\text{muSM2}')

sa = Parameter(name = 'sa',
               nature = 'internal',
               type = 'real',
               value = 'ta/cmath.sqrt(1 + ta**2)',
               texname = 's_{\\alpha }')

sh = Parameter(name = 'sh',
               nature = 'internal',
               type = 'real',
               value = 'th/cmath.sqrt(1 + th**2)',
               texname = 's_h')

