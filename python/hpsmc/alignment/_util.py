"""! alignment utilities"""

def getBeamspotConstraints(parMap):
    """! Put beamspot constraints on all parameters regardless of floating """
    s = '\n!Beamspot constraints\n'
    for d in ['u','v','w']:
        PEDE.logger.debug('d=',d)
        for t in ['t','r']:
            for iAxial in range(2):
                active = False
                PEDE.logger.debug('iAx=',iAxial)
                for p, name in utils.paramMap.iteritems():
                    PEDE.logger.debug('look at ', name, ' ', p)
                    if utils.getModuleNrFromDeName(name) != 0: continue
                    if (utils.isAxial(name) and iAxial==0) or (not utils.isAxial(name) and iAxial==1): continue
                    if utils.getDir(p) == d and utils.getType(p) == t:
                        PEDE.logger.debug('found one',name, ' ', p)
                        if not active:
                            PEDE.logger.debug('ACTIVATE')
                            s += 'Constraint 0.\n'    
                            s += '%s %.1f\n' % (p, 1.0)
                            active = True
                        else:
                            PEDE.logger.debug('ADD')
                            s += '%s %.1f\n' % (p, -1.0)
    return s

def getBeamspotConstraintsFloatingOnly(pars):
    """! put beamspot constraints onto floating parameters"""
    s = '\n!Beamspot constraints\n'
    written1 = 0
    written2 = 0
    written3 = 0
    written4 = 0
    written5 = 0
    written6 = 0
    for p in pars:
        line = p.toString()
        if ('98' or '99') in line:
            parNum = int(line.split()[0])
            isFloat = float(line.split()[2])
            if('1198') in line:
                if(isFloat==0): 
                    if(parNum < 20000 and written1==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, 1.0)
                        s += '%s %.1f\n' % (parNum+10000, -1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, 1.0)
                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, 1.0)
#                        s += '%s %.1f\n' % (parNum+1, -1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum+10000, 1.0)
#                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
#                        s += '\n'
#                        written1 = 1
                    elif(parNum > 20000 and written1==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, -1.0)
                        s += '%s %.1f\n' % (parNum-10000, 1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, -1.0)
                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, -1.0)
#                        s += '%s %.1f\n' % (parNum+1, 1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum-10000, -1.0)
#                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
#                        s += '\n'
                        written1 = 1
 
            if('2198') in line:
                if(isFloat==0): 
                    if(parNum < 20000 and written2==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, 1.0)
                        s += '%s %.1f\n' % (parNum+10000, -1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, 1.0)
                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, 1.0)
#                        s += '%s %.1f\n' % (parNum+1, -1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum+10000, 1.0)
#                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
#                        s += '\n'
                        written2 = 1
                    elif(parNum > 20000 and written2==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, -1.0)
                        s += '%s %.1f\n' % (parNum-10000, 1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, -1.0)
                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, -1.0)
#                        s += '%s %.1f\n' % (parNum+1, 1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum-10000, -1.0)
#                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
#                        s += '\n'
                        written2 = 1

            if('1298') in line:
                if(isFloat==0): 
                    if(parNum < 20000 and written3==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, 1.0)
                        s += '%s %.1f\n' % (parNum+10000, -1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, 1.0)
                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, 1.0)
#                        s += '%s %.1f\n' % (parNum+1, -1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum+10000, 1.0)
#                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
#                        s += '\n'
                        written3 = 1
                    elif(parNum > 20000 and written3==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, -1.0)
                        s += '%s %.1f\n' % (parNum-10000, 1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, -1.0)
                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, -1.0)
#                        s += '%s %.1f\n' % (parNum+1, 1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum-10000, -1.0)
#                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
                        written3 = 1

            if('2298') in line:
                if(isFloat==0): 
                    if(parNum < 20000 and written4==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, 1.0)
                        s += '%s %.1f\n' % (parNum+10000, -1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, 1.0)
                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
                        s += '\n'
#                       s += 'Constraint 0.\n'    
#                       s += '%s %.1f\n' % (parNum, 1.0)
#                       s += '%s %.1f\n' % (parNum+1, -1.0)
#                       s += 'Constraint 0.\n'    
#                       s += '%s %.1f\n' % (parNum+10000, 1.0)
#                       s += '%s %.1f\n' % (parNum+1+10000, -1.0)
#                       s += '\n'
                        written4 = 1
                    elif(parNum > 20000 and written4==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, -1.0)
                        s += '%s %.1f\n' % (parNum-10000, 1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, -1.0)
                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, -1.0)
#                        s += '%s %.1f\n' % (parNum+1, 1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum-10000, -1.0)
#                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
#                        s += '\n'
                        written4 = 1

            if('1398') in line:
                if(isFloat==0): 
                    if(parNum < 20000 and written5==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, 1.0)
                        s += '%s %.1f\n' % (parNum+10000, -1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, 1.0)
                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, 1.0)
#                        s += '%s %.1f\n' % (parNum+1, -1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum+10000, 1.0)
#                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
#                        s += '\n'
                        written5 = 1
                    elif(parNum > 20000 and written5==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, -1.0)
                        s += '%s %.1f\n' % (parNum-10000, 1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, -1.0)
                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, -1.0)
#                        s += '%s %.1f\n' % (parNum+1, 1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum-10000, -1.0)
#                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
#                        s += '\n'
                        written5 = 1

            if('2398') in line:
                if(isFloat==0): 
                    if(parNum < 20000 and written6==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, 1.0)
                        s += '%s %.1f\n' % (parNum+10000, -1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, 1.0)
                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, 1.0)
#                        s += '%s %.1f\n' % (parNum+1, -1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum+10000, 1.0)
#                        s += '%s %.1f\n' % (parNum+1+10000, -1.0)
#                        s += '\n'
                        written6 = 1
                    elif(parNum > 20000 and written6==0):
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum, -1.0)
                        s += '%s %.1f\n' % (parNum-10000, 1.0)
                        s += 'Constraint 0.\n'    
                        s += '%s %.1f\n' % (parNum+1, -1.0)
                        s += '%s %.1f\n' % (parNum+1-10000, -1.0)
                        s += '\n'
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum, -1.0)
#                        s += '%s %.1f\n' % (parNum+1, 1.0)
#                        s += 'Constraint 0.\n'    
#                        s += '%s %.1f\n' % (parNum-10000, -1.0)
#                        s += '%s %.1f\n' % (parNum+1-10000, 1.0)
#                        s += '\n'
                        written6 = 1

    return s

