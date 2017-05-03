C##########################################################
C#    Common block with the information needed            #
C#    for the Quarkonium state specifications             #
C##########################################################
C#                                                        #
C##########################################################
C# Variables containing the angular momentum information  #
C# The mg input follows the spectroscopic notation        #
C#  [(2S+1)LJC]  with                                     #
C#                                                        #
C#   2S+1 = 0,1                                           #
C#    L   = S,P                                           #
C#    J   = 0,1,2                                         #
C#    C   = 1,8                                           #
C##########################################################
C# Arrays containing the information on the color         #
C##########################################################
C# Examples:                                              #
C#                                                        #
C#       psinglet(1,0) = 1   !requesting 1 singlet        #
C#       psinglet(1,1) = 4   !Quark (c) is line 4         #
C#       psinglet(2,1) = 5   !AntiQuark (c~) is line 5    #
C#                                                        #
C#       poctet(1,0) = 1   !requesting 1 octet            #
C#       poctet(1,1) = 4   !Quark (c) is line 4           #
C#       poctet(2,1) = 5   !AntiQuark (c~) is line 5      #
C#                                                        #
C##########################################################

      integer psinglet(2,0:maxlines), poctet(2,0:maxlines)
      common/to_colorprojections/psinglet,poctet

C##########################################################
C#                                                        #
C#  Description:                                          #
C#                                                        #
C#       S_qn = Spin of the state = 0,1                   #
C#       Input is given in terms of Stot=2*S_qn +1        #
C#                                                        #
C#       L_qn = Orbital momentum  = 0,1                   #
C#       Input is given in terms of S (0) or P (1)        #
C#                                                        #
C#       J_qn = Total   momentum  = 0,1,2                 #
C#       Input is given in terms of J=0,1,2               #
C#                                                        #
C#       C_qn = color             = 1,8                   #
C#       Input is given in terms of C=1,8                 #
C#                                                        #
C##########################################################

      integer L_qn,S_qn,J_qn,C_qn
      logical onium,em_decay
      character*8 onium_ID
      common /to_onium/L_qn,S_qn,J_qn,C_qn,onium,em_decay,onium_ID







