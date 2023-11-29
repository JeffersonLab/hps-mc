ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      SUBROUTINE COUP1()

      IMPLICIT NONE
      INCLUDE 'model_functions.inc'

      DOUBLE PRECISION PI, ZERO
      PARAMETER  (PI=3.141592653589793D0)
      PARAMETER  (ZERO=0D0)
      INCLUDE 'input.inc'
      INCLUDE 'coupl.inc'
      GC_1 = 2.000000D+00*MDL_DOVERFPID*MDL_COMPLEXI
      GC_14 = -(MDL_COMPLEXI*MDL_GAN)
      GC_16 = -(MDL_COMPLEXI*MDL_GE)
      GC_4 = -(MDL_EE*MDL_COMPLEXI)
      END
