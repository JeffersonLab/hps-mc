ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      written by the UFO converter
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      DOUBLE PRECISION G
      COMMON/STRONG/ G

      DOUBLE COMPLEX GAL(2)
      COMMON/WEAK/ GAL

      DOUBLE PRECISION MU_R
      COMMON/RSCALE/ MU_R

      DOUBLE PRECISION NF
      PARAMETER(NF=0D0)
      DOUBLE PRECISION NL
      PARAMETER(NL=0D0)

      DOUBLE PRECISION MDL_MAP,MDL_MB,MDL_MC,MDL_MCHI,MDL_MD,MDL_MH
     $ ,MDL_MM,MDL_MNUL,MDL_MS,MDL_MT,MDL_MTA,MDL_MU,MDL_MW,MDL_MZ
     $ ,MDL_ME

      COMMON/MASSES/ MDL_MAP,MDL_MB,MDL_MC,MDL_MCHI,MDL_MD,MDL_MH
     $ ,MDL_MM,MDL_MNUL,MDL_MS,MDL_MT,MDL_MTA,MDL_MU,MDL_MW,MDL_MZ
     $ ,MDL_ME


      DOUBLE PRECISION MDL_WAP,MDL_WH,MDL_WT,MDL_WW,MDL_WZ

      COMMON/WIDTHS/ MDL_WAP,MDL_WH,MDL_WT,MDL_WW,MDL_WZ


      DOUBLE COMPLEX GC_3, GC_12, GC_13, GC_15

      COMMON/COUPLINGS/ GC_3, GC_12, GC_13, GC_15

