!-----------------------------------------------------------------------
!                       SUBROUTINE GSCOEF
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE GSCOEF(NLEG)
C
C     This subroutine computes the Goudsmit-Saunderson transport coef-
C  ficients from the elastic DCS. It must be linked to an external
C  function named DCSEL(RMU), RMU=(1-C0S(THETA))/2, which gives the DCS
C  per unit solid angle as a function of RMU.
C
C  NLEG is the number of terms included in the GS series
C
      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_coefgs.f'

      integer nleg
      double precision DCSEL

      integer nxc, nlm, j,i,l
      double precision dcsl, dcsu, xi, funxi

      double precision X(NGT),W(NGT)
      double precision PL(NGT),XC(100),F0(100),F1(100)
C
      NLM=MAX(500,MIN(NLEG/2,NGT/2))
      CALL GAULEG(X,W,NLM)
C
      J=1
      XC(J)=0.0D0
      DCSL=1.0D-1*DCSI(1)
      DCSU=1.0D+1*DCSI(1)
      DO I=2,NREDA
        IF((DCSL.GT.DCSI(I)).OR.(DCSU.LT.DCSI(I))
     1    .OR.(XMU(I)-XC(J).GT.0.1D0)) THEN
          J=J+1
          XC(J)=XMU(I)
          DCSL=1.0D-1*DCSI(I)
          DCSU=1.0D+1*DCSI(I)
        ENDIF
      ENDDO
C
      IF (XC(J).NE.1.0D0) THEN
        J=J+1
        XC(J)=1.0D0
      ENDIF
      NXC=J
C
      DO J=2,NXC
        F0(J-1)=(XC(J)+XC(J-1))/2.0D0
        F1(J-1)=(XC(J)-XC(J-1))/2.0D0
      ENDDO
C
      NLEGEN=NLEG
      IF(NLEG.GT.NGT) NLEGEN=NGT
      DO L=1,NLEGEN
        GL(L)=0.0D0
      ENDDO
C
C  ****  Gauss-Legendre integration of the GS transport integrals.
C
      DO I=1,NLM
        DO J=1,NXC-1
          XI=F0(J)+X(I)*F1(J)
          FUNXI=F1(J)*W(I)*DCSEL(XI)
          CALL LEGENP(1.0D0-2.0D0*XI,PL,NLEGEN)
          DO L=1,NLEGEN
            IF(L.EQ.1) THEN
              GL(L)=GL(L)+FUNXI
            ELSE
              GL(L)=GL(L)+(1.0D0-PL(L))*FUNXI
            ENDIF
          ENDDO
        ENDDO
      ENDDO
C
      CS0=GL(1)
      GL(1)=0.0D0
      DO L=2,NLEGEN
        GL(L)=GL(L)/CS0
      ENDDO
      CS0=2.0D0*CS0
      CS=CS0*TWOPI
      TCS1=CS*GL(2)
      TCS2=CS*GL(3)

      RETURN
      END
