!-----------------------------------------------------------------------
!                       SUBROUTINE GSDIST
!  Version: 060314-0815
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE GSDIST(S,RMU,PDF,NTERM)
C
C  Goudsmit-Saunderson multiple scattering distribution.
C
C  Input arguments:
C    S = path length in units of the elastic mean free path.
!    nterm == if nterm is negative, then the last angle didn't
!    converge so we'll use the dcs for this larger angle.  temp
!    fix until single scattering mode is in place
C    RMU =.5 * (1.0D0-COS(THETA)).
C  Output arguments:
C    NTERMS = number of terms needed to get convergence of the series.
C    PDF = probability distribution function of the final 'direction',
C          RMU, of electrons that have been scattered at least once.
C
      implicit none

      integer nterm
      double precision s, rmu, pdf, DCSEL
C
      include 'include/egs5_h.f'
      include 'include/egs5_coefgs.f'

      logical printDiag
      integer l
      double precision x, dxs, sum, sumf, f
      double precision  PL(NGT)

      printDiag = .false.

      IF(RMU.LT.0.0D0.OR.RMU.GT.1.000001D0) STOP 'GS error.'
      X=1.0D0-2.0D0*RMU
      if(nterm.lt.0) then
        SUM=S*DCSEL(RMU)/CS0
        go to 200
      end if
      CALL LEGENP(X,PL,NLEGEN)
C
C  ****  Legendre series.
C
C  -- DXS is the probability of no scattering, which corresponds to a
C     delta distribution at THETA=0. This unscattered component is
C     subtracted from the GS distribution to speed up convergence.
C
      ! trap to prevent underflow exceptions for thick targets
      if(s.lt.100.d0) then
        DXS=EXP(-S)
      else
        dxs = 0.d0
      endif

      SUM=0.0D0
      SUMF=0.0D0
      NTERM=NLEGEN
      DO L=1,NLEGEN
        ! trap to prevent underflow exceptions for thick targets
        if(s*GL(L).lt.100.d0) then
          F=(L-0.5D0)*(EXP(-S*GL(L))-DXS)
        else
          F=-(L-0.5D0)*DXS
        endif
        SUM=SUM+F*PL(L)
        SUMF=SUMF+F
        IF(ABS(F).LT.1.0D-6*ABS(SUM)) then
          NTERM=MIN(NTERM,L)
          if(l.gt.1) go to 100
        endif
      ENDDO
  100 continue
      IF(ABS(F).GT.1.0D-2*ABS(SUM)) THEN
        if(printDiag) then
        WRITE(26,1000) RMU,SUM,F
 1000   FORMAT(1X,'**  Warning. Low accuracy in GSSUM.',
     1        /' at RMU = ',1P,E12.5,', SUM =',E12.5,
     2        ',  last term =',E12.5)
C  ****  ...a negative value of SUM indicates lack of convergence.
        endif
        if(sum.gt.0d0) SUM=-SUM
      ENDIF
C  >>>>  At large angles, the GS distribution may be in serious error
C  due to truncation errors. When this happens, the absolute value of
C  the PDF is much smaller than at MU=0 and we can set PDF=S*DCS.
C
      IF(sum.lt.0d0 .or. ABS(SUM).LT.1.0D-4*SUMF) then
        SUM=S*DCSEL(RMU)/CS0
        !  temp fix until single scattering
        nterm = -1
      end if
C
 200  PDF=2.0D0*SUM
      RETURN
      END
