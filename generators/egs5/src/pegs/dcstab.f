!-----------------------------------------------------------------------
!                       SUBROUTINE DCSTAB
!  Version: 051219-1435
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE DCSTAB(E,IELEC)
C
C  This subroutine computes a table of the molecular elastic DCS for
C  electrons (IELEC=-1) or positrons (IELEC=+1) with kinetic energy
C  E (eV) by log-log cubic spline interpolation in E.
C
      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_cdcspl.f'
      include 'include/egs5_coefgs.f'

      integer ielec
      double precision e

      integer ia, ie
      double precision el
      double precision X(NEGRID),Y(NEGRID),
     1          A(NEGRID),B(NEGRID),C(NEGRID),D(NEGRID)
C
      DO IE=1,NEGRID
        X(IE)=LOG(ET(IE))
      ENDDO
      EL=LOG(E)
C
      DO IA=1,NREDA
        DO IE=1,NEGRID
          IF(IELEC.EQ.-1) THEN
            Y(IE)=LOG(EDCS(IE,IA))
          ELSE
            Y(IE)=LOG(PDCS(IE,IA))
          ENDIF
        ENDDO
        CALL SPLINE(X,Y,A,B,C,D,0.0D0,0.0D0,NEGRID)
        CALL FINDI(X,EL,NEGRID,IE)
        DCSIL(IA)=A(IE)+EL*(B(IE)+EL*(C(IE)+EL*D(IE)))
        DCSI(IA)=EXP(DCSIL(IA))
      ENDDO
C
      CALL SPLINE(XMU,DCSIL,RA,RB,RC,RD,0.0D0,0.0D0,NREDA)
      RETURN
      END
