!-----------------------------------------------------------------------
!                       SUBROUTINE ELINIT
!  Version: 060317-1425
!  Reference:  Based on code developed and provided by F. Salvat
!              for computing GS Mult Scat with PW cross sections
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      SUBROUTINE ELINIT(Z,STF,NELEM)
C
C  This subroutine reads atomic elastic cross sections for electrons and
C  positrons from the database files and determines the molecular cross
C  section as the incoherent sum of atomic cross sections.

C  Input arguments:
C    Z (1:NELEM) ..... atomic numbers of the elements in the compound.
C    STF (1:NELEM) ... stoichiometric indices.
C    NELEM ........... number of different elements.
C
      implicit none

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_ms.f'
      include 'include/egs5_uphiot.f'

      include 'pegscommons/scpspl.f'
      include 'pegscommons/mscom.f'
      include 'pegscommons/dercon.f'

      integer nelem, IZ(MXEPERMED)
      double precision STF(NELEM), Z(NELEM), zcor(20)
C
      integer i,ie,ia, iel, ielec, izz, izr, ns, ns1,ns2,ns3
      double precision zplus1, stff, enr, csin, csin1, csin2

      CHARACTER*1 LIT10(10),LIT1,LIT2,LIT3
      DATA LIT10/'0','1','2','3','4','5','6','7','8','9'/
      CHARACTER*24 FILE1,FILE2
C
      double precision EGRID(16)
      DATA EGRID/1.0D0,1.25D0,1.50D0,1.75D0,2.00D0,2.50D0,3.00D0,
     1 3.50D0,4.00D0,4.50D0,5.00D0,6.00D0,7.00D0,8.00D0,9.00D0,
     2 1.00D1/

      integer igrid
      double precision fgrid, e

      double precision g1e
      logical printDiag

      printDiag = .false.

C
      if(et(1).ne.1.d2) then
      IE=0
      IGRID=0
      FGRID=100.0D0
   10 IGRID=IGRID+1
      E=EGRID(IGRID)*FGRID
      IF(IGRID.EQ.16) THEN
        IGRID=1
        FGRID=10.0D0*FGRID
      ENDIF
      IE=IE+1
      ET(IE)=E
C     WRITE(6,'(I5,1P,3E15.7)') IE,ET(IE)
      IF(IE.LT.NEGRID) GO TO 10
      endif

!  first move the atomic numbers into an integer array
!  also compute (Z+1)/Z, so we can impose Z(Z+1) on
!  the cross sections.

      do i=1,nelem
        iz(i) = z(i)
        zcor(i) =  (z(i) + fudgeMS) / z(i)
      end do
C
C  initialize...
C
      DO IE=1,NEGRID
        ECS(IE)=0.0D0
        ETCS1(IE)=0.0D0
        ETCS2(IE)=0.0D0
        PCS(IE)=0.0D0
        PTCS1(IE)=0.0D0
        PTCS2(IE)=0.0D0
        DO IA=1,NREDA
          EDCS(IE,IA)=0.0D0
          PDCS(IE,IA)=0.0D0
        ENDDO
      ENDDO
C
C  ****  Read atomic DCS tables and compute the molecular DCS as the
C        incoherent sum of atomic DCSs.
C
      DO IEL=1,NELEM
        IZZ=IZ(IEL)
        STFF=STF(IEL)
        zplus1 = zcor(iel)
        NS=IZ(IEL)
        IF(NS.GT.999) NS=999
        NS1=NS-10*(NS/10)
        NS=(NS-NS1)/10
        NS2=NS-10*(NS/10)
        NS=(NS-NS2)/10
        NS3=NS-10*(NS/10)
        LIT1=LIT10(NS1+1)
        LIT2=LIT10(NS2+1)
        LIT3=LIT10(NS3+1)
C
        FILE1='data/dcslib/eeldx'//LIT3//LIT2//LIT1//'.tab'
        OPEN(UNIT=31,FILE=FILE1,STATUS='old')
        FILE2='data/dcslib/peldx'//LIT3//LIT2//LIT1//'.tab'
        OPEN(UNIT=32,FILE=FILE2,STATUS='old')
C
        DO IE=1,NEGRID
          READ(31,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,csin,csin1,csin2
          ECS(IE) = ECS(IE) + zplus1 * stff * csin
          ETCS1(IE) = ETCS1(IE) + zplus1 * stff * csin1
          ETCS2(IE) = ETCS2(IE) + zplus1 * stff * csin2
          if(printDiag) WRITE(6,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,ECS(IE),ETCS1(IE),ETCS2(IE)
          IF(IELEC.NE.-1.OR.IZR.NE.IZZ.OR.ABS(ENR-ET(IE)).GT.1.0D-3)
     1      STOP 'Corrupted data file.'
          READ(31,'(1P,10E12.5)') (DCSI(IA),IA=1,NREDA)
          DO IA=1,NREDA
            EDCS(IE,IA)=EDCS(IE,IA)+zplus1*STFF*DCSI(IA)
          ENDDO
C
          READ(32,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,csin,csin1,csin2
          PCS(IE) = PCS(IE) + zplus1 * stff * csin
          PTCS1(IE) = PTCS1(IE) + zplus1 * stff * csin1
          PTCS2(IE) = PTCS2(IE) + zplus1 * stff * csin2
          if(printDiag) WRITE(6,'(I3,I4,1P,E10.3,5E12.5)')
     1      IELEC,IZR,ENR,PCS(IE),PTCS1(IE),PTCS2(IE)
          IF(IELEC.NE.+1.OR.IZR.NE.IZZ.OR.ABS(ENR-ET(IE)).GT.1.0D-3)
     1      STOP 'Corrupted data file.'
          READ(32,'(1P,10E12.5)') (DCSI(IA),IA=1,NREDA)
          DO IA=1,NREDA
            PDCS(IE,IA)=PDCS(IE,IA)+zplus1*STFF*DCSI(IA)
          ENDDO
        ENDDO
C
        CLOSE(31)
        CLOSE(32)
      ENDDO

!  get spline coefficients for the scattering power.  because of the
!  switch to a different cross section above 100 MeV, we fill in
!  Wentzel points and then spline

      do ie = 1,negrid-1
        etl(ie) = dlog(et(ie))
        etcs1(ie) = dlog(etcs1(ie))
        ptcs1(ie) = dlog(ptcs1(ie))
      end do
      etcs1(negrid) = dlog(g1e(-1,110.d0+RM,-1))
      ptcs1(negrid) = dlog(g1e(+1,110.d0+RM,-1))
      etcs1(negrid+1) = dlog(g1e(-1,120.d0+RM,-1))
      ptcs1(negrid+1) = dlog(g1e(+1,120.d0+RM,-1))
      etcs1(negrid+2) = dlog(g1e(-1,130.d0+RM,-1))
      ptcs1(negrid+2) = dlog(g1e(+1,130.d0+RM,-1))
      etl(negrid) = dlog(110.d6)
      etl(negrid+1) = dlog(120.d6)
      etl(negrid+2) = dlog(130.d6)
      
      call spline(etl,etcs1,ag1e,bg1e,cg1e,dg1e,0.d0,0.d0,negrds)
      call spline(etl,ptcs1,ag1p,bg1p,cg1p,dg1p,0.d0,0.d0,negrds)

      RETURN
      END
