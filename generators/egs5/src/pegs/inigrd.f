!-----------------------------inigrd.f----------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

!  This subroutine initialized the cut-off dependent grids for the 
!  multiple scattering routines.  

      subroutine inigrd(ecut,emax,sflag)

      implicit none

!    Arguments:
!    ecut...     electron cut-off total energy
!    emax...     maximum electron total energy
!    sflag...    call dcsstor or not.

      integer sflag
      double precision ecut, emax

      include 'include/egs5_h.f'
      include 'include/egs5_cdcsep.f'
      include 'include/egs5_mscon.f'
      include 'include/egs5_uphiot.f'
      include 'include/egs5_useful.f'

      include 'pegscommons/mscom.f'
C
      integer i, j, k
      double precision e, edec, ecutev, emaxev

!  get the multiple scattering grid for this particular
!  problem.  using linear-log combination grid.
!  note that their are actually two grids here, because
!  the Barcelona MS distribution runs only to 100 MeV,
!  but the problem methodology extends to 10 TeV.  Therefore,
!  we track scattering power for the full energy range,
!  with the number of energy points given in the variable
!  nmscpw.  The Barcelona MS dist is based on the same
!  energy grid spacing, but is terminated above 100 MeV.  
!  The number of points is given in nmscate.  There is
!  a possibility of confusion in that the same array 
!  holds the energy grid values for both - mscate()

      ecutev = (ecut - RM) * 1.0d6
      emaxev = (emax - RM) * 1.0d6
      decade1 = dint(dlog10(ecutev))
      decade2 = dint(dlog10(emaxev))
!
!   Loop over the energy steps - start at the decade of ecut
!
      nmscate = 0
      nmscpw = 0
      do i = decade1,decade2
        edec = 10.d0**i
        k = 0
        do j = 1,NDEC
          k = k + 1
          e = edec * 10.d0**((j-1.d0)/NDEC)
!
!  find the first energy...
          if(e.ge.ecutev) then
            if(nmscpw.eq.0) then
              e = ecutev
              if(k.eq.1) then
                joffset = NDEC-1
              else
                joffset = k - 2 
              endif
            else
              e = edec * 10.d0**((j-2.d0)/NDEC)
            endif
!
!  find the last energy...
            if(e.gt.emaxev) then
              if(mscate(nmscpw).ge.emaxev) go to 200
              e = emaxev
            endif
!
            nmscpw = nmscpw + 1
            mscate(nmscpw) = e
            if(e .le. 1.d8) nmscate = nmscate + 1
          endif
        end do
      end do
200   continue
C
      if(sflag.eq.1) call dcsstor(ecut,emax)

C  ****  Angular grid (TH in deg, XMU=(1.0D0-COS(TH))/2).
C
C  do this just once.

      if(th(2).eq.1.d-4) return
      I=1
      TH(I)=0.0D0
      THR(I)=TH(I)*PI/180.0D0
      XMU(I)=(1.0D0-COS(THR(I)))/2.0D0
      I=2
      TH(I)=1.0D-4
      THR(I)=TH(I)*PI/180.0D0
      XMU(I)=(1.0D0-COS(THR(I)))/2.0D0
   20 CONTINUE
      I=I+1
      IF(TH(I-1).LT.0.9999D-3) THEN
        TH(I)=TH(I-1)+2.5D-5
      ELSE IF(TH(I-1).LT.0.9999D-2) THEN
        TH(I)=TH(I-1)+2.5D-4
      ELSE IF(TH(I-1).LT.0.9999D-1) THEN
        TH(I)=TH(I-1)+2.5D-3
      ELSE IF(TH(I-1).LT.0.9999D+0) THEN
        TH(I)=TH(I-1)+2.5D-2
      ELSE IF(TH(I-1).LT.0.9999D+1) THEN
        TH(I)=TH(I-1)+1.0D-1
      ELSE IF(TH(I-1).LT.2.4999D+1) THEN
        TH(I)=TH(I-1)+2.5D-1
      ELSE
        TH(I)=TH(I-1)+5.0D-1
      ENDIF
      THR(I)=TH(I)*PI/180.0D0
      XMU(I)=(1.0D0-COS(THR(I)))/2.0D0
      IF(I.LT.NREDA) GO TO 20

      RETURN
      END
!-------------------------last line of inigrd.f-------------------------
