!---------------------------------cone.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

! ----------------------------------------------------------------------
! Auxiliary (geometry) subroutine for use with the EGS5 Code System
! ----------------------------------------------------------------------
! This subroutine determines whether or not a right circular cone is 
! intersected by a particle trajectory and, if so, obtains the distance
! to the surface.  This is the KEK version that was modified by
! Hirayama (16 April 1996) to treat a cone having theta larger than 
! 90 degree.
! ----------------------------------------------------------------------
! Input arguments:
! ----------------
!   icon = cone ID number
!   infl = 1 means particle is inside cone
!        = 0 means particle is outside cone
! Output arguments:
! -----------------
!   ihit = 1 means particle intersects surface
!        = 0 means particle misses surface
!   tcon = distance to surface if intersected
!
! ----------------------------------------------------------------------
! Note: Data in the form of the cotangent of the half angle (cotal),
!       its square (cotal2), and the z offset of the cone are required
!       and are passed by common/codata/ (e.g., defined in MAIN).
! ----------------------------------------------------------------------

      subroutine cone(icon,infl,ihit,tcon)

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file
      include 'include/egs5_stack.f'     ! COMMONs required by EGS5 code

      include 'auxcommons/aux_h.f'   ! Auxiliary-code "header" file
      include 'auxcommons/condta.f'        ! Auxiliary-code COMMONs

      real*8 tcon                                            ! Arguments
      integer icon,infl,ihit

      real*8                                           ! Local variables
     * delcon,cpcon,sgncon,znp,wnp,cpcon1,cpcon2,
     * dcon1,dcon2,acon,bcon,bcon1,ccon,tcon1,
     * bprim,ccon1,root,tcon11,tcon22,tcon2
      integer
     * nofcn,itwopr

      data delcon/1.D-8/

      nofcn = infl
      ihit = 0
      itwopr = 0
      cpcon = cotal(icon)
      sgncon = sign(1.D0,cpcon)
      cpcon = cotal2(icon)
      znp = sgncon*(z(np) - smalll(icon))
      wnp = sgncon*w(np)
      cpcon1 = 1.D0 + cpcon
      cpcon2 = sqrt(cpcon1)
      dcon1 = x(np)*x(np) + y(np)*y(np)
      dcon2 = sqrt(dcon1)

      if (znp. ge. 0. .or. wnp .ge. 0.) then
        acon = (u(np)*u(np) + v(np)*v(np))*cpcon - wnp*wnp
        bcon1 = (x(np)*u(np) + y(np)*v(np))*cpcon
        bcon = bcon1 - znp*wnp
        ccon = dcon1*cpcon - znp*znp
        if (acon .eq. 0.) then
          if (bcon .ne. 0.0) then
            if (abs(ccon) .lt. delcon*znp*znp .and. znp .ge. 0.) then
              if (nofcn .eq. 1 .and. bcon .ge. 0. .or. 
     *            nofcn .eq. 0 .and. bcon .le. 0.) then
                tcon1 = -ccon/(2.D0*bcon)
                if (tcon1 .ge. 0.) then
                  if (znp+tcon1*wnp .ge. 0.) then
                    tcon = tcon1
                    ihit = 1
                  end if
                end if
              end if
            end if
          else
            tcon1 = cpcon2*znp*sign(1.D0,-wnp)
            if (tcon1 .ge. 0.) then
              tcon = tcon1
              ihit = 1
            end if
          end if
        else if (abs(ccon) .lt. delcon*znp*znp .and. znp .ge. 0.) then
          bprim = bcon1 - wnp*dcon2
          if (nofcn .eq. 1 .and. bprim .lt. 0.) then
            tcon1 = -2.D0*bcon/acon
            if (tcon1 .ge. 0.) then
              tcon = tcon1
              ihit = 1
            end if
          end if
        else if (nofcn .eq. 1 .and. ccon .gt. 0.) then
          tcon = delcon
          ihit = 1
        else if (nofcn .eq. 0 .and. ccon .lt. 0. .and. znp .ge. 0.) then
          tcon = delcon
          ihit = 1
        else
          ccon1 = bcon*bcon - acon*ccon
          if (ccon1 .ge. 0.) then
            root = sqrt(ccon1)
            if (bcon .gt. 0.) then
              tcon11 = -(bcon + root)/acon
            else
              tcon11 = -ccon/(bcon - root)
            end if
            if (bcon .lt. 0.) then
              tcon22 = -(bcon - root)/acon
            else
              tcon22 = -ccon/(bcon + root)
            end if
            if (tcon11 .ge. 0. .or. tcon22 .ge. 0.) then
              if (tcon11 .lt. 0.) then
                tcon1 = tcon22
              else
                if (tcon22 .lt. 0.) then
                  tcon1 = tcon11
                else
                  itwopr = 1
                  tcon1 = min(tcon11,tcon22)
                  tcon2 = max(tcon11,tcon22)
                end if
              end if
              if (znp+tcon1*wnp .ge. 0.) then
                tcon = tcon1
                ihit = 1
              else if (itwopr .eq. 1 .and. znp+tcon2*wnp .ge. 0.) then
                tcon = tcon2
                ihit = 1
              end if
            end if
          end if
        end if
      end if

      return

      end

!---------------------------last line of cone.f-------------------------
