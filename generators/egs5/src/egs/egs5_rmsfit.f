!-----------------------------egs5_rmsfit.f----------------------------
! Version: 060314-0855
! Read Coefficients for fit of GS distribution
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rmsfit

      implicit none

      include 'include/egs5_h.f'               ! Main EGS5 "header" file

      include 'include/egs5_media.f'     ! COMMONs required by EGS5 code
      include 'include/egs5_ms.f'
      include 'include/egs5_mscon.f'
      include 'include/egs5_misc.f'
      include 'include/egs5_usersc.f'
      include 'include/egs5_thresh.f'
      
      real*8                                           ! Local variables
     * dummy
      integer i,j,k,ib,il,iprt,ik1,idummy, nfmeds, hasGS
      integer nm, lmdl, lmdn, lok(MXMED)
      character buffer(72),mdlabl(8)
      logical openMS, useGS, doGS, GSFile
      real*8
     * charD0, efrch0, efrcl0, ue0, ae0

      data
     * mdlabl/' ','M','E','D','I','U','M','='/,
     * lmdl/8/,
     * lmdn/24/

! check to see if we have the data we need
      
      openMS = .false.
      useGS = .false.
      do k=1,nmed
        if(charD(k).eq.0.d0) then
          openMS = .true.
        endif
        if(useGSD(k).eq.0) then
          nmsgrd(k) = 1
          msgrid(1,k) = 0.d0
        else
          useGS = .true.
        endif
      end do

      if(.not.(useGS .or. openMS)) then
        return
      endif

      if(useGS) then
        do k=1,nmed
          if(useGSD(k).eq.0) then
            write(6,1001) k
            useGSD(k) = 1
          endif
        end do
      endif

!  Read the header about the materials in gsdist file:
!  If there's no data there, then read what's in the
!  msmatdat file, which was written earlier on this run.

      GSFile = .false.
      doGS=.false.
      open(UNIT=17,FILE='gsdist.dat',STATUS='old',ERR=14)
      GSFile = .true.
      go to 15
      !  all this is for corrupt gsdist.dat files
13    close(17)
14    open(UNIT=17,FILE='pgs5job.msfit',STATUS='old')

!  Make sure we have GS data is we're looking for that.
!  If we have the data, inform the user of what the
!  old constants were -- user inputs will be ignored.
!  If not a GS run, make sure that we have data in 
!  k1init arrays, i.e., the pegs file was compiled
!  without charD also.

15    read(17,*,end=13) nfmeds
      do 18 j = 1, nfmeds
        read(17,'(72a1)',end=1000) buffer
        read(17,*) hasGS, charD0, efrch0, efrcl0, ue0, ae0
        do 16 k=1,nmed
          do ib=1,lmdn
            il = lmdl + ib
            if (buffer(il) .ne. media(ib,k)) go to 16
            if (ib .eq. lmdn) go to 17
          end do
16      continue
        go to 18
        !  here if we are using this material
17      if(useGS .and. hasGS.eq.0) then
          write(6,1002) k
          doGS = .true.
        endif

        if(charD(k).eq.0.d0) then
          if(charD0.ne.0.d0) then
            if(useGS) then
              write(6,1003) k, charD0
            else
              write(6,1004) k
              stop
            endif
          else
            write(6,1005) k, efrch0, efrcl0
          endif
        endif

        !  make sure previous data was for less than or equal ue/ae
        if(useGS .and. (ue0.lt.ue(j) .or. ae0.gt.ae(j))) then
          write(6,1006) k, ae0, ue0, ae(j), ue(j)
          doGS = .true.
        endif

18    continue

1001  format(' WARNING in RMSFIT:  forcing use of GS Dist in media ',i3)
1002  format(' WARNING in RMSFIT: requested GS Multiple Scattering in ',
     * 'media ',i3,/,' but no data in gsdist.dat file.  Calling ',
     * 'elastino')
1003  format(' WARNING in RMSFIT:  using GS Multiple Scattering in ',
     * 'media ',i3,/,' but no characteristic dimension specified. ',
     * /,' Using old data from gsdist.dat with charD = ',1pe13.4)
1004  format(' ERROR in RMSFIT:  no characteristic dimension given for',
     * ' media ',i3,/,' and no data in pgs5job.pegs5dat file.  Please '
     * ,'re-run with call',/,' to PEGS or with charD.  Aborting.')
1005  format(' WARNING in RMSFIT: no characteristic dimension input for'
     * ,' media ',i3,/,' Using old data from gsdist.dat with: ',/,
     * ' efrach, efrachl = ',2(2x,1pe13.4))
1006  format(' WARNING in RMSFIT:  requested GS Multiple Scattering in',
     * ' media ',i3,/,' but data in gsdist.dat file does not ',
     * 'match data in pgs5job.pegs5dat:',/,
     * ' gsdist.dat:     AE, UE = ', 2(2x,1pe13.4),/,
     * ' pegs5dat:  AE, UE = ', 2(2x,1pe13.4),/,' Calling elastino.')
1007  format(' ERROR in RMSFIT:  requested GS Multiple Scattering in ',
     * 'media ',i3,/,' Can not use GS in conjunction with density ',
     * 'scaling requested ',/,' for region ',i8,' Aborting.')
1008  format(' WARNING in RMSFIT:  using GS Multiple Scattering in ',
     * 'media ',i3,/,' User requested K1 scaling will be ignored.')
1009  format(' WARNING in RMSFIT: requested GS Multiple Scattering',/,
     * ' but wrong charD data in gsdist.dat file.  Calling elastino')

!  return if not using GS distribution

      if(.not.useGS) then
        close(17)
        return
      endif

!  do more traps for bad user input data for GS dist case:

      do j = 1, nreg
        k = med(j)
        if(k.ne.0) then
          if( rhom(k).ne.rhor(j)) then
            write(6,1007) j,k
            stop
          end if
        end if
      end do

      do j = 1, nreg
        if( (k1Hscl(j) + k1Lscl(j)) .gt. 0.d0) then
          k1Hscl(j) = 0.d0
          k1Lscl(j) = 0.d0
          write(6,1008) j
        endif
      end do

      ! instead of checking charD, we can check k1max and k1min
      if(GSFile .and. .not.doGS) then
        read(17,'(72a1)',end=1000) buffer
        read(17,'(72a1)',end=1000) buffer
        do i = 1, NK1
          read(17,*) idummy, k1grd(1,i), k1grd(2,i)
        end do
        if(abs(k1mine-k1grd(1,1))/k1mine .gt. 1.d-6 .or. 
     *       abs(k1minp-k1grd(2,1))/k1minp .gt. 1.d-6 .or.
     *         abs(k1maxe-k1grd(1,NK1))/k1maxe .gt. 1.d-6 .or.
     *           abs(k1maxp-k1grd(2,NK1))/k1maxp .gt. 1.d-6) then
          write(6,1009)
          doGS = .true.
        else
          go to 30
        end if
      end if

      ! if we need to run elastino, we'll be writing the gsdist.dat
      ! file, so close whichever file we're reading on 17, open
      ! gsdist.dat, and run elastino. Then close the file and open it
      ! again for reading.  Finally, skip to the right spot
      if(doGS) then

        close(17)
        open(UNIT=17,FILE='gsdist.dat',STATUS='unknown')
        call elastino
        close(17)
        open(UNIT=17,FILE='gsdist.dat',STATUS='old')
        do j = 1, nfmeds*2 + 1
          read(17,'(72a1)',end=1000) buffer
        end do
      end if

!  read the k1 ladder for this problem

      read(17,'(72a1)',end=1000) buffer
      read(17,'(72a1)',end=1000) buffer
      do i = 1, NK1
        read(17,*) idummy, k1grd(1,i), k1grd(2,i)
      end do

 30   dk1log(1) = dlog(k1grd(1,2)/k1grd(1,1))
      dk1log(2) = dlog(k1grd(2,2)/k1grd(2,1))

!  look for the correct material in the file for each media,
!  just as HATCH does.  Read through the entire data file
!  looking for MEDIUM= strings.  When you find one, see if
!  that's a medium that's being used.  If so, match it up
!  with the list of media for the problem, and read in the
!  medium data.  If not, keep searching for the next MEDIUM=.
!
      nm = 0
      do k=1,nmed
        lok(k) = 0
      end do

5     continue
      read(17,'(72a1)',end=1000) buffer
      !--> look for the MEDIUM= string
      do ib=1,lmdl
        if (buffer(ib) .ne. mdlabl(ib)) go to 5
      end do
      !--> Match medium with the problem media
      do 6 k=1,nmed
        do ib=1,lmdn
          il = lmdl + ib
          if (buffer(il) .ne. media(ib,k)) go to 6
          if (ib .eq. lmdn) go to 7
        end do
 6    continue
      go to 5

 7    continue
      if (lok(k) .ne. 0) go to 5
      lok(k) = 1
      nm = nm + 1

      read(17,'(72a1)') buffer
      read(17,'(72a1)') buffer
      read(17,*) nmsgrd(k)
      read(17,'(72a1)') buffer
      read(17,*) initde(k)
      read(17,'(72a1)') buffer
      read(17,*) nmsdec(k)
      read(17,'(72a1)') buffer
      read(17,*) jskip(k)
      read(17,'(72a1)') buffer
      read(17,*) neqp(k)
      read(17,'(72a1)') buffer
      read(17,*) neqa(k)
!  decrement jskip to save in "bin = N * xi - jskip + 1" when we sample
      jskip(k) = jskip(k) - 1
      do iprt = 1,2
        read(17,'(72a1)') buffer
!  read in the electrons...
        do i = 1,nmsgrd(k)
          read(17,*) msgrid(i,k)
          do ik1 = 1,NK1
            read(17,'(72a1)') buffer
            read(17,*) pnoscat(iprt,i,ik1,k)
            read(17,'(72a1)') buffer
            eamu(1,iprt,i,ik1,k) = 0.0
!  note:  the '-1' is because the last angle in the first part
!  of the distribution is the same as the first angle in the second
            do j = 1, neqp(k) + neqa(k) - 1
              read(17,*) dummy, eamu(j+1,iprt,i,ik1,k), 
     &                    ebms(j,iprt,i,ik1,k), 
     &                    eetams(j,iprt,i,ik1,k)
              !--> do this now to save later
!              ebms(j,iprt,i,ik1,k) = ebms(j,iprt,i,ik1,k) * .25d0
            end do
!  read the cum pdf for the equally spaced angles
            read(17,'(72a1)') buffer
            do j = 1, neqa(k) + 1
              read(17,*) ecdf(j,iprt,i,ik1,k)
            end do
!  round-off fix
            ecdf(neqa(k)+1,iprt,i,ik1,k) = 1.d0
          end do
        end do
      end do

!  go back for more media or quit
      if (nm .ge. nmed) go to 9
      go to 5

 9    continue
      if (nmed .eq. 1) then
        write(6,5001)
      else
        write(6,5002) nmed
      end if
      close(17)
      return

!  media not found error:

1000  write(6,5003)
      write(6,5004)
      do k=1,nmed
        if (lok(k) .ne. 1) write(6,'(24a1)') (media(i,k),i=1,lmdn)
      end do
      close(17)
      stop

5001  format('Read GS Mult scat params for 1 medium')
5002  format('Read GS Mult scat params for ',i2,' media')
5003  format('End-of-file on gsdist.dat')
5004  format('The following media were not located:')

      end

!-----------------------last line of egs5_rmsfit.f----------------------
