!-----------------------------rluxinit.f--------------------------------
! Version: 051219-1435
! Reference: RLUXIN, RLUXGO, and RLUXUT of RANLUX, after James,
!            Computer Phys. Commun. 79 (1994) 111-114. 
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxinit
      
      implicit none

      !  inputs, from common block RLUXCOM in randomm.f

      !  integer luxlev  !  desired luxury level, default is 1,
                         !  levels 2 and higher should never be
                         !  needed

      !  integer inseed  !  seed for generating unique sequences,
                         !  in theory, any number between 1 and 2^31
                         !  "every possible value of INS gives rise 
                         !  to a valid, independent sequence which will 
                         !  not overlap any sequence initialized with 
                         !  any other value of INS.". 
 
      !  integer kount   !  number of randoms already used, for
      !  integer mkount  !  restart with rluxgo

      !  integer isdext  !  array of 25 ints for restart with rluxin

      include 'include/randomm.f'

!   LUXURY LEVELS.
!   ------ ------      The available luxury levels are:
!
!  level 0  (p=24): equivalent to the original RCARRY of Marsaglia
!           and Zaman, very long period, but fails many tests.
!  level 1  (p=48): considerable improvement in quality over level 0,
!           now passes the gap test, but still fails spectral test.
!  level 2  (p=97): passes all known tests, but theoretically still
!           defective.
!  level 3  (p=223): Any theoretically possible correlations have
!           very small chance of being observed.
!  level 4  (p=389): highest possible luxury, all 24 bits chaotic.

!  Luxury Level   0    *1*    2    3     4
!  Time factor    1     2     3    6    10

      integer i, tisdext

      !  see if isdext has been set
     
      tisdext = 0
      do i = 1,25
        tisdext = tisdext + isdext(i)
      end do

      if(tisdext.ne.0) then
        call rluxin                    ! restart with 25 seeds
      else
        call rluxgo
      endif
   
      return
      end

!--------------------last line of rluxinit.f----------------------------
!-------------------------------rluxgo.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxgo
      
      implicit none

      include 'include/randomm.f'

      integer jsdflt, itwo24, icons
      parameter (jsdflt=314159265)
      parameter (itwo24=2**24, icons=2147483563)

      real uni
      integer jseed, i,k, iseeds(24), izip, izip2, isk, inner, iouter

!  check luxury level:
      if (luxlev .le. 0 .or. luxlev .gt. maxlev) then
        write (6,'(a,i7)') ' illegal ranlux level in rluxgo: ',luxlev
        stop
      endif

      nskip = ndskip(luxlev)
      write(6,'(a,i2,a,i4)') ' ranlux luxury level set by rluxgo :',
     +        luxlev,'     p=', nskip+24

!  set seeds - any positive seed is valid:
      in24 = 0
      if (inseed .lt. 0) then
        write (6,'(a)') 
     +   ' Illegal initialization in rluxgo, negative input seed'
        stop
      else if (inseed .gt. 0) then
        jseed = inseed
        write(6,'(a,i12)')
     +   ' ranlux initialized by rluxgo from seed', jseed
      else
        jseed = jsdflt
        write(6,'(a)')' ranlux initialized by rluxgo from default seed'
      endif
      inseed = jseed
      twom24 = 1.
      do i= 1, 24
        twom24 = twom24 * 0.5
        k = jseed/53668
        jseed = 40014*(jseed-k*53668) -k*12211
        if (jseed .lt. 0)  jseed = jseed+icons
        iseeds(i) = mod(jseed,itwo24)
      end do

      twom12 = twom24 * 4096.

      do i= 1,24
        seeds(i) = real(iseeds(i))*twom24
        next(i) = i-1
      end do
      next(1) = 24

      i24 = 24
      j24 = 10
      carry = 0.
      if (seeds(24) .eq. 0.) carry = twom24
      
!     If restarting at a break point, skip kount + igiga*mkount
!     Note that this is the number of numbers delivered to
!     the user PLUS the number skipped (if luxury .gt. 0).

      if (kount+mkount .ne. 0)  then
        write(6,'(a,i20,a,i20)')
     +' Restarting ranlux with kount = ',kount,' and mkount = ',mkount
        do iouter= 1, mkount+1
          inner = igiga
          if (iouter .eq. mkount+1)  inner = kount
          do isk= 1, inner
            uni = seeds(j24) - seeds(i24) - carry
            if (uni .lt. 0.)  then
               uni = uni + 1.0
               carry = twom24
            else
               carry = 0.
            endif
            seeds(i24) = uni
            i24 = next(i24)
            j24 = next(j24)
          end do
        end do
!       Get the right value of in24 by direct calculation
        in24 = mod(kount, nskip+24)
        if (mkount .gt. 0)  then
           izip = mod(igiga, nskip+24)
           izip2 = mkount*izip + in24
           in24 = mod(izip2, nskip+24)
        endif
!       Now in24 had better be between zero and 23 inclusive
        if (in24 .gt. 23) then
           write (6,'(a/a,3i11,a,i5)')
     +    '  Error in RESTARTING with RLUXGO:','  The values', inseed,
     +     kount, mkount, ' cannot occur at luxury level', luxlev
           stop
        endif
      endif

      rluxset = .true.

      return
      end

!--------------------last line of rluxgo.f------------------------------
!------------------------------rluxin.f---------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxin
      
      implicit none

      include 'include/randomm.f'

      integer i, isd

      write(6,'(a)') ' full initialization of ranlux with 25 integers:'
      write(6,'(5x,5i12)') isdext

      twom24 = 1.

      do i = 1, 24
        next(i) = i-1
        twom24 = twom24 * 0.5
      end do
      next(1) = 24
      twom12 = twom24 * 4096.

      do i = 1, 24
        seeds(i) = real(isdext(i))*twom24
      end do
      carry = 0.
      if (isdext(25) .lt. 0)  carry = twom24
      isd = iabs(isdext(25))
      i24 = mod(isd,100)
      isd = isd/100
      j24 = mod(isd,100)
      isd = isd/100
      in24 = mod(isd,100)
      isd = isd/100
      luxlev = isd
      if (luxlev .le. maxlev) then
        nskip = ndskip(luxlev)
        write (6,'(a,i2)') 
     +       ' ranlux luxury level set by rluxin to: ', luxlev
      else
        write (6,'(a,i5)') ' ranlux illegal luxury rluxin: ',luxlev
        stop
      endif

      inseed = -1

      rluxset = .true.

      return
      end

!------------------last line of rluxin.f--------------------------------
!------------------------------rluxout.f--------------------------------
! Version: 051219-1435
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine rluxout
      
      implicit none

      include 'include/randomm.f'

      real twop12
      parameter (twop12=4096.)

      integer i

      do i = 1,24
        isdext(i) = int(seeds(i)*twop12*twop12)
      end do

      isdext(25) = i24 + 100*j24 + 10000*in24 + 1000000*luxlev
      if (carry .gt. 0.)  isdext(25) = -isdext(25)

      return
      end

!------------------last line of rluxout.f-------------------------------
