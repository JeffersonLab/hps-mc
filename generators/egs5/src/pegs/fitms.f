!----------------------------------fitms.f------------------------------
! Version: 060313-1235
! Reference: Based on code developed by BLIF and F. Salvat to compute
!            fit to GS MS distribution
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      subroutine fitms(x,p,cdf,na, ipart,eindex,ik1)

!  input required:
!    x(NA)...     reduced scattering angle (1-cos(thet))/2.
!    p(NA)...     distribution function
!    cdf(NA)...   cumulative distribution function
!    ipart...     positron or electron
!    eindex...    current energy index
!    ik1...       current scattering strength index
!
!  output (through common MSCON):
!
!    cumdist(nextra)... cumulative dist over the final eq angle intervals
!    amums(nfit1)... end points (in units of reduced cosine) of intervals
!    ams(nfit)...   first coefficient in expansions
!    bms(nfit)...   second coefficient in expansions
!    cms(nfit)...   third coefficient in expansions
!    etams(nfit)... 'screening parameter' in expansions

      implicit none
      
      include 'include/egs5_h.f'
      include 'include/egs5_mscon.f'

      integer ipart, eindex, na, ik1, jint
      double precision x(na), p(na), cdf(na)

      integer NB
      parameter (NB=NFIT+1)

      integer mesh, istart, iend, interval, i
      double precision fit(NB), r, a, b, c, n
      double precision sum, diff, biggest
      
      double precision p1, p2, x1, x2, n1, n2, ln21, alpha
      double precision F, G

      logical printDiag

      printDiag = .false.
      
      if(na.ne.NB) then
        write (6,*) 'Error:  number of MS dist points != NB.  Stop.'
        stop
      endif

      mesh = NB-1
         
      sum = 0
      biggest = 0

      do interval = 1, mesh

        istart = interval
        iend = interval + 1
        p1 = p(istart)
        p2 = p(iend)
        x1 = x(istart)
        x2 = x(iend)

        if(p2 .ne. p1) then
          r = sqrt(p2/p1)
          n = (r*x2 - x1)/(1 - r)
          a = p1*(x1 + n)**2
        else
          r = 1
          n = 1e10
          a = p1*n**2
        endif

        n1 = x1 + n
        n2 = x2 + n
        ln21 = log(n2/n1)
                      
        F = 1/n1 - 1/n2
        G = (n1 + n2)*ln21 - 2*(n2 - n1)

C....Determine the fitting coefficients
        alpha = cdf(iend) - cdf(istart) - a*F
            
C....Ignoring the c term for now
        b = alpha/G
        c = 0
            
        if (p2 .ne. p1) then
          b = b/a
          c = c/a
        else
          b = 0
          c = 0
        endif
            
!  test accuracy

        if(printDiag) then
        do i = istart, iend
          fit(i) = (a/(x(i) + n)**2)*
     &             (  1 + 
     &                b* (x(i) - x1)*(x2 - x(i)) + 
     &                c*((x(i) - x1)*(x2 - x(i)))**2
     &             )

          diff = abs((fit(i) - p(i))/p(i))
          biggest = max(biggest,diff)
          sum = sum + diff
          write(26,*) x(i), (fit(i) - p(i))/p(i)
        enddo
        write(26,'(''  Biggest relative difference = '',g14.7)') biggest
        write(26,'(''  Goodness of fit = '',g14.7)') sum/NA
        endif
            
        ams(ipart,eindex,ik1,interval) = a
        bms(ipart,eindex,ik1,interval) = b
        cms(ipart,eindex,ik1,interval) = c
        etams(ipart,eindex,ik1,interval) = n
        amums(ipart,eindex,ik1,interval) = x1

!  for the equally spaced intervals, we need a re-normalized
!  cdf over just the last bin.  Total CDF should be 1/NBFIT,
!  and the cdf at x(NBFIT-1) should be (NBFIT-1)/NBFIT, but...
!  because of the no-scattering probability, there is a 
!  discrepency, so we need to use the full expression, and we
!  assume the cdf over the first bin is the correct constant

        if(interval.ge.NBFIT) then
          jint = interval - NBFIT + 1
          cumdist(ipart,eindex,ik1,jint) =
     +                   (cdf(iend) - cdf(NBFIT)) / cdf(2)
        endif
      enddo

      cumdist(ipart,eindex,ik1,NEXFIT) = 1.d0
      amums(ipart,eindex,ik1,mesh+1) = x2

      return
      end
!-------------------------last line of fitms.f--------------------------
