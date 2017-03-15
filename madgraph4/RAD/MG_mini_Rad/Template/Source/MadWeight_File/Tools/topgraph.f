      subroutine graph_init
      implicit none
c
c    local
c
      character*40 t1, t2, t3, t4
c
c     gobal
c
      double precision GeVbin, etabin, xbin
      common/to_bin/ GeVbin, etabin, xbin
c
c
      call inihist
c
      GeVbin=20d0
      etabin=0.1d0
c
      t1='final state invariant mass '

c
       CALL MBOOK(1,t1,GeVbin,300d0,1300d0)

      return
      end


      subroutine FILL_plot(weight)
      implicit none
      include '../../nexternal.inc'
      include '../../genps.inc'
c
c     arguments
c
      double precision weight
c
c     local
c
      double precision Ptot(0:3), minv
      integer i,nu
c
c     common
c
      double precision momenta(0:3,-max_branch:2*nexternal)  ! records the momenta of external/intermediate legs     (MG order)
      double precision mvir2(-max_branch:2*nexternal)                  ! records the sq invariant masses of intermediate particles (MG order)
      common /to_diagram_kin/ momenta, mvir2
c---
c Begin code
c---
 
      do nu=0,3
        Ptot(nu)=0d0
        do i=3,nexternal
          Ptot(nu)=Ptot(nu)+momenta(nu,i)
        enddo 
      enddo 
      minv=dsqrt(Ptot(0)**2-Ptot(1)**2-Ptot(2)**2-Ptot(3)**2)

       CALL MFILL(1,minv,weight)

       return
       end
c


      subroutine final_histo(nb_run,tot)
      include 'dbook.inc'
c
c     arguments
c
      integer nb_run
      double precision tot
c
c     gobal
c
      double precision GeVbin, etabin, xbin
      common/to_bin/ GeVbin, etabin, xbin
c
c     local
c
      integer j
c
      OPEN(UNIT=98,file='plot.dat',STATUS='UNKNOWN')
      OPEN(UNIT=99,file='plot.top',STATUS='UNKNOWN')
c
      CALL MFINAL(1)
      CALL MOPERA(1   ,'F',   1,   1,
     & 1.d0/GeVbin/dble(nb_run),1.)
      HINT(1)=tot
      CALL MPRINT(1)
      CALL MTOP(1,50,'  M_INV  (GeV)     '
     & ,'d P/d M_inv   ','log')

c

       close(98)
       close(99)


       return
       end
