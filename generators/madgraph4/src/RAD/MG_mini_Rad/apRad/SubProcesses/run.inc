c**************************************************************
c    run.inc 
c**************************************************************
c
c     Scales
c
      real*8          scale,scalefact,alpsfact
      logical         fixed_ren_scale,fixed_fac_scale,hmult,fixed_couplings
      integer         ickkw
      common/to_scale/scale,scalefact,alpsfact,fixed_ren_scale,fixed_fac_scale,
     $                fixed_couplings,ickkw,hmult
c
c     Collider
c
      integer                                        lpp(2)
      double precision    ebeam(2), xbk(2),q2fact(2)
c...NATALIA ADDED THESE
      double precision mbeam(2)
      common/to_collider/ ebeam   , xbk   ,q2fact,   lpp, mbeam
c
c     BW treatment
c      
      double precision bwcutoff
      common/to_bwcutoff/ bwcutoff
