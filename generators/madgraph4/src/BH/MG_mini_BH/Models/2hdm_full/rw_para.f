
      subroutine setpara(param_name,readlha)
      implicit none
c
c     arguments
c     
      character*(*) param_name
      logical readlha
      integer iunit
      logical done

      include 'input.inc'
      include 'coupl.inc'

      integer maxpara
      parameter (maxpara=1000)
c
c     local
c
      integer npara
      character*20 param(maxpara),value(maxpara),ctemp

      if(readlha) then 
      call load_lha_para_m(npara,param,value,param_name)

c************************************************************************
c     EW couplings: **INPUT** values. Output values can be different!   *
c************************************************************************

      call get_real_m   (npara,param,value,"zmass"    ,zmass  ,91.188d0 )
      call get_real_m   (npara,param,value,"alpha"    ,alpha  ,127.93d0 )
      call get_real_m   (npara,param,value,"gfermi"   ,gfermi ,1.16637d-5)
      call get_real_m   (npara,param,value,"alphas"   ,alfas ,0.1172d0 )

      alpha=1d0/alpha
c************************************************************************
c     Z and W widths                                                    *
c************************************************************************

      call get_real_m   (npara,param,value,"zwidth" ,zwidth ,2.44139d0)
      call get_real_m   (npara,param,value,"wwidth" ,wwidth ,2.04759d0)

c************************************************************************
c     Higgs                                                             *
c************************************************************************

      call get_real_m   (npara,param,value,"mh1"  ,MH1  ,120d0    )
      call get_real_m   (npara,param,value,"mh2"  ,MH2  ,120d0    )
      call get_real_m   (npara,param,value,"mh3"  ,MH3  ,120d0    )
      call get_real_m   (npara,param,value,"mhc"  ,MHC  ,120d0    )
      call get_real_m   (npara,param,value,"wh1" ,WH1 ,0.0000d0 )
      call get_real_m   (npara,param,value,"wh2" ,WH2 ,0.0000d0 )
      call get_real_m   (npara,param,value,"wh3" ,WH3 ,0.0000d0 )
      call get_real_m   (npara,param,value,"whc" ,WHC ,0.0000d0 )

      call get_real_m   (npara,param,value,"th1"  ,TH1  ,1d0    )
      call get_real_m   (npara,param,value,"th2"  ,TH2  ,0d0    )
      call get_real_m   (npara,param,value,"th3"  ,TH3  ,0d0    )
      call get_real_m   (npara,param,value,"tn1"  ,TN1  ,0d0    )
      call get_real_m   (npara,param,value,"tn2"  ,TN2  ,1d0    )
      call get_real_m   (npara,param,value,"tn3"  ,TN3  ,0d0    )
      call get_real_m   (npara,param,value,"ta1"  ,TA1  ,0d0    )
      call get_real_m   (npara,param,value,"ta2"  ,TA2  ,0d0    )
      call get_real_m   (npara,param,value,"ta3"  ,TA3  ,1d0    )

      call get_real_m   (npara,param,value,"lambda1"  ,lambda1,1d0    )
      call get_real_m   (npara,param,value,"lambda2"  ,lambda2,1d0    )
      call get_real_m   (npara,param,value,"lambda3"  ,lambda3,0d0    )
      call get_real_m   (npara,param,value,"lambda4"  ,lambda4,0d0    )
      call get_real_m   (npara,param,value,"lambda5"  ,lambda5,0d0    )
      call get_real_m   (npara,param,value,"rlambda6"  ,Rlambda6,0d0    )
      call get_real_m   (npara,param,value,"rlambda7"  ,Rlambda7,0d0    )
      call get_real_m   (npara,param,value,"ilambda6"  ,Ilambda6,0d0    )
      call get_real_m   (npara,param,value,"ilambda7"  ,Ilambda7,0d0    )

c************************************************************************     
c     Fermions                                                          *
c************************************************************************     

      call get_real_m   (npara,param,value,"tmass"  ,TMASS  ,174.3d0   )
      call get_real_m   (npara,param,value,"twidth" ,twidth ,1.5083d0  )
      call get_real_m   (npara,param,value,"bmass"  ,bmass  ,4.7d0     )
      call get_real_m   (npara,param,value,"cmass"  ,cmass  ,0d0       )
      call get_real_m   (npara,param,value,"smass"  ,smass  ,1.777d0   )
      call get_real_m   (npara,param,value,"mumass"  ,mumass  ,0.1d0   )
      call get_real_m   (npara,param,value,"tamass"  ,tamass  ,1.7d0   )
      call get_real_m   (npara,param,value,"wmass"    ,wmass  ,80.419d0 )

c************************************************************************     
c     MSbar masses for Yukawa couplings                                 *
c************************************************************************     

      call get_real_m(npara,param,value,"mstmass"  ,mstmass  ,164.5d0)
      call get_real_m(npara,param,value,"msbmass"  ,msbmass  ,4.2d0  )
      call get_real_m(npara,param,value,"mscmass"  ,mscmass  ,1.42d0 )
      call get_real_m(npara,param,value,"mssmass"  ,mssmass  ,0.d0 )
      call get_real_m(npara,param,value,"msmumass"  ,msmumass  ,0.105d0 )
      call get_real_m(npara,param,value,"mstamass"  ,mstamass  ,1.77d0 )

c      call get_integer(npara,param,value," iyuk "  ,iyuk  ,1)

c************************************************************************     
c     CKM matrix elements                                               *
c    							                *
c************************************************************************     

      call get_real_m(npara,param,value,"vud"  ,Vud  ,0.975d0)
      call get_real_m(npara,param,value,"vus"  ,Vus  ,0.22d0)
      call get_real_m(npara,param,value,"vub"  ,Vub  ,0.003d0)
      call get_real_m(npara,param,value,"vcd"  ,Vcd  ,0.24d0)
      call get_real_m(npara,param,value,"vcs"  ,Vcs  ,0.973d0)
      call get_real_m(npara,param,value,"vcb"  ,Vcb  ,0.03d0)
      call get_real_m(npara,param,value,"vtd"  ,Vtd  ,0.01d0)
      call get_real_m(npara,param,value,"vts"  ,Vts  ,0.03d0)
      call get_real_m(npara,param,value,"vtb"  ,Vtb  ,0.999d0)

c************************************************************************     
c     Yukawa couplings                                                  *
c    							                *
c************************************************************************     
      call get_real_m(npara,param,value,"y1dr"  ,Y1DR  ,0d0)
      call get_real_m(npara,param,value,"y1di"  ,Y1DI  ,0d0)
      call get_real_m(npara,param,value,"y2dr"  ,Y2DR  ,0d0)
      call get_real_m(npara,param,value,"y2di"  ,Y2DI  ,0d0)
      call get_real_m(npara,param,value,"y3dr"  ,Y3DR  ,0d0)
      call get_real_m(npara,param,value,"y3di"  ,Y3DI  ,0d0)
      call get_real_m(npara,param,value,"y1sr"  ,Y1SR  ,0d0)
      call get_real_m(npara,param,value,"y1si"  ,Y1SI  ,0d0)
      call get_real_m(npara,param,value,"y2sr"  ,Y2SR  ,0d0)
      call get_real_m(npara,param,value,"y2si"  ,Y2SI  ,0d0)
      call get_real_m(npara,param,value,"y3sr"  ,Y3SR  ,0d0)
      call get_real_m(npara,param,value,"y3si"  ,Y3SI  ,0d0)
      call get_real_m(npara,param,value,"y1br"  ,Y1BR  ,0d0)
      call get_real_m(npara,param,value,"y1bi"  ,Y1BI  ,0d0)
      call get_real_m(npara,param,value,"y2br"  ,Y2BR  ,0d0)
      call get_real_m(npara,param,value,"y2bi"  ,Y2BI  ,0d0)
      call get_real_m(npara,param,value,"y3br"  ,Y3BR  ,0d0)
      call get_real_m(npara,param,value,"y3bi"  ,Y3BI  ,0d0)

      call get_real_m(npara,param,value,"y1ur"  ,Y1UR  ,0d0)
      call get_real_m(npara,param,value,"y1ui"  ,Y1UI  ,0d0)
      call get_real_m(npara,param,value,"y2ur"  ,Y2UR  ,0d0)
      call get_real_m(npara,param,value,"y2ui"  ,Y2UI  ,0d0)
      call get_real_m(npara,param,value,"y3ur"  ,Y3UR  ,0d0)
      call get_real_m(npara,param,value,"y3ui"  ,Y3UI  ,0d0)
      call get_real_m(npara,param,value,"y1cr"  ,Y1CR  ,0d0)
      call get_real_m(npara,param,value,"y1ci"  ,Y1CI  ,0d0)
      call get_real_m(npara,param,value,"y2cr"  ,Y2CR  ,0d0)
      call get_real_m(npara,param,value,"y2ci"  ,Y2CI  ,0d0)
      call get_real_m(npara,param,value,"y3cr"  ,Y3CR  ,0d0)
      call get_real_m(npara,param,value,"y3ci"  ,Y3CI  ,0d0)
      call get_real_m(npara,param,value,"y1tr"  ,Y1TR  ,0d0)
      call get_real_m(npara,param,value,"y1ti"  ,Y1TI  ,0d0)
      call get_real_m(npara,param,value,"y2tr"  ,Y2TR  ,0d0)
      call get_real_m(npara,param,value,"y2ti"  ,Y2TI  ,0d0)
      call get_real_m(npara,param,value,"y3tr"  ,Y3TR  ,0d0)
      call get_real_m(npara,param,value,"y3ti"  ,Y3TI  ,0d0)

      call get_real_m(npara,param,value,"y1er"  ,Y1ER  ,0d0)
      call get_real_m(npara,param,value,"y1ei"  ,Y1EI  ,0d0)
      call get_real_m(npara,param,value,"y2er"  ,Y2ER  ,0d0)
      call get_real_m(npara,param,value,"y2ei"  ,Y2EI  ,0d0)
      call get_real_m(npara,param,value,"y3er"  ,Y3ER  ,0d0)
      call get_real_m(npara,param,value,"y3ei"  ,Y3EI  ,0d0)
      call get_real_m(npara,param,value,"y1mur"  ,Y1MUR  ,0d0)
      call get_real_m(npara,param,value,"y1mui"  ,Y1MUI  ,0d0)
      call get_real_m(npara,param,value,"y2mur"  ,Y2MUR  ,0d0)
      call get_real_m(npara,param,value,"y2mui"  ,Y2MUI  ,0d0)
      call get_real_m(npara,param,value,"y3mur"  ,Y3MUR  ,0d0)
      call get_real_m(npara,param,value,"y3mui"  ,Y3MUI  ,0d0)
      call get_real_m(npara,param,value,"y1tar"  ,Y1TAR  ,0d0)
      call get_real_m(npara,param,value,"y1tai"  ,Y1TAI  ,0d0)
      call get_real_m(npara,param,value,"y2tar"  ,Y2TAR  ,0d0)
      call get_real_m(npara,param,value,"y2tai"  ,Y2TAI  ,0d0)
      call get_real_m(npara,param,value,"y3tar"  ,Y3TAR  ,0d0)
      call get_real_m(npara,param,value,"y3tai"  ,Y3TAI  ,0d0)
      G = DSQRT(16d0*atan(1d0)*ALFAS)   ! use setting of the param_card.dat @ NLO
      endif ! readlha

      call coup

      done=.true.
      return
 99   write(*,*) 'error in reading'
      return
      end



