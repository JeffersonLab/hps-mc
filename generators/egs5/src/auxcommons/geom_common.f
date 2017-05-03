!---------------------------geom_common.f-------------------------------
! Version: 061108-1759
! Reference:
!-----------------------------------------------------------------------
!23456789|123456789|123456789|123456789|123456789|123456789|123456789|12

      integer  MAX_BODY,MAX_ZONE,MAX_TVAL,MAX_GEOM,MAX_GTYPE,
     &         MAX_RPP,MAX_SPH,MAX_RCC,MAX_TRC,MAX_TOR,
     &         MAX_ARB,MAX_REC,MAX_ELL,MAX_BOX,MAX_WED,
     &         MAX_HEX,MAX_HAF,MAX_TEC,MAX_GEL
      parameter (MAX_BODY=100,MAX_ZONE=500,MAX_TVAL=2000,
     &           MAX_GEOM=400,MAX_GTYPE=14,
     &           MAX_RPP=200,MAX_SPH=200,MAX_RCC=200,
     &           MAX_TRC=200,MAX_TOR=200,
     &           MAX_ARB=200,MAX_REC=200,MAX_ELL=200,
     &           MAX_BOX=200,MAX_WED=200,MAX_HEX=200,
     &           MAX_HAF=200,MAX_TEC=200,MAX_GEL=200)
      integer MAX_IZN
      parameter (MAX_IZN=200)
c
      double precision atval
      integer itvalm,itverr
      common/TVALCG/atval(MAX_TVAL),itvalm,itverr
      integer nbzone,nbbody,iorcnt,izonin
      common/ZONDTA/ nbzone(MAX_BODY,MAX_ZONE),nbbody(MAX_ZONE),
     &               iorcnt(MAX_ZONE),izonin,
     &               zoneid(MAX_ZONE),zoneor(MAX_BODY,MAX_ZONE)
c
      character zoneid*3,zoneor*2
      
      double precision rpppnt
      integer nbrpp,irppin,irppuse
      common/RPPDTA/rpppnt(6,MAX_RPP),nbrpp(MAX_RPP),irppin,
     &              irppuse(MAX_RPP)
      double precision sphpnt
      integer nbsph,isphin,isphuse
      common/SPHDTACG/sphpnt(4,MAX_SPH),nbsph(MAX_SPH),isphin,
     &              isphuse(MAX_SPH)
      double precision rccpnt
      integer nbrcc,irccin,irccuse
      common/RCCDTA/rccpnt(7,MAX_RCC),nbrcc(MAX_RCC),irccin,
     &              irccuse(MAX_RCC)
      double precision trcpnt
      integer nbtrc,itrcin,itrcuse
      common/TRCDAT/trcpnt(8,MAX_TRC),nbtrc(MAX_TRC),itrcin,
     &              itrcuse(MAX_TRC)
      double precision torpnt
      integer nbtor,itorin,itoruse
      common/TORDTA/torpnt(8,MAX_TOR),nbtor(MAX_TOR),itorin,
     &              itoruse(MAX_TOR)
      double precision arbpnt,arbtbl
      integer nbarb,iarbin,iarbuse
      common/ARBDTA/arbpnt(30,MAX_ARB),arbtbl(26,MAX_ARB),
     &              nbarb(MAX_ARB),iarbin,iarbuse(MAX_ARB)
      double precision recpnt
      integer nbrec,irecin,irecuse
      common/RECDTA/recpnt(15,MAX_REC),nbrec(MAX_REC),irecin,
     &              irecuse(MAX_REC)
      double precision ellpnt
      integer nbell,iellin,ielluse
      common/ELLDTA/ellpnt(7,MAX_ELL),nbell(MAX_ELL),iellin,
     &              ielluse(MAX_ELL)
      double precision boxpnt
      integer nbbox,iboxin,iboxuse
      common/BOXDTA/boxpnt(15,MAX_BOX),nbbox(MAX_BOX),iboxin,
     &              iboxuse(MAX_BOX)
      double precision wedpnt
      integer nbwed,iwedin,iweduse
      common/WEDDTA/wedpnt(15,MAX_WED),nbwed(MAX_WED),iwedin,
     &              iweduse(MAX_WED)
      double precision hexpnt
      integer nbhex,ihexin,ihexuse
      common/HEXDTA/hexpnt(38,MAX_HEX),nbhex(MAX_HEX),ihexin,
     &              ihexuse(MAX_HEX)
      double precision hafpnt
      integer nbhaf,ihafin,ihafuse
      common/HAFDTA/hafpnt(5,MAX_HAF),nbhaf(MAX_HAF),ihafin,
     &              ihafuse(MAX_HAF)
      double precision tecpnt
      integer nbtec,itecin,itecuse
      common/TECDTA/tecpnt(16,MAX_TEC),nbtec(MAX_TEC),itecin,
     &              itecuse(MAX_TEC)
      double precision gelpnt
      integer nbgel,igelin,igeluse
      common/GELDTA/gelpnt(15,MAX_GEL),nbgel(MAX_GEL),igelin,
     &              igeluse(MAX_GEL)
c
      integer itblty,itblno,ityknd,igmmax,itbody
      common/GEOMID/itblty(MAX_GEOM),itblno(MAX_GEOM),
     &              ityknd(MAX_GTYPE),igmmax,itbody
c
      integer iznnxc,iznnxt,iznnxp,iznnxs
      common/ZONNXT/iznnxc(3,MAX_ZONE,0:MAX_ZONE),
     &              iznnxt(3,MAX_ZONE,0:MAX_ZONE),
     &              iznnxp(3,MAX_ZONE,0:MAX_ZONE),iznnxs(3,2)
c
      double precision cgmnst,cgeps1,cgeps2
      double precision rcceps,trceps
      double precision rppeps,spheps,toreps
      double precision elleps,arbeps,receps,wedeps,boxeps,
     &                 hafeps,hexeps,teceps,geleps
      common /epstbl/cgmnst,cgeps1,cgeps2,rcceps,trceps,
     &               rppeps,spheps,toreps,
     &               elleps,arbeps,receps,wedeps,boxeps,
     &               hafeps,hexeps,teceps,geleps
c
      integer irynow,ilpnow
      common /CMLOOP/irynow,ilpnow
c
      double precision DLNMAX
      parameter (DLNMAX=1.0D+20)
c
!----------------------last line of geom_common.f-----------------------
