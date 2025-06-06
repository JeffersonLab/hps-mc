      DOUBLE PRECISION FUNCTION DSIG1(PP,WGT,IMODE)
C     ****************************************************
C     
C     Generated by MadGraph5_aMC@NLO v. 3.4.2, 2023-01-20
C     By the MadGraph5_aMC@NLO Development Team
C     Visit launchpad.net/madgraph5 and amcatnlo.web.cern.ch
C     
C     Process: e- n > e- n ap WEIGHTED<=3 @1
C     *   Decay: ap > chi2 chi1 WEIGHTED<=1
C     *     Decay: chi2 > chi1 e+ e- WEIGHTED<=2
C     
C     RETURNS DIFFERENTIAL CROSS SECTION
C     Input:
C     pp    4 momentum of external particles
C     wgt   weight from Monte Carlo
C     imode 0 run, 1 init, 2 reweight, 
C     3 finalize, 4 only PDFs,
C     5 squared amplitude only (never
C     generate events)
C     Output:
C     Amplitude squared and summed
C     ****************************************************
      IMPLICIT NONE
C     
C     CONSTANTS
C     
      INCLUDE 'genps.inc'
      INCLUDE 'nexternal.inc'
      INCLUDE 'maxconfigs.inc'
      INCLUDE 'maxamps.inc'
      DOUBLE PRECISION       CONV
      PARAMETER (CONV=389379.66*1000)  !CONV TO PICOBARNS
      REAL*8     PI
      PARAMETER (PI=3.1415926D0)
C     
C     ARGUMENTS 
C     
      DOUBLE PRECISION PP(0:3,NEXTERNAL), WGT
      INTEGER IMODE
C     
C     LOCAL VARIABLES 
C     
      INTEGER I,ITYPE,LP,IPROC
      DOUBLE PRECISION EM1
      DOUBLE PRECISION N2
      DOUBLE PRECISION XPQ(-7:7),PD(0:MAXPROC)
      DOUBLE PRECISION DSIGUU,R,RCONF
      INTEGER LUN,ICONF,IFACT,NFACT
      DATA NFACT/1/
      SAVE NFACT
C     
C     STUFF FOR DRESSED EE COLLISIONS
C     
      INCLUDE '../../Source/PDF/eepdf.inc'
      DOUBLE PRECISION EE_COMP_PROD

      INTEGER I_EE
C     
C     EXTERNAL FUNCTIONS
C     
      LOGICAL PASSCUTS
      DOUBLE PRECISION ALPHAS2,REWGT,PDG2PDF,CUSTOM_BIAS
      INTEGER NEXTUNOPEN
C     
C     GLOBAL VARIABLES
C     
      INTEGER          IPSEL
      COMMON /SUBPROC/ IPSEL
C     MINCFIG has this config number
      INTEGER           MINCFIG, MAXCFIG
      COMMON/TO_CONFIGS/MINCFIG, MAXCFIG
      INTEGER MAPCONFIG(0:LMAXCONFIGS), ICONFIG
      COMMON/TO_MCONFIGS/MAPCONFIG, ICONFIG
C     Keep track of whether cuts already calculated for this event
      LOGICAL CUTSDONE,CUTSPASSED
      COMMON/TO_CUTSDONE/CUTSDONE,CUTSPASSED

      INTEGER SUBDIAG(MAXSPROC),IB(2)
      COMMON/TO_SUB_DIAG/SUBDIAG,IB
      INCLUDE 'coupl.inc'
      INCLUDE 'run.inc'
C     Common blocks
      CHARACTER*7         PDLABEL,EPA_LABEL
      INTEGER       LHAID
      COMMON/TO_PDF/LHAID,PDLABEL,EPA_LABEL
C     
C     local
C     
      DOUBLE PRECISION P1(0:3, NEXTERNAL)

C     
C     DATA
C     
      DATA EM1/1*1D0/
      DATA N2/1*1D0/
C     ----------
C     BEGIN CODE
C     ----------
      DSIG1=0D0

      IF(IMODE.EQ.1)THEN
C       Set up process information from file symfact
        LUN=NEXTUNOPEN()
        NFACT=1
        OPEN(UNIT=LUN,FILE='../symfact.dat',STATUS='OLD',ERR=20)
        DO WHILE(.TRUE.)
          READ(LUN,*,ERR=10,END=10) RCONF, IFACT
          ICONF=INT(RCONF)
          IF(ICONF.EQ.MAPCONFIG(MINCFIG))THEN
            NFACT=IFACT
          ENDIF
        ENDDO
 10     CLOSE(LUN)
        RETURN
 20     WRITE(*,*)'Error opening symfact.dat. No symmetry factor used.'
        RETURN
      ENDIF
C     Continue only if IMODE is 0, 4 or 5
      IF(IMODE.NE.0.AND.IMODE.NE.4.AND.IMODE.NE.5) RETURN


      IF (ABS(LPP(IB(1))).GE.1) THEN
          !LP=SIGN(1,LPP(IB(1)))
        EM1=PDG2PDF(LPP(IB(1)),11, IB(1),XBK(IB(1)),DSQRT(Q2FACT(IB(1))
     $   ))
      ENDIF
      IF (ABS(LPP(IB(2))).GE.1) THEN
          !LP=SIGN(1,LPP(IB(2)))
        N2=PDG2PDF(LPP(IB(2)),9000002, IB(2),XBK(IB(2))
     $   ,DSQRT(Q2FACT(IB(2))))
      ENDIF
      PD(0) = 0D0
      IPROC = 0
      IPROC=IPROC+1  ! e- n > e- n chi1 e+ e- chi1
      PD(IPROC)=EM1*N2
      PD(0)=PD(0)+DABS(PD(IPROC))
      IF (IMODE.EQ.4)THEN
        DSIG1 = PD(0)
        RETURN
      ENDIF
      IF(FRAME_ID.NE.6)THEN
        CALL BOOST_TO_FRAME(PP, FRAME_ID, P1)
      ELSE
        P1 = PP
      ENDIF
      CALL SMATRIX1(P1,DSIGUU)
      IF (IMODE.EQ.5) THEN
        IF (DSIGUU.LT.1D199) THEN
          DSIG1 = DSIGUU*CONV
        ELSE
          DSIG1 = 0.0D0
        ENDIF
        RETURN
      ENDIF
C     Select a flavor combination (need to do here for right sign)
      CALL RANMAR(R)
      IPSEL=0
      DO WHILE (R.GE.0D0 .AND. IPSEL.LT.IPROC)
        IPSEL=IPSEL+1
        R=R-DABS(PD(IPSEL))/PD(0)
      ENDDO

      DSIGUU=DSIGUU*REWGT(PP)

C     Apply the bias weight specified in the run card (default is 1.0)
      DSIGUU=DSIGUU*CUSTOM_BIAS(PP,DSIGUU,1)

      DSIGUU=DSIGUU*NFACT

      IF (DSIGUU.LT.1D199) THEN
C       Set sign of dsig based on sign of PDF and matrix element
        DSIG1=DSIGN(PD(0)*CONV*DSIGUU,DSIGUU*PD(IPSEL))
      ELSE
        WRITE(*,*) 'Error in matrix element'
        DSIGUU=0D0
        DSIG1=0D0
      ENDIF
C     Generate events only if IMODE is 0.
      IF(IMODE.EQ.0.AND.DABS(DSIG1).GT.0D0)THEN
C       Call UNWGT to unweight and store events
        CALL UNWGT(PP,DSIG1*WGT,1)
      ENDIF

      END
C     
C     Functionality to handling grid
C     




      SUBROUTINE PRINT_ZERO_AMP1()

      RETURN
      END

      INTEGER FUNCTION GET_NHEL1(HEL, IPART)
C     if hel>0 return the helicity of particule ipart for the selected
C      helicity configuration
C     if hel=0 return the number of helicity state possible for that
C      particle 
      IMPLICIT NONE
      INTEGER HEL,I, IPART
      INCLUDE 'nexternal.inc'
      INTEGER ONE_NHEL(NEXTERNAL)
      INTEGER                 NCOMB
      PARAMETER (             NCOMB=256)
      INTEGER NHEL(NEXTERNAL,0:NCOMB)
      DATA (NHEL(I,0),I=1,5) / 2, 2, 2, 2, 3/
      DATA (NHEL(I,   1),I=1,8) / 1, 1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,   2),I=1,8) / 1, 1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,   3),I=1,8) / 1, 1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,   4),I=1,8) / 1, 1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,   5),I=1,8) / 1, 1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,   6),I=1,8) / 1, 1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,   7),I=1,8) / 1, 1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,   8),I=1,8) / 1, 1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,   9),I=1,8) / 1, 1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  10),I=1,8) / 1, 1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  11),I=1,8) / 1, 1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  12),I=1,8) / 1, 1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  13),I=1,8) / 1, 1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  14),I=1,8) / 1, 1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  15),I=1,8) / 1, 1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  16),I=1,8) / 1, 1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  17),I=1,8) / 1, 1,-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  18),I=1,8) / 1, 1,-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  19),I=1,8) / 1, 1,-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  20),I=1,8) / 1, 1,-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  21),I=1,8) / 1, 1,-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  22),I=1,8) / 1, 1,-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  23),I=1,8) / 1, 1,-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  24),I=1,8) / 1, 1,-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  25),I=1,8) / 1, 1,-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  26),I=1,8) / 1, 1,-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  27),I=1,8) / 1, 1,-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  28),I=1,8) / 1, 1,-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  29),I=1,8) / 1, 1,-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  30),I=1,8) / 1, 1,-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  31),I=1,8) / 1, 1,-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  32),I=1,8) / 1, 1,-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  33),I=1,8) / 1, 1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  34),I=1,8) / 1, 1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  35),I=1,8) / 1, 1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  36),I=1,8) / 1, 1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  37),I=1,8) / 1, 1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  38),I=1,8) / 1, 1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  39),I=1,8) / 1, 1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  40),I=1,8) / 1, 1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  41),I=1,8) / 1, 1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  42),I=1,8) / 1, 1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  43),I=1,8) / 1, 1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  44),I=1,8) / 1, 1, 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  45),I=1,8) / 1, 1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  46),I=1,8) / 1, 1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  47),I=1,8) / 1, 1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  48),I=1,8) / 1, 1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  49),I=1,8) / 1, 1, 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  50),I=1,8) / 1, 1, 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  51),I=1,8) / 1, 1, 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  52),I=1,8) / 1, 1, 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  53),I=1,8) / 1, 1, 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  54),I=1,8) / 1, 1, 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  55),I=1,8) / 1, 1, 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  56),I=1,8) / 1, 1, 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  57),I=1,8) / 1, 1, 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  58),I=1,8) / 1, 1, 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  59),I=1,8) / 1, 1, 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  60),I=1,8) / 1, 1, 1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  61),I=1,8) / 1, 1, 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  62),I=1,8) / 1, 1, 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  63),I=1,8) / 1, 1, 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  64),I=1,8) / 1, 1, 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  65),I=1,8) / 1,-1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  66),I=1,8) / 1,-1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  67),I=1,8) / 1,-1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I,  68),I=1,8) / 1,-1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I,  69),I=1,8) / 1,-1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I,  70),I=1,8) / 1,-1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I,  71),I=1,8) / 1,-1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I,  72),I=1,8) / 1,-1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I,  73),I=1,8) / 1,-1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I,  74),I=1,8) / 1,-1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I,  75),I=1,8) / 1,-1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I,  76),I=1,8) / 1,-1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I,  77),I=1,8) / 1,-1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I,  78),I=1,8) / 1,-1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I,  79),I=1,8) / 1,-1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I,  80),I=1,8) / 1,-1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I,  81),I=1,8) / 1,-1,-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I,  82),I=1,8) / 1,-1,-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I,  83),I=1,8) / 1,-1,-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I,  84),I=1,8) / 1,-1,-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I,  85),I=1,8) / 1,-1,-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I,  86),I=1,8) / 1,-1,-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I,  87),I=1,8) / 1,-1,-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I,  88),I=1,8) / 1,-1,-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I,  89),I=1,8) / 1,-1,-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I,  90),I=1,8) / 1,-1,-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I,  91),I=1,8) / 1,-1,-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I,  92),I=1,8) / 1,-1,-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I,  93),I=1,8) / 1,-1,-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I,  94),I=1,8) / 1,-1,-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I,  95),I=1,8) / 1,-1,-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I,  96),I=1,8) / 1,-1,-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I,  97),I=1,8) / 1,-1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I,  98),I=1,8) / 1,-1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I,  99),I=1,8) / 1,-1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I, 100),I=1,8) / 1,-1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I, 101),I=1,8) / 1,-1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I, 102),I=1,8) / 1,-1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I, 103),I=1,8) / 1,-1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I, 104),I=1,8) / 1,-1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I, 105),I=1,8) / 1,-1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I, 106),I=1,8) / 1,-1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I, 107),I=1,8) / 1,-1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I, 108),I=1,8) / 1,-1, 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I, 109),I=1,8) / 1,-1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I, 110),I=1,8) / 1,-1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I, 111),I=1,8) / 1,-1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I, 112),I=1,8) / 1,-1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I, 113),I=1,8) / 1,-1, 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I, 114),I=1,8) / 1,-1, 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I, 115),I=1,8) / 1,-1, 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I, 116),I=1,8) / 1,-1, 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I, 117),I=1,8) / 1,-1, 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I, 118),I=1,8) / 1,-1, 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I, 119),I=1,8) / 1,-1, 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I, 120),I=1,8) / 1,-1, 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I, 121),I=1,8) / 1,-1, 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I, 122),I=1,8) / 1,-1, 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I, 123),I=1,8) / 1,-1, 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I, 124),I=1,8) / 1,-1, 1, 1, 1, 1, 1, 1/
      DATA (NHEL(I, 125),I=1,8) / 1,-1, 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I, 126),I=1,8) / 1,-1, 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I, 127),I=1,8) / 1,-1, 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I, 128),I=1,8) / 1,-1, 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I, 129),I=1,8) /-1, 1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I, 130),I=1,8) /-1, 1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I, 131),I=1,8) /-1, 1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I, 132),I=1,8) /-1, 1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I, 133),I=1,8) /-1, 1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I, 134),I=1,8) /-1, 1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I, 135),I=1,8) /-1, 1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I, 136),I=1,8) /-1, 1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I, 137),I=1,8) /-1, 1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I, 138),I=1,8) /-1, 1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I, 139),I=1,8) /-1, 1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I, 140),I=1,8) /-1, 1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I, 141),I=1,8) /-1, 1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I, 142),I=1,8) /-1, 1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I, 143),I=1,8) /-1, 1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I, 144),I=1,8) /-1, 1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I, 145),I=1,8) /-1, 1,-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I, 146),I=1,8) /-1, 1,-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I, 147),I=1,8) /-1, 1,-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I, 148),I=1,8) /-1, 1,-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I, 149),I=1,8) /-1, 1,-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I, 150),I=1,8) /-1, 1,-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I, 151),I=1,8) /-1, 1,-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I, 152),I=1,8) /-1, 1,-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I, 153),I=1,8) /-1, 1,-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I, 154),I=1,8) /-1, 1,-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I, 155),I=1,8) /-1, 1,-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I, 156),I=1,8) /-1, 1,-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I, 157),I=1,8) /-1, 1,-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I, 158),I=1,8) /-1, 1,-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I, 159),I=1,8) /-1, 1,-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I, 160),I=1,8) /-1, 1,-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I, 161),I=1,8) /-1, 1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I, 162),I=1,8) /-1, 1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I, 163),I=1,8) /-1, 1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I, 164),I=1,8) /-1, 1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I, 165),I=1,8) /-1, 1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I, 166),I=1,8) /-1, 1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I, 167),I=1,8) /-1, 1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I, 168),I=1,8) /-1, 1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I, 169),I=1,8) /-1, 1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I, 170),I=1,8) /-1, 1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I, 171),I=1,8) /-1, 1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I, 172),I=1,8) /-1, 1, 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I, 173),I=1,8) /-1, 1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I, 174),I=1,8) /-1, 1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I, 175),I=1,8) /-1, 1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I, 176),I=1,8) /-1, 1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I, 177),I=1,8) /-1, 1, 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I, 178),I=1,8) /-1, 1, 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I, 179),I=1,8) /-1, 1, 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I, 180),I=1,8) /-1, 1, 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I, 181),I=1,8) /-1, 1, 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I, 182),I=1,8) /-1, 1, 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I, 183),I=1,8) /-1, 1, 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I, 184),I=1,8) /-1, 1, 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I, 185),I=1,8) /-1, 1, 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I, 186),I=1,8) /-1, 1, 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I, 187),I=1,8) /-1, 1, 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I, 188),I=1,8) /-1, 1, 1, 1, 1, 1, 1, 1/
      DATA (NHEL(I, 189),I=1,8) /-1, 1, 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I, 190),I=1,8) /-1, 1, 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I, 191),I=1,8) /-1, 1, 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I, 192),I=1,8) /-1, 1, 1, 1, 1,-1, 1, 1/
      DATA (NHEL(I, 193),I=1,8) /-1,-1,-1,-1,-1, 1,-1,-1/
      DATA (NHEL(I, 194),I=1,8) /-1,-1,-1,-1,-1, 1,-1, 1/
      DATA (NHEL(I, 195),I=1,8) /-1,-1,-1,-1,-1, 1, 1,-1/
      DATA (NHEL(I, 196),I=1,8) /-1,-1,-1,-1,-1, 1, 1, 1/
      DATA (NHEL(I, 197),I=1,8) /-1,-1,-1,-1,-1,-1,-1,-1/
      DATA (NHEL(I, 198),I=1,8) /-1,-1,-1,-1,-1,-1,-1, 1/
      DATA (NHEL(I, 199),I=1,8) /-1,-1,-1,-1,-1,-1, 1,-1/
      DATA (NHEL(I, 200),I=1,8) /-1,-1,-1,-1,-1,-1, 1, 1/
      DATA (NHEL(I, 201),I=1,8) /-1,-1,-1,-1, 1, 1,-1,-1/
      DATA (NHEL(I, 202),I=1,8) /-1,-1,-1,-1, 1, 1,-1, 1/
      DATA (NHEL(I, 203),I=1,8) /-1,-1,-1,-1, 1, 1, 1,-1/
      DATA (NHEL(I, 204),I=1,8) /-1,-1,-1,-1, 1, 1, 1, 1/
      DATA (NHEL(I, 205),I=1,8) /-1,-1,-1,-1, 1,-1,-1,-1/
      DATA (NHEL(I, 206),I=1,8) /-1,-1,-1,-1, 1,-1,-1, 1/
      DATA (NHEL(I, 207),I=1,8) /-1,-1,-1,-1, 1,-1, 1,-1/
      DATA (NHEL(I, 208),I=1,8) /-1,-1,-1,-1, 1,-1, 1, 1/
      DATA (NHEL(I, 209),I=1,8) /-1,-1,-1, 1,-1, 1,-1,-1/
      DATA (NHEL(I, 210),I=1,8) /-1,-1,-1, 1,-1, 1,-1, 1/
      DATA (NHEL(I, 211),I=1,8) /-1,-1,-1, 1,-1, 1, 1,-1/
      DATA (NHEL(I, 212),I=1,8) /-1,-1,-1, 1,-1, 1, 1, 1/
      DATA (NHEL(I, 213),I=1,8) /-1,-1,-1, 1,-1,-1,-1,-1/
      DATA (NHEL(I, 214),I=1,8) /-1,-1,-1, 1,-1,-1,-1, 1/
      DATA (NHEL(I, 215),I=1,8) /-1,-1,-1, 1,-1,-1, 1,-1/
      DATA (NHEL(I, 216),I=1,8) /-1,-1,-1, 1,-1,-1, 1, 1/
      DATA (NHEL(I, 217),I=1,8) /-1,-1,-1, 1, 1, 1,-1,-1/
      DATA (NHEL(I, 218),I=1,8) /-1,-1,-1, 1, 1, 1,-1, 1/
      DATA (NHEL(I, 219),I=1,8) /-1,-1,-1, 1, 1, 1, 1,-1/
      DATA (NHEL(I, 220),I=1,8) /-1,-1,-1, 1, 1, 1, 1, 1/
      DATA (NHEL(I, 221),I=1,8) /-1,-1,-1, 1, 1,-1,-1,-1/
      DATA (NHEL(I, 222),I=1,8) /-1,-1,-1, 1, 1,-1,-1, 1/
      DATA (NHEL(I, 223),I=1,8) /-1,-1,-1, 1, 1,-1, 1,-1/
      DATA (NHEL(I, 224),I=1,8) /-1,-1,-1, 1, 1,-1, 1, 1/
      DATA (NHEL(I, 225),I=1,8) /-1,-1, 1,-1,-1, 1,-1,-1/
      DATA (NHEL(I, 226),I=1,8) /-1,-1, 1,-1,-1, 1,-1, 1/
      DATA (NHEL(I, 227),I=1,8) /-1,-1, 1,-1,-1, 1, 1,-1/
      DATA (NHEL(I, 228),I=1,8) /-1,-1, 1,-1,-1, 1, 1, 1/
      DATA (NHEL(I, 229),I=1,8) /-1,-1, 1,-1,-1,-1,-1,-1/
      DATA (NHEL(I, 230),I=1,8) /-1,-1, 1,-1,-1,-1,-1, 1/
      DATA (NHEL(I, 231),I=1,8) /-1,-1, 1,-1,-1,-1, 1,-1/
      DATA (NHEL(I, 232),I=1,8) /-1,-1, 1,-1,-1,-1, 1, 1/
      DATA (NHEL(I, 233),I=1,8) /-1,-1, 1,-1, 1, 1,-1,-1/
      DATA (NHEL(I, 234),I=1,8) /-1,-1, 1,-1, 1, 1,-1, 1/
      DATA (NHEL(I, 235),I=1,8) /-1,-1, 1,-1, 1, 1, 1,-1/
      DATA (NHEL(I, 236),I=1,8) /-1,-1, 1,-1, 1, 1, 1, 1/
      DATA (NHEL(I, 237),I=1,8) /-1,-1, 1,-1, 1,-1,-1,-1/
      DATA (NHEL(I, 238),I=1,8) /-1,-1, 1,-1, 1,-1,-1, 1/
      DATA (NHEL(I, 239),I=1,8) /-1,-1, 1,-1, 1,-1, 1,-1/
      DATA (NHEL(I, 240),I=1,8) /-1,-1, 1,-1, 1,-1, 1, 1/
      DATA (NHEL(I, 241),I=1,8) /-1,-1, 1, 1,-1, 1,-1,-1/
      DATA (NHEL(I, 242),I=1,8) /-1,-1, 1, 1,-1, 1,-1, 1/
      DATA (NHEL(I, 243),I=1,8) /-1,-1, 1, 1,-1, 1, 1,-1/
      DATA (NHEL(I, 244),I=1,8) /-1,-1, 1, 1,-1, 1, 1, 1/
      DATA (NHEL(I, 245),I=1,8) /-1,-1, 1, 1,-1,-1,-1,-1/
      DATA (NHEL(I, 246),I=1,8) /-1,-1, 1, 1,-1,-1,-1, 1/
      DATA (NHEL(I, 247),I=1,8) /-1,-1, 1, 1,-1,-1, 1,-1/
      DATA (NHEL(I, 248),I=1,8) /-1,-1, 1, 1,-1,-1, 1, 1/
      DATA (NHEL(I, 249),I=1,8) /-1,-1, 1, 1, 1, 1,-1,-1/
      DATA (NHEL(I, 250),I=1,8) /-1,-1, 1, 1, 1, 1,-1, 1/
      DATA (NHEL(I, 251),I=1,8) /-1,-1, 1, 1, 1, 1, 1,-1/
      DATA (NHEL(I, 252),I=1,8) /-1,-1, 1, 1, 1, 1, 1, 1/
      DATA (NHEL(I, 253),I=1,8) /-1,-1, 1, 1, 1,-1,-1,-1/
      DATA (NHEL(I, 254),I=1,8) /-1,-1, 1, 1, 1,-1,-1, 1/
      DATA (NHEL(I, 255),I=1,8) /-1,-1, 1, 1, 1,-1, 1,-1/
      DATA (NHEL(I, 256),I=1,8) /-1,-1, 1, 1, 1,-1, 1, 1/

      GET_NHEL1 = NHEL(IPART, IABS(HEL))
      RETURN
      END
