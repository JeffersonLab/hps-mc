      INTEGER FUNCTION NEXTUNOPEN()
C     *****************************************************************
C     ***
C     Returns an unallocated FORTRAN i/o unit.
C     *****************************************************************
C     ***

      LOGICAL EX
C     
      DO 10 N = 10, 300
      INQUIRE (UNIT=N, OPENED=EX)
      IF (.NOT. EX) THEN
        NEXTUNOPEN = N
        RETURN
      ENDIF
 10   CONTINUE
      STOP ' There is no available I/O unit. '
C     *************************
      END



      SUBROUTINE OPENDATA(TABLEFILE)
C     *****************************************************************
C     ***
C     generic subroutine to open the table files in the right
C      directories
C     *****************************************************************
C     ***
      IMPLICIT NONE
C     
      CHARACTER TABLEFILE*(*),UP*3,LIB*4,DIR*8,TEMPNAME*100
      DATA UP,LIB,DIR/'../','lib/','Pdfdata/'/
      INTEGER IU,NEXTUNOPEN,I
      EXTERNAL NEXTUNOPEN
      COMMON/IU/IU
C     
C     --   start
C     
      IU=NEXTUNOPEN()

C     First try system wide (for cluster if define)


C     Then try in the current directory (for cluster use)
 5    TEMPNAME=TABLEFILE
      OPEN(IU,FILE=TEMPNAME,STATUS='old',ERR=10)
      RETURN

 10   TEMPNAME=UP//TABLEFILE
      OPEN(IU,FILE=TEMPNAME,STATUS='old',ERR=20)
      RETURN

C     then try PdfData directory
 20   TEMPNAME=DIR//TABLEFILE
      OPEN(IU,FILE=TEMPNAME,STATUS='old',ERR=30)
      RETURN

 30   TEMPNAME=LIB//TEMPNAME
      OPEN(IU,FILE=TEMPNAME,STATUS='old',ERR=40)

 40   CONTINUE
      DO I=0,6
        OPEN(IU,FILE=TEMPNAME,STATUS='old',ERR=50)
        RETURN
 50     TEMPNAME=UP//TEMPNAME
        IF (I.EQ.6)THEN
          WRITE(*,*) 'Error: PDF file ',TABLEFILE,' not found'
          STOP
        ENDIF
      ENDDO

      PRINT*,'table for the pdf NOT found  !!!'

      RETURN
      END


