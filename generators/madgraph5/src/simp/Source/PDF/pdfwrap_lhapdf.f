      SUBROUTINE PDFWRAP
      IMPLICIT NONE
C     
C     INCLUDE
C     
      INCLUDE 'pdf.inc'
      INCLUDE '../alfas.inc'
      REAL*8 ZMASS
      DATA ZMASS/91.188D0/
      CHARACTER*150 LHAPATH
      CHARACTER*20 PARM(20)
      DOUBLE PRECISION VALUE(20)
      REAL*8 ALPHASPDF
      EXTERNAL ALPHASPDF


C     -------------------
C     START THE CODE
C     -------------------      

C     initialize the pdf set
      CALL FINDPDFPATH(LHAPATH)
      CALL SETPDFPATH(LHAPATH)
      VALUE(1)=LHAID
      PARM(1)='DEFAULT'
      CALL PDFSET(PARM,VALUE)
      CALL GETORDERAS(NLOOP)
      NLOOP=NLOOP+1
      ASMZ=ALPHASPDF(ZMASS)

      RETURN
      END


      SUBROUTINE FINDPDFPATH(LHAPATH)
C     *****************************************************************
C     ***
C     generic subroutine to open the table files in the right
C      directories
C     *****************************************************************
C     ***
      IMPLICIT NONE
C     
      CHARACTER LHAPATH*150,UP*3
      DATA UP/'../'/
      LOGICAL EXISTS
      INTEGER I


C     first try in the current directory
      LHAPATH='./PDFsets'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN

      DO I=1,6
        LHAPATH=UP//LHAPATH
        INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
        IF(EXISTS)RETURN
      ENDDO
      LHAPATH='lib/PDFsets'
      INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
      IF(EXISTS)RETURN
      DO I=1,6
        LHAPATH=UP//LHAPATH
        INQUIRE(FILE=LHAPATH, EXIST=EXISTS)
        IF(EXISTS)RETURN
      ENDDO
      PRINT*,'Could not find PDFsets directory, quitting'
      STOP

      RETURN
      END


