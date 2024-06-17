C$PROG BLANK     - Blanks 20 full words
C   
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/18/02
C     ******************************************************************
C
      SUBROUTINE BLANK(IWD)
C
      INTEGER*4 IWD(*)
      integer*4 X20202020
      data X20202020/Z'20202020'/
C
      DO 10 I=1,20
         IWD(I)=X20202020
   10 CONTINUE
      RETURN
      END
