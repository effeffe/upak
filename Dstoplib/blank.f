C$PROG BLANK     - Blanks 20 full words
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      SUBROUTINE BLANK(IWD)
C
      INTEGER*4 IWD(20)
      integer*4 X20202020
      data X20202020/Z'20202020'/
C
      DO 10 I=1,20
      IWD(I)=X20202020
   10 CONTINUE
      RETURN
      END
