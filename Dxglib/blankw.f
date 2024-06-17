C$PROG BLANKW    - Sets INTEGER*4 words to blank
C
C     ******************************************************************
C     BY WT MILNER AT HRIBF - LAST MODIFIED 04/08/02
C     ******************************************************************
C
      SUBROUTINE BLANKW(IWD,IA,IB)
C
      INTEGER*4 IWD(*)
      DATA   IBLANK /Z'20202020'/
C
      DO 10 I=IA,IB
      IWD(I)=IBLANK
   10 CONTINUE
      RETURN
      END
