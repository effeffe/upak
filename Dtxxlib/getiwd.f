C$PROG GETIWD
C
      SUBROUTINE GETIWD(LI,KLIS,IWD)
C
      INTEGER*4    IWD(30)
C
      CHARACTER*4  KLIS
C
      integer*4 X20202020
      data X20202020/Z'20202020'/

      READ(LI,20)(IWD(I),I=1,20)
   20 FORMAT(20A4)
C
      DO 30 I=21,30
      IWD(I)=X20202020
   30 CONTINUE
C
      IF(KLIS.NE.'LN03') CALL CLEANUP(IWD)
C
      RETURN
      END
