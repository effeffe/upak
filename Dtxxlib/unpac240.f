C$PROG UNPAC240
      SUBROUTINE UNPAC240(IWD,ICH,N)
      INTEGER*4 ICH(240,2),IWD(1),JTEMP
C   
C     ROUTINE TO UNPACK BYTES FROM IWD INTO LO-ORDER BYTES IN ICH
C   
      DO 10 I=1,N
      CALL ILBYTE(JTEMP,IWD,I-1)
      ICH(I,1)=JTEMP
      ICH(I,2)=0
   10 CONTINUE
      RETURN
      END
