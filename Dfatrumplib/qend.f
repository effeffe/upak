C$PROG QEND
      SUBROUTINE QEND (F,E,TH,DTH,ZO,B,TB,TMXB, RD,RDA,TL,TR,X,N,K,RD2)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/AAA/ Z(4), NZ(30), CONST(50), ZZ(25)
      EQUIVALENCE (CONST(34),XA)
C   
      EQUIVALENCE (ILRD  ,ZZ(2))
C   
      DIMENSION F(4), X(2), RD(2)
C   
C   
      NIT=100
      ILRD=1
      X(2)=X(1)
*     IF (RDA-RD2) 1,5,2
      if (rda .lt. rd2) then
1        DIR=-1.
*        GO TO 3
      else if (rda .gt. rd2) then
2        DIR=1.
      else if (rda .eq. rd2) then
         return
      endif
3     STEP=.04
      DO 4 NI=1,NIT
      DELQ=STEP*DIR
      X(2)=X(2)+DELQ
      CALL CROW (F,E,TH,DTH,ZO,B,TB,TMXB,   RD,TL,TR,X,N,K)
      IF (ABS (RD(2)-RD2).LE.0.01) GO TO 5
      IF (RD(2).GT.RD2.AND.DIR.GT.0.0) GO TO 4
      IF (RD(2).LT.RD2.AND.DIR.LT.0.0) GO TO 4
      DIR=-DIR
      STEP=STEP*.2
4     CONTINUE
      WRITE(7,6)
5     RETURN
C   
C   
6     FORMAT (93H MAXIMUM NUMBER OF ITTERATIONS REACHED IN SUBROUTINE QE
     1ND * * * * RESULTS MAY BE INACCURATE. )
      END
