C$PROG ENERGY
      SUBROUTINE ENERGY (Q,EL,IENGY,QGS,NQ)
      IMPLICIT     REAL*8(A-H,O-Z)
      DIMENSION    Q(10), EL(10)
      CHARACTER*4  IENGY
C   
C     ******************************************************************
C     THIS SUBROUTINE FILLS THE ARRAYS Q AND/OR EL DEPENDING ON
C     WHETHER EL OR Q WAS READ IN.
C     ******************************************************************
C   
      IF (IENGY.EQ.'EL  ') GO TO 2
      DO 1 I=1,NQ
      EL(I)=QGS-Q(I)
1     continue
      RETURN
2     DO 3 I=1,NQ
      EL(I)=Q(I)
      Q(I)=QGS-EL(I)
3     continue 
      RETURN
      END
