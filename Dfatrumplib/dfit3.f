C$PROG DFIT3
C
      REAL*8 FUNCTION DFIT3(X1,X2,X3,Y1,Y2,Y3,Q)
C
      IMPLICIT REAL*8 (A-H,O-Z)
C
      S1=X1*X1
      S2=X2*X2
      S3=X3*X3
C
      D=DET(1.0D0,X1,S1,1.0D0,X2,S2,1.0D0,X3,S3)
C
      A=DET(Y1,X1,S1,Y2,X2,S2,Y3,X3,S3)
C
      B=DET(1.0D0,Y1,S1,1.0D0,Y2,S2,1.0D0,Y3,S3)
C
      C=DET(1.0D0,X1,Y1,1.0D0,X2,Y2,1.0D0,X3,Y3)
C
      DFIT3=(A+B*Q+C*Q*Q)/D
C
      RETURN
      END
