C$PROG FIT3      - Fits 3 X,Y points & returns interpolated value
C   
C     ******************************************************************
C     By W.T. Milner at HRIBF - last modified 04/17/2000
C     ******************************************************************
C
      FUNCTION FIT3(X1,X2,X3,Y1,Y2,Y3,Q)
C
C     ------------------------------------------------------------------
C     FITS THE THREE POINTS - (X1,Y1), (X2,Y2), (X3,Y3) TO
C     FUNCTION OF THE FORM  - Y=A+B*X+C*X**2
C     AND RETURNS THE VALUE OF "Y" AT X=Q
C     ------------------------------------------------------------------
C
      DET(A1,A2,A3,B1,B2,B3,C1,C2,C3)=
     &    A1*(B2*C3-B3*C2)+A2*(B3*C1-B1*C3)+A3*(B1*C2-B2*C1)
C
      S1=X1*X1
      S2=X2*X2
      S3=X3*X3
      D=DET(1.,X1,S1,1.,X2,S2,1.,X3,S3)
      A=DET(Y1,X1,S1,Y2,X2,S2,Y3,X3,S3)
      B=DET(1.,Y1,S1,1.,Y2,S2,1.,Y3,S3)
      C=DET(1.,X1,Y1,1.,X2,Y2,1.,X3,Y3)
      FIT3=(A+B*Q+C*Q*Q)/D
      RETURN
      END
