C$PROG CALBRA
      SUBROUTINE CALBRA  (TABRD,TABCH,KMAX)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/AAA/ Z(4), NZ(30), CONST(50), ZZ(25)
      EQUIVALENCE (CONST(26),SE),(CONST(19),DEGRAD)
      DIMENSION EAP(3), TF(50), TCH(50,3), TB(50)  , RD(50,3), TABCH(150
     1), TABRD(150)
C      CCC
C   
C     ******************************************************************
C      THIS SUBROUTINE USES ALPHA PARTICLE CALIBRATION AND PRODUCES
C     TABLE OF RDIST VERSUS CHANNEL NUMBER.
C   
C     NAP = NUMBER OF ALPHA PARTICLE ENERGIES IN CALIBRATION
C     EAP(I) = ALPHA PARTICLE ENERGY
C     TL = LEFT FOCAL PLANE DIAL SETTING
C     TR = RIGHT FOCAL PLANE DIAL SETTING
C     TF(J) = FREQUENCY
C     TCH(J,I) = CHANNEL NUMBERS
C   
C     ******************************************************************
C   
      J=0
      TERMX=SE/2.0
      T=3728.3338
      XM1A=0.0
      XM3A=4.0026036
      EIN=0.0
      RECOIL=0.0
      THETA=0.0
      THE=0.0
      DTHETA=2.69
      IMIN=1
      IMAX=80
C   
C   
C     IF(NTER.NE0) ERROR
C     CM1.GT.0.0 CALIBRATION USING ELASTIC SCATTERING
      READ(5,7)FXFUK,TL,TR,EAP(1),EAP(2),EAP(3)
      READ(5,7)CM1,CM2,ZOU,THE,DTHE
      IF(CM1.GT.0.0)TERMX=SE/ZOU
      IF(CM1.GT.0.0)T=CM1*931.478
      NAP=FXFUK+0.5
1     J=J+1
      READ(5,7) TF(J),(TCH(J,I),I=1,NAP)
      TB(J)=TF(J)/4.2578
      IF (TF(J).GT.0.0) GO TO 1
      JMAX=J-1
      WRITE(7,9)(EAP(I),I=1,NAP)
      WRITE(7,10)TL,TR
      IF(CM1.LE.0.0)GO TO 13
      THE=THE*DEGRAD
      EIN=EAP(1)
      DTHETA=DTHE*DEGRAD/2.0
      XM3A=CM1
      XM1A=CM1
      THEDE=THE/0.01745
      CALL RELKIN(CM1,CM2,CM1,CM2,EIN,THE,THC,CMTL,E31,0.0,IJ)
      EAP(1)=E31
      WRITE(7,100)XM1A,CM2,XM3A,EIN,E31,THEDE,ZOU
  100 FORMAT(1H ,'MP , MT',2F12.5,'   MASS OUT',F10.5,'  BEAM ENERGY',
     1F5.1,'  ENERGY OUT',F6.2,/,1X,' LAB ANGLE',F6.2,'  ZOUT',F5.0)
   13 DO 21 I=1,NAP
      DO 20 J=1,JMAX
      A     =TERMX*DSQRT(EAP(I)**2+2.0*T*EAP(I))/TB(J)
      CALL OPTIC (A,EAP(I),XM1A,XM3A,EIN,THE,DTHETA,RECOIL,DTFP,DEXIT,
     1XI2PP,DK,ANOK)
      CALL DISTAN  (TL,TR,DEXIT,DTFP,XI2PP,RD(J,I),DELKIN)
20    CONTINUE
21    CONTINUE
      WRITE(7,11)
      DO 3 J=1,JMAX
      WRITE(7,6)J,TF(J),(TCH(J,I),RD(J,I),I=1,NAP)
3     CONTINUE
C     NOW REORDER TCH(J,I) AND RD(J,I) INTO LONG ARRAYS STARTING AT
C     LOWEST RD(J,I)
      DO 41 I=1,NAP
      DO 40 J=1,JMAX
      K=(I-1)*JMAX+J
      TABCH(K)=TCH(J,I)
      TABRD(K)=RD(J,I)
40      continue
41      continue
      KMAX=NAP*JMAX
      DO 51 I=1,KMAX
      IP1=I+1
      IF (IP1.GT.KMAX) IP1=KMAX
      DO 50 J=IP1,KMAX
      IF (TABRD(I).LE.TABRD(J)) GO TO 50
      TEMP=TABRD(I)
      TABRD(I)=TABRD(J)
      TABRD(J)=TEMP
      TEMP=TABCH(I)
      TABCH(I)=TABCH(J)
      TABCH(J)=TEMP
50    CONTINUE
51    CONTINUE
C   
C     TABRD(I) NOW IS IN AN ARRAY WITH SMALLEST RDIST AT LOWER END
      RETURN
C   
C   
6     FORMAT (I10,F12.5,3(5X,2F12.5))
    7 FORMAT(8F10.0)
9     FORMAT (1H1,50X,23HMAGNET CALIBRATION DATA,///,10X,55HALPHA PARTIC
     1LE CALIBRATION ENERGIES * * * * EALPHA(1) =,F10.5,5H  MEV,/,54X,11
     2HEALPHA(2) =,F10.5,5H  MEV,/,54X,11HEALPHA(3) =,F10.5,5H  MEV,//)
10    FORMAT (//,45X,21H  LEFT DIAL READING =,F10.5,/,45X,21H RIGHT DIAL
     1 READING =,F10.5,//)
11    FORMAT (1H ,8X,1HJ,6X,4HFREQ,9X,10HCHANNEL(1),5X,8HRDIST(1),7X,10H
     1CHANNEL(2),6X,8HRDIST(2),9X,10HCHANNEL(3),9X,8HRDIST(3),/)
      END