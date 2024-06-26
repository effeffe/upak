C$PROG OPTIC
      SUBROUTINE OPTIC (A,E3,XM1A,XM3A,EIN,THETA,DTHETA,RECOIL,DTFP,DEXI
     1T,XI2PP,DK,ANOK)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/AAA/ Z(4), NZ(30), CONST(50), ZZ(25)
      EQUIVALENCE (CONST(14),COMEGA), (CONST(16),CXI1P),
     1(CONST(23),RP),(CONST(24),RPP),(CONST(25),SCOMEG),(CONST(29),ST
     2HETA),(CONST(30),SXI1P),(CONST(33),TSTH),(CONST(35),XI1P),(CON
     3ST(36),X0),(CONST(37),Y0),(CONST(38),ZL1P)
C   
      EQUIVALENCE (IFLAG ,ZZ(3)),
     &            (ZMH   ,ZZ(14))
C   
C      CCC
C   
C     ******************************************************************
C     THIS SUBROUTINE DOES THE ACTUAL @RAY-TRACING@ THROUGH THE
C     SPECTROMETER.
C     ******************************************************************
C   
      DER1=0.0
      DER2=0.0
      DO 1 I=1,2
      A=A-.01*(-1.)**I
      XI1PP=DASIN((RPP**2-RP**2)/(2.*A*RPP)+(RP*SXI1P/RPP))
      CXI1PP=DCOS(XI1PP)
      AA1=A-RP*SXI1P
      AA2=A-RPP*DSIN(XI1PP)
      R23=RP**2+A*A-2.*A*RP*SXI1P
      PHIP=DACOS(((AA1)*(AA2)-RP*RPP*CXI1P*CXI1PP)/R23)
      TPHIP=DTAN(PHIP)
      TPHIP2=DTAN(PHIP/2.)
      SPHIP=DSIN(PHIP)
      AP=A/.134
      X1=(A*TPHIP*TPHIP2-Y0+X0*TSTH)/(TPHIP+TSTH)
      Y1=(A*TPHIP2-X1)*TPHIP
      B=DSQRT((X1-A*TPHIP2)**2+Y1**2)-A*TPHIP2
      COS10=DCOS(DATAN(B/AP))-1.
      T34=DATAN(B/AP)
      PHIS=T34+(COS10/(COS(T34)*TAN(PHIP+STHETA+T34)))
      XISPP=1.309-PHIP-PHIS
      XI2P=XISPP
      SIN300 =A/SCOMEG
       CC1=155.925
      U1=DSIN(PHIP+PHIS+STHETA)
      CU1=1.-DCOS(PHIS)
      ARG=B+A*TPHIP2
      BB=CC1-(ARG)*SPHIP/DSIN(STHETA)-(AP*(CU1)/DCOS(PHIS))*U1
      XI2PP=DASIN(DSIN(COMEGA+XI2P)-BB/SIN300 )
      DER1=DER1-PHIS*(-1.)**I
      DER2=DER2-XI2PP*(-1.)**I
1     continue
      DER1=DER1/.01
      DER2=DER2/.01
      SIN100 =A/DSIN(PHIP-XI1P-XI1PP)
      G1P=CXI1P*DCOS(PHIP-XI1PP)*SIN100
      G1PP=CXI1PP*DCOS(PHIP-XI1P)*SIN100
      F1=CXI1P*CXI1PP*SIN100
      ZL1PP=F1*F1/(ZL1P-G1P)+G1PP
      CXI2P=DCOS(XI2P)
      SXI2P=DSIN(XI2P)
      SIN200 =AP/DSIN(PHIS+XI1PP+XISPP)
      GSP=CXI1PP*DCOS(PHIS+XISPP)*SIN200
      GSPP=DCOS(XISPP)*DCOS(PHIS+XI1PP)*SIN200
      FS=CXI1PP*DCOS(XISPP)*SIN200
      ZLSPP=FS*FS/(-ZL1PP-GSP)+GSPP
      CXI2PP=DCOS(XI2PP)
      SCOXI2=DSIN(COMEGA-XI2PP)
      PHIPP=COMEGA+XI2P-XI2PP
      G2P=CXI2P*DCOS(PHIPP+XI2PP)*SIN300
      G2PP=CXI2PP*DCOS(PHIPP-XI2P)*SIN300
      F2=CXI2P*CXI2PP*SIN300
      ZL2PP=F2*F2/(-ZLSPP-G2P)+G2PP
      DEXIT=(SCOXI2+SXI2P)*SIN300
      ZMH=-F1*FS*F2/((ZL1P-G1P)*(-ZL1PP-GSP)*(-ZLSPP-G2P))
      SXXP=DSIN(XI1P+XI1PP-PHIP)
      S12=SXXP
      DER=((-2.*A*(1.-DCOS(PHIP)))-RP*(RPP**2-RP**2)*S12/(2.*A*A*CXI1PP)
     1)/(A*A*SPHIP+RP*RPP*SXXP)
      SCO12=SCOXI2
      D=(SCO12+SXI2P-A*(DER+DER1)*CXI2P-A*DCOS(COMEGA-XI2PP)*DER2)*CXI2P
     1P/SCOMEG
      ANO=DSQRT(XM1A*XM3A*EIN/E3)/(RECOIL+XM3A)
      ANOK=ANO*DSIN(THETA)/(1.-ANO*DCOS(THETA))
      DK=(A*D*ZMH-ZL2PP*DTHETA)*ANOK
      DTFP=ZL2PP+DK
*     IF (IFLAG-1) 3,2,2
      if (iflag .ge. 1) then
 2       WRITE(7,4)
         WRITE(7,5)XI1PP,ZL1PP,XI2P,ZLSPP,XI2PP,ZL2PP,ZMH,DEXIT,DTFP
      endif
3     CONTINUE
      RETURN
C   
C   
4     FORMAT (1H0,8H XI1PP =,2X,8H ZL1PP =,2X,7H XI2P =,3X,8H ZLSPP =,2X
     1,8H XI2PP =,2X,8H ZL2PP =,2X,6H ZMH =,2X,8H DEXIT =,2X,7H DTFP =)
5     FORMAT (1H ,9F10.5)
      END
