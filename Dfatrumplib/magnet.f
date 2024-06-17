C$PROG MAGNET
      SUBROUTINE MAGNET
      IMPLICIT REAL*8(A-H,O-Z)
c     INTEGER ABSORB
      character*4 ABSORB
      COMMON/AAA/ V(4), NZ(30), CONST(50), ZZ(25)
      DIMENSION Q(10), EL(10), TABRD(150), TABCH(150), W(10)
      DIMENSION JRAK(6),TBACK(2)
      DIMENSION FXFUK(2)
      EQUIVALENCE (CONST(19),DEGRAD), (CONST(26),SE), (CONST(34),XA),
     1(NZ(9),JRAK(1))
C   
      EQUIVALENCE (SOLN2 ,ZZ(1)),
     &            (IFLAG ,ZZ(3)),
     &            (TWO   ,ZZ(13)),
     &            (ZMH   ,ZZ(14)),
     &            (OMEGA ,ZZ(15))
C   
      DIMENSION IATM(4)
C
      CHARACTER*4    IENGY
C
      CHARACTER*320  CLWD
C
      INTEGER*4      IWD(20),LWD(2,40),ITYP(40)
C
      EQUIVALENCE   (CLWD,LWD)
C
      DATA TRUE,FALSE/1.0,0.0/
C   
C   
      IMASS=0
      IMIN=1
      IMAX=80
      CALL CALBRA  (TABRD,TABCH,KMAX)
   40 CALLED = FALSE
      READ(5,55)JRAK
   55 FORMAT(6A4)
      READ(5,50)IWD
      CALL GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
      READ(CLWD,24)DTHETA,ZOUT,EIN,FLAG,IENGY,FXFUK
      READ(5,50)IWD
      CALL GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
      READ(CLWD,26)ANGI
      READ(5,50)IWD
      CALL GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
      READ(CLWD,26)FD,TL,TR,CAL
    1 IF(CALLED.NE.1.0)GO TO 52
      READ(5,55)JRAK
      READ(5,50)IWD
      CALL GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
      READ(CLWD,24)DTHETA,ZOUT,EIN,FLAG,IENGY,FXFUK
   52 CONTINUE
      ISOLN2=FXFUK(1)+0.5
      IA=FXFUK(2)+0.5
      IF (DTHETA.EQ.0.0) GO TO 17
      READ(5,50)IWD
      CALL GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
      READ(CLWD,18)TARGT,TBACK,TBACKT,ZTB,FXFUK(1),ABSORB
      NORDER=FXFUK(1)+0.5
      READ(5,50)IWD
      CALL GREAD(IWD,LWD,ITYP,NF,IMIN,IMAX,NTER)
      READ(CLWD,26)W
      CALLED = TRUE
      IFLAG=FLAG
      DTHET = DTHETA
      TARGB=2*TBACKT
      EI=EIN
      IF (ISOLN2.EQ.2) SOLN2= TRUE
      DO 2 I=1,10
      Q(I)=W(I)
      IF (W(I).NE.0.0) NQ=I
2     CONTINUE
      IF(W(1).EQ.0.0.AND.W(2).EQ.0.0) NQ = 1
      CALL PARTFI(JRAK,24,V,IATM,NZ,QGS,IFAIL,ISOR)
      IF (IFAIL.EQ.0) GO TO 3
      WRITE(7,28)JRAK
      GO TO 1
3     XM1A=V(1)
      XM2A=V(2)
      XM3A=V(3)
      XM4A=V(4)
      CALL ENERGY (Q,EL,IENGY,QGS,NQ)
      DTHETA=DTHETA*DEGRAD/2.
      T=XM3A*XA
      TERMX=SE/ZOUT
      THDEG=ANGI
      THETA=THDEG*DEGRAD
      BD=FD/4.2578
C   
      ZT = NZ(2)
      ZZ(7) = ZT
      ZZ(11) = ZTB
      ZPOUT = NZ(3)
      ZPIN = NZ(1)
      IF (TARGT.EQ.0.0) GO TO 4
      IF (NORDER.EQ.1.AND.TBACKT.NE.0.0) CALL ELOSS (EIN,ZPIN,XM1A,
     XZTB,TARGB,'SOLI')
c    XZTB,TARGB,4HSOLI)
      CALL ELOSS(EIN,ZPIN,XM1A,ZT,TARGT,ABSORB)
4     ETARG = EIN
      WRITE(7,19)JRAK,QGS,EI,ETARG,XM1A,TARGT,XM2A,TBACK(1),TBACK(2),
     1XM3A,TBACKT,XM4A,NORDER,ZOUT
      WRITE(7,22)BD,FD
      WRITE(7,20)TL,TR
      WRITE(7,21)THDEG,DTHET
C   
      IF(SOLN2.EQ.1.0) WRITE(7,33)
      IF(TWO.EQ.1.0) WRITE(7,32)
      WRITE(7,31)
      NOT=1
      IF (W(3).EQ.0.0.AND.W(10).GT.0.0) NOT=2
      GO TO 6
5     W(1)=W(1)+W(10)
      IF (W(1).GT.W(2).AND.IA.EQ.1) GO TO 40
      IF (W(1).GT.W(2).AND.IA.EQ.0) GO TO 1
      Q(1)=W(1)
      IF (IENGY.EQ.'EL  ') Q(1)=QGS-W(1)
      EL(1)=QGS-Q(1)
6     DO 16 J=1,NQ
      CALL RELKIN  (XM1A,XM2A,XM3A,XM4A,EIN,THETA,THCM3,CMTOLB,E31,Q(J),
     1IJ)
      IF (IJ.EQ.0.AND.IA.EQ.1) GO TO 40
      IF (IJ.EQ.0.AND.IA.EQ.0) GO TO 1
      IF (TARGT.EQ.0.0) GO TO 7
      CALL ELOSS(E31,ZPOUT,XM3A,ZT,TARGT,ABSORB)
      IF (NORDER.EQ.2.AND.TBACKT.NE.0.0) CALL ELOSS (E31,ZPOUT,XM3A,
     XZTB,TARGB,'SOLI')
c    XZTB,TARGB,4HSOLI)
7     RECOIL=XM1A+XM2A-XM3A-Q(J)/XA
      A=TERMX*DSQRT(E31**2+2.*T*E31)/BD
*      IF (A-30.) 8,9,9
      if (a .lt. 30.) then
 8       WRITE(7,29)Q(J)
         GO TO 15
      endif 
*9     IF (A-91.) 11,11,10
9     if (a .gt. 91.0) then
 10      WRITE(7,30)Q(J)
         GO TO 15
      endif
11    CALL OPTIC (A,E31,XM1A,XM3A,EIN,THETA,DTHETA,RECOIL,DTFP,DEXIT,XI2
     1PP,DK,ANOK)
      CALL DISTAN  (TL,TR,DEXIT,DTFP,XI2PP,RDIST,DELKIN)
      IF (RDIST.LT.TABRD(1).AND.CAL.EQ.0.0) GO TO 12
      IF (RDIST.GT.TABRD(KMAX).AND.CAL.EQ.0.0) GO TO 13
      GO TO 14
 12   WRITE(7,36)RDIST
      GO TO 15
13    WRITE(7,35)RDIST
      GO TO 15
14    CHAN=0.0
      IF(CAL.EQ.0.0) CALL CHANFI  (TABRD,TABCH,KMAX,RDIST,CHAN)
      DELTAD = 2*DELKIN*DTHETA/ZMH*DCOS(XI2PP+OMEGA)
      DELTAD = ABS(DELTAD)
      WRITE(7,34)Q(J),THCM3,CMTOLB,E31,A,ANOK,DK,RDIST,EL(J),CHAN,DELKIN
     X,DELTAD
15    CONTINUE
      IF (NOT.EQ.2) GO TO 5
16    CONTINUE
      IF(IA.EQ.1) GO TO 40
      GO TO 1
C   
C   
   18 FORMAT(E8.0,2A4,3E8.0,A4)
19    FORMAT (1H1,50X,31HOUTPUT FOR SUBROUTINE MAGNET   ,///,1H ,58X,8HR
     1EACTION,/,1H ,53X,6A4,/,51X,6H QGS =,F10.5,5H  MEV,///,41H KINEMAT
     2IC VARIABLES * * * * EBOMB(LAB) =,F10.5,5H  MEV,10X,34HENERGY LOSS
     3 INFO * * * * ETARGET =,F10.5,5H  MEV/,37X,5H M1 =,F10.5,5H  AMU,2
     46X,18HTARGET THICKNESS =,F10.5,8HMG/SQ-CM,/,37X,5H M2 =,F10.5,5H
     5AMU,28X,16HTARGET BACKING =,2A4,/,37X,5H M3 =,F10.5,5H  AMU,24X,20
     6H BACKING THICKNESS =,F10.5,8HMG/SQ-CM,/,37X,5H M4 =,F10.5,5H  AMU
     7,29X,15HBACKING ORDER =,I4,/,36X,6HZOUT =,F5.0,/)
20    FORMAT (//,42X,21H  LEFT DIAL READING =,F10.5,/,42X,21H RIGHT DIAL
     1 READING =,F10.5)
21    FORMAT (//,42X,21H REACTION LAB ANGLE =,F10.5,5H  DEG,/,53X,10HAPE
     1RTURE =,F10.5,5H  DEG,//)
22    FORMAT (/,48X,15H MAGNET FIELD =,F10.5,11H  KILOGAUSS,/,51X,12H FR
     1EQUENCY =,F10.5,11H  MEGAHERTZ,//)
   24 FORMAT(4E8.0,A4,4X,2E8.0)
   26 FORMAT(10E8.0)
28    FORMAT (14H1FINDER FAILED,10X,6A4)
29    FORMAT (1H ,17H LESS THAN 30. CM,F9.5)
30    FORMAT (1H ,17H MORE THAN 91. CM,F9.5)
31    FORMAT (1H ,8H Q VALUE,1X,10H THETA3 CM,2X,7H CMTOLB,2X,7H E3 LAB,
     14X,4H RHO,2X,9H .5DE/EDT,10H KIN SHIFT,3X,6H RDIST,3X,7H ELEVEL,1X
     2,12H CHANNEL NO.,1X,7H DELKIN,9H  DELTA D ,/)
32    FORMAT (54H TWO SOLN TO RELKIN***PUNCH A 2 IN COL 73 OF JRAK CARD/
     1)
33    FORMAT (38H CALCULATION FOR SECOND SOLN TO RELKIN,//)
   34 FORMAT(1H ,F9.4, F9.3, F9.4, 2F10.4, F9.4, 3F10.4, F10.2,
     1 2F10.4)
35    FORMAT (9H RDIST = ,F10.4,33H IS TO THE LEFT OF THE DETECTOR  )
36    FORMAT (9H RDIST = ,F10.4,33H IS TO THE RIGHT OF THE DETECTOR )
   50 FORMAT(20A4)
   17 RETURN
      END
