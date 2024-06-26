C$PROG ELOSS
      SUBROUTINE ELOSS (EBOMB,ZP,MP,DZTAR,TARGT,ABSORB)
      REAL*8 EBOMB,ZP,MP,DZTAR,TARGT
      INTEGER*4 KON(18)
C      CCC
      DIMENSION IBUF(192),XBUF(192)
      DIMENSION XVAL(76,3), YVAL(76,3), ANAME(3), CHGA(3), YVALMD(76)
      EQUIVALENCE (IBUF(1),YVAL(1,1)),(XBUF(1),YVAL(1,1)),
     1 (KON(1),A),(KON(2),B),(KON(3),C),(KON(4),D),(KON(5),E),
     2 (KON(6),ICODE),(KON(7),AMUI),(KON(8),BNAME),(KON(9),CHGI),
     3 (KON(10),AG),(KON(11),BG),(KON(12),CG),(KON(13),DG),
     4 (KON(14),EG),(KON(15),ICODG),(KON(16),AMUIG),
     5 (KON(17),BNAMEG),(KON(18),CHGIG)
      DIMENSION ZT(12), EAMU(38), AMUA(3), ZTG(12)
C     INTEGER*4   ABSORB
      character*4   ABSORB
C
      CHARACTER*4 KINDAB(5)
C
      DATA  (ZTG(I),I=1,12)/1.,4.,7.,8.,10.,18.,36.,54.,86.,0.,0.,0./
      DATA  (ZT(I),I=1,12)/4.,6.,13.,22.,28.,32.,40.,47.,63.,73.,79.,92.
     1/
      DATA  (EAMU(I),I=1,38)/.0125,.016,.02,.025,.032,.04,.05,.06,.07,.0
     18,.09,.1,.125,.16,.2,.25,.32,.4,.5,.6,.7,.8,.9,1.,1.25,1.6,2.,2.5,
     23.2,4.,5.,6.,7.,8.,9.,10.,11.0,12.0/
C
      KINDAB(1)='SOLI'
      KINDAB(2)='GAS '
      KINDAB(3)='MYLA'
      KINDAB(4)='CH2 '
      KINDAB(5)='WATE'
C     *********
C   
C     READ IN THE RATIO CARDS FOR 24 ABSORBERS AND THE COEFFICIENT
C     CARDS FOR AS MANY IONS AS DESIRED (TWO CARDS FOR EACH ION)
C   
C     *********
      NCARDS=0
      ZTARG=DZTAR
      M=1
      DO 101 L=1,3
      XVAL(1,L)=EAMU(1)
      DO 100 I=3,75,2
      J=(I-1)/2+1
      XVAL(I,L)=EAMU(J)
100   CONTINUE
101   CONTINUE
      LU=1
      DO 8002 I=1,5
      IF(ABSORB.EQ.KINDAB(I)) GO TO 8004
 8002 CONTINUE
      STOP 111
 8004 IGO=I
      GO TO (8006,8008,8030,8040,8050),IGO
 8006 JMAX=12
      JAKOF=0
      GO TO 8010
 8008 JMAX=9
      JAKOF=12
      DO 8009 I=1,12
         ZT(I)=ZTG(I)
 8009 continue
 8010 DO 8012 J=2,JMAX
*      IF(ZT(J)-ZTARG)8012,8016,8014
      if (zt(j) .eq. ztarg) then
         goto 8016
      elseif (zt(j) .gt. ztarg) then
         goto 8014
      endif
 8012 CONTINUE
 8014 IF (ZTARG.LT.((ZT(J)+ZT(J-1))/2)) J=J-1
 8016 CONTINUE
      IF(J.LT.2) J=2
      IF(J.GE.JMAX) J=JMAX-1
      NSKIP=JAKOF+J-2
      GO TO 8060
 8030 NSKIP=21
      GO TO 8060
 8040 NSKIP=24
      GO TO 8060
 8050 NSKIP=27
 8060 NWN=41*NSKIP+1
      ISEC=(NWN-1)/64
      N=NWN-64*ISEC-1
      CALL BUFLOD(IBUF,768,ISEC)
      DO 8070 L=1,3
      DO 8065 J=2,76,2
      N=N+1
      XVAL(J,L)=XBUF(N)
 8065 CONTINUE
      CHGA(L)=XBUF(N+1)
      AMUA(L)=XBUF(N+2)
      ANAME(L)=XBUF(N+3)
      N=N+3
 8070 continue
      KK=1
      NREAD=ZP+0.5
      LL=37
      NWN=30*41+18*(NREAD-1)+1
      ISEC=(NWN-1)/64
      N=NWN-64*ISEC
      CALL BUFLOD(IBUF,512,ISEC)
      DO 8075 J=1,18
      KON(J)=IBUF(N)
      N=N+1
 8075 continue
      IAMUI=AMUI
      AAMUI=IAMUI
*      IF (AMUI-AAMUI-0.5) 11,10,10
      if ((amui-aamui-0.5) .ge. 0.0) then
10       IAMUI=IAMUI+1
      endif
11    CONTINUE
C     *********
C   
C     FIND THE STOPPING POWER IN ALUMINUM AS A FUNCTION
C     OF THE ENERGY PER NUCLEON
C   
C     *********
    9 IF(LL.NE.75) GO TO 8888
      A=AG
      B=BG
      C=CG
      D=DG
      E=EG
      ICODE=ICODG
      AMUI=AMUIG
      BNAME=BNAMEG
      CHGI=CHGIG
 8888 A3=1.0/(2.0*E)
      A1=-D*A3
      A2=-C*A3
      A4=C*C-4.0*E*A
      A5=2.0*C*D-4.0*E*B
      A6=D*D+4.0*E
      DO 20 I=KK,LL,2
      XX=XVAL(I,1)
      X=ALOG10(XX*100.0)
*     IF (ICODE-1) 14,14,13
      if (icode .gt. 1) then
13       Y=A1+A2*X+A3*SQRT((A4*X+A5)*X+A6)
*      GO TO 15
      else
14       Y=A1+A2*X-A3*SQRT((A4*X+A5)*X+A6)
      endif
15    EXX=Y-2.0
      YVALMD(I)=(10.0**EXX)
*      IF (I-1) 20,20,16
      if (i .gt. 1) then
16       XX=(XVAL(I,1)+XVAL(I-2,1))*0.5
         X=ALOG10(XX*100.0)
*         IF (ICODE-1) 18,18,17
         if (icode .gt. 1) then 
17          Y=A1+A2*X+A3*SQRT((A4*X+A5)*X+A6)
*         GO TO 19
         else
18          Y=A1+A2*X-A3*SQRT((A4*X+A5)*X+A6)
         endif
19       EXX=Y-2.0
         YVALMD(I-1)=(10.0**EXX)
      endif
20    CONTINUE
*      IF (KK-1) 32,21,22
      if (kk .eq. 1) then
21       KK=39
         LL=75
         GO TO 9
      else if (kk .lt. 1) then
         goto 32
      endif
22    CONTINUE
C     *********
C   
C     MODIFY THE SLOPE OF THE INITIAL PORTION OF EACH
C     ALUMINUM STOPPING POWER CURVE
C   
C     *********
      YMOD=-23.3394+SQRT(106.*106.-(100.-CHGI)*(100.-CHGI))
      EMOD=(10.**(YMOD/50.))/100.
      DO 24 JMD=1,75,2
*     IF (XVAL(JMD,1)-EMOD) 24,23,23
         if (xval(jmd,1) .ge. emod) then
23          JMOD=JMD
            GO TO 25
         endif
24    CONTINUE
25    IF (CHGI.GE.20.) SLOPE=.43865+SQRT(.04202-(CHGI/1000.-.21402)**2)
      IF (CHGI.LT.20.) SLOPE=0.5
      XINCPT=ALOG10(YVALMD(JMOD))-SLOPE*ALOG10(XVAL(JMOD,1))
      KMOD=JMOD-2
      DO 26 JMD=1,KMOD,2
      YVALMD(JMD)=10.**(SLOPE*ALOG10(XVAL(JMD,1))+XINCPT)
      XMOD=(XVAL(JMD,1)+XVAL(JMD+2,1))*0.5
      YVALMD(JMD+1)=10.**(SLOPE*ALOG10(XMOD)+XINCPT)
26    CONTINUE
C     *********
C   
C     MULTIPLY ALUMINUM STOPPING POWER CURVE
C     BY EACH ABSORBER RATIO CURVE
C   
C     *********
      DO 271 L=1,3
      YVAL(1,L)=YVALMD(1)*XVAL(2,L)
      DO 270 I=3,75,2
      YVAL(I,L)=YVALMD(I)*XVAL(I+1,L)
      YVAL(I-1,L)=YVALMD(I-1)*(XVAL(I+1,L)+XVAL(I-1,L))*0.5
270   CONTINUE
271   CONTINUE
C     *********
C   
C     ELIMINATE THE 1/Z SQ FACTOR FROM THE STOPPING POWER
C   
C     *********
      AAA=CHGI*CHGI
      DO 281 L=1,3
      DO 280 I=1,75
      YVAL(I,L)=YVAL(I,L)*AAA
280    CONTINUE
281    CONTINUE
      IZ=CHGI
C     *********
C   
C     CALCULATE ENERGY LOSS USING THE STOPPING POWER
C   
C     *********
      EPAMU=EBOMB/MP
      DO 29 I=3,75,2
*      IF (XVAL(I,1)-EPAMU) 29,31,30
         if (xval(i,1) .gt. epamu) then 
            goto 30
         else if (xval(i,1) .eq. epamu) then
            goto 31
         endif
29    CONTINUE
30    IF (EPAMU.LT.((XVAL(I,1)+XVAL(I-2,1))/2)) I=I-2
31    CONTINUE
      I2=I
      IF (I2.LT.3) I2=3
      IF (I2.GT.73) I2=73
      I1=I2-2
      I3=I2+2
      DO 9002 I = 1,5
      IF(ABSORB.EQ.KINDAB(I)) GO TO 9004
 9002 CONTINUE
 9004 IGT = I
      GO TO (808,808,809,809,809),IGT
  808 R1=FIT3(CHGA(1),CHGA(2),CHGA(3),YVAL(I1,1),YVAL(
     XI1,2)  ,YVAL(I1,3)  ,ZTARG)
      R2=FIT3(CHGA(1),CHGA(2),CHGA(3),YVAL(I2,1),YVAL(
     XI2,2)  ,YVAL(I2,3)  ,ZTARG)
      R3=FIT3(CHGA(1),CHGA(2),CHGA(3),YVAL(I3,1),YVAL(
     XI3,2)  ,YVAL(I3,3)  ,ZTARG)
      GO TO 810
  809 R1 = YVAL(I1,1)
      R2 = YVAL(I2,1)
      R3 = YVAL(I3,1)
  810 RT=FIT3(XVAL(I1,1),XVAL(I2,1),XVAL(I3,1),R1,R2,R3,EPAMU)
      ETARG=EBOMB-RT*(TARGT/2.0)
      EBOMB=ETARG
C   
C   
      RETURN
32    STOP
C   
      END
