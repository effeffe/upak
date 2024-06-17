C$PROG DISTAN
      SUBROUTINE DISTAN  (XL,XR,DEXIT,DTFP,XI2PP,RDIST,DELKIN)
      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/AAA/ Z(4), NZ(30), CONST(50), ZZ(25)
      EQUIVALENCE (CONST(2),ACONST), (CONST(3),AK0), (CONST(4),ALP), (CO
     1NST(5),AM), (CONST(6),AN), (CONST(9),BETA), (CONST(10),BL), (CONST
     2(13),CL), (CONST(17),DB), (CONST(21),DL0), (CONST(31),TLC), (CONST
     3(32),TRC)
C   
C   
      TL=XL-TLC
      TR=XR-TRC
      T1=1./DTAN(BETA)
      AA=ACONST*BL*T1+ALP
      BB=-(ACONST*(CL-DB*T1)-DB)
       CC=-(AN*TL-DL0+AK0-AN*TR-ACONST*(DL0*T1-AN*TL*T1+AM))
      AAA=AA*AA+BB*BB
      OMEGA=DASIN((-AA*CC-DSQRT(BB**4-CC*CC*BB*BB+AA*AA*BB*BB))/AAA)
      DLP=DL0-DB*DCOS(OMEGA)-AN*TL
      TRIG=DSIN(OMEGA)-DCOS(OMEGA)/DTAN(XI2PP)
      FAC3=AM-BL*DCOS(OMEGA)+BL*DSIN(OMEGA)*T1
      DLHP=DLP*T1+FAC3
      DN=(DLHP-DEXIT+34.6)/DTAN(XI2PP)-23.124-DLP
      RDIST=ALP-DN/TRIG
      DTFPP=((ALP-RDIST)*DSIN(OMEGA)+23.124+DLP)/DCOS(XI2PP)
      DELKIN=DTFPP-DTFP
      RETURN
      END
