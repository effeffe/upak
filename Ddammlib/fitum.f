C$PROG FITUM     - Main peak fitting setup & management routine
C
C     ******************************************************************
C     BY WT MILNER AT HHIRF - LAST MODIFIED 02/12/2003
C     ******************************************************************
C
      SUBROUTINE FITUM(IERR)
C
C     ------------------------------------------------------------------
      COMMON/LLL/ MSSG(28),NAMPROG(2),LOGUT,LOGUP,LISFLG,MSGF
      INTEGER*4   MSSG,NAMPROG,LOGUT,LOGUP
      CHARACTER*4 LISFLG,MSGF
      CHARACTER*112 CMSSG
      EQUIVALENCE (CMSSG,MSSG)
C     ------------------------------------------------------------------
      COMMON/ML01/ IWD(20),LWD(2,40),ITYP(40),NF,NTER
      INTEGER*4    IWD,    LWD,      ITYP,    NF,NTER
C     ------------------------------------------------------------------
      COMMON/ML02/ IWDRAW(20)
      INTEGER*4    IWDRAW
C     ------------------------------------------------------------------
      COMMON/DML1/ NAMFIL(20,20),KFIL(20),LUC(20),LIN,LCM,LCI
      INTEGER*4                           LUC,    LIN,LCM,LCI
      CHARACTER*4  NAMFIL,       KFIL
C     ------------------------------------------------------------------
      COMMON/SM01/ I1,I2,NPK,NCOMP,IFBGD,QFN,QFLO,NVLU,NVLUI,DXFAC
C   
      COMMON/SM02/COMP(2048,44),YCAL(2048),BGD(2048),DATA(2048),WT(2048)
C   
      COMMON/SM03/ XP(4,44),XPSAV(4,44),IPO(176),JPO(176)
C   
      COMMON/SM04/ KXF(4,44),KFUN(44),BETA(50),IVF(4)
C   
      COMMON/SM06/ NUMITS,ICON
C     ------------------------------------------------------------------
      COMMON/SM11/ MDYHMS(5),KINFD,MINSY,MAXSY
      CHARACTER*4            KINFD
      CHARACTER*20 CMDYHMS
      EQUIVALENCE (MDYHMS,CMDYHMS)
C     -----------------------------------------------------------------
      COMMON/SM05/ PAT(500,15),NPAT,MAXPAT,NUPAT,FWA,FWB,FWC,ASLO,ASHI
C   
      COMMON/SM09/ XYP(50,2),NBXY
C   
C     ------------------------------------------------------------------
      COMMON/SM08/ MARFS(4,20),JLOX,JHIX,JLOF,JHIF,SLOX,SHIX,SLOF,SHIF
C
      CHARACTER*4  MARFS
      INTEGER*4                JLOX,JHIX,JLOF,JHIF
      REAL*4                                       SLOX,SHIX,SLOF,SHIF
C     ------------------------------------------------------------------
      COMMON/MAINV8/ A(50,50),B(50,50),DET,IFS
      REAL*8         A,       B,       DET
C     ------------------------------------------------------------------
      COMMON/SM10/ ERR(18),ISAV,KFIT
C     ------------------------------------------------------------------
      COMMON/SM12/ LTITL(20)
      CHARACTER*80 CTITL
      EQUIVALENCE (CTITL,LTITL)
C     ------------------------------------------------------------------
      DIMENSION    DAT(2048,3),TP(4)
      INTEGER*4    LAT(500,15)
C     ------------------------------------------------------------------
C   
      COMMON/SM15/ IXC(50),YC(50),XCOR(50)
C   
      COMMON/SM16/ IHOLF(4,44),JPU(44),XOR(44)
C     ------------------------------------------------------------------
      COMMON/SM17/ IORD(500),ITEM(500),LOUXX(14,50)
      CHARACTER*56                    CLOUXX(50)
      EQUIVALENCE (CLOUXX,LOUXX)
C     ------------------------------------------------------------------
      COMMON/SM18/ PLIM(352),GUESS(176),PARV(176)
C     ------------------------------------------------------------------
      COMMON/SM19/ ISKIP(2,4),JSKIP(2,4),PMIN(4),PMAX(4),KVAR(4)
      CHARACTER*4                                        KVAR
C     ------------------------------------------------------------------
      COMMON/SM20/ RESUL(12,44),FOUT(50),ITRITL(10)
C     ------------------------------------------------------------------
      COMMON/SM21/ NDXI(4),JDATE(3),JTIME(2),TITLE(20)
      INTEGER*4                              TITLE
C     ------------------------------------------------------------------
      COMMON/SM22/ IDATA(2048),IDAT(2048,3),ISKF(2048)
C   
C     ------------------------------------------------------------------
      COMMON/SM23/ JYLOG,JDSP,KDSP,MXDL,MXFR,MAXH,
     &             NOP,NBC,KINBAK,MARKS,ECA,ECB,ECC,
     &             DEL,DELFAC,NMUL,XSTEP,DXMAX,FWLO,FWHI,FALO,FAHI,
     &             MAXNC,MAXVP,KFUNS,NWOOD,KPPL,ASUM,ID,NSKIP,RESOK,
     &             ILO,IHI,SETW
      CHARACTER*4  JDSP,KDSP,KINBAK,NWOOD,KPPL,MARKS,SETW,RESOK
C     ------------------------------------------------------------------
      COMMON/SM26/ KSORFIT, KFITFIL(20)
      CHARACTER*4  KSORFIT, KFITFIL
      CHARACTER*80 CKFITFIL
      EQUIVALENCE (CKFITFIL, KFITFIL)
C     ------------------------------------------------------------------
      COMMON/SM28/ RELI(44),NREL,NPKK,KRELF
      CHARACTER*4                     KRELF
C     ------------------------------------------------------------------
C
      CHARACTER*4  KMD
C   
      EQUIVALENCE (DAT,IDAT)
      EQUIVALENCE (LAT(1,1),PAT(1,1))
      EQUIVALENCE (KMD    ,LWD(1,1))
C   
      DATA         PCE/0.0/
      DATA         CTITL/' '/
C   
      SAVE
C
C     ------------------------------------------------------------------
C   
      IERR=0
C
      IF(KMD.EQ.'RFIT') GO TO 130
      IF(KMD.EQ.'FIT ') GO TO 140
      IF(KMD.EQ.'LFIT') GO TO 150
      IF(KMD.EQ.'RLFI') GO TO 150
      IF(KMD.EQ.'GFIT') GO TO 160
C   
  130 ERR(18)=-1.0                !SET RESUME-FIT-FLAG FOR GFIT
      GO TO (140,150,160),KFIT    !RESUME PREVIOUS FIT PER KFIT
C   
  140 KFIT=1
      RESOK='NO  '
      GO TO 170
C   
  150 KFIT=2
      RESOK='NO  '
      GO TO 170
C   
  160 KFIT=3
      RESOK='NO  '
C   
  170 MSGF='    '
C
      CALL SORGET(LWD(1,2),KSORFIT,IOF)
C
      IF(NF.EQ.IOF+1) GO TO 200
      CALL MILV(LWD(1,IOF+2),ID,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      IF(NF.EQ.IOF+2) GO TO 200
      IF(NF.EQ.IOF+3.AND.ITYP(IOF+3).EQ.1) GO TO 180
      CALL MILV(LWD(1,IOF+3),ILO,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
      CALL MILV(LWD(1,IOF+4),IHI,XV,KIND,IERR)
      IF(IERR.NE.0) GO TO 3000
C   
      GO TO 200
C   
  180 ILO=JLOF
      IHI=JHIF
C   
  200 IF(ILO.LT.0.OR.ILO.GE.IHI) GO TO 3000
      IF(IHI-ILO.GE.MXFR-1) GO TO 3000
C   
C     ------------------------------------------------------------------
C     SET UP PARM "LIMITS" (MIN AND MAX FRACTION TO CHANGE)
C     ------------------------------------------------------------------
C   
      DXFAC=XSTEP/DEL
      PMAX(1)=DXMAX/DXFAC
      PMIN(1)=-PMAX(1)
      PMIN(2)=FWLO
      PMAX(2)=FWHI
      PMIN(3)=FALO
      PMAX(3)=FAHI
      PMIN(4)=PMIN(3)
      PMAX(4)=PMAX(3)
C   
C     TEST FOR USE OF PREVIOUSLY DETERMINED PARMS REQUEST **********
C   
      IF(KMD.EQ.'RFIT'.OR.KMD.EQ.'RLFI') GO TO 235
C   
C     DETERMINE WHICH PEAKS FALL INTO SPECIFIED RANGE **************
C   
      NPK=0
      NUPAT=0
      XLO=ILO
      XHI=IHI
C   
C     LOAD PEAK POSITIONS FROM "PAT" INTO "XP" *********************
C   
      IF(NPAT.GT.0) GO TO 208
  202 WRITE(CMSSG,203)
  203 FORMAT('NO PEAKS WITHIN RANGE OF FIT            ')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
  208 NREL=0
      DO 230 J=1,NPAT
      IF(LAT(J,10).EQ.0) GO TO 230
      IF(PAT(J,1).LT.XLO.OR.PAT(J,1).GT.XHI) GO TO 230
      NPK=NPK+1
      IF(NPK.GT.MAXNC) NPK=MAXNC
      JPU(NPK)=J
      RELI(NPK)=0.0
      IF(KRELF.NE.'OFF ')  RELI(NPK)=PAT(J,13)
      IF(RELI(NPK).GT.0.0) NREL=NREL+1
      DO 210 I=1,4
      XP(I,NPK)=PAT(J,I)
      IHOLF(I,NPK)=LAT(J,I+5)
  210 CONTINUE
      XP(2,NPK)=XP(2,NPK)/1.66478
C   
      KFUN(NPK)=LAT(J,5)
      IF(KFUN(NPK).LE.0) KFUN(NPK)=KFUNS
C   
C     DETERMINE WHICH PARMS ARE TO BE VARIED - "PERIOD" ************
C   
      DO 220 I=1,4
      IF(KVAR(I).EQ.'UIND') GO TO 212
      IF(KVAR(I).EQ.'ULOC') GO TO 212
      IF(KVAR(I).EQ.'FIX ') GO TO 214
      IF(IHOLF(I,NPK).GT.0) GO TO 214
  212 KXF(I,NPK)=0
      GO TO 220
  214 KXF(I,NPK)=1
  220 CONTINUE
  230 CONTINUE
C
      CALL RELINOR
C   
C     SET UP "INDEPENDENT VARATION" FLAGS **************************
C   
  235 IF(NPK.LT.1) GO TO 202
      DO 240 I=1,4
      IVF(I)=0
      IF(KVAR(I).EQ.'UIND'.OR.KVAR(I).EQ.'CIND') IVF(I)=1
      IF(KVAR(I).EQ.'FIX ') IVF(I)=1
  240 CONTINUE
C   
C     SET UP POINTERS FOR PARMS WHICH ARE TO VARY INDEPENDENTLY ****
C   
      K=0
      DO 260 J=1,NPK
      DO 250 I=1,4
      IF(IVF(I).EQ.0.OR.KXF(I,J).EQ.1) GO TO 250
      K=K+1
      IPO(K)=I
      JPO(K)=J
  250 CONTINUE
  260 CONTINUE
      NVLU=K
C   
C     SET UP THOSE PARMS WHICH ARE TO VARY "TOGETHER" **************
C   
      NVLUI=NVLU
      DO 270 I=1,4
      IF(IVF(I).EQ.0) NVLU=NVLU+1
  270 CONTINUE
C   
C     SET UP INITIAL GUESS AND LIMITS ******************************
C   
      IF(NVLUI.LT.1) GO TO 290
      DO 280 I=1,NVLUI
      IP=IPO(I)
      IF(IP.EQ.1) GUESS(I)=0.0
      IF(IP.GT.1) GUESS(I)=1.0
      PLIM(I)=PMIN(IP)
      N=NVLU+I
      PLIM(N)=PMAX(IP)
  280 CONTINUE
  290 NDX=NVLUI
      DO 300 I=1,4
      IF(IVF(I).EQ.1) GO TO 300
      NDX=NDX+1
      PLIM(NDX)=PMIN(I)
      IF(I.EQ.1) GUESS(NDX)=0.0
      IF(I.GT.1) GUESS(NDX)=1.0
      N=NVLU+NDX
      PLIM(N)=PMAX(I)
  300 CONTINUE
C   
C     ------------------------------------------------------------------
C     SHIFT PEAK POSITIONS SUCH THAT FIRST CHAN OF RANGE CORRESPONDS TO
C     ------------------------------------------------------------------
C   
      K1=ILO
      K2=IHI
      IF(K2-K1.GE.MXFR) K2=K1+MXFR-1
      I1=1
      I2=K2-K1+1
      ISHIFT=ILO-1
      SHIFT=ISHIFT
      DO 310 J=1,NPK
      XOR(J)=XP(1,J)
      XP(1,J)=XP(1,J)-SHIFT
  310 CONTINUE
C   
C     ALSO SHIFT "SKIP" POINTERS  **********************************
C   
      DO 330 J=1,4
      DO 320 I=1,2
      JSKIP(I,J)=ISKIP(I,J)
      IF(ISKIP(I,J).NE.0) JSKIP(I,J)=JSKIP(I,J)-ISHIFT
  320 CONTINUE
  330 CONTINUE
C   
      DO 340 J=1,NBXY
      XCOR(J)=XYP(J,1)
      IXC(J)=XYP(J,1)-SHIFT+0.5
      YC(J)=XYP(J,2)
  340 CONTINUE
C   
C     SAVE INITIAL VALUES OF PARAMETERS ****************************
C   
      DO 360 J=1,NPK
      DO 350 I=1,4
      XPSAV(I,J)=XP(I,J)
  350 CONTINUE
  360 CONTINUE
C   
C     READ IN THE DATA *********************************************
C   
      NDXI(1)=K1+1
      IERR=0
      NCHD=NDXI(1)+I2
      CALL SPKINS(KSORFIT,ID,IDATA,NDXI,I2,NCHD,IERR)
      IF(IERR.NE.0) RETURN
      IF(K1+I2.GT.NCHD) GO TO 430
      GO TO 435
C   
  430 WRITE(CMSSG,432)
  432 FORMAT('INVALID RANGE OF DATA       ',3(4H    ))
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
C     LOAD IDATA INTO DATA (I.E. FLOAT IT) *************************
C   
  435 NUMCTS=0
      DO 440 K=1,I2
      DATA(K)=IDATA(K)
      NUMCTS=NUMCTS+IDATA(K)
  440 CONTINUE
      IF(NUMCTS.GT.2) GO TO 444
      WRITE(CMSSG,441)
  441 FORMAT('DATA CONTAINS LESS THAN 3 COUNTS        ')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C   
C     ------------------------------------------------------------------
C     DETERMINE BACKGROUND TYPE AND SUBTRACT FIXED BGD IF REQUESTED
C     ------------------------------------------------------------------
C   
  444 IFBGD=0
      IF(KINBAK.EQ.'FIX '.AND.NBXY.GT.1) IFBGD=1
      IF(IFBGD.EQ.0) GO TO 500
      NDO=NBXY-1
      DO 445 I=I1,I2
      BGD(I)=0.0
  445 CONTINUE
      DO 460 J=1,NDO
      KDELX=IXC(J+1)-IXC(J)
      IF(KDELX.LT.1) GO TO 460
      DYDX=(YC(J+1)-YC(J))/FLOAT(KDELX)
      YO=YC(J)
      IIX=IXC(J)-1
      KDO=KDELX+1
      DO 450 I=1,KDO
      IIX=IIX+1
      IF(IIX.LT.I1.OR.IIX.GT.I2) GO TO 450
      BGD(IIX)=YO+DYDX*FLOAT(I-1)
  450 CONTINUE
  460 CONTINUE
C
      DO 470 I=I1,I2
      DENO=ABS(DATA(I))
      IF(DENO.LT.1.0) DENO=1.0
      WT(I)=1.0/DENO
      DATA(I)=DATA(I)-BGD(I)
  470 CONTINUE
      GO TO 512
C
  500 DO 510 I=I1,I2
      DENO=ABS(DATA(I))
      IF(DENO.LT.1.0) DENO=1.0
      WT(I)=1.0/DENO
  510 CONTINUE
C   
C     ------------------------------------------------------------------
C     SET WT(I) TO ZERO FOR ANY CHANNELS TO BE OMITTED FROM FIT
C     ------------------------------------------------------------------
C   
  512 NSKIP=0
      DO 520 J=1,4
      IA=JSKIP(1,J)
      IB=JSKIP(2,J)
      IF(IA.LT.I1) IA=I1
      IF(IB.GT.I2) IB=I2
      IF(IA.GT.IB) GO TO 520
      NSKIP=NSKIP+1
      DO 515 I=IA,IB
      WT(I)=0.0
  515 CONTINUE
  520 CONTINUE
C   
C     SET UP ANY VARIABLE BACKGROUND COMPONENTS ********************
C   
      N=NPK
      IF(NREL.GT.1) N=NPK-NREL+1
      NPKK=N
      IF(NBC.LT.1.OR.IFBGD.GT.0) GO TO 532
      DO 530 K=1,NBC
      N=N+1
      IEX=K-1
      DO 525 I=I1,I2
      COMP(I,N)=I**IEX
  525 CONTINUE
  530 CONTINUE
  532 NCOMP=N
C
      IF(NCOMP.GT.30) GO TO 2000
C
      IF(NWOOD.EQ.'OFF ') GO TO 550
C
      CALL PKFIT
C
      BETSUM=0.0
      NCOMP=NCOMP+1
C
      IF(NCOMP.GT.30) GO TO 2000
C
      N=NCOMP
C
      DO 533 I=1,NPK
      CALL GETBETA(I,JDUM,BETAJ,ADUM)
      BETSUM=BETSUM+BETAJ
  533 CONTINUE
C
      DO 534 I=I1,I2
      COMP(I,N)=0.0
  534 CONTINUE
C
      DO 545 NN=1,NPK
      DISPL=-XP(1,NN)
      WOOD=0.3535*XP(2,NN)
      CALL GETBETA(NN,JDUM,BETAJ,ADUM)
      WOODF=BETAJ/BETSUM
      DO 540 I=I1,I2
      XG=I
      EXG=(XG+DISPL)/WOOD
      IF(EXG.GT. 30.0) GO TO 540
      IF(EXG.LT.-30.0) GO TO 536
      COMP(I,N)=COMP(I,N)+WOODF/(1.0+EXP(EXG))
      GO TO 540
  536 COMP(I,N)=COMP(I,N)+WOODF
C
  540 CONTINUE
  545 CONTINUE
C   
C     ------------------------------------------------------------------
C     DO THE NON-LINEAR DRILL
C     ------------------------------------------------------------------
C   
  550 FOFX=0.0
      NUMITS=0
C   
      WRITE(CMSSG,555)(IWD(I),I=1,7)
  555 FORMAT(1H>,7A4,'FITTING                        *')
      CALL MESSLOG1(LOGUT,LOGUP)
C   
      GO TO (562,564,566),KFIT
C   
  562 CALL SMIN(NVLU,NMUL,DEL,DELFAC,PLIM,GUESS,PARV,FOFX)
      QFN=FOX(PARV)
      GO TO 570
C   
  564 CALL PKFIT
      GO TO 570
C   
  566 CALL GFITT(NBC,KVAR,YO,DYDX,ERR,FOFX,ISAV)
      IF(FOFX.LE.0.0) THEN
                      IERR=1
                      RETURN
                      ENDIF
C
      DO 568 J=1,NPK
         TP(1)=XP(1,J)
         TP(2)=XP(2,J)
         TP(3)=0.0
         TP(4)=0.0
         CALL SFUNK(1,1,I1,I2,TP,COMP(1,J))
 568  CONTINUE
C
      QFN=FOFX
C     
 570  CALL MILDATE2(JDATE)
      CALL MILTIME(JTIME)
C     
      WRITE(CMDYHMS,575)JDATE,JTIME
 575  FORMAT(2A4,A1,1X,2A4,'  ')
C     
      IF(KSORFIT.EQ.'M   ') THEN
         CTITL=' '
         DO 576 I=1,20
            KFITFIL(I)='    '
 576     CONTINUE
         WRITE(CTITL,577)(IWD(I),I=1,5),ID,CMDYHMS,QFN
 577     FORMAT(' ',5A4,'  Memory-Buffer-',I1,'    ',A18,
     &        'QFN=',F7.2)
cgenerates runtime errors
         WRITE(CKFITFIL,578) ID 
 578     FORMAT('Memo','ry-B','uffe','r-  ',I1)
         GO TO 586
      ENDIF
C     
      CALL LUGET(KSORFIT,IDUM,JFI,IDUM,IERR)
      JFI=JFI-1
      WRITE(CTITL,580)(IWD(I),I=1,5),(NAMFIL(I,JFI),I=1,5),
     &     CMDYHMS,QFN
 580  FORMAT(' ',5A4,5A4,A18,' QFN=',F7.2)
C     
      DO 585 I=1,20
         KFITFIL(I)=NAMFIL(I,JFI)
 585  CONTINUE
C     
C     ------------------------------------------------------------------
C     CONVERT PEAK POSITIONS TO NON-OFFSET VALUES
C     ------------------------------------------------------------------
C     
 586  DO 590 I=1,NPK
         XP(1,I)=XP(1,I)+SHIFT
 590  CONTINUE
C     
      ASUM=0.0
      LDX=1
      DO 600 J=1,NPK
C     
         CALL GETBETA(J,JDUM,BETAJ,AJJ)
C     
         GAREA=1.7724539*XP(2,J)*BETAJ
         FWI=1.66478*XPSAV(2,J)
         FWF=1.66478*XP(2,J)
         ALI=XPSAV(3,J)
         ALF=XP(3,J)
         AHI=XPSAV(4,J)
         AHF=XP(4,J)
         EP=ECA+ECB*XP(1,J)+ECC*XP(1,J)**2
C     
         IF(KFIT.NE.3) PCE=100.0*SQRT(ABS(QFN*A(J,J)))/BETA(J) !SAM
         IF(KFIT.EQ.3) PCE=100.0*ERR(J+5)/GAREA !GFIT
C     
         AREA=GAREA*(1.0+0.2815*(XP(3,J)+XP(4,J)))
     1        +0.10094*(XP(3,J)**2+XP(4,J)**2)
     2        -0.016813*(XP(3,J)**3+XP(4,J)**3)
         III=XP(1,J)-SHIFT
         IF(III.LT.I1) III=I1
         IF(III.GT.I2) III=I2
         ASUM=ASUM+AREA
         RESUL(1,J)=XOR(J)
         RESUL(2,J)=XP(1,J)
         RESUL(3,J)=EP
         RESUL(4,J)=AREA
         RESUL(5,J)=GAREA
         RESUL(6,J)=PCE
         RESUL(7,J)=FWI
         RESUL(8,J)=FWF
         RESUL(9,J)=ALI
         RESUL(10,J)=ALF
         RESUL(11,J)=AHI
         RESUL(12,J)=AHF
C     
         IF(KFIT.LT.3) GO TO 592
C     
         RESUL(1,J)=ERR(J)      !GFIT
         RESUL(4,J)=ERR(J+5)    !GFIT
         RESUL(7,J)=ERR(J+10)   !GFIT
C     
 592     IAREA=AREA
         WRITE(CLOUXX(J),595)J,XP(1,J),IAREA,PCE,FWF,ALF,AHF
 595     FORMAT(I4,F8.1,I10,F8.1,F8.2,2F8.3,'  ')
C     
 600  CONTINUE
C     
      RESOK='YES '
      RETURN
C     
C     ------------------------------------------------------------------
C     Return error messages
C     ------------------------------------------------------------------
C     
 2000 WRITE(CMSSG,2005)NCOMP
 2005 FORMAT('#-OF-PEAKS + #-OF-BGD-TERMS =',I3,'  MAX NUMBER IS 30')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
C     
 3000 WRITE(CMSSG,3005)
 3005 FORMAT('SYNTAX ERROR, ILLEGAL COMMAND, OR ILLEGAL VALUE')
      CALL MESSLOG(LOGUT,LOGUP)
      IERR=1
      RETURN
      END
      
