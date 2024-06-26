C$PROG PAGER
C
      SUBROUTINE PAGER(NLN,NLPG,NPAG,IHI)
C
      COMMON/BBB/  IFMTA,IFMTB,IDATE(3),IFODX,JUSTON
      CHARACTER*4                             JUSTON
      CHARACTER*16 IFMTA,IFMTB
C
      COMMON/CCC/ ICHAP,NCHAP(10)
C
      COMMON/TOGG/ KTOG
C
      COMMON/YYY/ KLIS,KAUTOSP,KPAGLAB,KTOFLAG,NPAGSP
      CHARACTER*4 KLIS,KAUTOSP,KPAGLAB,KTOFLAG
C
      INTEGER*4 LINE(30),LINE20(20),KPAG(2),KHED(11)
C
      CHARACTER*8   CPAG
C
      EQUIVALENCE  (CPAG,KPAG)
C
      CHARACTER*120 CLINE
C
      EQUIVALENCE  (CLINE,LINE)
C
      CHARACTER*4  TPFCALL
C
      EQUIVALENCE (LINE20,LINE)
C
      DATA NLPR,ITPF/0,Z'0C202020'/
C
      DATA TPFCALL/'NO  '/
C
      DATA KTOG/2/
C
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C   
C     *************************************************************
C     NLN IS THE NO. OF LINES PRINTED JUST PRIOR TO CALL
C     NLPR = NO. OF LINES PRINTED ON THIS PAGE
C     NLPG = NO. OF LINES PER PAGE (TO BE PRINTED)
C     NPAG # THE CURRENT PAGE NO.
C     IHI # LARGEST COL. IN WHICH A CHARACTER MAY BE SET
C     *************************************************************
C   
      NLPR=NLPR+NLN
      IF(NLPR.LT.NLPG) RETURN
C   
      NLPR=0
      NPAG=NPAG+1
      KTOG=3-KTOG
C
      CALL BLANKIT(LINE,30)
C   
      WRITE(CLINE,20)IDATE
   20 FORMAT(2A4,A1)
C   
      WRITE(CPAG,30)NPAG
   30 FORMAT('PAGE',I4)
C   
      KHED(1)=ICHAP
      DO 40 I=1,10
      KHED(I+1)=NCHAP(I)
   40 CONTINUE
C   
      IA=11
      IB=IHI-9
      DO 50 I=IA,IB
      CALL ISBYTE(ichar('.'),LINE,I-1)
   50 CONTINUE
C   
      IUP=LSNB(KHED,1,44)
      IF(IUP.LE.0) GO TO 105
      NBLNK=IHI-IUP-16
      IF(NBLNK.LT.0) NBLNK=0
      NSKIP=NBLNK/2
      N=8+NSKIP
      NA=N
C   
      DO 60 I=1,IUP
      N=N+1
      CALL ILBYTE(IT,KHED,I-1)
      CALL ISBYTE(IT,LINE,N-1)
      IF(N.GE.IHI) GO TO 100
   60 CONTINUE
C   
  100 NB=N+1
      IF(NA.GT.8)   CALL ISBYTE(ichar(' '),LINE,NA-1)
      IF(NB.LT.IHI) CALL ISBYTE(ichar(' '),LINE,NB-1)
  105 N=IHI-8
      DO 110 I=1,8
      N=N+1
      CALL ILBYTE(IT,KPAG,I-1)
      CALL ISBYTE(IT,LINE,N-1)
  110 CONTINUE
C   
      IFODX=3-IFODX
      IF(TPFCALL.NE.'NO  ') WRITE(7,120)ITPF
  120 FORMAT(1H ,A4)
      TPFCALL='YES '
      NLPR=1
C
      IF(KPAGLAB.NE.'YES ') RETURN
C
      CALL FONTMO('DO  ','BON$')
C
      CALL TRIMOUT(LINE20,80)
C
      CALL FONTMO('RES ','BON$')
      NLPR=NLPR+1
C
      IF(NPAGSP.LE.0) RETURN
C
      DO 130 I=1,NPAGSP
      WRITE(7,125)
  125 FORMAT(1H )
  130 CONTINUE
      NLPR=NLPR+NPAGSP
      RETURN
      END
