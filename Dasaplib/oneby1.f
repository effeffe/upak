C$PROG ONEBY1
      SUBROUTINE ONEBY1
      COMMON/CCC/ BGD(256),WT(256),YCAL(256),GAU(200),NG(16)
      COMMON/DDD/ IHOL(16),XP(16),AREA(16),BETA(18),LOCA(250),BIAS
      COMMON/EEE/ ID,ILO,IHI,KWD,FWHMA,FWHMB,EOOO,GAINN,WDNC,DELCH,BSTD
      COMMON/FFF/ HWID(16),QFN,QFLO,IFBGD,NTRY,MSN,NCH,KPK,I1,I2
      DIMENSION XPG(16)
C
      SAVE
C
      IF(KPK.LT.1) RETURN
      DO 10 I=1,16
         XPG(I)=XP(I)
   10 CONTINUE
      DO 100 KP=1,KPK
         JP=NG(KP)
*      IF(IHOL(JP))15,15,100
         IF (IHOL(JP) .GT. 0) THEN
            GOTO 100
         ENDIF
   15    KTRY=0
         CALL PKFIT
         QFLO=QFN
         XP(JP)=XP(JP)+DELCH
         CALL PKFIT
*      IF(QFN-QFLO)20,100,30
         IF (QFN .LT. QFLO) THEN
   20       DELP=DELCH
            QFLO=QFN
         ELSE IF (QFN .GT. QFLO) THEN
*      GO TO 50
   30       XP(JP)=XPG(JP)
            DELP=-DELCH
         ELSE 
              GOTO 100
         ENDIF
   50    CONTINUE
         XP(JP)=XP(JP)+DELP
         CALL PKFIT
*      IF(QFN-QFLO)60,100,80
         IF (QFN .LT. QFLO) THEN
   60       QFLO=QFN
            KTRY=KTRY+1
            IF(KTRY.GE.NTRY) GO TO 100
            GO TO 50
         ELSE IF (QFN .GT. QFLO) THEN
   80       XP(JP)=XP(JP)-DELP
            CALL PKFIT
         ENDIF
  100 CONTINUE
      RETURN
      END
