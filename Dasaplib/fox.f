C$PROG FOX
      FUNCTION FOX(X)
C
      COMMON/FFF/ HWID(16),QFN,QFLO,IFBGD,NTRY,MSN,NCH,KPK,I1,I2
      COMMON/IIX/ WSAV(16)
C
      DIMENSION X(64)
C
      SAVE
C
      DO 10 I=1,KPK
      HWID(I)=WSAV(I)*X(I)
   10 CONTINUE
C
      CALL PKFIT
      FOX=QFN
      RETURN
      END
