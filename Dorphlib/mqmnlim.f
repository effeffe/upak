C$PROG MQMNLIM   - Checks table for liquid drop shell correction in rang
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 06/18/2002 - for gnu
C     ******************************************************************
C
      FUNCTION MQMNLIM(IZ,IN)
C
      INTEGER*4 NLIM(2,140)
C
      DATA ((NLIM(I,J),I=1,2),J=1,50)/
     &  1,  0,   1,  0,   1,  0,   1,  0,   1,  0,
     &  1,  0,   1,  0,   1,  0,   1,  0,  10, 28,
     & 10, 28,  10, 44,  10, 46,  10, 50,  11, 50,
     & 10, 50,  11, 50,  10, 50,  13, 50,  10, 60,
     & 13, 62,  10, 64,  15, 66,  12, 78,  17, 78,
     & 12, 82,  17, 82,  14, 82,  23, 82,  20, 82,
     & 25, 82,  20, 82,  27, 82,  22, 96,  28, 96,
     & 24,100,  29,100,  26,104,  31,104,  28,108,
     & 33,118,  29,126,  36,126,  31,126,  37,126,
     & 32,126,  39,126,  34,126,  41,126,  36,126/
      DATA ((NLIM(I,J),I=1,2),J=51,100)/
     & 49,126,  44,126,  51,140,  46,144,  53,144,
     & 48,148,  55,148,  50,152,  57,152,  53,156,
     & 59,156,  54,160,  61,160,  57,178,  65,180,
     & 60,184,  67,184,  63,184,  70,184,  66,184,
     & 73,184,  69,184,  75,184,  72,184,  77,184,
     & 72,184,  79,184,  76,184,  82,186,  78,192,
     & 85,194,  81,210,  95,210,  89,210,  97,210,
     & 92,210,  97,210,  93,210, 100,210,  94,210,
     &103,210,  98,210, 107,210, 102,210, 111,210,
     &105,210, 114,210, 108,210, 118,210, 112,210/
      DATA ((NLIM(I,J),I=1,2),J=101,140)/
     &123,210, 116,210, 126,210, 120,210, 129,210,
     &123,210, 133,210, 126,210, 137,210, 130,210,
     &141,210, 135,210, 145,210, 138,210, 149,210,
     &142,210, 156,210, 145,210, 157,210, 150,210,
     &160,210, 152,210, 163,210, 159,210, 165,210,
     &158,210, 177,210, 171,210, 182,210, 174,210,
     &185,210, 178,210, 189,210, 182,210, 193,210,
     &186,210, 194,210, 190,210, 196,210, 192,210/
C
      SAVE
C
C     ------------------------------------------------------------------
C
      IF(IZ.LT.10)  GO TO 10
      IF(IZ.GT.140) GO TO 10
      IF(IN.LT.NLIM(1,IZ)) GO TO 10
      IF(IN.GT.NLIM(2,IZ)) GO TO 10
      MQMNLIM=1
      RETURN
C
   10 MQMNLIM=0
      RETURN
      END
