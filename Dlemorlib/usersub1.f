C$PROG USERSUB1  - Dummy USERSUB1 - does nothing
C
C     ******************************************************************
C     BY W.T. MILNER AT HRIBF - LAST MODIFIED 04/24/2002
C     ******************************************************************
C
      SUBROUTINE USERSUB1(IBUF)
C
      IMPLICIT NONE
C
C     ------------------------------------------------------------------
      INTEGER*2    IBUF(*)
C     ------------------------------------------------------------------
      SAVE
C     ------------------------------------------------------------------
C
C     DUMMY SUBROUTINE - JUST TO AVOID LINK DIAGNOSTICS
C
      RETURN
      END
