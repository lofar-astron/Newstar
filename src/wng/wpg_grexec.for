C*GREXEC -- PGPLOT device handler dispatch routine
C+
      SUBROUTINE GREXEC(IDEV,IFUNC,RBUF,NBUF,CHR,LCHR)
C---
      INCLUDE 'WNG_DEF'
C---
      INTEGER IDEV, IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C---
      INTEGER NDEV
      PARAMETER (NDEV=1)
C---
      GOTO(1) IDEV
      IF (IDEV.EQ.0) THEN
          RBUF(1) = NDEV
          NBUF = 1
      ELSE
	  CALL WNCTXT(F_TP,'Unknown device code in GREXEC: !SL',IDEV)
	  CALL WNGEX				!QUIT
      END IF
      RETURN
C---
    1 CALL XWDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      RETURN
C
      END

