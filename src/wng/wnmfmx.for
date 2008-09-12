C+ WNMFMX.FOR
C  WNB 910318
C
C  Revisions:
C
	SUBROUTINE WNMFMX(N,B,FAC,MAXV,PMAX,MINV,PMIN)
C
C  Normalize a buffer and return max/min
C
C  Result:
C
C	CALL WNMFMX( N_J:I, B_E(0:N-1):IO, FAC_D:I, MAXV_E:IO, PMAX_J:O,
C					MINV_E:IO, PMIN_J:O)
C				Multiply B(0:N-1) with FAC and find MAXV at
C				PMAX and MINV at PMIN
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER N
	REAL B(0:*)
	DOUBLE PRECISION FAC
	REAL MAXV
	INTEGER PMAX
	REAL MINV
	INTEGER PMIN
C
C  Function references:
C
C
C  Data declarations:
C
C-
	R0=FAC
	DO I=0,N-1				!NORMALIZE
	  B(I)=R0*B(I)
	END DO
	I=0					!FIND MAX/MIN
	DO WHILE (I.LT.N)
	  IF (B(I).GT.MAXV) THEN
	    PMAX=I
	    MAXV=B(PMAX)
	  END IF
	  IF (B(I).LT.MINV) THEN
	    PMIN=I
	    MINV=B(PMIN)
	  END IF
	  I=I+1
	END DO
C
	RETURN
C
C
	END
