C+ WNMITN.FOR
C  WNB 900312
C
C  Revisions:
C	WNB 930503	Make N complex into 2N real
C
	LOGICAL FUNCTION WNMITN(MAR)
C
C  Triangularize normal equations
C
C  Result:
C
C	WNMITN_L = WNMITN( MAR_J:I)
C				Triangularize normal equations
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
	INTEGER MAR				!AREA POINTER
C
C  Entry points:
C
C
C  Function references:
C
C
C  Data declarations:
C
	INTEGER N
C-
	N=A_J(MAR+1)
	J1=A_J(MAR+5)					!INDEX NORMAL EQUATIONS
C
C REAL
C
	WNMITN=.TRUE.					!ASSUME OK
	DO I=0,N-1					!DECOMPOSE
	  I3=J1+((2*N-I-1)*I)/2
	  DO I1=I,N-1
	    DO I2=0,I-1
	      I4=J1+((2*N-I2-1)*I2)/2
	      A_D(I3+I1)=A_D(I3+I1)-A_D(I4+I)*A_D(I4+I1)/A_D(I4+I2)
	    END DO
	  END DO
	  IF (A_D(I3+I).LE.0) THEN
	    WNMITN=.FALSE.				!CANNOT DO
	    RETURN
	  END IF
	END DO
C
	RETURN
C
C
	END
