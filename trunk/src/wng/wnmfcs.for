C+ WNMFCS.FOR
C  WNB 910318
C
C  Revisions:
C
	SUBROUTINE WNMFCS(N,A)
C
C  FFT help routines
C
C  Result:
C
C	CALL WNMFCS( N_J:I, A_X(0:N-1):IO)
C				Swaps the two halves of A.
C	CALL WNMFRC( N_J:I, A_X(0:N-1):IO)
C				Takes the input as REAL(0:N-1), and converts
C				it into COMPLEX(0:N-1)
C	CALL WNMFCR( N_J:I, A_X(0:N-1):IO)
C				Takes the input as COMPLEX(0:N-1), and
C				converts it into REAL(0:N-1)
C	CALL WNMFSN( N_J:I, B_E(0:N-1):IO, TAB_E(0:N-1):I, VAL_E:I)
C				Multiply B(0:N-1) with TAB(0:N-1)*VAL
C	CALL WNMFIN( N_J:I, B_E(0:N-1):IO, TAB_E(0:N-1):I, VAL_E:I)
C				Multiply B(0:N-1) with TAB(N-1:0)*VAL
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
	COMPLEX A(0:*)
	REAL B(0:*)
	REAL TAB(0:*)
	REAL VAL
C
C  Function references:
C
C
C  Data declarations:
C
	COMPLEX C8
C-
C
C WNMFCS
C
	J=N/2					!N/2
	DO I=0,J-1				!SWAP
	  C8=A(I)
	  A(I)=A(J+I)
	  A(J+I)=C8
	END DO
C
	RETURN
C
C WNMFRC
C
	ENTRY WNMFRC(N,A)
C
	DO I=N-2,0,-2				!CONVERT
	  J=I/2
	  A(I+1)=CMPLX(AIMAG(A(J)),0.)
	  A(I)=CMPLX(REAL(A(J)),0.)
	END DO
C
	RETURN
C
C WNMFCR
C
	ENTRY WNMFCR(N,A)
C
	DO I=0,N-2,2				!CONVERT
	  J=I/2
	  A(J)=CMPLX(REAL(A(I)),REAL(A(I+1)))
	END DO
C
	RETURN
C
C WNMFSN
C
	ENTRY WNMFSN(N,B,TAB,VAL)
C
	DO I=0,N-1
	  B(I)=B(I)*TAB(I)*VAL
	END DO
C
	RETURN
C
C WNMFIN
C
	ENTRY WNMFIN(N,B,TAB,VAL)
C
	J=N
	DO I=0,N-1
	  J=J-1
	  B(I)=B(I)*TAB(J)*VAL
	END DO
C
	RETURN
C
C
	END
