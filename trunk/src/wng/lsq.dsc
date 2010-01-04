!+ LSQ.DSC
!  WNB 950328
!
!  Revisions:
!
%REVISION=WNB=950328="Original version"
!
!	Define the LSQ control area
!
%COMMENT="LSQ.DSC defines the LSQ  (Least SQuares) control area"
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
%ALIGN						!ALIGN STRUCTURES
!-
.PARAMETER
	LSQ_T	MF:	\			!TYPE FOR:
				/REAL,COMPLEX, \ !REAL,COMPLEX,
				MULTIPLE, \	!MULTIPLE KNOWN SIDES (M)
				CONSTRAINT, \	!CONSTRAINT EQUATIONS (N-NUN)
				PREC, \		!PRECISION GIVEN
				NOINIT/		!DO NOT INIT (LOCAL ONLY)
	LSQ_I	MF:	\			!INIT TYPE:
				/NORM,KNOWN, \	!NORMAL EQUATIONS, KNOWN PART,
				NONLIN, \	!NON-LINEAR PART
				PREC/		!PRECISION FACTOR
	LSQ_I_SOL J/LSQ_I_NORM+LSQ_I_KNOWN/	!SOLUTION PART
	LSQ_I_ALL J/LSQ_I_SOL+LSQ_I_NONLIN/	!CLEAR ALL
	LSQ_C	MF:	\			!CONDITION EQUATION TYPE:
				/REAL, \	!REAL COEFFICIENTS
				COMPLEX, \	!COMPLEX COEFFICIENTS
				CCOMPLEX, \	!2 (CONJUGATE) COMPLEX !COEFFICIENTS
				DCOMPLEX, \	!2 SEPARATE COMPLEX COEFFICIENTS
				NONORM, \	!DO NOT DO NORMAL EQUATIONS
				NOKNOWN/	!DO NOT DO KNOWN PART
	LSQ_U	MF:(2*LSQ_T__H)	\		!USAGE BITS
				/INVERTED, \	!INVERTED MATRIX PRESENT
				TRIANGLE, \	!TRIANGULARISED
				NONLIN/		!NON-LINEAR SOLUTION
	DPREC	D/1.E-6/		!DEFAULT TEST PRECISION
	NLFAC	D/0.001/		!START NON-LINEAR FACTOR
!
.BEGIN=LSQ
	SIZE	J			!TOTAL LENGTH ALLOCATED
	BITS	J			!TYPE OF LSQ (E.G. COMPLEX)
	DBL	J			!POINTER FOR DOUBLE PARTS
	NUN	J			!# OF UNKNOWNS
	M	J			!# OF KNOWNS
	N	J			!SIZE OF MATRIX
	R	J			!RANK OF NORMAL EQUATIONS
	PIV	J			!A_J POINTER TO PIVOT TABLE (J(N))
	NORM	J			!A_D POINTER TO NORMAL EQUATIONS (D(N.(N+1)/2))
	KNOWN	J			!A_D POINTER TO KNOWN PART (D(N.M))
	ERROR	J			!A_D POINTER TO ERROR PART (D(4M))
	SOL	J			!A_D POINTER TO HIGH PRECISION SOLUTION (D(N))
	NAR	J			!SAVE AREA FOR NON-LINEAR
	FACTOR	D			!PRECISION
	NONLIN	D			!NON-LINEAR LEVENBERG FACTOR
.ALIGN=LB_D				!ALIGN FOR WORKAREA
.END					!END DEFINITION
!
.BEGIN=LERR
	N	D			!NUMBER OF CONDITION EQUATIONS
	W	D			!SUM WEIGHTS
	LL	D			!SUM KNOWN TERMS SQUARED
	CHI2	D			!CHI SQUARED CALCULATED
.END
.PARAMETER
	LERR__N	J/LERR__L/LB_D/		!# OF ELEMENTS IN LERR
!-
