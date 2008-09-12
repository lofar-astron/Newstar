C+ NMOBMF.FOR
C  WNB 930826
C
C  Revisions:
C	WNB 930928	Split off NMOBMV; cater for multiple instruments
C	CMV 930917	Various corrections, changed direction of change
C	CMV 930921	Changed direction back
C	WNB 930928	Cater for multiple instruments
C	WNB 931008	Remove BEMLIM etc.
C
	LOGICAL FUNCTION NMOBMF(INST,FRQ)
C
C  Get correct beam definition
C
C  Result:
C
C	NMOBMF_L = NMOBMF( INST_J:I, FRQ_D:I)
C				Get (de-)beam factors for specified
C				frequency range and INSTrument
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'BMD_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER INST		!INSTRUMENT
	DOUBLE PRECISION FRQ	!FREQUENCY
C
C  Function references:
C
	LOGICAL NMOBMR		!READ VALUES
C
C  Data declarations:
C
C-
C
C INIT
C
	NMOBMF=.FALSE.				!ASSUME ERROR
	IF (.NOT.NMOBMR()) RETURN		!READ DATA IF NECESSARY
	IF (INST.LT.0 .OR. INST.GE.BEMNIN) RETURN !UNKNOWN INSTRUMENT
	BEMCIN=INST				!CURRENT INSTRUMENT
	BEMCFP=0				!CURRENT FACTOR POINTER
	DO I=0,BEMCOD(3,INST)-1			!GET FACTOR
	  BEMCFP=I*BEMCOD(2,INST)		!FACTOR POINTER
	  IF (BEMFQ(I,INST).GE.FRQ) GOTO 20	!READY
	END DO
 20	CONTINUE
C
	NMOBMF=.TRUE.				!FOUND
	RETURN
C
C
	END
