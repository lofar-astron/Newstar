C+ NMOBEM.FOR
C  WNB 910814
C
C  Revisions:
C	WNB 910909	Correct factor
C	WNB 911115	Change minimum value
C	WNB 920602	Use BEMLIM
C	WNB 930826	New beam factors
C	WNB 930928	Multiple instruments
C	WNB 931006	Text
C	WNB 931008	Limit (de-)beaming to non (de-)beam
C
	SUBROUTINE NMOBEM
C
C  (De-)beam source list
C
C  Result:
C
C	CALL NMOBEM		Correct source list for beam
C	CALL NMOBED		De-correct source list for beam
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
	INCLUDE 'MDH_O_DEF'	!MODEL HEADER
	INCLUDE 'MDL_O_DEF'	!MODEL LINE
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNFOP		!OPEN FILE
	LOGICAL NMOBMR		!READ BEAM DATA
	LOGICAL NMOBMF		!GET BEAM RANGE
	DOUBLE PRECISION NMOBMV	!BEAM VALUES
C
C  Data declarations:
C
	LOGICAL DEB				!SWITCH BEAM/DEBEAM
	BYTE MDL(0:MDLHDL-1)			!SOURCE LINE
	  INTEGER MDLJ(0:MDLHDL/4-1)
	  REAL MDLE(0:MDLHDL/4-1)
	  EQUIVALENCE (MDL,MDLJ,MDLE)
	INTEGER RANGE(0:1)			!SHOW RANGE
	  DATA RANGE/0,1000000/
C-
C
C INIT
C
	DEB=.FALSE.				!BEAM
	GOTO 10
C
C NMOBED
C
	ENTRY NMOBED
C
	DEB=.TRUE.				!DE-BEAM
	GOTO 10
C
 10	CONTINUE
	IF (.NOT.NMOBMR()) GOTO 900		!GET DATA
C
C INIT SOURCE LIST
C
	CALL NMOHZD(GDES)			!CLEAR HEADER DATA
	CALL NMORDS(FCAOUT)			!READ SOURCES
	CALL NMORDM(7,-1)			!AND ADD THEM
	CALL NMOPTT(F_TP,RANGE)			!SHOW DATA
C
C GET REFERENCE DATA
C
	IF (GDESJ(MDH_TYP_J).LE.0) THEN
	  CALL WNCTXT(F_TP,'!/Cannot (de-)beam local type source list')
	  GOTO 900
	END IF
	IF (.NOT.NMOBMF(IAND(MDHINS_M,GDESJ(MDH_BITS_J)),
	1		GDESD(MDH_FRQ_D))) THEN !GET BEAM RANGE
	  CALL WNCTXT(F_TP,'!/Cannot (de-)beam: no beam data available')
	  GOTO 900
	END IF
C
C CONVERT
C
	DO I=0,GDESJ(MDH_NSRC_J)-1
	  J=GDESJ(MDH_MODP_J)+I*MDLHDL-A_OB
	  CALL WNGMV(MDLHDL,A_B(J),MDL)		!GET MODEL
	  I1=MDL(MDL_TP_B)			!TYPE
	  IF ((DEB .AND. IAND(I1,MDLBEM_M).NE.0) .OR.
	1		(.NOT.DEB .AND. IAND(I1,MDLBEM_M).EQ.0)) THEN !DO
	    D1=NMOBMV(GDESD(MDH_FRQ_D),MDLE(MDL_L_E),MDLE(MDL_M_E),
	1		BEMLIM,DEB)		!GET VALUE
	    MDLE(MDL_I_E)=D1*MDLE(MDL_I_E)	!DO
	  END IF
	  IF (DEB) THEN
	    I1=IAND(I1,NOT(MDLBEM_M))		!SET DE-BEAM
	  ELSE
	    I1=IOR(I1,MDLBEM_M)			!SET BEAM
	  END IF
	  MDL(MDL_TP_B)=I1
	  CALL WNGMV(MDLHDL,MDL,A_B(J))		!SET MODEL
	END DO
C
C REWRITE DATA
C
	IF (.NOT.WNFOP(FCAOUT,FILOUT,'U')) THEN
	  CALL WNCTXT(F_TP,'Cannot output data')
	  GOTO 901
	END IF
	CALL NMOWRS(FCAOUT,GDES)		!WRITE DATA BACK
	CALL NMOPTT(F_TP,RANGE)			!SHOW DATA
C
	RETURN
C
C ERRORS
C
 900	CONTINUE
	CALL WNFCL(FCAOUT)
 901	CONTINUE
C
	RETURN
C
C
	END
