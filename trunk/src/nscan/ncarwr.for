C+ NCARWR.FOR
C  WNB 900822
C
C  Revisions:
C	WNB 930606	Use LB_ iso L_
C	WNB 930825	Use XYX bits
C
	LOGICAL FUNCTION NCARWR(FCA,STH,SCN,SCH,SOL,SPOL,TSOL,JAV,EAV,DAV)
C
C  Write redundancy data to scan
C
C  Result:
C
C	NCARWR_L = NCARWR( FCA_J:I, STH_B(0:*):I, SCN_J:I, SCH_B(0:*):O,
C			SOL_E(0:*,0:1,0:1):I, SPOL_J:I, TSOL_J:I,
C			JAV,EAV,DAV)
C				Read scan number SCN from FCA, using the
C				set header STH. The scan header SCH, after
C				filling with data from SOLution and AVerages
C				will be written and
C				returned for all four polarisations.
C				SPOL and TSOL indicate the polarisations
C				(1=x, 8=y) and type (1=gain, 2=phase) to do.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA			!FILE CONTROL AREA
	BYTE STH(0:*)			!CURRENT SET HEADER
	INTEGER SCN			!SCAN TO DO
	BYTE SCH(0:*)			!SCAN HEADER
	REAL SOL(0:STHTEL-1,0:1,0:1)	!SOL. X/Y, G/P
	INTEGER SPOL			!POL TO DO
	INTEGER TSOL			!GAIN/PHASE TO DO
	INTEGER JAV(0:STHIFR-1,0:4,0:1,0:1) !AVERAGES
	REAL EAV(0:STHIFR-1,0:4,0:1,0:1)
	REAL*8 DAV(0:STHIFR-1,0:4,0:1,0:1)
C
C  Function references:
C
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
	INTEGER WNGARA			!ADDRESS OF VARIABLE
C
C  Data declarations:
C
	INTEGER STHP,STHPI,STHPJ	!SET HEADER POINTER
	INTEGER SCHP,SCHPI,SCHPJ,SCHPE	!SCAN HEADER POINTER
	INTEGER*2 LDAT(0:2,0:4*STHIFR-1) !DATA BUFFER
	INTEGER CDPOL(0:1)
	  DATA CDPOL/XX_P,YY_P/		!POLARISATION TYPES
C-
C
C INIT
C
	NCARWR=.TRUE.				!ASSUME OK
	STHP=WNGARA(STH(0))			!ADDRESS SET HEADER
	STHPI=(STHP-A_OB)/LB_I
	STHPJ=(STHP-A_OB)/LB_J
	SCHP=WNGARA(SCH(0))			!ADDRESS SCAN HEADER
	SCHPI=(SCHP-A_OB)/LB_I
	SCHPJ=(SCHP-A_OB)/LB_J
	SCHPE=(SCHP-A_OB)/LB_E
	IF (SCN.LT.0 .OR. SCN.GE.A_J(STHPJ+STH_SCN_J)) GOTO 900 !UNKNOWN SCAN
	I=A_J(STHPJ+STH_SCNL_J)			!LENGTH SCAN
	J=A_J(STHPJ+STH_SCNP_J)+SCN*I		!POINTER TO SCAN
C
C READ A SCAN
C
	IF (.NOT.WNFRD(FCA,SCHHDL,SCH,J)) GOTO 900 !READ SCAN HEADER
	DO I=0,1				!POL.
	  IF (IAND(SPOL,CDPOL(I)).NE.0) THEN	!TO DO
	    DO I1=0,1				!GAIN, PHASE
	      IF (IAND(TSOL,2**I1).NE.0) THEN	!TO DO
	        DO I2=0,STHTEL-1		!TEL.
	          I3=I1+2*I2+2*STHTEL*I		!OFFSET
	          A_E(SCHPE+SCH_REDC_E+I3)=SOL(I2,I,I1) !SET CORRECTION
	          A_E(SCHPE+SCH_ALGC_E+I3)=0.	!NO ALIGN
	          A_E(SCHPE+SCH_OTHC_E+I3)=0.	!NO OTHERS
		  I4=2*I2+I1
		  A_E(SCHPE+SCH_REDNS_E+I4)=EAV(0,0,I1,I) !SAVE NOISE
		  A_E(SCHPE+SCH_ALGNS_E+I4)=0
		  A_E(SCHPE+SCH_OTHNS_E+I4)=0
	        END DO
	      END IF
	    END DO
	  END IF
	END DO
	IF (.NOT.WNFWR(FCA,SCHHDL,SCH,J)) GOTO 900 !REWRITE SCAN HEADER
C
	RETURN
C
C ERROR
C
 900	CONTINUE
	NCARWR=.FALSE.
C
	RETURN
C
C
	END
