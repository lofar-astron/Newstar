C+ NSCSWC.FOR
C  WNB 910208
C
C  Revisions:
C	WNB 910820	Add extinction, refraction, Faraday
C	WNB 921216	Do not copy noise for deleted scan
C	JPH 930614	Legibilise. Contract some small loops
C	WNB 930708	Correct Parameter format
C	WNB 930803	CBITS_DEF
C	CMV 940331	Select telescopes to copy corrections for
C	WNB 940811	Remove AIFR/MIFR zero
C	CMV 950220	Change handling of AOTH corrections
C       HjV 950511	Set AOTHUSED
C	CMV 031231	Changed GPS,XYS from BYTE to LOGICAL
C
	LOGICAL FUNCTION NSCSWC(FCA,STHJ,SCN,COR,TCOR,
	1				GPS,XYS,TELS,CAP,CDAP,ZAP)
C
C  Write scan correction data
C
C  Result:
C
C	NSCSWC_L = NSCSWC( FCA_J:I, STHJ_B(0:*):I, , SCN_J:I,
C			COR(0:*,G:P,X:Y):I, TCOR_J:I,
C			GPS_L(G:P):I, XYS_L(X:Y):I, TELS_B(0:*),
C			CAP_J:I, CDAP_J:I, ZAP_J:I)
C	The file FCA with set header STH and scan number SCN will have 
C corrections submitted in COR added to its SCH table indicated by the mask
C TCOR, - whose value may be CAP_RED, _ALG or _OTH.
C	GPS(G/P) and XYS(X/Y) are logical byte masks showing which parts of
C COR are valid.
C	The bits in CAP, CDAP indicate how the corresponding corrections 
C currently in the target table must be treated: 
C	CAP indicates that the current correction
C must be retained, ZAP that it must be set to 0; if both are true, the 
C implication is that the correction must be absorbed into the correction 
C identified by TCOR.
C** Old situation:
C**	CDAP indicates that the existing "applied" correction must be 
C** subtracted from the current one. (NOTE: it would seem to me that the applied
C** correction should then be cleared, but this does not happen.)	  
C** New situation
C  CDAP indicates that an existing "applied" correction has been deapplied
C  from the data on which the present correction was based. It is assumed that
C  the user will also deapply this correction in any further use. 
C				
C
C	NSCSWU_L = NSCSWU( FCA_J:I, STHJ_B(0:*):I, , SCN_J:I,
C			COR(0:*,G:P,X:Y):I, TCOR_J:I,
C			GPS_L(G:P):I, XYS_L(X:Y):I, TELS_B(0:*),
C			CAP_J:I, CDAP_J:I, ZAP_J:I, ME_E(G:P,X:Y):I)
C				As NSCSWC, but also write the scan noise.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'			!SET HEADER
	INCLUDE 'SCH_O_DEF'			!SCAN HEADER
C
C Entry points:
C
	LOGICAL NSCSWU				!WRITE MEAN ERRORS
C
C  Parameters:
C
	INTEGER G,P
	  PARAMETER (G=0, P=1)			!gain/phase index
	INTEGER X,Y
	  PARAMETER (X=0, Y=1)			!X/Y index
	INTEGER RED,ALG,OTH
	  PARAMETER (RED=1, ALG=2, OTH=3)	!corrn table index
C
C  Arguments:
C
	INTEGER FCA				!FILE CONTROL AREA
	INTEGER STHJ(0:*)			!CURRENT SET HEADER
	INTEGER SCN				!SCAN TO DO
	REAL COR(0:STHTEL-1,G:P,X:Y)		!CORRECTIONS G,P X,Y
	INTEGER TCOR				!TYPE CORRECTION
	LOGICAL GPS(G:P)			!GAIN/PHASE PRESENT
	LOGICAL XYS(X:Y)			!X/Y PRESENT
	BYTE TELS(0:*)				!TELESCOPES SELECTED
	INTEGER CAP				!APPLY CORRECTIONS
	INTEGER CDAP				!DE-APPLY CORRECTIONS
	INTEGER ZAP				!ZERO CORRECTIONS
	REAL ME(G:P,X:Y)			!GAIN/PHASE X/Y NOISES
C
C  Function references:
C
	LOGICAL WNFRD				!READ DATA
	LOGICAL WNFWR				!WRITE DATA
	INTEGER WNGARA				!ADDRESS OF VARIABLE
	REAL WNGENR				!ANGLE -180,180
C
C  Data declarations:
C
	LOGICAL		DOME			!SWITCH FOR M.E.
	INTEGER		SCNP			!SCH file address
	INTEGER		RPTR, WPTR		!read and write pointers to
						! correction arrays in SCH
	INTEGER		INDX			!index into these arrays
	INTEGER		IXY, IGP		!X/Y, gain/phase loop indices
	INTEGER		ITEL			!telescope loop index
	REAL		GN, PH			!gain. phase accumulators
	INTEGER		BIT			!correction mask bit
	INTEGER		CTYP			!corection type index
	LOGICAL		WARN_CDAP
	DATA		WARN_CDAP/.FALSE./
	SAVE		WARN_CDAP
	BYTE SCH(0:SCHHDL-1)			!SCAN HEADER
	  INTEGER SCHJ(0:SCHHDL/4-1)
	  REAL SCHE(0:SCHHDL/4-1)
	  EQUIVALENCE (SCH,SCHJ,SCHE)
C-
C
C INIT
C
	NSCSWC=.TRUE.				!ASSUME OK
	DOME=.FALSE.				!NO M.E. WRITE
	GOTO 10
C
C NSCSWU
C
	ENTRY NSCSWU(FCA,STHJ,SCN,COR,TCOR,
	1			GPS,XYS,TELS,CAP,CDAP,ZAP,ME)
C
C INIT
C
	NSCSWU=.TRUE.				!ASSUME OK
	DOME=.TRUE.				!WRITE M.E.
	GOTO 10
C
C READ SCAN HEADER
C
 10	CONTINUE
	IF (SCN.LT.0 .OR. 
	1	SCN.GE.STHJ(STH_SCN_J)) GOTO 900!UNKNOWN SCAN
	SCNP=STHJ(STH_SCNP_J)+SCN*STHJ(STH_SCNL_J)!SCAN POINTER
	IF (.NOT.WNFRD(FCA,SCHHDL,SCH,SCNP))
	1	 GOTO 900			!READ SCAN HEADER
C
C SET CORRECTIONS
C
C select table to be updated
C
	WPTR=SCH_REDC_E				!ASSUME REDUNDANCY
	IF (IAND(TCOR,CAP_RED).NE.0) THEN	!FIND CORRECT TYPE
	ELSE IF (IAND(TCOR,CAP_ALG).NE.0) THEN
	  WPTR=SCH_ALGC_E
	ELSE IF (IAND(TCOR,CAP_OTH).NE.0) THEN
	  WPTR=SCH_OTHC_E
	END IF
C
C assemble corrections. GN and PH contributions are accumulated
C  unconditionally; whether or not the end result will be used depends on the
C  settings in GPS. (It is assumed that COR contains no values that might cause
C  arithmetic exceptions.)
C
	DO IXY=X,Y				!X,Y
	 IF (XYS(IXY)) THEN			!does COR contain this polsn?
	   DO ITEL=0,STHTEL-1
	    IF (TELS(ITEL)) THEN
	      INDX=2*ITEL+2*STHTEL*IXY		!offset in SCH corrns array
	      GN=COR(ITEL,G,IXY)		!start with new gain and phase
	      PH=COR(ITEL,P,IXY)		! corrections in accum.
	      IF (IAND(CDAP,CAP_OTH).NE.0)
	1	 THEN				! CDAP:
C data were read in with old corrns deapplied.
C Ignore this for the present corrections, but issue a warning
C
C		GN=GN-SCHE(SCH_AOTHC_E+INDX+G)	!  subtract
C		PH=PH-SCHE(SCH_AOTHC_E+INDX+P)	!   the old ones
		IF (.NOT.WARN_CDAP) THEN
		  CALL WNCTXT(F_TP,'!AS!/!AS',
	1	'*** Current corrections derived with /DE_APPY=OTH ***',
	1	'*** Programs will use /DE_APPLY=OTH in any '//
	2	'further processing   ***')
		  WARN_CDAP=.TRUE.
	        END IF
		SCHJ(SCH_AOTHUSED_J)=1		!AOTH DE-APPLIED
	      ELSE
		SCHJ(SCH_AOTHUSED_J)=0   
	      END IF
C
	      BIT=1				!mask bit for CAP, ZAP
	      RPTR=SCH_REDC_E			!start at REDC table
	      DO CTYP=RED,OTH			!loop: REDC, ALGC, OTHC tables
		IF (IAND(ZAP,BIT).NE.0) THEN!
C
C if both CAP and ZAP are specified for this correction, the interpretation is 
C  that the current correction must be transferred from the table to the data 
C if only ZAP is specified, it is simply destroyed
C
		  IF (IAND(CAP,BIT).NE.0) THEN	! ZAP  CAP:
		    GN=GN+SCHE(RPTR+INDX+G)	!  add
		    PH=PH+SCHE(RPTR+INDX+P)	!   current
		  END IF
		  IF (GPS(G)) THEN		! ZAP  (CAP 
						!CAP)
		    SCHE(RPTR+INDX+G)=0		!  clear current gain,
		    IF (CTYP.EQ.OTH) 
	1		SCHE(SCH_EXT_E)=0	!   extinction
		  END IF
		  IF (GPS(P)) THEN		
		    SCHE(RPTR+INDX+P)=0		!    phase
		    IF (CTYP.EQ.OTH) THEN
		      SCHE(SCH_REFR_E)=0	!     troposph. refraction
		      SCHE(SCH_IREF_E)=0	!      ionosph. refraction
		      SCHE(SCH_CLKC_E)=0	!	and clock
		    ENDIF
		  END IF
		ELSE
		  IF (IAND(CAP,BIT).NE.0) THEN	! !ZAP	  CAP: nop
		  ELSE				! !ZAP	!CAP:
		    GN=GN-SCHE(RPTR+INDX+G)	!  subract current 
		    PH=PH-SCHE(RPTR+INDX+P)
		  END IF
		END IF
C
		BIT=2*BIT			!set bitmask and pointer
		RPTR=RPTR+SCH_ALGC_E-SCH_REDC_E !for next correction type
	      END DO				!end CTYP loop
C
	      IF (GPS(G)) SCHE(WPTR+INDX+G)=
	1		SCHE(WPTR+INDX+G)+GN	!add accum. to current
	      IF (GPS(P)) SCHE(WPTR+INDX+P)=
	1		WNGENR(SCHE(WPTR+INDX+P)+PH)!(modulo 2*pi)
	    END IF
	   END DO				!end of tel. loop
	 END IF
	END DO					!end of XY loop
C
C ZERO M.E. (xxxNS tables) where ZAP requested
C
	BIT=1					!ZAP test BIT
	WPTR=SCH_REDNS_E			!point WPTR at RED table
	DO CTYP=RED,OTH				!RED, ALG, OTH
	  IF (IAND(ZAP,BIT).NE.0) THEN		!ZERO THIS CORRECTION?
	    DO IXY=X,Y
	      DO IGP=G,P
		IF (GPS(IGP)) THEN		!if this component
		  IF (XYS(IXY))			! is specified,
	1		 SCHE(WPTR+2*IXY+IGP)=0 ! zero it
		END IF
	      ENDDO
	    END DO
	  END IF
	  BIT=2*BIT				!NEXT CORRECTION bit
	  WPTR=WPTR+SCH_ALGNS_E-SCH_REDNS_E	!next table
	END DO
C
C zero atmosphere and clock parameters
C
	IF (IAND(ZAP,CAP_XTNC).NE.0) THEN	!zero EXTINCTION?
	  SCHE(SCH_EXT_E)=0
	END IF
	IF (IAND(ZAP,CAP_REFR).NE.0) THEN	!zero REFRACTION?
	  SCHE(SCH_REFR_E)=0
	END IF
	IF (IAND(ZAP,CAP_FAR).NE.0) THEN	!zero FARADAY?
	  SCHE(SCH_FARAD_E)=0
	END IF
	IF (IAND(ZAP,CAP_IREF).NE.0) THEN	!zero REFRACTION?
	  SCHE(SCH_IREF_E)=0
	END IF
	IF (IAND(ZAP,CAP_CLK).NE.0) THEN	!zero CLOCK CORRECTION?
	  SCHE(SCH_CLKC_E)=0
	END IF
C
C SET M.E. (xxxNS tables)
C
	IF (DOME) THEN				!M.E. ASKED
C
C  point WPTR at target ME table
C
	  WPTR=SCH_REDNS_E			!ASSUME REDUNDANCY
	  IF (IAND(TCOR,CAP_RED).NE.0) THEN	!FIND CORRECT TYPE
	  ELSE IF (IAND(TCOR,CAP_ALG).NE.0) THEN
	    WPTR=SCH_ALGNS_E
	  ELSE IF (IAND(TCOR,CAP_OTH).NE.0) THEN
	    WPTR=SCH_OTHNS_E
	  END IF
C
	  DO IXY=X,Y				!X,Y
	    IF (XYS(IXY)) THEN			!PRESENT
	      DO IGP=G,P			!gain, phase
		IF (GPS(IGP)) THEN
		  IF (IAND(SCHJ(SCH_BITS_J),
	1		FL_ALL)			!scan not deleted
	1		.EQ.0 .OR.
	1		ABS(ME(IGP,IXY)).GT.	! or new ME > old   
	1		ABS(SCHE(WPTR+2*IXY+IGP)))
	1	    SCHE(WPTR+2*IXY+IGP)=ME(IGP,IXY)
		END IF
	      ENDDO				!end G,P loop
	    END IF
	  END DO				!end X,Y loop
	END IF					!END DOME
C
C REWRITE SCAN HEADER
C
	IF (.NOT.WNFWR(FCA,SCHHDL,SCH,SCNP)) GOTO 900 !WRITE SCAN HEADER
C
	RETURN
C
C ERROR
C
 900	CONTINUE
	NSCSWC=.FALSE.
C
	RETURN
C
C
	END
