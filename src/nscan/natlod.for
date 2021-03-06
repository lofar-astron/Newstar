C+ NATLOD.FOR
C  WNB 920506
C
C  Revisions:
C	WNB 921028	Add selections
C	WNB 921125	More selections
C	WNB 930405	Add IF selection
C	WNB 930819	Remove STH
C	CMV 931220	Changed parameters of call to NSCPFL
C
	SUBROUTINE NATLOD
C
C  Load ATNF WSRT data into SCN file
C
C  Result:
C
C	CALL NATLOD	will load ATNF data in SCN file
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NAT_DEF'
	INCLUDE 'RPF_DEF'		!RPFITS DATA
	INCLUDE 'GFH_O_DEF'		!GENERAL FILE HEADER
	INCLUDE 'SGH_O_DEF'		!SUB-GROUP HEADER
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNFOP,WNFOPF		!OPEN FILE
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
	INTEGER WNFEOF			!FILE POSITION
	DOUBLE PRECISION WNGDNF		!NORM. ANGLE
	INTEGER WNCALN			!STRING LENGTH
	LOGICAL WNDLNG			!LINK SUB-GROUP
	LOGICAL WNGGVM			!GET MEMORY
	LOGICAL NATRPO			!READ FIRST HEADER
	LOGICAL NATRPH			!READ NEXT HEADER
C
C  Data declarations:
C
	INTEGER FCAPT			!INPUT FILE POINTER
	CHARACTER*6 LTXT		!LABEL NAME
	INTEGER FCAT			!TMP FILE DESCRIPTOR
	INTEGER TAB(MAX_SU,MAX_IF,0:10)	!TMP DATA
	  REAL TABE(MAX_SU,MAX_IF,0:10)
	  EQUIVALENCE (TAB,TABE)
	LOGICAL FILLED			!DATA READ IN BUFFER
	INTEGER IFRS			!INTERFEROMETERS SEEN
	INTEGER VISP,BUFP		!DATA BUFFER PTRS
C-
C
C INIT
C
	IF (.NOT.WNFOP(FCAT,'NSCAN.TMP','WT')) THEN !OPEN TMP FILE
	  CALL WNCTXT(F_TP,'Cannot open TMP file (!XJ)',E_C)
	  GOTO 900
	END IF
	J1=0					!JOB COUNT
 30	CONTINUE
	J1=J1+1					!NEXT JOB
	IF (J1.GT.NJOB) GOTO 900		!READY
	J=0					!START LABEL INPUT
	IF (.NOT.WNDLNG(GFH_LINKG_1,0,SGH_GROUPN_1,FCAOUT,SGPH(0),
	1		SGNR(0))) THEN
	  CALL WNCTXT(F_TP,'!/Cannot create sub-group')
	  GOTO 800				!NEXT JOB
	END IF					!SUB-GROUP LINKED
	CALL WNCTXT(F_P,'!_')			!NEW PAGE
	CALL WNCTXT(F_TP,'!/Job !UJ\: Group !UJ',J1,SGNR(0))
C
C DO A LABEL
C
 10	CONTINUE
	J=J+1					!COUNT INPUT LABEL
	IF (NLAB(J1).LT.0) THEN			!ALL LABELS ON TAPE
	  J0=J					!NEXT INPUT LABEL
	ELSE IF (J.LE.NLAB(J1)) THEN
	  J0=ILAB(J,J1)				!NEXT INPUT LABEL
	ELSE
	  GOTO 800				!READY WITH JOB
	END IF
C
C OPEN INPUT
C
	IF (UNIT.EQ.'D') THEN			!DISK INPUT
	  IF (INDEX(IFILE(J1),'.').EQ.0) THEN	!NO EXT.
	    CALL WNCTXS(LTXT,'!6$ZJ',J0)	!MAKE LABEL NAME
	    IF (.NOT.WNFOP(IMCA,IFILE(J1)(1:WNCALN(IFILE(J1)))
	1		//'.'//LTXT,'R')) THEN
	      CALL WNCTXT(F_TP,'Cannot find file !AS\.!AS',
	1		IFILE(J1),LTXT)
	      GOTO 800				!STOP JOB
	    END IF
	  ELSE
	    IF (.NOT.WNFOP(IMCA,IFILE(J1)(1:WNCALN(IFILE(J1))),
	1		'R')) THEN
	      CALL WNCTXT(F_TP,'Cannot find file !AS',
	1		IFILE(J1))
	      GOTO 800				!STOP JOB
	    END IF
	  END IF
	ELSE					!TAPE INPUT
	  IF (.NOT.WNFOPF(IMCA,' ','R',0,0,0,J0)) THEN
	    CALL WNCTXT(F_TP,'!/Cannot find label !UJ',J0)
	    GOTO 800				!NEXT JOB
	  END IF
	END IF
C
C READ FIRST HEADER AND TABLES
C
 20	CONTINUE
	IF (.NOT.NATRPO(IMCA,FCAPT)) THEN	!READ HEADER
	  CALL WNCTXT(F_TP,'Read error header at !XJ',FCAPT)
	  GOTO 700				!NEXT LABEL
	END IF
C
C GET POLARISATIONS TO DO
C
 21	CONTINUE
C
C CHECK SELECTION
C
	J2=0					!NO BANDS
	DO I=1,N_IF				!BANDS
	  TAB(1,I,8)=0				!NOT SELECT
	  IF (NIFS(J1).GE.0) THEN		!MUST CHECK FOR IF
	    DO I1=1,NIFS(J1)
	      IF (IIFS(I1,J1).EQ.I) THEN
		TAB(1,I,8)=1			!CHECK THIS ONE FOR BAND
	      END IF
	    END DO
	  ELSE
	    TAB(1,I,8)=1			!CHECK THIS ONE FOR BAND
	  END IF
	  IF (TAB(1,I,8).NE.0) THEN		!COULD WANT THIS IF
	    TAB(1,I,8)=0			!ASSUME NOT
	    IF (NBND(J1).GE.0) THEN		!MUST CHECK
	      DO I1=1,NBND(J1)
	        IF (ABS(LOG(DCL*100./IF_FREQ(I)/IBND(I1,J1))/LOG(2D0))
	1		.LE.0.5) THEN		!FOUND SELECTION
		  J2=J2+1
		  TAB(1,I,8)=1
	        END IF
	      END DO
	    ELSE
	      TAB(1,I,8)=1			!SELECT
	      J2=J2+1
	    END IF
	  END IF
	END DO
	J3=0					!NONE SELECTED
	DO I=1,N_SU				!SOURCES
	  TAB(I,1,9)=0				!NOT SELECTED
	  IF (NSRC(J1).GE.0) THEN		!MUST CHECK
	    DO I1=1,NSRC(J1)
	      I2=WNCALN(ISRC(I1,J1))		!LENGTH CHECK NAME
	      IF (ISRC(I1,J1)(1:I2).EQ.SU_NAME(I)(1:I2)) THEN
	        J3=J3+1				!COUNT
		TAB(I,1,9)=1			!SET SELECTED
	      END IF
	    END DO
	  ELSE
	    J3=J3+1				!COUNT
	    TAB(I,1,9)=1			!SET SELECTED
	  END IF
	END DO
	IF (J2.EQ.0 .OR. J3.EQ.0) GOTO 50	!NOTHING SELECTED
C
C READ DATA IN TMP
C
	I0=1					!FREQ. CHANNELS
	I1=1					!POL.
	DO I=1,N_IF
	  I0=MAX(I0,IF_NFREQ(I))
	  I1=MAX(I1,IF_NSTOK(I))
	END DO
	IF (.NOT.WNGGVM(LB_X*I0*I1,VISP)) THEN	!VIS. BUFFER
 41	  CONTINUE
	  CALL WNCTXT(F_TP,'No memory for visibility buffers')
	  CALL WNGEX				!STOP
	END IF
	IF (.NOT.WNGGVM(MXNIFR*LB_X*I0*I1,BUFP)) GOTO 41 !TIME SLICE BUFFER
	FILLED=.FALSE.				!NO DATA YET
 40	CONTINUE
	CALL NATLRD(IMCA,FCAPT,FCAT,TAB,TABE,FILLED,IFRS,VISP,BUFP) !READ
C
C MAKE SCN FILE
C
	IFRS=IAND(IFRS,NOT('148841'X))		!DELETE AUTO CORRELATIONS
	CALL NATLWD(FCAT,TAB,TABE,BUFP,IFRS,J1)	!WRITE SCN FILE AND HEADERS
	IF (FILLED) GOTO 40			!MORE DATA
	CALL WNGFVM(LB_X*I0*I1,VISP)		!FREE MEMORY BUFFERS
	CALL WNGFVM(MXNIFR*LB_X*I0*I1,BUFP)
C
C MORE HEADERS
C
 50	CONTINUE
	IF (NATRPH(IMCA,FCAPT)) GOTO 21		!NEXT HEADER
	IF (E_C.NE.3) THEN			!NOT EOF
	  IF (NATRPH(IMCA,FCAPT)) GOTO 21	!LOOK AGAIN
	END IF
C
C FINISH HEADER
C
 600	CONTINUE
C
C FINISH LABEL
C
 700	CONTINUE
	CALL WNFCL(IMCA)			!CLOSE LABEL
	GOTO 10					!NEXT LABEL
C
C FINISH JOB
C
 800	CONTINUE
	GOTO 30					!NEXT JOB
C
C READY
C
 900	CALL WNFCL(IMCA)			!CLOSE INPUT
	CALL WNFDMO(IMCA)			!DISMOUNT INPUT
	CALL WNFCL(FCAT)			!CLOSE/DELETE TMP FILE
	CALL NSCPFH(F_TP,FCAOUT)		!SHOW FILE HEADER
	CALL NSCPFL(F_TP,FCAOUT,NODOUT,.FALSE.)	!SHOW LAYOUT
	CALL WNFCL(FCAOUT)			!CLOSE OUTPUT
C
	RETURN					!READY
C
C
	END
