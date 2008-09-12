C+ NFLPRT.FOR
C  WNB 900810
C
C  Revisions:
C	JPH 930323	comments; 6 --> 3*LB_I. - L_I/L_B --> LB_I
C	HjV 930423	Change name of some keywords
C	JPH 930518	Call WNDSTI to display "." sector name
C	WNB 930607	New weights; remove WNDSTI; remove edit stmt
C			Remove NSCEDI (nowhere used/documented, problems):
C				but cater for print only
C	WNB 930608	Add NAME and FLAGS in SECTOR_ACTION
C	WNB 930609	Add HA/POL limits for FLAGS
C	JPH 930611	Add polarisation to data output headers/footers
C	WNB 930614	Add polarisation to flag headings; delete F option
C	HJV 930618	Symbolic names for mask bits in CBITS_O_DEF
C	WNB 930618	Rename from NSCPRT
C	WNB 930623	Add SCAN T option
C	CMV 930803	Output telescope D in Flag-count and SCAN Flags
C	WNB 930807	Change to CBITS_DEF
C	WNB 930825	New pol. selection
C	WNB 930826	New HA range
C	HjV 930914	NSCIFS is now a function iso. a subroutine
C	CMV 930929	Initialise POLNM together with POLC
C	CMV 931105	Change default for SET_ACTION to NEXT
C	JPH 931123	For W option, show zero weights i.s.o. dots
C	WNB 931215	No auto show if edit
C	CMV 931220	Add FILE_ACTION option OVERVIEW
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	CMV 940209	Pass Category code to NSCPSH
C	CMV 940228	Add option to print corrected data
C	CMV 940303	Correct stupid error (of CMV) -180/+180 binning
C	CMV 940315	Change format for printing catagory to !AF
C	CMV 940415	Do not scale phase while printing data
C	CMV 940415	Correct double use of I3 in T/W options (use OPOL)
C	CMV 940420	Correct test for UNCOR (was OPT(1:3).EQ.'U')
C	CMV 940425	Add TP and GN options for total power data 
C	CMV 940428	Split off 'T' option in NCATEL
C	CMV 940429	Add IFH option for SECTOR_ACTION
C	CMV 940506	Add IFR option for SCAN_ACTION
C	CMV 940518	Add IFRS argument to NCATEL
C	JPH 940725	Remove '.' from diagonal of D and W display
C	JPH 941213	Remove GOTO 200 after FLAG display
C       HjV 960403	Small structure changes (Less goto's, more if/else/endif)
C			in scan action and data display part.
C			Type text for COR/UNCOR and if something NOT present
C	CMV 000928	Correct bug in COR output (J reused), allow skipping over
C			set-boundaries when stepping scans
C
C
	SUBROUTINE NFLPRT
C
C  Show/edit data in SCN file
C
C  Result:
C
C	CALL NFLPRT	will show/edit data in SCN file
C
C PIN references:
C
C	INPUT_SCN_NODE
C	SCN_SETS
C	FILE_ACTION
C	SET_ACTION
C	SCAN_ACTION
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
	INCLUDE 'OHW_O_DEF'		!OH BLOCK
	INCLUDE 'NFL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
	LOGICAL WNDNOD,WNDNOC		!GET/CHANGE NODE NAME
	LOGICAL WNFOP			!OPEN FILE
	LOGICAL WNFRD			!READ FILE
	LOGICAL WNDSTA			!GET SETS TO DO
	CHARACTER*32 WNTTSG		!SET NAME
	LOGICAL NSCSTG			!GET A SET
	LOGICAL NSCPLS			!SELECT POL.
	LOGICAL NSCHAS			!SELECT HA
	LOGICAl NSCIFS			!Select/deselect interferometers
	REAL WNGEFD			!FRACTIONS TO DEGREES
	LOGICAL NMOMSC			!Set model in SCN file
	LOGICAL NMORDH			!Read model header
	LOGICAL NSCSCR			!Read corrected data
	LOGICAL NSCSIF			!READ IFR TABLE
	LOGICAL NSCGIF			!Read IF/Total Power data
	LOGICAL NSCGF2			!Show IF header
C
C  Data declarations:
C
	CHARACTER*24 ACT		!ACTION ASKED
	LOGICAL NEWHA			!DISPLAY NEW HA
	LOGICAL GOSCN			!SHOW SCN FOR NEW SET
	INTEGER STHP			!SUB-GROUP POINTER
	INTEGER SCHP
	INTEGER SNAM(0:7)		!SET NAME
	INTEGER*2 IFRT(0:STHIFR-1)	!IFR TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)    ! IFR TABLE
	REAL ANG(0:2,0:STHIFR-1)        ! DIPOLE ANGLES
	CHARACTER*80 IFRC		!IFR LIST
	CHARACTER*12 F1000		!FORMATS
	REAL HARAN(0:1)			!HA RANGE
C
	INTEGER POLC			!index SELECTED POLARISATION in CDAT
	CHARACTER*2 POLNM		! and its ASCII name
	  DATA POLNM/'XX'/
	INTEGER SPOL,OPOL		!POL. CODE and Offset in ODAT
	INTEGER PPOL(0:3,1:4,0:1)	!POL. SELECT XX,XY,YX,YY FOR 
					! NPOL=1:4:
	  DATA  PPOL/XX_P,0,0,0, XX_P,0,0,YY_P, 0,0,0,0,
	1			XX_P,XY_P,YX_P,YY_P, !BITS
	1	     0,0,0,0, 0,0,0,1, 0,0,0,0, 0,1,2,3/ !OFFSETS
C
	INTEGER IFRCP(2,4)		!OFFSETS FOR IFR CORRECTION POINTERS
	  DATA IFRCP/SCH_IFRAC_J, 0,
	1	     SCH_IFRMC_J, 1,
	1	     SCH_AIFRAC_J,0,
	1	     SCH_AIFRMC_J,1/	!0:additive, 1:multiplicative
	CHARACTER*50 IFRCC(4)		!Descriptions
	  DATA IFRCC/'Additive ifr. correction (apply)',
	1	     'Multiplicative ifr. correction (apply)',
	1	     'Additive ifr. correction (de-apply)',
	1	     'Multiplicative ifr. correction (de-apply)'/
C
	LOGICAL DO_COR			!Show corrected data?
	  DATA  DO_COR/.FALSE./
	INTEGER CORAP,CORDAP			!Corrections to apply/deapply
	INTEGER NSRC(0:2)			!Source counts in model
	INTEGER STP				!Source type of model
	DOUBLE PRECISION SRA,SDEC,SFRQ		!Model info
	REAL UV0(0:3)				!Basic uv coordinates
	REAL LM0(0:1)				!Basic source displacement
	DOUBLE PRECISION FRQ0			!Basic frequency
	REAL TF(0:1)				!Integr. time, bandwidth
	INTEGER MINST				!Instrument
	COMPLEX CV1,CV2				!Complex data,model
C
	REAL WGT(0:STHIFR-1,0:3)		!Weights
	REAL DAT(0:1,0:STHIFR-1,0:3)		!Data
	  COMPLEX CDAT(0:STHIFR-1,0:3)
	  EQUIVALENCE(DAT,CDAT)
	COMPLEX CMOD(0:3,0:STHIFR-1)		!Model IQUV
	COMPLEX CAMOD(0:STHIFR-1,0:3)		!Model XYX
C
	REAL HA				!HA
	REAL R2
	CHARACTER CATEG*32		!CATEGORY/TYPE/SPEFU
	CHARACTER*(STHTEL) TELS		!TELESCOPE NAMES
	  DATA TELS/'0123456789ABCD'/
	BYTE IFRS(0:STHTEL-1,0:STHTEL-1) !IFR SELECTION
	INTEGER IFRSX(0:STHTEL-1,0:STHTEL-1)	!IFR SELECTION
	INTEGER ICNT(0:STHTEL-1,0:STHTEL-1,0:1) !DATA/FLAG COUNTS
C
	REAL PCGAN(0:STHTEL-1,0:1)	!BUFFER FOR GAINS
	REAL PCPHA(0:STHTEL-1,0:1)	!BUFFER FOR PHASE OFFSETS
	REAL IFBUF(0:STHTEL-1,0:1)	!BUFFER FOR Total Powers/Gains
	REAL IFRCOR(2,0:4*STHIFR-1)	!BUFFER FOR Interferometer corrections
C
	BYTE STH(0:STHHDL-1)		!SET HEADER
	  INTEGER STHJ(0:STHHDL/4-1)
	  INTEGER*2 STHI(0:STHHDL/2-1)
	  REAL STHE(0:STHHDL/4-1)
	  EQUIVALENCE (STH,STHJ,STHI,STHE)
	BYTE SCH(0:SCHHDL-1)		!SCAN HEADER
	  INTEGER SCHJ(0:SCHHDL/LB_J-1)
	  EQUIVALENCE (SCH,SCHJ)
	BYTE OHW(0:OHW__L-1)		!OH BLOCK
	INTEGER*2 ODAT(0:2,0:800)	!DATA
	REAL RDAT(0:STHIFR-1,1:2)	!CORRECTED DATA
C
	CHARACTER*78 TEXT(-1:STHTEL)	!TEXT LINES
	CHARACTER*1 MODE		!FILE-OPEN MODE
	COMPLEX C0
C-
C
C INIT
C
C*****************************************************************************
C GET NODE 
C
 100	CONTINUE
	MODE='R'				!ASSUME READ-ONLY FOR NOW
	CALL WNFCL(FCAIN)
	IF (.NOT.WNDNOD('INPUT_SCN_NODE',' ','SCN',MODE,NODIN,IFILE)) THEN !NODE
C*******
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN	!READY WITH SHOW
	  CALL WNCTXT(F_TP,'Node does not exist')
	  GOTO 100
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  RETURN				!END
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  GOTO 100				!MUST SPECIFY
	END IF
	IF (.NOT.WNFOP(FCAIN,IFILE,MODE)) THEN	!OPEN SCN FILE
	  CALL WNCTXT(F_TP,'Cannot open file attached to node')
	  GOTO 100
	END IF
	CALL NSCPFH(F_TP,FCAIN)			!PRINT FILE HEADER
C
C*****************************************************************************
C FILE ACTION
C
 101	CONTINUE
C FILE_ACTION *******
	IF (.NOT.WNDPAR('FILE_ACTION',ACT,LEN(ACT),J,'CONT')) THEN !FILE ACTION
	  IF (E_C.EQ.DWC_ENDOFLOOP) THEN	!^Z
 102	    CONTINUE
	    CALL WNFCL(FCAIN)			!CLOSE FILE
	    GOTO 100				!RETRY NODE
	  END IF
	  GOTO 101				!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  GOTO 102				!READY
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  ACT='CONT'				!ASSUME CONT
	END IF
! *** LAYOUT
	IF (ACT.EQ.'LAYOUT') THEN		!SHOW LAYOUT
	  CALL NSCPFL(F_TP,FCAIN,NODIN,.FALSE.)	!SHOW LAYOUT
! *** OVERVIEW
	ELSE IF (ACT.EQ.'OVERVIEW') THEN	!SHOW OVERVIEW
	  CALL NSCPFL(F_TP,FCAIN,NODIN,.TRUE.)	!SHOW OVERVIEW
	ELSE IF (ACT.EQ.'SHOW') THEN		!SHOW DETAILS
	  CALL NSCXFH(F_TP,FCAIN)
! *** EDIT
	ELSE IF (ACT.EQ.'EDIT') THEN		!EDIT
	  IF (MODE.EQ.'R') THEN			!CHANGE TO UPDATE MODE
	    MODE='U'				!MAKE UPDATE
	    CALL WNFCL(FCAIN)			!CLOSE FILE
	    IF (.NOT.WNDNOC(' ',' ','SCN',MODE,' ',IFILE)) THEN !CHANGE DATES
	      CALL WNCTXT(F_TP,'Node is not writable')
	      GOTO 100
	    END IF
	    IF (.NOT.WNFOP(FCAIN,IFILE,MODE)) THEN !OPEN SCN FILE
	      CALL WNCTXT(F_TP,'Cannot write to file attached to node')
	      GOTO 100
	    END IF
	  END IF
	  CALL NSCEFH(F_TP,FCAIN)		!EDIT HEADER
	ELSE IF (ACT.EQ.'CONT') THEN		!CONT
	  GOTO 200				!DO SET
	ELSE
	  GOTO 102				!QUIT
	END IF
	GOTO 101				!UNKNOWN
C
C*****************************************************************************
C SETS
C
 200	CONTINUE
	GOSCN=.FALSE.	!DO NOT SHOW SCN RIGHT AWAY
C SETS *******
	IF (.NOT.WNDSTA('SCN_SETS',MXNSET,SETS,FCAIN)) GOTO 102 !GET SETS TO DO
	IF (SETS(0,0).EQ.0) GOTO 102		!NONE
 201	CONTINUE				!DO NEXT SET
	IF (.NOT.NSCSTG(FCAIN,SETS,STH,STHP,SNAM)) GOTO 102 !GET SET
C
	CATEG='??/??/??'			!Reset category
	IF (STHJ(STH_OHP_J).NE.0) THEN		!Read OH if any
	   IF (WNFRD(FCAIN,STHJ(STH_NOH_J),OHW,
	1		STHJ(STH_OHP_J))) THEN
	      CALL WNCTXS(CATEG,'!AF/!AF/!AF',
	1	           OHW(OHW_CATEG_1),OHW_CATEG_N,  !Astr. category
	1	           OHW(OHW_SPEFU_1),OHW_SPEFU_N,  !Special functions
	1	           OHW(OHW_TYPE_1), OHW_TYPE_N)   !Obs. type-code
	   END IF
	END IF
C
	CALL NSCPSH(F_TP,STH,SNAM,CATEG)	!SHOW SET HEADER
C
C READ INTERFEROMETER TABLE AND GET SELECTION 
C  (do it here since we need it in the Scan actions)
C
	IF (.NOT.NSCSIF(FCAIN,STH,IFRT,IFRA,ANG)) THEN !READ IFR TABLE
	   CALL WNCTXT(F_TP,'Error reading interferometer table')
	   GOTO 201				!RETRY NEXT SET
	END IF
	DO I=0,STHTEL-1			!SET ALL NOT PRESENT
	  DO I1=0,STHTEL-1
	    IFRSX(I1,I)=-1
	    IFRS(I1,I)=.FALSE.
	  END DO
	END DO
	DO I=0,STHJ(STH_NIFR_J)-1		!MAKE SELECTION
	   IFRSX(MOD(IFRT(I),256),IFRT(I)/256)=I
	   IFRS(MOD(IFRT(I),256),IFRT(I)/256)=.TRUE.
	END DO
	IF (GOSCN) GOTO 400
C
C*****************************************************************************
C SET ACTION
C
 301	CONTINUE
C SET_ACTION *******
	IF (.NOT.WNDPAR('SECTOR_ACTION',ACT,
	1	LEN(ACT),J,'NEXT')) THEN	!Was 'CONT', 931105 CMV
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 101	!^Z, RETRY FILE ACTION
	  GOTO 301				!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  GOTO 101				!RETRY FILE ACTION
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  ACT='CONT'				!ASSUME CONT
	END IF
! *** NEXT
	IF (ACT.EQ.'NEXT') THEN			!NEXT SET
	  GOTO 201				!NEXT SET
! *** NAME
	ELSE IF (ACT.EQ.'NAME') THEN		!FULL NAME
	  CALL WNCTXT(F_TP,'!/Input name:  !AS',WNTTSG(SNAM,0))
	  CALL WNDSTI(FCAIN,SNAM)
	  CALL WNCTXT(F_TP,'Index name:  !AS',WNTTSG(SNAM,0))
! *** IFH
	ELSE IF (ACT.EQ.'IFH') THEN		!SHOW IF-HEADER
	  JS=NSCGF2(FCAIN,STH,F_TP)
! *** IFRS
	ELSE IF (ACT.EQ.'IFRS') THEN		!SHOW INTERFEROMETERS
	  CALL WNCTXT(F_TP,' ')
	  DO I=0,STHJ(STH_NIFR_J)-1,25		!SHOW LIST
	    IFRC=' '				!EMPTY STRING
	    DO I1=I,MIN(STHJ(STH_NIFR_J),I+25)-1
	      J=MOD(IFRT(I1),256)
	      J1=IFRT(I1)/256
	      CALL WNCTXS(IFRC(3*(I1-I)+1:3*(I1-I)+2),
	1		'!1$XJ!1$XJ',J,J1)
	    END DO
	    CALL WNCTXT(F_TP,'!AS',IFRC)	!SHOW
	  END DO
	  IF (.NOT.NSCIFS(100,IFRS)) GOTO 301 	!SHOW SELECTION
! *** FLAGS
	ELSE IF (ACT.EQ.'FLAGS') THEN
	  IF (.NOT.NSCHAS(1,HARAN)) GOTO 301	!GET HA RANGE
	  IF (.NOT.NSCPLS(1,SPOL)) GOTO 301	!SELECT POL.
	  DO I=0,1				!CLEAN COUNTS
	    DO I1=0,STHTEL-1
	      DO I2=0,STHTEL-1
	        ICNT(I2,I1,I)=0
	      END DO
	    END DO
	  END DO
 302	  CONTINUE				!ADD NEXT SET
	  DO J=0,STHJ(STH_SCN_J)-1		!ALL SCANS
	    R0=STHE(STH_HAB_E)+J*STHE(STH_HAI_E) !HA OF SCAN
	    IF (R0.GE.HARAN(0)-STHE(STH_HAI_E)/2+1E-5 .AND.
	1		R0.LE.HARAN(1)+STHE(STH_HAI_E)/2-1E-5) THEN !DO
	      SCHP=STHJ(STH_SCNP_J)+J*STHJ(STH_SCNL_J) !SCAN HEADER POINTER
	      IF (.NOT.WNFRD(FCAIN,SCHHDL,SCH,SCHP)) THEN !READ SCAN
 303	        CONTINUE
	        CALL WNCTXT(F_TP,'Error reading scan file')
	        GOTO 200			!RETRY SETS
	      END IF
	      IF (.NOT.WNFRD(FCAIN,
	1		3*LB_I*STHI(STH_PLN_I)*STHJ(STH_NIFR_J),
	1		ODAT(0,0),SCHP+SCHHDL))
	1			GOTO 303	!READ DATA
	      DO I1=0,3				!DO ALL POL.
	        IF (IAND(PPOL(I1,STHI(STH_PLN_I),0),SPOL).NE.0) THEN !SEL.
	          I0=PPOL(I1,STHI(STH_PLN_I),1)	!OFFSET
	          DO I2=0,STHJ(STH_NIFR_J)-1	!ALL INTERFEROMETERS
	            I=STHI(STH_PLN_I)*I2+I0	!DATA POINTER
	            IF (ODAT(0,I).NE.0) THEN	!DATA PRESENT
		      I4=MOD(IFRT(I2),256)	!TELESCOPES
		      I5=IFRT(I2)/256
		      ICNT(I4,I5,0)=ICNT(I4,I5,0)+1 !COUNT
	              I3=ODAT(0,I)		!WEIGHT/FLAGS
		      IF (IAND(I3,FL_ALL).NE.0 .OR.
	1			IAND(SCHJ(SCH_BITS_J),FL_ALL).NE.0) THEN
		        ICNT(I4,I5,1)=ICNT(I4,I5,1)+1 !COUNT FLAGS
		      END IF
	            END IF
	          END DO			!IFRS
	        END IF
	      END DO				!POL.
	    END IF
	  END DO				!SCAN
	  IF (NSCSTG(FCAIN,SETS,STH,STHP,SNAM)) THEN !MORE SETS
	    IF (.NOT.WNFRD(FCAIN,(LB_I)*STHJ(STH_NIFR_J),IFRT,
	1		STHJ(STH_IFRP_J))) THEN	!READ IFRS
	      CALL WNCTXT(F_TP,'Error reading scan file')
	      GOTO 201				!RETRY NEXT SET
	    END IF
	    GOTO 302				!ADD DATA
	  END IF
	  CALL WNCTXT(F_TP,'!/!32C\Flag count')	!SHOW RESULT
	  TEXT(-1)=' '
	  DO I=0,STHTEL-1
	    TEXT(-1)(I*5+6:I*5+6)=TELS(I+1:I+1)
	  END DO
	  TEXT(-1)(74:74)='.'
	  CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(-1))
	  DO I=0,STHTEL				!SET NO DATA
	    TEXT(I)=' '
	    DO I2=0,STHTEL-1
	      TEXT(I)(5*I2+6:5*I2+6)='.'
	    END DO
	    IF (I.NE.STHTEL) THEN
	      TEXT(I)(74:74)=TELS(I+1:I+1)
	    ELSE
	      TEXT(I)(74:74)='.'
	    END IF
	    IF (I.NE.0) THEN
	      TEXT(I)(1:1)=TELS(I:I)
	    END IF
	  END DO
	  DO I=0,STHTEL-1			!SHOW COUNTS
	    DO I1=0,STHTEL-1
	      IF (ICNT(I1,I,1).GT.0) THEN
		CALL WNCTXS(TEXT(I1)(5*I+3:5*I+7),
	1			'!5$UJ',MIN(ICNT(I1,I,1),9999))
	      END IF
	      IF (ICNT(I1,I,0).GT.0) THEN
		CALL WNCTXS(TEXT(I+1)(5*I1+3:5*I1+7),
	1			'!5$UJ',MIN(ICNT(I1,I,0),9999))
	      END IF
	    END DO
	  END DO
	  DO I=0,STHTEL				!SHOW DATA
	    CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(I))
	  END DO
	  CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(-1)) !BOTTOM
	  CALL WNCTXT(F_TP,'!32C\Data count!/')	!HEADINGS
!!	  GOTO 200				!ASK NEW SETS
! *** SHOW
	ELSE IF (ACT.EQ.'SHOW') THEN		!SHOW DETAILS
	  CALL NSCXSH(F_TP,FCAIN,STHP,SNAM)
! *** EDIT
	ELSE IF (ACT.EQ.'EDIT') THEN		!EDIT
	  IF (MODE.EQ.'R') THEN			!CHANGE TO UPDATE MODE
	    MODE='U'				!MAKE UPDATE
	    CALL WNFCL(FCAIN)			!CLOSE FILE
	    IF (.NOT.WNDNOC(' ',' ','SCN',MODE,' ',IFILE)) THEN !CHANGE DATES
	      CALL WNCTXT(F_TP,'Node is not writable')
	      GOTO 100
	    END IF
	    IF (.NOT.WNFOP(FCAIN,IFILE,MODE)) THEN !OPEN SCN FILE
	      CALL WNCTXT(F_TP,'Cannot write to file attached to node')
	      GOTO 100
	    END IF
	  END IF
	  CALL NSCESH(F_TP,FCAIN,STHP,SNAM)	!EDIT
! *** CONT
	ELSE IF (ACT.EQ.'CONT') THEN		!CONT
	  GOTO 400				!DO DATA
	ELSE
	  GOTO 101				!QUIT
	END IF
	GOTO 301				!UNKNOWN
C
C*****************************************************************************
C SCAN ACTION
C
 400	CONTINUE
	HA=STHE(STH_HAB_E)-STHE(STH_HAI_E)	!START HA
	NEWHA=.TRUE.				!DISPLAY HA
	GOSCN=.FALSE.				!RESET JUMP FLAG
	POLC=0					!START POL.
	POLNM='XX'
	DO_COR=.FALSE.
C
 401	CONTINUE
	IF (NEWHA) THEN				!DISPLAY HA
	  IF (HA.GE.STHE(STH_HAB_E)+
	1	STHJ(STH_SCN_J)*STHE(STH_HAI_E)) THEN
	    GOSCN=.TRUE.			!JUMP BACK FOR NEXT SET
	    GOTO 201				!GET NEXT SET
	  END IF
	  HA=MIN(MAX(STHE(STH_HAB_E),HA),STHE(STH_HAB_E)+
	1		(STHJ(STH_SCN_J)-1)*STHE(STH_HAI_E))	!CORRECT HA
	  J=NINT((HA-STHE(STH_HAB_E))/STHE(STH_HAI_E))	!SCAN NUMBER
	  OPOL=PPOL(POLC,STHI(STH_PLN_I),1)		!OFFSET IN ODAT
	  SCHP=STHJ(STH_SCNP_J)+J*STHJ(STH_SCNL_J)	!SCAN HEADER POINTER
	  IF (.NOT.WNFRD(FCAIN,SCHHDL,SCH,SCHP)) THEN	!READ SCAN
 411	    CONTINUE
	    CALL WNCTXT(F_TP,'Error reading scan file')
	    GOTO 400				!RETRY
	  END IF
	  IF (.NOT.WNFRD(FCAIN,
	1	3*LB_I*STHI(STH_PLN_I)*STHJ(STH_NIFR_J),
	1	ODAT(0,0),SCHP+SCHHDL))
	1		GOTO 411		!READ DATA
	  CALL NSCPSL(F_TP,SCH,SNAM,STH,J)	!SHOW SCAN HEADER
	  NEWHA=.FALSE.
	END IF
C
C SCAN_ACTION *******
	IF (.NOT.WNDPAR('SCAN_ACTION',ACT,LEN(ACT),J,'>')) THEN !ACTION
	  IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 301	!^Z, RETRY SET ACTION
	  GOTO 401				!RETRY
	ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	  GOTO 301				!RETRY SET ACTION
	ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	  ACT='>'				!ASSUME >
	END IF
	CALL WNCAUC(ACT)			!MAKE UC
! *** >
	IF (ACT(1:1).EQ.'>') THEN		!>
	  IF (ACT(2:).EQ.' ') THEN
	    I=1					!ASSUME 1 STEP
	  ELSE
	    WRITE (UNIT=F1000,FMT=1001,ERR=403) LEN(ACT)-1
 1001	    FORMAT('(BN,I',I4.4,')')
	    READ(UNIT=ACT(2:),FMT=F1000,ERR=403) I !GET I
	  END IF
	  HA=HA+I*STHE(STH_HAI_E)		!SELECT HA
	  NEWHA=.TRUE.				!DISPLAY HA
! *** <
	ELSE IF (ACT(1:1).EQ.'<') THEN		!<
	  IF (ACT(2:).EQ.' ') THEN
	    I=1					!ASSUME 1 STEP
	  ELSE
	    WRITE (UNIT=F1000,FMT=1001,ERR=403) LEN(ACT)-1
	    READ(UNIT=ACT(2:),FMT=F1000,ERR=403) I !GET I
	  END IF
	  HA=HA-I*STHE(STH_HAI_E)		!SELECT HA
	  NEWHA=.TRUE.				!DISPLAY HA
! *** Q
	ELSE IF (ACT(1:1).EQ.'Q') THEN		!QUIT
	  GOTO 301
! *** XX
	ELSE IF (ACT.EQ.'XX') THEN		!XX
	  POLNM='XX'
	  POLC=0
	  NEWHA=.TRUE.				!DISPLAY HA
! *** YY
	ELSE IF (ACT.EQ.'YY') THEN		!YY
	  IF (STHI(STH_PLN_I).EQ.2.OR.
	1	STHI(STH_PLN_I).EQ.4) THEN
	    POLNM='YY'
	    POLC=3
	    NEWHA=.TRUE.			!DISPLAY HA
	  ELSE
	    GOTO 402
	  END IF
! *** XY
	ELSE IF (ACT.EQ.'XY') THEN		!XY
	  IF (STHI(STH_PLN_I).EQ.4) THEN
	    POLNM='XY'
	    POLC=1
	    NEWHA=.TRUE.			!DISPLAY HA
	  ELSE
	    GOTO 402
	  END IF
! *** YX
	ELSE IF (ACT.EQ.'YX') THEN		!YX
	  IF (STHI(STH_PLN_I).EQ.4) THEN
	    POLNM='YX'
	    POLC=2
	    NEWHA=.TRUE.			!DISPLAY HA
	  ELSE
	    GOTO 402
	  END IF
C
C *** COR, UNCOR
C
	ELSE IF (ACT(1:1).EQ.'C') THEN
 	  CALL NSCSAD(CORAP,CORDAP)			!Get corrections
	  IF (IAND(CORDAP,CAP_MOD).EQ.0) THEN	!No DEAPPLY=MODEL
	     CALL WNCTXT(F_TP,'Choose CLEAR and QUIT '//
     &		  'if you do not want model subtraction')
	     CALL NMODAW(NSRC(0),STH)			!Get model
	     IF (NSRC(0).GT.0) THEN			!Model given
	        CALL NMOMUI()				! so get the type
		IF (.NOT.NMOMSC(FCAIN,SETS)) THEN	! and set it
		   CALL WNCTXT(F_TP,'Error in model calculation')
	           GOTO 401
	        END IF
	        IF (.NOT.NMORDH(6,STP,SRA,SDEC,SFRQ)) THEN !Init for sector
	           CALL WNCTXT(F_TP,'Error: cannot initialise model')
	           CALL WNCTXT(F_TP,
	1	      'You may need write access to the SCN-file')
	           GOTO 401
	        ELSE
	           CALL NMOMST(STP,SRA,SDEC,STH,LM0,FRQ0,TF,MINST) !GET DATA
	        END IF
	     ELSE
	        CALL WNCTXT(F_TP,'No model subtraction')
	     END IF
	  ELSE
	     CALL WNCTXT(F_TP,
     &	       'The model present in the SCN file will be subtracted')
	  END IF
 	  CALL NSCSAD(CORAP,CORDAP)			!Get corrections
	  DO_COR=.TRUE.
	  CALL WNCTXT(F_TP,'D & A options will show corrected data,')
	  CALL WNCTXT(F_TP,'use UNCOR to show raw values again.')
	ELSE IF (ACT(1:1).EQ.'U') THEN
	  DO_COR=.FALSE.
	  CALL WNCTXT(F_TP,'D & A options will show raw data,')
	  CALL WNCTXT(F_TP,'use COR to show corrected values again.')
C
C *** TP, GN
C
        ELSE IF (ACT.EQ.'TP'.OR.ACT.EQ.'GN') THEN       !Total power/Gain
          IF (STHJ(STH_IFHP_J).NE.0) THEN		!Can do
	    TEXT(-1)=' '				!HEADING
	    DO I=0,STHTEL-1
	      TEXT(-1)(I*5+12:I*5+12)=TELS(I+1:I+1)
	    END DO
	    CALL WNCTXT(F_TP,'!/ !#$AS',LEN(TEXT(0)),TEXT(-1)) !SHOW HEADING
C
	    IF (ACT.EQ.'TP') THEN
	       IF (NSCGIF('TPon',FCAIN,STHJ,HA,HA,IFBUF)) THEN
	          CALL WNCTXT(F_TP,'!Q1\TPon  X:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,0))
	          CALL WNCTXT(F_TP,'!Q1\      Y:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,1))
	       END IF
	       IF (NSCGIF('TPoff',FCAIN,STHJ,HA,HA,IFBUF)) THEN
	          CALL WNCTXT(F_TP,'!Q1\TPoff X:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,0))
	          CALL WNCTXT(F_TP,'!Q1\      Y:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,1))
	       END IF
	     ELSE
	       IF (NSCGIF('Gain',FCAIN,STHJ,HA,HA,IFBUF)) THEN
	          CALL WNCTXT(F_TP,'!Q1\Gain  X:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,0))
	          CALL WNCTXT(F_TP,'!Q1\      Y:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,1))
	       END IF
	       IF (NSCGIF('Tsys',FCAIN,STHJ,HA,HA,IFBUF)) THEN
	          CALL WNCTXT(F_TP,'!Q1\Tsys  X:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,0))
	          CALL WNCTXT(F_TP,'!Q1\      Y:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,1))
	       END IF
	       IF (NSCGIF('GNCAL',FCAIN,STHJ,HA,HA,IFBUF)) THEN
	          CALL WNCTXT(F_TP,'!Q1\GNC   X:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,0))
	          CALL WNCTXT(F_TP,'!Q1\      Y:!10C!5$#E10.0',
	1			STHTEL,IFBUF(0,1))
	       END IF
	     END IF
	     CALL WNCTXT(F_TP,' ')
          ELSE
            CALL WNCTXT(F_TP,'No IF data available in this scan-file')
          END IF
C
C*****************************************************************************
C DATA DISPLAY
C
C D, A ***
	ELSE IF (ACT(1:1).EQ.'D' .OR. ACT(1:1).EQ.'A') THEN !DATA
C
C	Do corrections: read data through NSCSCR, get model
C
	  IF (DO_COR) THEN
	    J=NINT((HA-STHE(STH_HAB_E))/STHE(STH_HAI_E))	!SCAN NUMBER
	    IF (.NOT.NSCSCR(FCAIN,STH,IFRT,J,	   
     &	           CORAP,CORDAP,SCH,WGT,CDAT)) THEN     !Get corrected data
	      CALL WNCTXT(F_TP,'Error reading corrected data')
	      GOTO 401					!Error correcting
	    ELSE IF (NSRC(0).GT.0) THEN
	      CALL NMOMUV(STP,SRA,SDEC,STH,SCH,UV0)	!Get model UV data
	      CALL NMOMU4(0,FCAIN,J,STH,UV0,LM0,FRQ0,
     &	                  STHE(STH_RTP_E),STHI(STH_PLN_I),
     &	                  STHJ(STH_NIFR_J),IFRT,TF,MINST,CMOD) ! Model data
	      CALL NMOCIX(STH,SCH,ANG,CAMOD,CMOD)		!Convert
	    END IF
C
C	No corrections: just copy into complex array
C	  
	  ELSE
	    DO I1=0,STHJ(STH_NIFR_J)-1	!ALL INTERFEROMETERS
	       I=STHI(STH_PLN_I)*I1+OPOL
	       CDAT(I1,POLC)=CMPLX(ODAT(1,I),ODAT(2,I))
	    END DO
	  END IF
C
C	Convert to ampl/phase or cos/sin
C
	  DO I1=0,STHJ(STH_NIFR_J)-1			! SCAN ALL IFRs
	     CV1=CDAT(I1,POLC)				! DATA
	     CV2=0					! NO MODEL
	     IF (DO_COR.AND.NSRC(0).GT.0) CV2=CAMOD(I1,POLC)	! MODEL
	     IF (ACT(1:1).EQ.'A') THEN
 	        CALL WNMAAM(1,CV1-CV2,RDAT(I1,1))	!MAKE AMPL
	        CALL WNMAPH(1,CV1-CV2,RDAT(I1,2))	!MAKE PHASE
 	        RDAT(I1,2)=WNGEFD(RDAT(I1,2))		!MAKE DEGREES
		IF (RDAT(I1,2).GT.180) RDAT(I1,2)=RDAT(I1,2)-360.  !-180..180
	     ELSE
	        RDAT(I1,1)=REAL(CV1-CV2)
	        RDAT(I1,2)=AIMAG(CV1-CV2)
	     END IF
	  END DO
C
C	Find extrema
C
	  R0=-1.				!FIND MAX
	  DO I1=0,STHJ(STH_NIFR_J)-1
	    I=STHI(STH_PLN_I)*I1+OPOL		!DATA POINTER
	    IF (ODAT(0,I).NE.0) THEN
	      IF (ACT(1:1).EQ.'A') THEN
		R0=MAX(R0,RDAT(I1,1))
	      ELSE
		R0=MAX(R0,ABS(RDAT(I1,1)))
		R0=MAX(R0,ABS(RDAT(I1,2)))
	      END IF
	    END IF
	  END DO
	  IF (R0.GE.10000.) THEN		!SET SCALE
	    R0=10.
	  ELSE
	    R0=1.
	  END IF
	  IF (ACT(1:1).EQ.'A') THEN		!HEADING
	    CALL WNCTXT(F_TP,'!/!26C\!AS Amplitude (!E5.0 W.U.)',
	1		POLNM,R0)
	  ELSE
	    CALL WNCTXT(F_TP,'!/!29C\!AS Cos (!E5.0 W.U.)',
	1		POLNM, R0)
	  END IF
	  TEXT(-1)=' '
	  DO I=0,STHTEL-1
	    TEXT(-1)(I*5+6:I*5+6)=TELS(I+1:I+1)
	  END DO
	  TEXT(-1)(74:74)='.'
	  CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(-1))
	  DO I=0,STHTEL				!SET NO DATA
	    TEXT(I)=' '
	    DO I2=0,STHTEL-1
	      IF (I.NE.I2) TEXT(I)(5*I2+6:5*I2+6)='.'
	    END DO
	    IF (I.NE.STHTEL) THEN
	      TEXT(I)(74:74)=TELS(I+1:I+1)
	    ELSE
	      TEXT(I)(74:74)='.'
	    END IF
	    IF (I.NE.0) THEN
	      TEXT(I)(1:1)=TELS(I:I)
	    END IF
	  END DO
C
	  DO I2=0,STHJ(STH_NIFR_J)-1
	     I=STHI(STH_PLN_I)*I2+OPOL		!DATA POINTER
	     IF (ODAT(0,I).NE.0) THEN		!DATA PRESENT
		IF (ACT(1:1).EQ.'A') THEN	!AMPL/PHASE
		  R1=RDAT(I2,1)/R0
		  R2=RDAT(I2,2)			!DO NOT SCALE PHASE
		ELSE				!COS/SIN
		  R1=RDAT(I2,1)/R0
		  R2=RDAT(I2,2)/R0
		END IF
		CALL WNCTXS(TEXT(MOD(IFRT(I2),256))(5*(IFRT(I2)/256)+3:
	1			5*(IFRT(I2)/256)+7),
	1			'!5$E6.0',R1)
		CALL WNCTXS(TEXT(IFRT(I2)/256+1)(5*MOD(IFRT(I2),256)+3:
	1			5*MOD(IFRT(I2),256)+7),
	1			'!5$E6.0',R2)
	     END IF
	  END DO
	  DO I=0,STHTEL				!SHOW DATA
	    CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(I))
	  END DO
	  CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(-1)) !BOTTOM
	  IF (ACT(1:1).EQ.'A') THEN		!HEADING
	    CALL WNCTXT(F_TP,'!26C\!AS Phase (deg)!/',POLNM)
	  ELSE
	    CALL WNCTXT(F_TP,'!29C\!AS Sin (!E5.0 W.U.)!/',
	1		POLNM,R0)
	  END IF
! *** W
	ELSE IF (ACT(1:1).EQ.'W') THEN		!WEIGHT
	  R1=0.					!FIND MAX
	  DO I1=0,STHJ(STH_NIFR_J)-1
	    I=STHI(STH_PLN_I)*I1+OPOL		!DATA POINTER
	    IF (ODAT(0,I).NE.0) THEN
	      I2=ODAT(0,I)			!WEIGHT/FLAGS
	      R1=MAX(R1,FLOAT(IAND(I2,'000000ff'X))*
	1		(1.-STHE(STH_WFAC_E)))
	    END IF
	  END DO
	  R0=1					!SET SCALE
	  IF (R1.GT.0) THEN			!CAN DO
	    DO WHILE (R1.GE.1000.*R0)
	      R0=R0*10.
	    END DO
	    DO WHILE (R1.LT.100.*R0)
	      R0=R0/10.
	    END DO
	  END IF
	  CALL WNCTXT(F_TP,'!/!26C\!AS Data weight (*!E12.4)',
	1		POLNM,R0)
	  TEXT(-1)=' '
	  DO I=0,STHTEL-1
	    TEXT(-1)(I*5+6:I*5+6)=TELS(I+1:I+1)
	  END DO
	  TEXT(-1)(74:74)='.'
	  CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(-1))
	  DO I=0,STHTEL				!SET NO DATA
	    TEXT(I)=' '
	    DO I2=0,STHTEL-1
	      IF (I.NE.I2) TEXT(I)(5*I2+6:5*I2+6)='.'
	    END DO
	    IF (I.NE.STHTEL) THEN
	      TEXT(I)(74:74)=TELS(I+1:I+1)
	    ELSE
	      TEXT(I)(74:74)='.'
	    END IF
	    IF (I.NE.0) THEN
	      TEXT(I)(1:1)=TELS(I:I)
	    END IF
	  END DO
	  DO I2=0,STHJ(STH_NIFR_J)-1
	     I=STHI(STH_PLN_I)*I2+OPOL		!DATA POINTER
!!!	     IF (ODAT(0,I).NE.0) THEN		!DATA PRESENT
	        I3=ODAT(0,I)			!WEIGHT/FLAGS
	        R1=IAND(I3,'000000ff'X)*(1.-STHE(STH_WFAC_E))/R0 !WEIGHT
		IF (IAND(FL_ALL,I3).NE.0) R1=-R1 !INDICATE FLAGGING
		I3=IAND('000000ff'X,ISHFT(I3,-8)) !FLAGS
		CALL WNCTXS(TEXT(MOD(IFRT(I2),256))(5*(IFRT(I2)/256)+3:
	1			5*(IFRT(I2)/256)+7),
	1			'!5$E6.0',R1)
		IF (I3.NE.0) THEN
		  CALL WNCTXS(TEXT(IFRT(I2)/256+1)(5*MOD(IFRT(I2),256)+3:
	1			5*MOD(IFRT(I2),256)+7),
	1			'   !2$XJ',I3)
	        END IF
!!!	     END IF
	  END DO
	  DO I=0,STHTEL				!SHOW DATA
	    CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(I))
	  END DO
	  CALL WNCTXT(F_TP,'!#$AS',LEN(TEXT(0)),TEXT(-1)) !BOTTOM
	  CALL WNCTXT(F_TP,'!32C!AS Data flags!/',
	1		POLNM)			!HEADINGS
! *** T
	ELSE IF (ACT(1:1).EQ.'T') THEN		!TELESCOPE GAIN/PHASE
	  CALL NCATL1(FCAIN,STH,HA,HA,IFRS,PCGAN,PCPHA,F_TP)	!GET AND SHOW
! *** S
	ELSE IF (ACT(1:1).EQ.'S') THEN		!SHOW HEADER DETAILS
	  CALL NSCXSL(F_TP,FCAIN,SCHP)
! *** I
	ELSE IF (ACT(1:1).EQ.'I') THEN		!SHOW TELESCOPE HEADERS
	  DO I=1,4				!ALL POINTERS
	    J0=SCHJ(IFRCP(1,I))			!GET POINTER
	    IF (J0.NE.0) THEN			!AVAILABLE
	      IF (WNFRD(FCAIN,4*STHJ(STH_NIFR_J)*LB_X,
	1		IFRCOR,J0)) THEN	 !READ IFR DATA
	        CALL WNCTXT(F_TP,'!/!AS',IFRCC(I)) !DESCRIPTION
	        CALL WNCTXT(F_TP,'Ifr         XX             XY'//
	1		'               YX              YY') !HEADER
		IF (IFRCP(2,I).EQ.1) THEN	 !MULTIPLICATIVE
	           CALL WNCTXT(F_TP,'        %       deg     %'//
	1		'       deg     %       deg     %       deg')
	           DO I1=0,4*STHJ(STH_NIFR_J)-1	 !MAKE % and deg
		     IFRCOR(1,I1)=100*(EXP(IFRCOR(1,I1))-1)
		     IFRCOR(2,I1)=DEG*IFRCOR(2,I1)
	           END DO
		ELSE				 !ADDITIVE
		   CALL WNCTXT(F_TP,'        cos     sin     cos'//
	1		'     sin     cos     sin     cos     sin')
		END IF
C
	        DO I1=0,STHTEL-1			 !ALL TELESCOPES
	          DO I2=0,STHTEL-1
	            IF (IFRS(I1,I2)) THEN		 !PRESENT
	              CALL WNCTXT(F_TP,'!Q1!1$XJ!1$XJ  !8$8E8.2',
	1		    I1,I2,IFRCOR(1,4*IFRSX(I1,I2))) !SHOW VALUES
	            END IF
	          END DO
	        END DO
C
	        CALL WNCTXT(F_TP,' ')
	      END IF				!IF CAN READ
	    ELSE				!IF NOT AVAILABLE
	      CALL WNCTXT(F_TP,'!/!AS  NOT available',IFRCC(I)) !DESCRIPTION
	      CALL WNCTXT(F_TP,' ')
	    END IF				!IF AVAILABLE
	  END DO				!NEXT POINTER
! *** E
	ELSE IF (ACT(1:1).EQ.'E') THEN		!EDIT HEADER DETAILS
	  IF (MODE.EQ.'R') THEN			!CHANGE TO UPDATE MODE
	    MODE='U'				!MAKE UPDATE
	    CALL WNFCL(FCAIN)			!CLOSE FILE
	    IF (.NOT.WNDNOC(' ',' ','SCN',MODE,' ',IFILE)) THEN !CHANGE DATES
	      CALL WNCTXT(F_TP,'Node is not writable')
	      GOTO 100
	    END IF
	    IF (.NOT.WNFOP(FCAIN,IFILE,MODE)) THEN !OPEN SCN FILE
	      CALL WNCTXT(F_TP,'Cannot write to file attached to node')
	      GOTO 100
	    END IF
	  END IF
	  CALL NSCESL(F_TP,FCAIN,SCHP)
	ELSE					!HA
	  WRITE (UNIT=F1000,FMT=1002,ERR=403) LEN(ACT)
 1002	  FORMAT('(BN,F',I4.4,'.0)')
	  READ(UNIT=ACT,FMT=F1000,ERR=403) HA
	  HA=HA/360.				!MAKE FRACTIONS
	  NEWHA=.TRUE.
	END IF
	GOTO 401				!NEXT ACTION
C
 402	CONTINUE
	CALL WNCTXT(F_T,'Polarisation !AS not available', ACT)
	GOTO 401				!NEXT ACTION
C
 403	CONTINUE
	CALL WNCTXT(F_T,'Format error in HA or unknown option')
	GOTO 401				!NEXT ACTION
C
C
	END
