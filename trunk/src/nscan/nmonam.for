C+ NMONAM.FOR
C  CMV 940428
C
C  Revisions:
C	CMV 940428	Created
C	CMV 940503	Made logical function
C	CMV 940622	Correct accuracy of comparison, add DO_SHOW
C
	LOGICAL FUNCTION NMONAM(IMDL,GDES,STR,DO_SHOW)
C
C  Find proper name for model component
C
C  Result:
C
C	FOUND_L =  NMONAM( IMDL(0:*)_B:I, GDES(0:*)_B:I, STR_C*(*):O,
C	                   DO_SHOW_L:I)
C		Return in STR an identification for the component in IMDL 
C		using the header information in GDES. Identification is
C		either the ID value in MDL or the name derived from a
C		file with coordinates and proper names. Returns .TRUE.
C		if proper name found for source. If DO_SHOW is True, the
C	        match is reported on the screen.
C
C	CALL NMONM1()
C		Initialise the file with proper names, return .TRUE. if
C		name list loaded.
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MDH_O_DEF'			!MODEL HEADER
	INCLUDE 'MDL_O_DEF'			!MODEL LINE
C
C  Entries:
C
	LOGICAL NMONM1				!INITIALISE NAMELIST
C
C  Parameters:
C
	INTEGER MAXNAM				!NUMBER OF NAMES IN LIST
	   PARAMETER(MAXNAM=50)
C
C  Arguments:
C
	BYTE IMDL(0:*)				!MODEL COMPONENT
	BYTE GDES(0:*)				!MODEL HEADER
	CHARACTER STR*(*)			!OUTPUT STRING
	LOGICAL DO_SHOW				!SHOW MATCH
C
C  Function references:
C
	INTEGER WNGARA				!GET ADDRESS
	LOGICAL WNDPAR				!GET NAME OF INPUT FILE
	LOGICAL WNCAFF				!FORMAT LINE
	LOGICAL WNCACD				!GET REAL*8 NUMBER
	DOUBLE PRECISION WNGDND			!NORMALISE ANGLES
C
C  Data declarations:
C
	INTEGER NLIST				!NUMBER IN LIST
	  DATA NLIST/0/				!NONE YET
	REAL*8 XX_L(MAXNAM),YY_L(MAXNAM)	!COORDINATE LIST
	CHARACTER*10 ID_L(MAXNAM)		!NAME LIST
	SAVE NLIST,XX_L,YY_L,ID_L
C
	INTEGER LUN				!UNIT FOR INPUT FILE
	CHARACTER*80 LINE,FLINE			!TEMP STRINGS
	BYTE MDL(0:MDLHDL-1)			!SOURCE ENTRY
	  INTEGER MDLJ(0:MDLHDL/4-1)
	  REAL MDLE(0:MDLHDL/4-1)
	  EQUIVALENCE (MDL,MDLJ,MDLE)
C-
C
C INIT
C
	NMONAM=.FALSE.				!NOT YET FOUND
	CALL WNGMV(MDLHDL,IMDL,MDL)		!GET SOURCE
	J=WNGARA(GDES)				!ADDRESS HEADER
	J1=(J-A_OB)/LB_J			! AS J
	J2=(J-A_OB)/LB_D			! AD D
C
	IF (A_J(J1+MDH_TYP_J).LE.0) THEN	!LOCAL MODE
	   CALL NMOEXT(MDL)			!MAKE CORRECT FORMAT
	   D0=MDLE(MDL_L_E)
	   D1=MDLE(MDL_M_E)
	ELSE
	   CALL WNMCLM(A_D(J2+MDH_RA_D),A_D(J2+MDH_DEC_D),
	1		MDLE(MDL_L_E),MDLE(MDL_M_E),D0,D1) !MAKE RA/DEC
	   CALL NMOEXT(MDL)			!MAKE CORRECT FORMAT
	END IF
C
	CALL WNCTXS(STR,'!UJ',MDLJ(MDL_ID_J))	!DEFAULT ID
	D0=WNGDND(D0*360)*3600.			!MAKE ARCSEC
	D1=WNGDND(D1*360)*3600.
C
	DO I=1,NLIST				!TRY ALL NAMES
	   IF (ABS(D0-XX_L(I)).LT.1 .AND.
	1	ABS(D1-YY_L(I)).LT.1.) THEN	!MATCH 1 arcsec
	      STR=ID_L(I)			 	!FOUND NAME
	      IF (DO_SHOW)
	1        CALL WNCTXT(F_T,'!UJ = !AS',MDLJ(MDL_ID_J),STR)  !SHOW
	      NMONAM=.TRUE.				!SET FOUND
	   END IF
	END DO
C
	RETURN
C
	ENTRY NMONM1()
C
	NMONM1=.FALSE.				!NO LIST YET
	NLIST=0
C
C	Get name of list file and open
C
 90	CONTINUE
	IF (.NOT.WNDPAR('NAMES_FILE',LINE,LEN(LINE),J0,
	1	'SOURCE.TXT')) THEN
	   IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 900    !READY
	   CALL WNCTXT(F_TP,'No proper file specified')
	   GOTO 90
        END IF
        IF (J0.LE.0) GOTO 900                   !READY
C
        LUN=0 
        CALL WNGLUN(LUN)                        !GET LUN TO USE
        IF (LUN.EQ.0) THEN
	  CALL WNCTXT(F_TP,'Cannot get unit for file')
	  GOTO 900
	END IF
        OPEN (UNIT=LUN,FILE=LINE,STATUS='OLD',ERR=83) !OPEN INPUT
C
C READ DATA
C
        NLIST = 0                               !Entry counter
	DO WHILE (NLIST.LT.MAXNAM)
          READ (UNIT=LUN,FMT=1000,END=87,ERR=83) LINE
 1000   FORMAT(A)
          I=1                                    !DATA POINTER
          IF (WNCAFF(LINE,I,FLINE)) THEN  	 !NOT COMMENT-LINE (!)
             I=1
             CALL WNCASB(FLINE,I)               	!SKIP BLANK
             IF (WNCACD(FLINE,I,10,XX_L(NLIST+1))) THEN	!GET RA
	        CALL WNCASB(FLINE,I)
		IF (WNCACD(FLINE,I,10,YY_L(NLIST+1))) THEN !GET DEC
	           CALL WNCASB(FLINE,I)
		   ID_L(NLIST+1)=FLINE(I:)		!GET NAME
		   NLIST=NLIST+1			!COUNT ENTRY
		   NMONM1=.TRUE.			!GOT SOMETHING
		END IF
	     END IF
	  END IF
	END DO
 87     CONTINUE
	CLOSE(UNIT=LUN)
	CALL WNGLUF(LUN)	
C
	DO I=1,NLIST
	   XX_L(I)=WNGDND(XX_L(I))*3600.		!MAKE ARCSEC
	   YY_L(I)=WNGDND(YY_L(I))*3600.
	END DO
C
	RETURN
C
C ERROR RETURN
C
 83     CONTINUE
        CALL WNCTXT(F_TP,'Cannot open/read file')
        IF (LUN.NE.0) THEN
	   CLOSE(UNIT=LUN)
	   CALL WNGLUF(LUN)	
        END IF
 900	CONTINUE
C
	END