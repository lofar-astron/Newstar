C+ NGILOD.FOR
C  GvD 920525
C
C  Revisions:
C       HjV 920723      Replace DWARF-routines by Newstar-routines
C       HjV 920827      Add NGI.DEF
C       HjV 930215      Change GDI calls to N_GDI calls
C                       Add OPTION
C	WNB 930330	Add DISPLAY
C	WNB 930406	Correct set printing
C	WNB 930416	Correct set show
C	WNB 930427	Make automatic GIDS environment
C	WNB 930510	Use NGIDAT, remove R-series
C	WNB 930514	Changed NGIMAP
C	CMV 930913	Change checks depending on MAPTYP
C	CMV 931025	Add re-open to allow for resizing (see NGIDAT.FOR)
C	CMV 940120	Change position of NGIREC to avoid clearing first map
C
	LOGICAL FUNCTION NGILOD()
C
C  Load one or more maps for GIDS
C
C  Result:
C       NGILOD_L = NGILOD()	Get a map (MAPTYP='MAP') or data 
C					(MAPTYP='IFRS' or 'CHAN')
C
C       
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'NGI_DEF'
	INCLUDE 'MPH_O_DEF'
	INCLUDE 'STH_O_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
	INTEGER WNCALN			!STRING LENGTH
	LOGICAL NMASTG			!GET A SET (WMP)
	LOGICAL NSCSTG           	!GET A SET (SCN)
	CHARACTER*32 WNTTSG		!PRINT SET NAME
	LOGICAL NGIREC			!RECORD MAP INTO GIDS-DISPLAY
	LOGICAL NGIDMP			!Load WMP map into gids
	LOGICAL NGIDIF			!Load SCN data as (HA,Ifr) map
	LOGICAL NGIDCH			!Load SCN data as (HA,Chan) map 
	LOGICAL NGIDOP			!OPEN CONNECTION TO GIDS WINDOW
	LOGICAL NGIDCL			!CLOSE CONNECTION TO GIDS WINDOW
C
C  Data declarations:
C
	CHARACTER*32 STR		!SET NAME
	CHARACTER*2 POLNAM(0:3)		!NAME OF POLARISATION
	  DATA POLNAM/'XX','XY','YX','YY'/
	INTEGER POLCOD(0:3)		!POLARISATION CODE
	  DATA POLCOD/XX_P,XY_P,YX_P,YY_P/
	INTEGER IPOL			!CURRENT POL. BITS
	INTEGER CCHAN			!CURRENT CHANNEL
	LOGICAL FOUND			!Channel in range
	LOGICAL FIRST			!First map in this call
	INTEGER MPHP			!MAP HEADER PTR
	INTEGER STHP                    !SET HEADER POINTER
	INTEGER IFRS(0:1)		!CURRENT IFR'S
	BYTE MPH(0:MPHHDL-1)		! N-map header
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  REAL MPHE(0:MPHHDL/4-1)
	  EQUIVALENCE (MPH,MPHJ,MPHE)
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE)
C-
	NGILOD=.TRUE.				!ASSUME OK
C
C Re-open GIDS window
C
	IF (.NOT.NGIDOP(GID)) THEN
	   CALL WNCTXT (F_TP,'Error re-opening GIDS display')
	   NGILOD=.FALSE.
	   RETURN
	END IF
C
C Check if at least one map can be loaded, record first map
C (if necessary) to prevent clearing it in NGISET (defimg call)
C
	FIRST=.TRUE.
	IF (.NOT.NGIREC(.TRUE.)) GOTO 901	!No more space
C
C Set up the size of the window etc.
C
	CALL NGISET(TEAR)
C
C Load maps from WMP file
C
	IF (MAPTYP.EQ.'MAP') THEN			!WMP MAP
	   DO WHILE (NMASTG(FCAIN,SETS,MPH,MPHP,SETNAM))
	     IF (NRA.EQ.MPHJ(MPH_NRA_J) .AND.
	1		NDEC.EQ.MPHJ(MPH_NDEC_J)) THEN !CAN DO
	        PTR=MPHP
	        CALL WNCTXS (STR,'Set !AS',WNTTSG(SETNAM,0)) !SET NAME
		IF (.NOT.FIRST) THEN
		   IF (.NOT.NGIREC(.TRUE.)) GOTO 901	!No more space
		END IF
	        CALL WNCTXT(F_TP,'Loading !AS (!AS)',NODIN,STR)
	        IF (.NOT.NGIDMP(MPHJ,NODIN,STR)) GOTO 900 !ERROR
	        IF (.NOT.NGIREC(.FALSE.)) GOTO 901	!Cannot record
		FIRST=.FALSE.
	     END IF
	   END DO
C
C Load data from SCN file as (HA,IFR) maps
C
	ELSE IF (MAPTYP.EQ.'IFRS'.OR.
	1	 MAPTYP.EQ.'BASE') THEN		!SCN FILE DATA: (HA,IFRS)
	   
	   DO IPOL=0,3
	     IF (IAND(SPOL,POLCOD(IPOL)).NE.0) THEN	!SELECTED THIS POL.
	        DO CCHAN=RCHAN(0),RCHAN(1)		!ALL CHANNELS
	          FOUND=.FALSE.
	          CALL WNDSTR(FCAIN,SETS)             !RESET SET SEARCH
	          DO WHILE (.NOT.FOUND.AND.
	1               NSCSTG(FCAIN,SETS,STH,STHP,SETNAM)) !FIND CHANNEL
	             FOUND=(CCHAN.EQ.STHI(STH_CHAN_I))
	          END DO
	          IF (FOUND) THEN
	            CALL WNCTXS (STR,'Ch. !UJ (!AS)',
	1		         CCHAN,POLNAM(IPOL))	!SET NAME
		    IF (.NOT.FIRST) THEN
		      IF (.NOT.NGIREC(.TRUE.)) GOTO 901	!No more space
		    END IF
	            CALL WNCTXT(F_TP,'Loading !AS (!AS)',NODIN,STR)
	            IF (.NOT.NGIDIF(IPOL,CCHAN,NODIN,STR)) GOTO 900 !ERROR
		    IF (.NOT.NGIREC(.FALSE.)) GOTO 901	!Cannot record
		    FIRST=.FALSE.
	          END IF
	        END DO
	     END IF
	   END DO
C
C Load data from SCN file as (HA,CHAN) maps
C
	ELSE IF (MAPTYP.EQ.'CHAN') THEN		!SCN FILE DATA: (HA,CHAN)
	   DO IPOL=0,3
	     IF (IAND(SPOL,POLCOD(IPOL)).NE.0) THEN !SELECTED THIS POL.
	        DO I1=0,STHTEL-1
	          DO I2=I1,STHTEL-1
	            IF (SIFRS(I1,I2)) THEN		!SELECTED
		        IFRS(0)=I1
		        IFRS(1)=I2
		        STR='Ifr '//TELNAM(I1+1:I1+1)//
	1			    TELNAM(I2+1:I2+1)//
	1		    ' ('//POLNAM(IPOL)//')'		!IFR NAME + POL
		        IF (.NOT.FIRST) THEN
		          IF (.NOT.NGIREC(.TRUE.)) GOTO 901	!No more space
		        END IF
	                CALL WNCTXT(F_TP,'Loading !AS (!AS)',NODIN,STR)
		        IF (.NOT.NGIDCH(IFRS,IPOL,NODIN,STR)) GOTO 900	!ERROR
			IF (.NOT.NGIREC(.FALSE.)) GOTO 901	!Cannot record
			FIRST=.FALSE.
		    END IF
	          END DO
		END DO
	     END IF
	   END DO
C
C No other options yet...
C
	ELSE					!ERROR
	   CALL WNCTXT('Unknown MAPTYP !AS',MAPTYP)
	ENDIF
	GOTO 800				!READY
C
 900	CONTINUE
	CALL WNCTXT(F_TP,'Error in loading of map!/')
 901	CONTINUE
	NGILOD=.FALSE.
 800	CONTINUE
C
C Free any memory
C
	CALL NGICLR()
C
C Close GIDS window again
C
	JS=NGIDCL(GID)
C
	RETURN
C
C
	END
