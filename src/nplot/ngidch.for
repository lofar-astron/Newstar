C+ NGIDCH.FOR
C  CMV 930914
C
C  Revisions:
C	CMV 930914	Created
C	CMV 931123	Add option for corrected data
C	CMV 931216	Revise for use with NSCSCF
C	WNB 931221	Correct IAND use; format typo
C	CMV 940203	Use call to NGICOV
C	CMV 940622	Handle DO_BLANK here
C	CMV 000828	Offset for X-index (cause problems with mosaic)
C
	LOGICAL FUNCTION NGIDCH(IFRS,IPOL,MID,SID)
C
C  Load data as a (HA,CHAN) map into the GIDS-display
C
C  Result:
C
C       NGIDCH_L = NGIDCH( IFRS_I(2):I,  MID_C(*):I, SID_C(*):I)
C				Load map in GIDS display using:
C				IFRS(2) the two interferometers
C				IPOL	current polarisation
C				MID,SID GIDS Headers
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'NGI_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER IFRS(2)
	INTEGER IPOL
	CHARACTER MID*(*),SID*(*)  !GIDS Header
C
C  Function references:
C
	INTEGER N_GDI_IMWRITE, N_GDI_GRWRITE, N_GDI_GRCLEAR
C
	LOGICAL NSCSTG		!READ SET HEADER
	CHARACTER*32 WNTTSG	!PRINT SET NAME
	LOGICAL NGITRA		!SHOW DATA
	LOGICAL NGISFL		!HANDLE FLAGS
	LOGICAL NGICOV		!CLEAR DATA AND OVERLAYS
	LOGICAL NGICDT		!READ AND CONVERT DATA
C
C  Data declarations:
C
	REAL    RDAT(0:STHIFR-1,0:3)	!DATA VALUE (ifr, pol)
	INTEGER FLG(0:STHIFR-1,0:3)	!FLAGS/WEIGHTS (ifr, pol)
	INTEGER*2 IFRT(0:STHIFR-1)      !IFR TABLE
	LOGICAL HEAD			!FLAG SET IN SCAN HEADER
	INTEGER STHP            	!SET HEADER POINTER
	REAL    HACUR			!CURRENT HOUR ANGLE
	INTEGER XX,YY			!ARRAY OFFSETS
	INTEGER CCHAN			!Current channel
C
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE)
C
C-
	NGIDCH=.TRUE.				!ASSUME OK
C
C	Clear map data and (some) flags
C
	JS=NGICOV(IPOL,-1)			!No current channel
C
C	Loop over all sets and fill in the appropriate pixels
C
C	We use a single loop and calculate the pixel based on the HA 
C	and the Channel number. This is not the most efficient (we access
C	the array in a scattered fashion) but it works and can be changed
C	later if delays are unacceptably large.
C
C	Get the next header and channel number
C
	CALL WNDSTR(FCAIN,SETS)             !RESET SET SEARCH
	DO WHILE (NSCSTG(FCAIN,SETS,STH,STHP,SETNAM))
	   CCHAN=STHI(STH_CHAN_I)		!CURRENT CHANNEL
C
C	Check hour-angle increment
C
	   IF (STHE(STH_HAI_E).NE.HAINC) THEN
	      CALL WNCTXT(F_TP,
	1	'Error: Sector !AS has hour-angle increment !EAF12.7',
	1	WNTTSG(SETNAM,0),STHE(STH_HAI_E)) 
	      GOTO 990
	   END IF
C
C	Loop through all scans in the sector
C
	   DO I=0,STHJ(STH_SCN_J)-1		!ALL SCANS
	      HACUR=STHE(STH_HAB_E)+I*STHE(STH_HAI_E) !HA OF SCAN
	      IF (HACUR.GE.HARAN(0) .AND. HACUR.LT.HARAN(1)) THEN !IN RANGE
C
C	Read scan-header, corrected data and flags
C
	        IF (.NOT.NGICDT(I,STH,HEAD,IFRT,FLG,RDAT)) THEN !READ DATA
		   CALL WNCTXT(F_TP,'Error reading scan !UJ',I)
		   GOTO 990
		END IF
C
C	Check if data present
C
	        DO I1=0,STHJ(STH_NIFR_J)-1			!SCAN ALL IFRS
	           IF (MOD(IFRT(I1),256).EQ.IFRS(1).AND.
	1	           IFRT(I1)/256.EQ.IFRS(2)) THEN	!Take this one
	           XX=(HACUR-HARAN(0))/HAINC+0.01
	           YY=CCHAN-RCHAN(0)
		   IPTR=YY*XSIZ+XX
		   IF (IPTR.GE.LBUF) THEN
		      CALL WNCTXT(F_TP,'Out of range???')
		      IPTR=LBUF-1
		   END IF
C
	           IF (IAND(FLG(I1,IPOL),'0000ffff'X) .NE.0) THEN !DATA PRESENT
C
C	Scale data into buffer
C
			IF (RDAT(I1,IPOL).LT.RANGE(1)) THEN
			   I5=MINCOL
			ELSE IF (RDAT(I1,IPOL).GT.RANGE(2)) THEN
			   I5=MAXCOL
			ELSE
			   I5=NINT(RDAT(I1,IPOL)*CSCALE+CZERO)
			END IF
			IF (I5.GT.127) I5=I5-256  !MAP 256 TO -1
			A_B(DMAP+IPTR)=I5
C
C	Set graphics plane 1 if data flagged, optionally blank data
C
			IF (HEAD.OR.
	1	            IAND('0000ff00'X,FLG(I1,IPOL)).NE.0) THEN
			   A_B(DFLG+IPTR)=1
	                   IF (DO_BLANK) A_B(DMAP+IPTR)=BLANK
			ENDIF
C
	             END IF			! IF DATA PRESENT
	           END IF			! IF CORRECT IFR
	        END DO				! LOOP OVER IFRs
	      END IF				! IF IN HOUR-ANGLE RANGE
	   END DO				! LOOP OVER SCANS
	END DO					! LOOP OVER SETS
C
C	Write the data
C
	IF (.NOT.NGITRA(MID,SID)) GOTO 990
C
C	If flagging required, enter regions mode and make flagfile
C
	IF (DO_FLAG) JS=NGISFL(IPOL,IFRS(1)+256*IFRS(2),-1)  !THIS IFR ONLY
C

	GOTO 800
C
C ERRORS
C
 990	CONTINUE
	NGIDCH=.FALSE.
C
 800	CONTINUE
C
	RETURN
	END
