C+ NGIDIF.FOR
C  CMV 930913
C
C  Revisions:
C	CMV 930913	Created
C	CMV 931123	Add option for corrected data
C	CMV 931216	Revise for use with NSCSCF
C	WNB 931221	Correct IAND usage; format typo
C	CMV 940203	Use call to NGICOV
C	CMV 940622	Handle DO_BLANK here
C
	LOGICAL FUNCTION NGIDIF(IPOL,CCHAN,MID,SID)
C
C  Load data into the GIDS-display as an (HA,IFR) map
C
C  Result:
C
C       NGIDIF_L = NGIDIF(IPOL, CCHAN, MID_C(*):I, SID_C(*):I)
C				Load map in GIDS display using:
C				IPOL    Current polarisation
C				CCHAN   Current channel
C				MID,SID Header names
C	Remaining parameters from NGI_DEF
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
	INTEGER IPOL		!Current polarisation
	INTEGER CCHAN		!Current channel
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
	LOGICAL NGICDT		!CONVERT AND SCALE DATA
C
C  Data declarations:
C
	REAL    RDAT(0:STHIFR-1,0:3)	!DATA VALUE (ifr, pol)
	INTEGER FLG(0:STHIFR-1,0:3)	!FLAGS/WEIGHTS (ifr, pol)
	INTEGER*2 IFRT(0:STHIFR-1)      !IFR TABLE
	LOGICAL HEAD			!FLAG SET IN SCAN HEADER
	INTEGER STHP            	!SET HEADER POINTER
	INTEGER HA0			!HOUR ANGLE OF FIRST SCAN IN SECTOR
	INTEGER XX,YY			!ARRAY OFFSETS
C
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE)
C
C-
	NGIDIF=.TRUE.				!ASSUME OK
C
C	Clear map data and (some) flags
C
	JS=NGICOV(IPOL,CCHAN)
C
C	Loop through all sectors with current channel
C
	CALL WNDSTR(FCAIN,SETS)             !RESET SET SEARCH
	DO WHILE (NSCSTG(FCAIN,SETS,STH,STHP,SETNAM)) !FIND CHANNEL
	  IF (STHI(STH_CHAN_I).EQ.CCHAN) THEN
C
C	Check hour-angle increment
C
	    IF (STHE(STH_HAI_E).NE.HAINC) THEN
	       CALL WNCTXT(F_TP,
	1	 'Error: Set !AS has hour-angle increment !EAF12.7',
	1	 WNTTSG(SETNAM,0),STHE(STH_HAI_E)) 
	       GOTO 990
	    END IF
C
C	Loop through all scans in the sector
C
	    HA0=(STHE(STH_HAB_E)-HARAN(0)+HAINC/2.0)/HAINC
	    DO I=0,STHJ(STH_SCN_J)-1		!ALL SCANS
	       XX=HA0+I				!HA OF SCAN
	       IF (XX.GE.0.AND.XX.LT.XSIZ) THEN 	!IN RANGE

C
C	Read scan-header, corrected data and flags
C
	         IF (.NOT.NGICDT(I,STH,HEAD,IFRT,FLG,RDAT)) THEN !READ DATA
		   CALL WNCTXT(F_TP,'Error reading scan !UJ',I)
		   GOTO 990
		 END IF
C
C	Loop over all interferometers for the requested polarisation (IPOL)
C
		 DO I1=0,STHJ(STH_NIFR_J)-1	!SCAN ALL IFRS
	           YY=IDXLUT(MOD(IFRT(I1),256),IFRT(I1)/256)
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
	        END DO				! LOOP OVER IFRS
C
	      END IF				! IF IN HOUR-ANGLE RANGE
	    END DO				! LOOP OVER SCANS
	  END IF
	END DO					! LOOP OVER SECTORS
C
C	Transfer map and flags to GIDS
C
	IF (.NOT.NGITRA(MID,SID)) GOTO 990
C
C	If flagging required, enter regions mode and make flagfile
C
	IF (DO_FLAG) JS=NGISFL(IPOL,-1,CCHAN)
C
	GOTO 800
C
C ERRORS
C
 990	CONTINUE
	NGIDIF=.FALSE.
C
 800	CONTINUE
C
	RETURN
	END
