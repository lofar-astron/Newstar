C+ NGIDAT.FOR
C  WNB 930510
C
C  Revisions:
C	WNB 930514	Use NGID routines
C	WNB 930514	Calculate realistic max/min range
C	HjV 930518	Change some keywords
C	WNB 930621	Remove AREA; add FLAG, CLEAR, SAVE, WRITE
C	WNB 930622	Rename some options
C	WNB 930803	CBITS_DEF
C	CMV 930930	Change LOAD to MAP, add DATA option
C	CMV 930930	Reset NRMAPS for GCLEAR
C	CMV 931025	Close GIDS again, re-open in NGILOD (for resize)
C	CMV 931123	Option to plot corrected data with model subtract
C	CMV 931213	Added BASE option for MAPTYP, use prescan to get
C	                decent scaling, add use of DEFIMG
C	CMV 931220	Pass FCA of input file to WNDXLP and WNDSTA/Q
C	CMV 931220	Do not use first scans for range determination
C	WNB 931221	Changed FLOAT to REAL; to CEIL/FLOOR; MAX to MIN
C	CMV 940111	Initialise NGICDT, use MPH min/max if possible
C	CMV 940203	Added ALL_POLS keyword, changed UNLOAD/SAVE
C	CMV 940218	Added BLANK_FLAGS keyword, quit if open fails
C	CMV 940225	Enable default model file (use NMODAW)
C	CMV 940316	Reload CAP/CDAP since INTERN may be used
C	CMV 940415	Cleared text if internal model used
C	CMV 940817	Added CLIPFLAG option
C	CMV 000828	HARAN(0) is always an integer of HAINC
C
	SUBROUTINE NGIDAT(ACT)
C
C  Get NGIDS program parameters
C
C  Result:
C
C       CALL NGIDAT( ACT_L:I)	will ask and set all program parameters
C				If ACT .true. open display first
C
C  PIN references:
C
C       OPTION
C	MAP_COMPRESS
C	MAP_RANGE
C	MAP_SEQUENCES
C	USER_FLAG
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'MPH_O_DEF'
	INCLUDE 'STH_O_DEF'
	INCLUDE 'FLH_O_DEF'
	INCLUDE 'NGI_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	LOGICAL ACT			!TRUE IF FIRST CALL
C
C  Function references:
C
	LOGICAL WNDPAR                  !GET DWARF PARAMETER
	LOGICAL WNDNOD                  !GET NODE NAME
	LOGICAL WNFOP                   !OPEN FILE
	LOGICAL WNFRD			!READ DISK
	LOGICAL WNDSTQ			!GET SETS TO DO
	INTEGER WNMEJC			!CEIL
	INTEGER WNMEJF			!FLOOR
	LOGICAL NMASTG           	!GET A SET
	LOGICAL NSCSTG           	!GET A SET
	LOGICAL NGIDOP			!OPEN GIDS
	LOGICAL NSCHAS			!GET HOUR-ANGLE RANGE
	LOGICAL NSCPLS			!GET Polarisations
	LOGICAL NSCIF1			!GET INTERFEROMETERS
	INTEGER NGIDCL			!CLOSE GIDS
	LOGICAL NGICDT			!READ AND CONVERT DATA
	INTEGER N_GDI_REMOVE		!REMOVE GIDS MAP
	LOGICAL NFLFLS,NFLFL0,NFLFL9,NFLFL5,NFLFL7 !FLAG AREA HANDLING
	LOGICAL NMOMSC			!Set model data
C
C  Data declarations:
C
	CHARACTER OLDTYP*16		!Temp save value of MAPTYP
	REAL OMIN,OMAX			!Temp save old range
	LOGICAL FULL_AREA		!Flag wether full map asked
	INTEGER MPHP			!MAP HEADER POINTER
	INTEGER STHP                    !SET HEADER POINTER
	INTEGER SEQS(2)			!DELETE RANGE
	BYTE LBT			!TEMP TO GET DWARF LOGICAL
C
	BYTE MPH(0:MPHHDL-1)            !MAP HEADER
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  REAL MPHE(0:MPHHDL/4-1)
	  EQUIVALENCE (MPH,MPHJ,MPHE)
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER STHJ(0:STH__L/LB_J-1)
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE)
        BYTE FLH(0:FLHHDL-1)    	!FLAG HEADER
          INTEGER FLHJ(0:FLHHDL/LB_J-1)
          REAL FLHE(0:FLHHDL/LB_E-1)
          EQUIVALENCE (FLH,FLHJ,FLHE)
C
	REAL RBUF(0:8191)               !DATA BUFFER
	  REAL RDAT(0:STHIFR-1,0:3)	!DATA VALUE (ifr, pol)
	  EQUIVALENCE (RBUF,RDAT)	!SAVE SPACE
C
	INTEGER FLG(0:STHIFR-1,0:3)	!FLAGS/WEIGHTS (ifr, pol)
	INTEGER*2 IFRT(0:STHIFR-1)      !IFR TABLE
C-
C
C INIT
C
	IF (ACT) THEN                           !FIRST TIME
	  IF (.NOT.NGIDOP(GID)) THEN
	    CALL WNCTXT (F_TP,'Error opening GIDS display')
	    OPTION='QUIT'
	    GOTO 900
	  END IF
C
C  We cannot resize the GIDS window if a client is connected.
C  So we close the connection here and re-open in NGILOD.
C  This allows for resizing before a map is loaded.
C
	  JS=NGIDCL(GID)
	  OPTION='MAP'
	  NRMAP=0				!NO MAPS LOADED
	  DO_FLAG=.FALSE.			!NO FLAGGING
	  DO_CLIP=.FALSE.			!NO CLIPPING
	  DO_BLANK=.FALSE.			!DO NOT BLANK FLAGS
	  NSRC(0)=0				!NO MODEL SO FAR
	  DEFIMG=.TRUE.				!NEED TO SET UP
	  RANGE(1)= 1E30			!Initialise range
	  RANGE(2)=-1E30
	  NODIN='""'				!No node known yet
	END IF
	ACT=.FALSE.                             !NOT FIRST TIME
C
C GET OPTION
C
 100	CONTINUE
	IF (.NOT.WNDPAR('OPTION',OPTION,LEN(OPTION),J0,OPTION)) THEN
	  OPTION='QUIT'                         !ASSUME END
	ELSE IF (J0.LE.0) THEN
	  OPTION='QUIT'                         !ASSUME END
	END IF
C
C QUIT
C
	IF (OPT.EQ.'QUI') THEN			!READY
	  JS=NGIDCL(GID)			!CLOSE GIDS CONNECTION
	  JS=NFLFL9(DFAR)			!MAKE SURE FLAG FILE GONE
	  GOTO 900
C
C MAP
C
	ELSE IF (OPT.EQ.'MAP') THEN
	  IF (MAPTYP.NE.'MAP') DEFIMG=.TRUE.	!New type, need re-define
	  MAPTYP='MAP'				!Ordinary MAPS from WMP
 200	  CONTINUE
	  IF (.NOT.WNDNOD('INPUT_WMP_NODE',NODIN,'WMP','R',NODIN,FILIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100	!RETRY OPTION
	    GOTO 200				!RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 200				!RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 200				!MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAIN,FILIN,'R')) GOTO 200 !OPEN INPUT
 201	  CONTINUE
	  IF (.NOT.WNDSTQ('WMP_SETS',MXNSET,SETS(0,0),FCAIN)) THEN
	    GOTO 200				!RETRY FILE
	  END IF
	  IF (NMASTG(FCAIN,SETS,MPH,MPHP,SETNAM)) THEN !FIND A SET
	    CALL WNDSTR(FCAIN,SETS)             !RESET SET SEARCH
	  ELSE
	    GOTO 201				!RETRY
	  END IF
C
C GET AREA
C
	  DO I=0,3                              !SET DEFAULT
	    TAREA(I)=0                          !DEFAULT AREA
	    FAREA(I)=0                          !FULL AREA
	    MXAREA(I)=0				!MAX. AREA
	  END DO
	  FAREA(2)=MPHJ(MPH_NRA_J)              !LINE LENGTH
	  FAREA(3)=MPHJ(MPH_NDEC_J)
	  TAREA(2)=FAREA(2)                     !DEFAULT=FULL
	  TAREA(3)=FAREA(3)
	  MXAREA(2)=FAREA(2)                    !MAX. AREA
	  MXAREA(3)=FAREA(3)
	  CALL NMADAR(1,J0,FAREA,2,MXAREA,TAREA,PAREA,
	1               TEAR,PEAR)              !GET AREA
	  IF (J0.LE.0) GOTO 200			!NO AREA GIVEN
C
C	Check if the user wanted the whole map
C
	  FULL_AREA=.TRUE.
	  DO I=0,3
	     IF (TAREA(I).NE.FAREA(I)) FULL_AREA=.FALSE.
	  END DO
	  IF (FULL_AREA) CALL WNCTXT(F_T,'Full map asked')
C
C COMPRESS
C
	  IF (.NOT.WNDPAR('MAP_COMPRESS',COMPR,LB_J,J0,'1')) THEN
	    GOTO 200				!RETRY
	  END IF
	  IF (J0.EQ.0) GOTO 200
	  IF (J0.LT.0) COMPR=1			!DEFAULT
C
C DETERMINE MAX/MIN (save old for a while)
C
	  OMIN=RANGE(1)
	  OMAX=RANGE(2)
	  NRA = MPHJ(MPH_NRA_J)			!RA-DIMENSION
	  NDEC = MPHJ(MPH_NDEC_J)		!DEC-DIMENSION
	  RANGE(1)=1E30				!MINIMUM
	  RANGE(2)=-1E30	          	!MAXIMUM
	  DO WHILE(NMASTG(FCAIN,SETS,MPH,MPHP,SETNAM)) !ALL SETS
	    IF (NRA.EQ.MPHJ(MPH_NRA_J) .AND.
	1		NDEC.EQ.MPHJ(MPH_NDEC_J)) THEN !CAN DO
C
C	Full area: can take the max and min from the map-header
C
	     IF (FULL_AREA) THEN
	      RANGE(2)=MAX(RANGE(2),MPHE(MPH_MAX_E)) !SET MAX
	      RANGE(1)=MIN(RANGE(1),MPHE(MPH_MIN_E)) !SET MIN
C
C	Sub-area: calculate actual range
C
	     ELSE
	      DO I=TEAR(2),TEAR(3)                  !ALL LINES
	        IF (.NOT.WNFRD(FCAIN,LB_E*MPHJ(MPH_NRA_J),
	1               RBUF,MPHJ(MPH_MDP_J)+
	1               (I+MPHJ(MPH_NDEC_J)/2)*LB_E*MPHJ(MPH_NRA_J))) THEN
	          CALL WNCTXT(F_TP,'Error reading map data')
	          CALL WNGEX			!STOP PROGRAM
	        END IF
	        DO I1=TEAR(0)+MPHJ(MPH_NRA_J)/2,
	1               TEAR(1)+MPHJ(MPH_NRA_J)/2 !ALL POINTS
		  IF (RBUF(I1).NE.0.0) THEN	!FORGET EMPTY UV POINTS
	            RANGE(2)=MAX(RANGE(2),RBUF(I1)) !SET MAX
	            RANGE(1)=MIN(RANGE(1),RBUF(I1)) !SET MIN
		  END IF
	        END DO
	      END DO
	     END IF
	    END IF
	  END DO
	  IF (.NOT.WNDPAR('MAP_RANGE',RANGE,2*LB_E,J0,
	1                               A_B(-A_OB),RANGE,2)) GOTO 200 !RETRY
	  IF (J0.EQ.0) GOTO 200
	  IF (RANGE(1).NE.OMIN.OR.RANGE(2).NE.OMAX) DEFIMG=.TRUE.
C
C DATA
C
	ELSE IF (OPT.EQ.'DAT') THEN
C
C	Inform about flagging mode
C
	  IF (DO_FLAG) THEN
	    JS=NFLFLS(DFAR,FLH)
	    CALL WNCTXT(F_T,
	1	'Flagging mode is on, !UJ flags in list',
	1	FLHJ(FLH_FLFN_J))
	  END IF
C
C GET PLOTTING AXES
C
C	Horizontal axis will always be Hourangle, Vertical axis may be
C	Interferometers, Baseline or Channel number.
C
C	If Channel number choosen: sequence in IFR
C	If IFR/Basel choosen:      sequence in Sets (Channel number)
C
 315      CONTINUE
	  OLDTYP=MAPTYP
	  IF (MAPTYP.NE.'IFRS'.AND.MAPTYP.NE.'BASE'	!DEFAULT
	1	              .AND.MAPTYP.NE.'CHAN') THEN
	     MAPTYP='IFRS'
	     RANGE(1)=+1E10				!Range for MAP no
	     RANGE(2)=-1E10				! use for SCN
	  END IF
	  IF (.NOT.WNDPAR('PLOT_TYPE',MAPTYP,LEN(MAPTYP),J0,MAPTYP))
	1               THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100   !RETRY OPTION
	    GOTO 315				 !RETRY
	  END IF	
	  IF (OLDTYP.NE.MAPTYP) DEFIMG=.TRUE.	!New type, need re-define
C
C	Get input SCN file
C
 300	  CONTINUE
	  IF (.NOT.WNDNOD('INPUT_SCN_NODE',NODIN,'SCN','R',NODIN,FILIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 315	!RETRY PLOT-OPTION
	    GOTO 300				!RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 315				!RETRY PLOT-OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 300				!MUST SPECIFY
	  END IF
C
C	We may want to change flags and do things with models, so
C	open with Update. If this fails, try to open with Read only.
C
	  IF (.NOT.WNFOP(FCAIN,FILIN,'U')) THEN
	     IF (.NOT.WNFOP(FCAIN,FILIN,'R')) GOTO 300
	  END IF
C
C	Get sectors to operate upon
C
 301	  CONTINUE
	  IF (.NOT.WNDSTQ('SCN_SETS',MXNSET,SETS(0,0),FCAIN)) THEN
	    GOTO 300				!RETRY FILE
	  END IF
C
C	Check if we have at least one, find ha-increment and channel range
C
	  IF (NSCSTG(FCAIN,SETS,STH,STHP,SETNAM)) THEN !FIND A SET
	    HAINC=STHE(STH_HAI_E)		!GET INCREMENT
	    HARAN(0)=STHE(STH_HAB_E)		!GET START
	    HARAN(1)=STHE(STH_HAB_E)+HAINC*STHJ(STH_SCN_J)	!GET END
	    NIFR=STHJ(STH_NIFR_J)		!Assume all have the same
	    DO I1=0,STHTEL-1
	       TELPOS(I1)=STHE(STH_RTP_E+I1)
	    END DO
	    RCHAN(0)=STHI(STH_CHAN_I)	!WE HAD THE FIRST SET ALREADY
	    RCHAN(1)=STHI(STH_CHAN_I)
	    DO WHILE (NSCSTG(FCAIN,SETS,STH,STHP,SETNAM)) !SCAN THE REST
	       I1=STHI(STH_CHAN_I)			  !Get channel range
	       IF (I1.LT.RCHAN(0)) RCHAN(0)=I1
	       IF (I1.GT.RCHAN(1)) RCHAN(1)=I1
	       R0=STHE(STH_HAB_E)
	       IF (R0.LT.HARAN(0)) HARAN(0)=R0		  !Get HA range
	       IF (R0+HAINC*STHJ(STH_SCN_J).GT.HARAN(1)) 
	1	   HARAN(1)=R0+HAINC*STHJ(STH_SCN_J)
	    END DO
	    CALL WNCTXT(F_TP,'Channel range: !UJ - !UJ',
	1			RCHAN(0),RCHAN(1))
	    IF (MAPTYP.EQ.'CHAN') THEN
	       TEAR(2)=RCHAN(0)
	       TEAR(3)=RCHAN(1)
	    ELSE
	       TEAR(2)=0
	       TEAR(3)=STHTEL*STHTEL-1
	    ENDIF
	    CALL WNDSTR(FCAIN,SETS)             !RESET SET SEARCH
	  ELSE
	    GOTO 301				!RETRY
	  END IF
C
C GET AREA = HA-RANGE and Polarisations/Interferometers
C
	  D0=HARAN(0)
	  IF (.NOT.NSCHAS(0,HARAN)) GOTO 300	!GET HA RANGE
	  J0=(HARAN(0)-D0)/HAINC
	  HARAN(0)=D0+J0*HAINC			!Integer number of HAINCs
	  TEAR(0)=WNMEJF(HARAN(0)/HAINC)
	  TEAR(1)=WNMEJC(HARAN(1)/HAINC)
C
	  IF (.NOT.NSCPLS(2,SPOL)) GOTO 300	!GET POLARISATION SELECTION
C
	  IF (MAPTYP.EQ.'CHAN') THEN
	     IF (.NOT.NSCIF1(2,SIFRS,STHJ)) GOTO 300 !GET IFRS
	  ENDIF
C
C GET DATA TYPE
C
C	Since GIDS uses the same clipping for all planes, plotting Amp 
C	and Phase in the same sequence is pretty useless (either of them
C	will show up as rubbish in most cases).
C
C	In principle, we could display I,Q,U,V with the DATA option,
C	however, too keep things simple at first we forget about that
C	for the while. It may be inserted later here and in NGICDT
C
 306      CONTINUE
	  IF (.NOT.WNDPAR('DATA_TYPE',DATTYP,LEN(DATTYP),J0,'AMPL'))
	1               THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 300   !RETRY FILE
	    GOTO 306
	  END IF
	  COMPR=1				!No compression allowed
C
C For DATA option, get corrections and model
C
  308	  CONTINUE
 	  CALL NSCSAD(CORAP,CORDAP)			!Get corrections
	  IF (IAND(CORDAP,CAP_MOD).EQ.0) THEN	!No DEAPPLY=MODEL
	     CALL WNCTXT(F_TP,'Choose CLEAR and QUIT '//
     &		  'if you do not want model subtraction')
	     CALL NMODAW(NSRC(0),STH)			!Get model
 	     CALL NSCSAD(CORAP,CORDAP)			!Get corrections
	     IF (NSRC(0).GT.0) THEN			!Model given
	        CALL NMOMUI()				! so get the type
		IF (.NOT.NMOMSC(FCAIN,SETS)) THEN	! and set it
		   CALL WNCTXT(F_TP,'Error in model calculation')
	           GOTO 308
	        END IF
	     ELSE
	        CALL WNCTXT(F_TP,'No model subtraction')
	     END IF
	  END IF
	  IF (IAND(CORDAP,CAP_MOD).NE.0) THEN		!DEAPPLY=MODEL
	     CALL WNCTXT(F_TP,
     &	       'The model present in the SCN file will be subtracted')
	  END IF
C
C Save old max and min: if the same, we can continue a sequence
C
	  OMIN=RANGE(1)
	  OMAX=RANGE(2)
C
C Do a prescan on the first sector to get an idea of the range
C We use all scans from the first sector to get a reasonable range, 
C but exclude the first and last few since they are most likely to
C contain execptional values. Please mind the range is just used as
C a default for the range prompt.
C
	  CALL WNCTXT(F_T,'Calculating range...')
	  CALL WNDSTR(FCAIN,SETS)             		!RESET SET SEARCH
	  IF (NSCSTG(FCAIN,SETS,STH,STHP,SETNAM)) THEN 	!READ FIRST HEADER
	     JS=NGICDT(0,STH,JS,IFRT,FLG,RDAT)		!Initialise
C
C	For amplitudes we calculate a second minimum for points above 100, 
C	and count the number of points with values below 100.
C	This allows us to give a more useful minimum (a full 
C	histogram analysis might be better but is slower).
C
	     I3=0					!Counter
	     R0=1E30					!Biased minimum
C
	     DO I=2,STHJ(STH_SCN_J)-3			!SOME SCANS
	       IF (NGICDT(I,STH,JS,IFRT,FLG,RDAT)) THEN !READ DATA
	          DO I1=0,STHJ(STH_NIFR_J)-1		!ALL IFRs
	             DO I2=0,3				!ALL POLs
C
C	Scale based on points with data and no flags
C
	                IF (IAND(FLG(I1,I2),'000000ff'X).NE.0.AND.
	1	            IAND(FLG(I1,I2),'0000ff00'X).EQ.0) THEN
	                    IF (RDAT(I1,I2).GT.RANGE(2)) 
	1			RANGE(2)=RDAT(I1,I2)	!NEW MAXIMUM
			    IF (DATTYP(1:3).EQ.'AMP') THEN
			      IF (RDAT(I1,I2).LT.100.) THEN
			  	I3=I3+1			!Count low points
			      ELSE IF (RDAT(I1,I2).LT.R0) THEN
			  	R0=RDAT(I1,I2)		!Biased minimum
			      END IF
			    END IF
	                    IF (RDAT(I1,I2).LT.RANGE(1)) 
	1			RANGE(1)=RDAT(I1,I2)	!NEW MINIMUM
		        END IF
	             END DO
	          END DO
	       END IF
	     END DO
	  END IF
	  CALL WNDSTR(FCAIN,SETS)             		!RESET SET SEARCH
C
C Ask the range, if not the same as before: need to initialise GIDS again
C
	  IF (RANGE(1).GE.RANGE(2)) THEN		!No data found
	     RANGE(1)=0
	     RANGE(2)=100
	  ELSE IF (RANGE(1).LT.100..AND.
	1	   I3.GT.0.AND.I3.LT.100) THEN		!Low points
	     CALL WNCTXT(F_TP,'Found only !UJ points <100, '//
	1	    'discard true minumum !E',I3,RANGE(1))
	     RANGE(1)=R0
	  END IF
	  IF (.NOT.WNDPAR('MAP_RANGE',RANGE,2*LB_E,J0,
	1                          A_B(-A_OB),RANGE,2)) GOTO 300 !RETRY
	  IF (J0.EQ.0) GOTO 300
	  IF (RANGE(1).NE.OMIN.OR.RANGE(2).NE.OMAX) DEFIMG=.TRUE.
C
C Should flagged data points be set to blank?
C
	  DO_BLANK=.FALSE.
	  IF (.NOT.WNDPAR('BLANK_FLAGS',LBT,LB_B,J0,'NO')) THEN
	    GOTO 100				!RETRY
	  END IF
	  IF (LBT) DO_BLANK=.TRUE.
	  IF (J0.LE.0)  DO_BLANK=.FALSE.
C
C
C
C ---	This option is there for downward compatibility
C
C
C POINT/FLAG
C
	ELSE IF (OPT.EQ.'POI' .OR. OPT.EQ.'FLA') THEN
	  IF (NRMAP.LE.0) THEN
	    CALL WNCTXT(F_TP,'Load a map first')
	    OPTION='MAP'
	    GOTO 100
	  END IF
	  IF (OPT.EQ.'FLA') THEN
	    IF (.NOT.NFLFL0(DFAR)) THEN
	      CALL WNCTXT(F_TP,'Error getting flag file/area')
	      OPTION='QUIT'
	      GOTO 100
	    END IF
	    IF (UFL.EQ.0) UFL=FL_MAN		!ASSUME MANUAL FLAG
	  END IF
C
C ---	The remaining options are handled completely inside this routine
C

C
C FLAG
C
	ELSE IF (OPT.EQ.'DOF'.OR.OPT.EQ.'CLI') THEN
	  IF (.NOT.NFLFL0(DFAR)) THEN
	     CALL WNCTXT(F_TP,'Error getting flag file/area')
	     OPTION='QUIT'
	     GOTO 100
	  END IF
	  DO_FLAG=.TRUE.			!SET FLAGGING TOGGLE
	  UFL=FL_MAN				!DEFAULT IS MANUAL FLAG
	  ALLCH=.TRUE.
	  ALLPOL=.TRUE.
	  IF (OPT.EQ.'DOF') THEN		!Using regions
	     DO_CLIP=.FALSE.
	     CALL WNCTXT(F_T,
     &	           '-------------------------------------------')
	     CALL WNCTXT(F_T,
     &		   ' Flag data after a plane has been loaded:')
	     CALL WNCTXT(F_T,
     &		   '  Goto Etc menu in GIDS, press Region')
	     CALL WNCTXT(F_T,
     &		   '  Press Define to set flags with the mouse')
	     CALL WNCTXT(F_T,
     &		   '   Left:   Draw  linepiece of polygon')
	     CALL WNCTXT(F_T,
     &		   '   Middle: Erase linepiece of polygon')
	     CALL WNCTXT(F_T,
     &		   '   Right:  Close polygon')
	     CALL WNCTXT(F_T,
     &		   '  Press Ready when done')
	     CALL WNCTXT(F_T,
     &	           '-------------------------------------------')
	  ELSE					!Using cliplevels
	     DO_CLIP=.TRUE.
	     CALL WNCTXT(F_T,'You have to specify a cliplevel later')
	  END IF
	  CALL WNCTXT(F_T,'NB: You should load a plane NOW')
	  CALL WNCTXT(F_T,'You cannot flag in the current plane')
C
  400	  IF (.NOT.WNDPAR('ALL_CHAN',LBT,LB_B,J0,'YES')) THEN
	    GOTO 100				!BACK TO MENU
	  END IF
	  IF (.NOT.LBT) ALLCH=.FALSE.
	  IF (J0.EQ.0) 	GOTO 100
	  IF (J0.LE.0)  ALLCH=.TRUE.
	  IF (.NOT.WNDPAR('ALL_POLS',LBT,LB_B,J0,'YES')) THEN
	    GOTO 400				!RETRY
	  END IF
	  IF (.NOT.LBT) ALLPOL=.FALSE.
	  IF (J0.EQ.0) 	GOTO 400
	  IF (J0.LE.0)  ALLPOL=.TRUE.
C
	  CALL WNDDA3('USER_FLAG',UFL)
	  IF (UFL.EQ.0) UFL=FL_MAN		!ASSUME MANUAL FLAG
	  OPTION='DATA'
	  GOTO 100
C
C NOFLAG
C
	ELSE IF (OPT.EQ.'NOF') THEN
	  JS=NFLFL9(DFAR)			!MAKE SURE FLAG FILE GONE
	  DO_FLAG=.FALSE.			!RESET FLAGGING TOGGLE
	  OPTION='DATA'
	  GOTO 100
C
C CLEAR
C
	ELSE IF (OPT.EQ.'CLE') THEN
	  JS=NFLFL9(DFAR)			!MAKE SURE FLAG FILE GONE
	  OPTION='DOFLAG'
	  GOTO 100
C
C UNLOAD/WRITE
C
	ELSE IF (OPT.EQ.'UNL' .OR. OPT.EQ.'WRI') THEN
	  IF (.NOT.NFLFL0(DFAR)) THEN
	    CALL WNCTXT(F_TP,'Error getting flag file/area')
	  ELSE
	    JS=NFLFLS(DFAR,FLH)
	    IF (FLHJ(FLH_FLFN_J).EQ.0) THEN
		CALL WNCTXT(F_TP,'No flags to be saved')
	    ELSE IF (OPT.EQ.'UNL') THEN
	      JS=NFLFL5(DFAR)			!SAVE FLAG AREA
	    ELSE
	      JS=NFLFL7(DFAR,-1,NODIN,SETS)	!WRITE FLAG FILE
	    END IF
	  END IF
	  OPTION='QUIT'
	  GOTO 100
C
C CLEAR GIDS DISPLAY
C
	ELSE IF (OPT.EQ.'GCL') THEN
	  SEQS(1)=1
	  SEQS(2)=MAX(1,NRMAP)
	  IF (.NOT.WNDPAR('MAP_SEQUENCES',SEQS,2*LB_J,J0,
	1          	 A_B(-A_OB),SEQS,2)) GOTO 200 !RETRY
	  IF (J0.EQ.0) GOTO 100			!Retry option
	  IF (J0.LT.0) THEN
	    SEQS(1)=1
	    SEQS(2)=MXNSEQ
	  END IF
	  DO I=SEQS(1),SEQS(2)
	    I0=N_GDI_REMOVE(GID,I)		!REMOVE SEQUENCE FROM GIDS
	  END DO
	  NRMAP=0				!CLEAR NUMBER OF MAPS
	  CALL WNCTXT(F_TP,'Sequence cleared')
	  DEFIMG=.TRUE.				!Re-define for next map
	  OPTION='DATA'				!New default option
	  GOTO 100
C
C READY
C
	END IF
C
 900    CONTINUE
	RETURN                                  !READY
C
C
	END
