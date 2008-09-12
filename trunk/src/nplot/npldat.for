C+ NPLDAT.FOR
C  WNB 910617
C
C  Revisions:
C       WNB 910913      New loops
C       WNB 910918      Add plot of XY,YX only
C       WNB 910930      New logics
C       WNB 911217      Change halftone
C       WNB 911220      Add polarisation
C       WNB 920812      Wrong residual message
C       WNB 920831      Add PLUVO
C       WNB 921102      Correct for full HA range
C       WNB 921221      Default plotter EPS
C       HjV 930209      Add default countours
C                       Change default SIZE: 1.3, 1.3
C                       Change default PLOTTER: PSP
C	WNB 930401	Default size dependent on orientation
C	HjV 930423	Change name of some keywords and some text
C	WNB 930517	Calculate range without empty UV points
C	HjV 930722	Change PLUVO in IFR_MODE
C	WNB 930824	Change telescope selection
C	WNB 930825	New pol. selection
C	WNB 930826	New HA range
C	CMV 931104	Add GDI and fire X windows in advance
C       CMV 931210      Add '???_LOOPS' argument to WNDXLP
C       CMV 931220      Pass FCA of input file to WNDXLP and WNDSTA/Q
C       HjV 940104      Change DATA_TYPES, add PPP, change HASC 
C       HjV 940224      Add mosaik test
C       HjV 940324      Correct HA-scale default (was done afterwards)
C       HjV 940413	Also use loops for testing HA-range
C	CMV 940418	Use NMODAW (default model name) and reread CAP/CDAP
C	CMV 940420	Make PLTSRC integer to allow more options
C	CMV 940426	Add plotting of IF data
C	CMV 940428	Initialise namelist for annotation using NMONM1
C	CMV 940516	Change default HA-range
C	CMV 940620	Correct HA-range for X11, handle Internal model option
C	CMV 940622	Add EDIT option to PLOT_POSITIONS
C	CMV 940622	Add INTERFEROMETER option
C	CMV 940817	Options to ignore pixel coordinate axes
C	CMV 940822	Save some more defaults
C	JPH 940824	Open files readonly
C	CMV 940829	Ask TICK_TYPE also if no pixel-coordinates asked
C	CMV 940927	Answer * to SCALES gives the default
C	CMV 941005	Correct default for OPTIONS if IFDATA choosen
C	CMV 941118	Correct jump if # at WMP_LOOPS prompt
C       WNB 950120      Accept only X,Y for TEL option
C       HjV 950120	Correct HA_SCALE
C       HjV 950503	Re-open SCN-file for update when models found
C       HjV 950705	Add keyword PLOT_FORMAT, PLOT_HEADING
C       HjV 970718	Change PLOT_HEADING (problem on HP with logical)
C	JPH 950817	Remove x11 close (WQDVCL)
C			Remove plot device GDI
C			Be more careful in truncating PLDEV to 2 chars (so no
C			crash when backtracking with ctrl-D)
C			PLTYP 16 chars
C	JPH 960305	Split IF block at label 10 into a block before and
C			 after with the same condition: WAW03 compiler objects
C			 against GOTOs into a block.
C	JPH 960306	Reactivate X11 close (cf. JPH 950817)
C			Modify HAB calculation to work with ST
C	JPH 960402	STxxx options to select HA or ST ordinate
C			Backtracking from PLOT_HEADING prompt
C       HjV 960415      Do not ask IFR_MODE when OPTION=TELESCOPE
C	JPH 960523	ST selection via OPTION i.s.o. IFR_MODE (change mandated
C			 by HjV 960415).
C	JPH 960619	ST selection through HA_MODE keyword
C	JPH 960726	SETNAM argument for NPLSST
C	JPH 960730	Select scale defaults for AGAIN, PGAIN data
C	JPH 960805	Add ANNOTN.
C			ST_INIT in common i.s.o. NPLSST argument 
C	WNB 970605	Add COORD_PREC default 256
C
C
	SUBROUTINE NPLDAT(ACT)
C
C  Get NPLOT program parameters
C
C  Result:
C
C       CALL NPLDAT( ACT_L:IO)  will ask and set all program parameters
C
C  PIN references:
C
C       PLOTTER
C       PLOT_FORMAT
C       PLOT_HEADING
C       OPTION
C	IF_MODE
C	IFR_MODE
C       SCN_NODE
C       WMP_NODE
C       SCN_SETS
C	WMP_SETS
C       POLARISATION
C       APPLY
C       DE_APPLY
C       DATA_TYPES
C       DATA_TYPE
C       APPLY
C       DE_APPLY
C       SCALE_AMPL
C       SCALE_PHASE
C       HA_SCALE
C       FULL_CONT
C       DOT_CONT
C       HALFTONE
C       RANGE
C       TRANSFORM
C       COORD
C       COORD_TYPE
C       PLOT_POSITIONS
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'             !SET HEADER
	INCLUDE 'MPH_O_DEF'
	INCLUDE 'NPL_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	LOGICAL ACT                     !ASK PLOTTER (TRUE)
C
C  Function references:
C
	LOGICAL WNDPAR                  !GET DWARF PARAMETER
	LOGICAL WNDNOD,WNDNOC           !GET/CHANGE NODE NAME
	LOGICAL WNDXLN                  !NEXT LOOP
	LOGICAL WNDXLP                  !GET LOOPS TO DO
	LOGICAL WNFOP                   !OPEN FILE
	LOGICAL WNFRD                   !READ DATA
	LOGICAL WNDSTQ,WNDSTA           !GET SETS TO DO
	CHARACTER*32 WNTTSG             !SHOW SET NAME
	LOGICAL NSCSTG,NMASTG           !GET A SET
	LOGICAL NSCSTL			!GET A SET
	LOGICAL NSCIF1			!SELECT INTERFEROMETERS
	LOGICAL NSCTL1			!SELECT TELESCOPES
	LOGICAL NSCPLS			!SELECT POLARISATION
	LOGICAL NSCHAS			!SELECT HA RANGE
	LOGICAL WQDVOP			!TOO FIRE OFF X11/GIDS WINDOW
	LOGICAL NMONM1			!OPEN NAMELIST FOR MODEL
	LOGICAL NMORDX			!READ MODEL FROM SECTOR
	LOGICAL NMORDM			!MOVE MODEL INTO OTHER INDEX
	INTEGER WNCALN			!GET LENGTH OF STRING
C
C  Data declarations:
C
	INTEGER MXAREA(0:3)             !MAX. MAP AREA
	  DATA MXAREA/0,0,0,0/
	INTEGER FAREA(0:3)              !FULL MAP AREA
	INTEGER TREA(0:3),PREA(0:3),PPREA(0:3) !HELP AREAS
	CHARACTER*16 LPTYP(4)           !PLOT TYPE
	INTEGER HISBAD                  !HISTOGRAM BUFFER POINTER
	INTEGER NRPOL,NRRUL             !POL, RULE SEEN
	INTEGER NRCON,NRHALF            !CONTOUR, HALFTONE SEEN
	REAL MPNOS                      !NOISE
	REAL TRIN(5)                    !TRANSFORM DATA
	CHARACTER*24 LOPT               !LOCAL OPTION
	INTEGER STHP                    !SET POINTER
	REAL LBUF(0:8191)               !DATA BUFFER
	BYTE STH(0:STH__L-1)		!SET HEADER
	  INTEGER*2 		STHI(0:STH__L/LB_I-1)
	  INTEGER 		STHJ(0:STH__L/LB_J-1)
	  REAL 			STHE(0:STH__L/LB_E-1)
	  double precision 	sthd(0:sth__l/lb_d-1)
	  EQUIVALENCE 		(STH,STHI,STHJ,STHE,STHD)
	BYTE MPH(0:MPHHDL-1)            !MAP HEADER
	  INTEGER MPHJ(0:MPHHDL/4-1)
	  EQUIVALENCE (MPH,MPHJ)
	BYTE AMPH(0:MPHHDL-1)           !MAP HEADER (ANGLES)
	  INTEGER AMPHJ(0:MPHHDL/4-1)
	  EQUIVALENCE (AMPH,AMPHJ)
	REAL CVAL
	INTEGER CSET(0:7,0:1)           !TEST SET NAMES
	CHARACTER*256 DFLTXT
	INTEGER DQTMP			!Used to fire off X11 windows
C
	CHARACTER CRDNAM(-6:6)*4	!Names of coordinate types
	DATA CRDNAM/'ODEG','ODDE','ORAD','ODRA','OLM','ODLM',
	1	    'NONE','DLM','LM','DRA','RAD','DDE','DEG'/
	CHARACTER TYPNAM(1:3)*4		!Names of tick types
	DATA TYPNAM/'TICK','DOT','FUL'/
	CHARACTER SRCNAM(0:3)*4		!Names for PLOT_POSITIONS
	DATA SRCNAM/'NO','YES','NAME','EDIT'/
C
	REAL HA(0:1)			!HA RANGE IN DATA
	REAL OHA(0:1)			!HA RANGE IN PREVIOUS DATA
	LOGICAL GOT_IFR,GOT_TEL		!CHECK IF IFRS/TELS SELECTED
	INTEGER NRCHAN  		!# CHANNELS
	REAL HAB			! local HAB or ST
CC	INTEGER INIT			! for NPLSST
	CHARACTER*10 HAMODE		! HA/ST/SEQ mode
	LOGICAL LOGGAIN			! 'gain/phase residuals' flag
	SAVE HA,OHA,GOT_IFR,GOT_TEL	!SAVE FOR DEFAULTS
C-
C
C INIT
C
	IF (ACT) THEN                           !FIRST TIME
	  ACT=.FALSE.                             !NOT FIRST TIME
	  OPTION='QUIT'
	  PLDEV='PP'
	  NODIN=' '
	  SETS(0,0)=0
	  HARA(0)=-179.99/360.
	  HARA(1)=179.99/360.
	  IFR_MODE='NORMAL'
	  IF_MODE=' '
	  SPOL=XY_M
	  GOT_IFR=.FALSE.
	  GOT_TEL=.FALSE.
	  HA(0)=1 				!START RANGE HA
	  HA(1)=-1
	  NDATTP=2
	  DATTYP(1)='AMPL'
	  DATTYP(2)='PHASE'
	  MAPDTYP='DATA'
	  DO I=0,3                              !INIT MAP AREAS
	    TAREA(I)=0                          !DEFAULT AREA
	    FAREA(I)=0                          !FULL AREA
	  END DO
	  CRD=0					!NO ANNOTATION OF MAPS
	  CRDTYP=1				!TICKS
	  PLTSRC=0				!NO POSITIONS
	END IF
C
	OHA(0)=HA(0)				!SAVE OLD RANGE
	OHA(1)=HA(1)
	HA(0)=1 				!START RANGE HA
	HA(1)=-1
	FNAM=' '				!INIT FIELD NAME
	NO_MORE=.FALSE.				!MAKE ALL PLOTS
 	HAMODE=' '
C
C GET OPTION
C
 100    CONTINUE
	IF (IF_MODE.NE.' ') OPT='IFD'		!RESET OPTION
	IF (.NOT.WNDPAR('OPTION',OPTION,LEN(OPTION),J0,OPTION)) THEN
	  OPTION='QUIT'                         !ASSUME END
	ELSE IF (J0.LE.0) THEN
	  OPTION='QUIT'                         !ASSUME END
	END IF
	IF (OPT.EQ.'QUI') RETURN                !READY
C
C Special hour-angle conversions
C
 	IF (OPT.EQ.'SPE') THEN
2 	  IF (.NOT.WNDPAR('HA_MODE',HAMODE,LEN(HAMODE),J0,HAMODE)) THEN
	    HAMODE=' '                         !ASSUME END
	  ELSE IF (J0.LE.0) THEN
	    HAMODE=' '                         !ASSUME END
	  END IF
 	  ST_MODE=0
	  IF (HAMODE(1:1).EQ.'I') THEN
	    HAMODE=HAMODE(2:)
	    IF (.NOT.WNDPAR('HA_INTEGRATION',HAINT,LB_E,J0,'*')) GOTO 100
	  ENDIF
 	  IF (HAMODE.EQ.'ST') THEN
	    ST_MODE=1				! ST i.s.o. HA
 	  ELSEIF (HAMODE.EQ.'SEQUENCE') THEN 
	    ST_MODE=-1				! monotonous quasi-ST
 	  ENDIF
  	  IF (.NOT.WNDPAR('ANNOTATION',ANNOTN,LEN(ANNOTN),J0,ANNOTN))
	1	ANNOTN=' '
 	  GOTO 100				! now select primary option
 	ENDIF
C
C GET PLOTTER
C
	IF (PLDEV(1:1).EQ.'P' .OR. PLDEV(1:1).EQ.'E') PLDEV(3:3)=' '
	IF (.NOT.WNDPAR('PLOTTER',PLDEV,LEN(PLDEV),J0,PLDEV)) THEN
	  GOTO 100
	ELSE IF (J0.LE.0) THEN
	  GOTO 100
	END IF
C
C GET PLOT-FORMAT
C ONLY FOR (ENCAPSULATED) POSTSCRIPT
C
	IF (PLDEV(1:2).EQ.'EL' .OR. PLDEV(1:2).EQ.'EP' .OR.
	1   PLDEV(1:2).EQ.'PL' .OR. PLDEV(1:2).EQ.'PP' ) THEN
	  IF (.NOT.WNDPAR('PLOT_FORMAT',PLDEV(3:3),LEN(PLDEV(3:3)),
	1	J0,PLDEV(3:3))) THEN
	    GOTO 100
	  ELSE IF (J0.LE.0) THEN
	    GOTO 100
	  END IF
	END IF
C
C FIRE OFF X11 Stuff (Open and close again)
C
CC	IF (PLDEV.EQ.'GDI'.OR.PLDEV.EQ.'X11') THEN
	IF (PLDEV.EQ.'X11') THEN
	   DQTMP=0
	   IF (.NOT.WQDVOP(DQTMP,PLDEV)) GOTO 100
	   CALL WQDVCL(DQTMP)
	END IF
C
C TELESCOPE ERRORS/RESIDUALS/DATA/MODEL
C
	IF (OPT.EQ.'TEL' .OR. OPT.EQ.'DAT' .OR. OPT.EQ.'MOD' .OR.
	1   OPT.EQ.'RES' .OR. OPT.EQ.'INT' .OR. OPT.EQ.'IFD') THEN
C
C IF-Data is plotted much alike to TELescope data, so we set the option
C to TEL and make the difference in IFR_MODE. This saves us the trouble
C of changing tests on OPT.EQ.'TEL' in a lot of files...
C
	  IF_MODE=' '				!Not IF-data
	  IF (OPT.EQ.'IFD') THEN		!Get IF Option
	    OPT='TEL'
	    IF (.NOT.WNDPAR('IF_MODE',IF_MODE,
	1	LEN(IF_MODE),J0,'TSYS')) THEN
	      GOTO 100				!RETRY OPTION
	    ELSE IF (J0.LE.0) THEN
	      IF_MODE='TSYS'
	    END IF
	  END IF
C
          IF (OPT.NE.'TEL') THEN                !Not relevant for TELESCOPE
	    IF (.NOT.WNDPAR('IFR_MODE',IFR_MODE,
	1	LEN(IFR_MODE),J0,IFR_MODE)) THEN
	      GOTO 100				!RETRY OPTION
	    ELSE IF (J0.LE.0) THEN
	      IFR_MODE='NORMAL'
	    END IF
	  END IF
	  IF (IFR_MODE.EQ.'SPECTRAL') THEN
  	    PLUVO=.TRUE.
	  ELSE
	    PLUVO=.FALSE.
	  END IF
C
C GET SCAN FILE
C
	ENDIF
 10     CONTINUE
	IF (OPT.EQ.'TEL' .OR. OPT.EQ.'DAT' .OR. OPT.EQ.'MOD' .OR.
	1   OPT.EQ.'RES' .OR. OPT.EQ.'INT' .OR. OPT.EQ.'IFD') THEN
	  IF (.NOT.WNDNOD('SCN_NODE',NODIN,'SCN','R',NODIN,FILIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100  !RETRY OPTION
	    GOTO 10                             !RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100                            !RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 10                             !MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAIN,FILIN,'R')) GOTO 10 !OPEN INPUT
C
C GET SETS
C
 11       CONTINUE
	  IF (.NOT.WNDXLP('SCN_LOOPS',FCAIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10   !SCAN NODE AGAIN
	    GOTO 11
	  END IF
	  IF (.NOT.WNDSTQ('SCN_SETS',MXNSET,SETS(0,0),FCAIN)) THEN
	    GOTO 10                             !RETRY FILE
	  END IF
	  IF (.NOT.NSCSTG(FCAIN,SETS,STH,STHP,SETNAM)) GOTO 10 !FIND A SET
	  CALL WNDSTR(FCAIN,SETS)		!RESET SET SEARCH
C
C FIND HA_RANGE, NR. OF CHANNELS AND TEST IF MOSAIC
C	
	  NRCHAN=0
	  CSET(0,0)=-1
	  CSET(0,1)=-1
	  MOSAIK=.FALSE.
	  CALL WNDXLI(LPOFF)				!CLEAR OFFSET
	  DO WHILE (WNDXLN(LPOFF))
	    ST_INIT=ST_MODE				! init for NPLSST
	    DO WHILE(NSCSTL(FCAIN,SETS,STH(0),STHP,SETNAM,LPOFF)) !NEXT SET
	      IF (CSET(0,0).EQ.-1) THEN
	        DO I1=0,7
	          CSET(I1,0)=SETNAM(I1)
	        END DO
	        CALL WNGMTS(STH_FIELD_N,STH(STH_FIELD_1),FNAM) !SET FIELD NAME
	        OBSDY(1)=STHI(STH_OBS_I)                    !OBS. DAY
	        OBSDY(2)=STHI(STH_OBS_I+1)                  !OBS. YEAR
	      END IF
	      IF (CSET(0,1).NE.-1) THEN
	        IF ((CSET(0,1).EQ.SETNAM(0)).AND.
	1	  	(CSET(1,1).EQ.SETNAM(1))) THEN
	          IF (CSET(2,1).NE.SETNAM(2)) THEN
  		    MOSAIK=.TRUE.
	          ELSE
		    IF ((CSET(3,1).EQ.SETNAM(3)).AND.
	1	      	(CSET(4,1).NE.SETNAM(4))) MOSAIK=.TRUE.
	          END IF
	        END IF
	      END IF
	      DO I1=0,7
	        CSET(I1,1)=SETNAM(I1)
	      END DO
	      IF (ST_MODE.NE.0) THEN
 	        CALL NPLSST(STHD,STHE(STH_HAB_E),SETNAM,HAB)
	      ELSE
	        HAB=STHE(STH_HAB_E)
	      ENDIF
	      R0=HAB-STHE(STH_HAI_E)/2.			!START SET HA
  	      IF (R0.LT.HA(0)) HA(0)=R0			!NEW HA-RANGE
	      R0=HAB+(STHJ(STH_SCN_J)-0.5)*STHE(STH_HAI_E)!END HA
 	      IF (R0.GT.HA(1)) HA(1)=R0
	      NRCHAN=NRCHAN+1
	    END DO! sectors
	    IF (HAMODE.EQ.'SEQUENCE') GOTO 12		! assume all loops
							!  cover same range
	  END DO ! loops
 12	  CONTINUE

	  CALL WNDXLI(LPOFF)				!CLEAR OFFSET
	  CALL WNDSTR(FCAIN,SETS)       	 	!RESET SET SEARCH
	  IF ((IFR_MODE.EQ.'BAND').AND.(NRCHAN.LT.2)) THEN
	    CALL WNCTXT(F_TP,'IFR_MODE !AS with !UJ channels not useful',
	1	  IFR_MODE,NRCHAN)
	    GOTO 10
	  END IF
	  IF (HA(0).NE.OHA(0).OR.HA(1).NE.OHA(1)) THEN	!NEW RANGE IN DATA
	     HARA(0)=HA(0)				!SO CHANGE DEFAULT
	     HARA(1)=HA(1)
	  END IF
C
C GET HA RANGE
C
 13       CONTINUE
	  IF (ST_MODE.EQ.0) THEN
	    IF (.NOT.NSCHAS(0,HARA)) GOTO 10      	!GET HA RANGE
	  ENDIF
	  HARA(0)=MAX(HARA(0),HA(0))			!LIMIT TO INPUT
	  HARA(1)=MIN(HARA(1),HA(1))
	  IF (HARA(1)-HARA(0).LT.1./360.) HARA(1)=HARA(0)+1./360. 
C
C GET POLARISATION
C
 14       CONTINUE
	  IF (.NOT.NSCPLS(0,SPOL)) GOTO 10	!GET POLARISATION SELECTION
CC#135	  IF (IF_MODE.NE.' '.AND.IAND(SPOL,YX_M).NE.0) 
	  IF (OPT.EQ.'TEL' .AND. IAND(SPOL,YX_M).NE.0) 
	1      SPOL=IAND(SPOL,XY_M)		!IF/TEL only X,Y
C
C GET MODEL
C
	  IF (OPT.EQ.'RES' .OR. OPT.EQ.'MOD') THEN
	    CALL NMODAW(NSRC(0),STH)            !GET MODEL
	    CALL NSCSAD(CORAP,CORDAP)           !GET CORRECTIONS AGAIN
	    IF (NSRC(0).GT.0) THEN		!YES, models found
	      CALL NMOMUI			!GET TYPE
C We found models, so re-open the SCN-file for Update mode
	      CALL WNFCL(FCAIN)			!CLOSE FILE
	      IF (.NOT.WNDNOC(' ',' ','SCN','U',' ',FILIN)) THEN !CHANGE DATES
		  CALL WNCTXT(F_TP,'Node is not writable')
		  GOTO 10
	      END IF
	      IF (.NOT.WNFOP(FCAIN,FILIN,'U')) THEN	!OPEN SCN FILE
		  CALL WNCTXT(F_TP,'Cannot write to file attached to node')
		  GOTO 10
	      END IF
	    ELSE IF (IAND(CORDAP,CAP_MOD).NE.0) THEN  !DEAPPLY=MODEL
	      IF (OPT.EQ.'RES') THEN		!RESIDUAL OF INTERNAL MODEL
	        CALL WNCTXT(F_TP,
	1	      'The model present in the SCN file will be used')
	        NSRC(0)= -1			!INDICATE NOT REDUNDANCY
	      ELSE
	        CALL WNCTXT(F_TP,
	1	      'Extracting the internal model...')
	        DO WHILE (NSCSTG(FCAIN,SETS,STH,STHP,SETNAM))
	           IF (STHJ(STH_MDL_J).NE.0) THEN       !THIS ONE
	             IF (NMORDX(FCAIN,STHJ(STH_MDL_J),7)) THEN   !FOUND
	                IF (NMORDM(7,-1)) GOTO 800                !READY
	             END IF
	           END IF
	        END DO
        	CALL WNCTXT(F_TP,'No model found in scan-file')
	        GOTO 10				!RETRY FILE IF NO MODEL
 800	        CONTINUE			!FOUND, ENTER NMODAW AGAIN
	        CALL WNDSTR(FCAIN,SETS)		!RESET SET SEARCH
	        CALL WNCTXT(F_TP,
	1	     'You may read additional components now...')
	        CALL NMODAW(NSRC(0),STH)	!GET MODEL AGAIN
	        CALL NMOMUI			!GET TYPE
	      END IF
	    ELSE IF (OPT.EQ.'MOD') THEN
	       GOTO 10                           !RETRY FILE IF NO MODEL
	    ELSE
	      CALL WNCTXT(F_TP,'Redundancy residuals selected')
	    END IF
	  ELSE
	    NSRC(0)=0                           !SET NO MODEL
	  END IF
C
C GET TELESCOPES/INTERFEROMETERS
C
 15       CONTINUE
	  IF (OPT.EQ.'TEL') THEN
	    IF (GOT_TEL) THEN
	       IF (.NOT.NSCTL1(0,STELS,STHJ)) GOTO 10 !GET TELESCOPES TO DO
	    ELSE
	       IF (.NOT.NSCTL1(1,STELS,STHJ)) GOTO 10 !GET TELESCOPES TO DO
	    END IF
	    GOT_TEL=.TRUE.
	  ELSE
	    IF (GOT_IFR) THEN
	      IF (.NOT.NSCIF1(0,SIFRS,STHJ)) GOTO 10 !GET IFRS
	    ELSE IF (OPTION(4:4).EQ.'P') THEN
	      IF (.NOT.NSCIF1(4,SIFRS,STHJ)) GOTO 10 !GET IFRS
	    ELSE IF (OPT.EQ.'DAT' .OR. OPT.EQ.'INT' .OR. 
	1               (OPT.EQ.'RES' .AND. NSRC(0).EQ.0)) THEN
	      IF (.NOT.NSCIF1(2,SIFRS,STHJ)) GOTO 10 !GET IFRS
	    ELSE IF (OPT.EQ.'MOD' .OR.
	1               (OPT.EQ.'RES' .AND. NSRC(0).NE.0)) THEN
	      IF (.NOT.NSCIF1(3,SIFRS,STHJ)) GOTO 10 !GET IFRS
	    END IF
	    GOT_IFR=.TRUE.
	  END IF
C
C GET CORRECTIONS TO DO
C
	  CALL NSCSAD(CORAP,CORDAP)             !GET CORRECTIONS TO PLOT
C
C GET DATA TYPES
C
 16       CONTINUE
C
C	Dwarf refuses array of characters as default. Should work, but
C	I don't want to search for the bug. Just fill in default string
C	here.
C
	  DFLTXT=' '
	  I1=0
	  DO I=1,NDATTP
	     DFLTXT(I1+1:)=DATTYP(I)
	     I1=WNCALN(DFLTXT)+1
	     DFLTXT(I1:I1)=','
	  END DO
C
	  IF (.NOT.WNDPAR('DATA_TYPES',DATTYP,MXNDTP*LEN(DATTYP(1)),
	1            NDATTP,DFLTXT(:I1-1))) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10   !RETRY FILE
	    GOTO 16
	  END IF
	  IF (NDATTP.EQ.0) GOTO 10
	  IF (NDATTP.LT.0) THEN
	     DATTYP(1)='AMPLITUDE'
	     DATTYP(2)='PHASE'
	     NDATTP=2
	  END IF
	  IF (IF_MODE.NE.' ') THEN		!IF Option
	    I=1
	    DO WHILE (I.LE.NDATTP)
	      IF (DATTYP(I)(1:1).NE.'A') THEN
	        CALL WNCTXT(F_TP,'!AS plots not relevant for IF_DATA',
	1	      DATTYP(I))
		NDATTP=NDATTP-1
	        DO I2=I,NDATTP
		  DATTYP(I2)=DATTYP(I2+1)
		END DO
	      ELSE
	        I=I+1
	      END IF
	    END DO
	    IF (NDATTP.EQ.0) THEN
	      CALL WNCTXT(F_TP,'Only Amplitude plots relevant for IF_DATA')
	      GOTO 16
	    END IF
	  END IF
	  L0=.FALSE.                            !GET SCALES
	  L1=.FALSE.
	  DO I=1,NDATTP
	    IF (DATTYP(I)(1:2).EQ.'AP') THEN
	      L1=.TRUE.                         !PHASE SCALE
	      L0=.TRUE.                         !AMPL SCALE
	    ELSE IF (DATTYP(I)(1:1).EQ.'P') THEN
	      L1=.TRUE.                         !PHASE SCALE
	    ELSE
	      L0=.TRUE.                         !AMPL SCALE
	    END IF
	    IF ((DATTYP(I)(1:2).EQ.'AP').OR.(DATTYP(I)(1:2).EQ.'CS')) THEN
	       PLOTAP=.TRUE.
	    ELSE
	       PLOTAP=.FALSE.
	    ENDIF
	  END DO
	  IF (PLOTAP.AND.MOSAIK) THEN
	    CALL WNCTXT(F_TP,'AP or CS not possible for Mosaic observation')
	    GOTO 16
	  END IF
C
C GET PLOTS PER PAGE
C
 19	  CONTINUE
	  PPP(1)=1
	  PPP(2)=1
	  IF (.NOT.WNDPAR('PLOTS_PER_PAGE',PPP,2*LB_J,J0,A_B(-A_OB),
	1		PPP,2)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10   !RETRY FILE
	    GOTO 19
	  END IF
	  IF (J0.EQ.0) GOTO 10
	  IF (J0.LT.0) THEN
	    PPP(1)=1
	    PPP(2)=1
	  END IF
	  PPPNR=-1                              !PLOT NUMBER ON PAGE
C
C GET DATA SCALE
C
 17       CONTINUE
	  LOGGAIN=.FALSE.
	  DO I=1,NDATTP
	    IF ((DATTYP(I)(1:2).EQ.'AP').OR.(DATTYP(I)(1:2).EQ.'CS')) GOTO 18
	    IF ((DATTYP(I)(1:2).EQ.'AG').OR.(DATTYP(I)(1:2).EQ.'PG'))
	1	LOGGAIN=.TRUE. 
	  END DO
	  SCAL(1)=100                           !DEFAULTS
	  SCAL(2)=10
	  IF (OPT.EQ.'RES') THEN                !GET NOISES
	    IF (NSRC(0).NE.0) THEN		!ALIGN
	      DO I1=0,1
		SCAL(I1+1)=1			!LOWEST DEFAULT
		DO I=0,1
		  SCAL(I1+1)=MAX(STHE(STH_REDNS_E+2*I+I1),SCAL(I1+1))
		  SCAL(I1+1)=MAX(STHE(STH_ALGNS_E+2*I+I1),SCAL(I1+1))
		END DO
		SCAL(I1+1)=4*SCAL(I1+1)		!CORRECT SCALE
	      END DO
	    ELSE				!REDUN
	      DO I1=0,1
		SCAL(I1+1)=1			!LOWEST DEFAULT
		DO I=0,1
		  SCAL(I1+1)=MAX(STHE(STH_REDNS_E+2*I+I1),SCAL(I1+1))
		END DO
		SCAL(I1+1)=4*SCAL(I1+1)		!CORRECT SCALE
	      END DO
	    END IF
	  END IF
	  IF (L0) THEN
	    IF (IF_MODE(1:2).EQ.'TP') THEN
	      IF (.NOT.WNDPAR('SCALE_AMPL',SCAL(1),LB_E,J0,'500.')) THEN
		IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY FILE
		GOTO 17
	      END IF
	      IF (J0.EQ.0) GOTO 10
	      IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) SCAL(1)=500
	    ELSE IF (OPT.EQ.'TEL' .OR. LOGGAIN) THEN
	      IF (.NOT.WNDPAR('SCALE_AMPL',SCAL(1),LB_E,J0,'4.')) THEN
		IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY FILE
		GOTO 17
	      END IF
	      IF (J0.EQ.0) GOTO 10
	      IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) SCAL(1)=4
	    ELSE IF (OPT.EQ.'DAT' .OR. OPT.EQ.'MOD' 
	1			  .OR. OPT.EQ.'INT') THEN
	      IF (.NOT.WNDPAR('SCALE_AMPL',SCAL(1),LB_E,J0,'100.')) THEN
		IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY FILE
		GOTO 17
	      END IF
	      IF (J0.EQ.0) GOTO 10
	      IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) SCAL(1)=100
	    ELSE IF (OPT.EQ.'RES') THEN
	      IF (.NOT.WNDPAR('SCALE_AMPL',SCAL(1),LB_E,J0,
	1                       A_B(-A_OB),SCAL(1),1)) THEN
		IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY FILE
		GOTO 17
	      END IF
	      IF (J0.EQ.0) GOTO 10
	      IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) SCAL(1)=100
	    END IF
	  END IF
C
	  IF (L1) THEN
	    IF (OPT.EQ.'TEL' .OR. LOGGAIN) THEN
	      IF (.NOT.WNDPAR('SCALE_PHASE',SCAL(2),LB_E,J0,'2.')) THEN
		IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY FILE
		GOTO 17
	      END IF
	      IF (J0.EQ.0) GOTO 10
	      IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) SCAL(2)=2
	    ELSE IF (OPT.EQ.'DAT' .OR. OPT.EQ.'MOD' 
	1			  .OR. OPT.EQ.'INT') THEN
	      IF (.NOT.WNDPAR('SCALE_PHASE',SCAL(2),LB_E,J0,'10.')) THEN
		IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY FILE
		GOTO 17
	      END IF
	      IF (J0.EQ.0) GOTO 10
	      IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) SCAL(2)=10
	    ELSE IF (OPT.EQ.'RES') THEN
	      IF (.NOT.WNDPAR('SCALE_PHASE',SCAL(2),LB_E,J0,
	1               A_B(-A_OB),SCAL(2),1)) THEN
		IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10 !RETRY FILE
		GOTO 17
	      END IF
	      IF (J0.EQ.0) GOTO 10
	      IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) SCAL(2)=10
	    END IF
	  END IF
C
C HA SCALE
C
 18       CONTINUE
	  IF (IFR_MODE.NE.'BAND') THEN
	    DO I=1,NDATTP
	      IF ((DATTYP(I)(1:2).EQ.'AP').OR.(DATTYP(I)(1:2).EQ.'CS')) THEN
	        R0=10.*PPP(1)
	      ELSE
	        R0=30.*(HARA(1)-HARA(0))*PPP(2)
CC Before 950120 it was as below, but for ELSE it was wrong
CC Just leave it here temporary, in case of
C	      ELSE IF (PLDEV.EQ.'GDI'.OR.PLDEV.EQ.'X11') THEN
C	        R0=30.*(HARA(1)-HARA(0))*PPP(2)
C	      ELSE
CC	        R0=15.*(HARA(1)-HARA(0))*PPP(2)
	      ENDIF
	    END DO
	    IF (.NOT.WNDPAR('HA_SCALE',HASC,LB_E,J0,A_B(-A_OB),R0,1)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10   !RETRY FILE
	      GOTO 18
	    END IF
	    IF (J0.EQ.0) GOTO 10
	    IF (J0.LT.0.OR.E_C.EQ.DWC_WILDCARD) HASC=R0
	    HASC=HASC/10.                         !MAKE PER MM
	  END IF
C
C MAP
C
	ELSE IF (OPT.EQ.'MAP') THEN
C
C GET FILE
C
 20       CONTINUE
	  IF (.NOT.WNDNOD('WMP_NODE',NODIN,'WMP','R',NODIN,FILIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100  !RETRY OPTION
	    GOTO 20                             !RETRY
	  ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
	    GOTO 100                            !RETRY OPTION
	  ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
	    GOTO 20                             !MUST SPECIFY
	  END IF
	  IF (.NOT.WNFOP(FCAIN,FILIN,'R')) GOTO 20 !OPEN INPUT
C
C GET SETS
C
 21       CONTINUE
	  IF (.NOT.WNDXLP('WMP_LOOPS',FCAIN)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20   !MAP NODE AGAIN
	    GOTO 21
	  END IF
	  IF (.NOT.WNDSTQ('WMP_SETS',MXNSET,SETS(0,0),FCAIN)) THEN
	    GOTO 20                             !RETRY FILE
	  END IF
	  IF (NMASTG(FCAIN,SETS,MPH,STHP,SETNAM)) THEN !FIND A SET
	    CALL WNDSTR(FCAIN,SETS)             !RESET SET SEARCH
	  ELSE
	    GOTO 21                             !RETRY
	  END IF
C
C GET PLOT TYPES
C
 28       CONTINUE
	  I=0                                   !COUNT SETS
	  NRPOL=0                               !NOT POL
	  NRRUL=0                               !NOT RULE
	  NRCON=0                               !NOT CONTOUR
	  NRHALF=0                              !NOT HALFTONE
	  DO WHILE (NMASTG(FCAIN,SETS,MPH,STHP,SETNAM))
	    CALL WNCTXT(F_TP,'Map: !AS',WNTTSG(SETNAM,0)) !SHOW SET
	    IF (.NOT.WNDPAR('PLOT_TYPE',LPTYP,64,J0,'CONT,HALF')) THEN
	      CALL WNDSTR(FCAIN,SETS)           !RESET SET SEARCH
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20 !RETRY FILE
	      GOTO 28
	    END IF
	    IF (J0.EQ.0) THEN
	      CALL WNDSTR(FCAIN,SETS)           !RESET SET SEARCH
	      GOTO 20                           !RETRY FILE
	    END IF
	    IF (J0.LT.0) THEN                   !DEFAULT
	      J0=2
	      LPTYP(1)='CONTOUR'
	      LPTYP(2)='HALFTONE'
	    END IF
	    I=I+1                               !COUNT SET
	    PTYP(I)=0                           !NO PLOT TYPE
	    DO I1=1,J0
	      IF (LPTYP(I1).EQ.'CONTOUR') THEN
		PTYP(I)=IOR(PTYP(I),1)
		NRCON=NRCON+1
	      ELSE IF (LPTYP(I1).EQ.'HALFTONE') THEN
		PTYP(I)=IOR(PTYP(I),2)
		NRHALF=NRHALF+1 
	      ELSE IF (LPTYP(I1).EQ.'POLARISATION') THEN
		PTYP(I)=IOR(PTYP(I),4)
		NRPOL=NRPOL+1
	      ELSE IF (LPTYP(I1).EQ.'RULED') THEN
		PTYP(I)=IOR(PTYP(I),8)
		NRRUL=NRRUL+1
	      END IF
	    END DO
	    IF (IAND(4,PTYP(I)).NE.0) THEN      !GET ANGLE SET
	      IF (.NOT.WNDSTA('ANGLE_WMP_SET',1,ASET(0,I),FCAIN)) THEN
		GOTO 20                         !RETRY FILE
	      END IF
	      IF (NMASTG(FCAIN,ASET(0,I),AMPH,STHP,SETNAM)) THEN !FIND A SET
		CALL WNDSTR(FCAIN,ASET(0,I))    !RESET SEARCH
	      ELSE
		GOTO 20                         !NO SUCH SET
	      END IF
	    END IF
	  END DO
C
C GET DATA TYPES
C
 22       CONTINUE
	  IF (.NOT.WNDPAR('DATA_TYPE',MAPDTYP,LEN(MAPDTYP),
	1               J0,MAPDTYP)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20   !RETRY FILE
	    GOTO 22
	  END IF
	  IF (J0.EQ.0) GOTO 20
	  IF (J0.LT.0) MAPDTYP='DATA'
C
C GET AREA
C
	  IF (MPHJ(MPH_NRA_J).NE.FAREA(2).OR.
	1	MPHJ(MPH_NDEC_J).NE.FAREA(3)) THEN	!NEW AREA
	    DO I=0,3                              !SET DEFAULT
	      TAREA(I)=0                          !DEFAULT AREA
	      FAREA(I)=0                          !FULL AREA
	    END DO
	    FAREA(2)=MPHJ(MPH_NRA_J)              !LINE LENGTH
	    FAREA(3)=MPHJ(MPH_NDEC_J)
	    TAREA(2)=FAREA(2)                     !DEFAULT=FULL
	    TAREA(3)=FAREA(3)
	    MXAREA(2)=FAREA(2)                    !MAX. AREA
	    MXAREA(3)=FAREA(3)
	  END IF
	  CALL NMADAR(1,J0,FAREA,0,MXAREA,TAREA,PAREA,
	1               TEAR,PEAR)              !GET AREA
	  IF (J0.LE.0) GOTO 20                  !NO AREA GIVEN
C
C DETERMINE MAX/MIN
C
	  MPMXMN(2)=-1E30                       !MAX
	  MPMXMN(1)=1E30                        !MIN
	  CALL WNMHS8(HISBAD,+1,0.25E0)         !INIT HISTOGRAM
	  DO I=TEAR(2),TEAR(3)                  !ALL LINES
	    IF (.NOT.WNFRD(FCAIN,LB_E*MPHJ(MPH_NRA_J),
	1               LBUF,MPHJ(MPH_MDP_J)+
	1               (I+MPHJ(MPH_NDEC_J)/2)*LB_E*MPHJ(MPH_NRA_J))) THEN
	      CALL WNCTXT(F_TP,'Error reading map data')
	      CALL WNGEX                        !STOP PROGRAM
	    END IF
	    CALL WNMHS1(HISBAD,TAREA(2),LBUF(TEAR(0)+MPHJ(MPH_NRA_J)/2)) !HISTO
	    DO I1=TEAR(0)+MPHJ(MPH_NRA_J)/2,
	1               TEAR(1)+MPHJ(MPH_NRA_J)/2 !ALL POINTS
	      IF (LBUF(I1).NE.0.0) THEN
		  MPMXMN(2)=MAX(MPMXMN(2),LBUF(I1)) !SET MAX
		  MPMXMN(1)=MIN(MPMXMN(1),LBUF(I1)) !SET MIN
	      ENDIF
	    END DO
	  END DO
	  CALL WNMHS4(HISBAD,MPNOS,F_TP)        !GET NOISE
	  CALL WNMHS9(HISBAD)                   !FREE HISTOGRAM
	  CALL WNCTXT(F_TP,'Range: !2E7',MPMXMN) !SHOW MAX/MIN
C
C GET SIZE
C
 23	  CONTINUE
	  IF (PLDEV(2:2).EQ.'P') THEN		!PORTRAIT
	    SIZE(1)=1.3
	    SIZE(2)=1.3
	  ELSE
	    SIZE(1)=1
	    SIZE(2)=1
	  END IF
	  IF (.NOT.WNDPAR('SIZE',SIZE,2*LB_E,J0,A_B(-A_OB),
	1		SIZE,2)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20   !RETRY FILE
	    GOTO 23
	  END IF
	  IF (J0.EQ.0) GOTO 20
	  IF (J0.LT.0) THEN
	    IF (PLDEV(2:2).EQ.'P') THEN		!PORTRAIT
	      SIZE(1)=1.3
	      SIZE(2)=1.3
	    ELSE
	      SIZE(1)=1
	      SIZE(2)=1
	    END IF
	  END IF
C
C GET CONTOURS
C
 24       CONTINUE
	  IF (NRCON.EQ.0) THEN                          !NO CONTOURS
	    NCF=0
	    NCD=0
	    GOTO 25
	  END IF
	  CVAL = MAX(0.001,2*MPNOS)                     ! 2 SIGMA (W.U.)
	  CVAL = 2.**(INT(LOG(CVAL)/LOG(2.)))           ! ROUNDED TO POWER OF 2 
	  IF (CVAL.GE.MPMXMN(2)) CVAL = CVAL/2.         ! LOWER IF NECESSARY
	  NCF = 0                                       ! NR OF FULL CONTOURS
	  NCD = 0                                       ! NR OF DOTTED CONTOURS
	  DO WHILE ((CVAL.LT.MPMXMN(2)).AND.(NCF.LT.MXNC/2)) 
	    NCF = NCF+1
	    FCONT(NCF) = CVAL                           ! FULL CONTOURS (>0)
	    IF (-CVAL.GT.MPMXMN(1)) THEN
	      NCD = NCD+1
	      DCONT(NCD) = -CVAL                        ! DOTTED CONTOURS (<0)
	    ENDIF
	    CVAL = CVAL*SQRT(2.)                        ! LOGARITHMIC SUCCESSION
	  END DO
	  CALL WNCTXS (DFLTXT,' !#E6 ',NCF,FCONT)       ! ENCODE FCONT(NCF)
	  IF (.NOT.WNDPAR('FULL_CONT',FCONT,MXNC*LB_E,NCF,DFLTXT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20
	    GOTO 24
	  END IF
	  IF (NCF.LT.0) NCF=0
	  CALL WNCTXS (DFLTXT,' !#E6 ',NCD,DCONT)       ! ENCODE DCONT(NCD)
	  IF (.NOT.WNDPAR('DOT_CONT',DCONT,MXNC*LB_E,NCD,DFLTXT)) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20
	    GOTO 24
	  END IF
	  IF (NCD.LT.0) NCD=0
C
C GET HALFTONE
C
 25       CONTINUE
	  IF (NRHALF.EQ.0) THEN
	    HALF=-1                                     !NO HALFTONE
	    GOTO 30
	  END IF
	  IF (.NOT.WNDPAR('HALFTONE',LOPT,LEN(LOPT),J0,'NONE')) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20           !RETRY FILE
	    GOTO 25
	  END IF
	  IF (J0.LE.0) LOPT='NONE'
	  IF (LOPT(1:3).EQ.'CON') THEN
	    HALF=0
	  ELSE IF (LOPT(1:3).EQ.'STE') THEN
	    HALF=1
	  ELSE IF (LOPT(1:3).EQ.'PAT') THEN
	    HALF=2
	  ELSE
	    HALF=-1
	  END IF
	  IF (HALF.GE.0) THEN
	    IF (.NOT.WNDPAR('RANGE',RANGE,2*LB_E,J0,A_B(-A_OB),
	1               MPMXMN,2)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20         !RETRY FILE
	      GOTO 25
	    END IF
	    IF (J0.LE.0) THEN
	      RANGE(1)=MPMXMN(1)
	      RANGE(2)=MPMXMN(2)
	    END IF
	    DO I=0,MXNTRF                               !DEFAULT TRANSFORM
	      TRF(I)=I
	    END DO
	    IF (.NOT.WNDPAR('TRANSFORM',TRIN,5*LB_E,J0,'""')) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20         !RETRY FILE
	      GOTO 25
	    END IF
	    IF (J0.GT.0) THEN                           !MAKE TRANSFORM
	      DO I=NINT(MAX(0.,TRIN(1)*MXNTRF)),
	1               NINT(MIN(FLOAT(MXNTRF),TRIN(2)*MXNTRF))
		R1=1.
		R0=0.
		DO I1=3,J0
		  R0=R0+R1*TRIN(I1)*MXNTRF
		  R1=R1*I/MXNTRF
		END DO
		TRF(I)=R0
	      END DO
	    END IF
	  END IF
C
C GET POL. SCALE
C
 30       CONTINUE
	  PSCAL=0.                                      !NO POL.
	  IF (NRPOL.NE.0) THEN
	    PRANGE(1)=0.1*MPMXMN(2)
	    PRANGE(2)=MPMXMN(2)
	    IF (.NOT.WNDPAR('POL_RANGE',PRANGE,2*LB_E,J0,A_B(-A_OB),
	1               PRANGE,2)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20         !RETRY FILE
	      GOTO 30
	    END IF
	    IF (J0.LE.0) THEN
	      PRANGE(1)=0.1*MPMXMN(2)
	      PRANGE(2)=MPMXMN(2)
	    END IF
	    IF (.NOT.WNDPAR('POL_SCALE',PSCAL,LB_E,J0,
	1               A_B(-A_OB),MAX(1.,PRANGE(2))/2.)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10		!RETRY FILE
	      GOTO 30
	    END IF
	    IF (J0.EQ.0) GOTO 10
	    IF (J0.LT.0) PSCAL=MAX(1.,PRANGE(2))/2.
	    PSCAL=PSCAL/10.                             !MAKE PER MM
	    IF (PSCAL.NE.0) PSCAL=1./PSCAL              !CORRECT FORMAT
	    IF (.NOT.WNDPAR('POL_TYPE',LPTYP,16,J0,'POL')) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20         !RETRY FILE
	      GOTO 30
	    END IF
	    IF (J0.EQ.0) GOTO 10
	    IF (J0.LT.0) LPTYP(1)='POL'
	    POLMAG=.FALSE.
	    IF (LPTYP(1).EQ.'MAG') POLMAG=.TRUE.
	    FAREA(0)=TAREA(0)
	    FAREA(1)=TAREA(1)
	    FAREA(2)=TAREA(2)                           !LINE LENGTH
	    FAREA(3)=TAREA(3)
	    TREA(0)=TAREA(0)
	    TREA(1)=TAREA(1)
	    TREA(2)=MAX(3,FAREA(2)-2)                   !DEFAULT=-2
	    TREA(3)=MAX(3,FAREA(3)-2)
	    MXAREA(2)=FAREA(2)                          !MAX. AREA
	    MXAREA(3)=FAREA(3)
	    CALL NMADAR(1,J0,FAREA,0,MXAREA,TREA,PREA,
	1               PTEAR,PPREA)                    !GET AREA
	    IF (J0.LE.0) GOTO 20                        !NO AREA GIVEN
	  END IF
C
C GET RULE SCALE
C
 31       CONTINUE
	  RSCAL=0.                                      !NO RULE
	  IF (NRRUL.NE.0) THEN
	    IF (.NOT.WNDPAR('RULE_RANGE',RRANGE,2*LB_E,J0,A_B(-A_OB),
	1               MPMXMN,2)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20         !RETRY FILE
	      GOTO 31
	    END IF
	    IF (J0.LE.0) THEN
	      RRANGE(1)=MPMXMN(1)
	      RRANGE(2)=MPMXMN(2)
	    END IF
	    IF (.NOT.WNDPAR('RULE_SCALE',RSCAL,LB_E,J0,
	1               A_B(-A_OB),MAX(1.,RRANGE(2))/2.,1)) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 10		!RETRY FILE
	      GOTO 30
	    END IF
	    IF (J0.EQ.0) GOTO 10
	    IF (J0.LT.0) RSCAL=MAX(1.,RRANGE(2))/2.
	    RSCAL=RSCAL/10.                             !MAKE PER MM
	    IF (RSCAL.NE.0) RSCAL=1./RSCAL              !CORRECT FORMAT
	    FAREA(0)=TAREA(0)
	    FAREA(1)=TAREA(1)
	    FAREA(2)=TAREA(2)                           !LINE LENGTH
	    FAREA(3)=TAREA(3)
	    TREA(0)=TAREA(0)
	    TREA(1)=TAREA(1)
	    TREA(2)=MAX(3,FAREA(2)-2)                   !DEFAULT=-2
	    TREA(3)=MAX(3,FAREA(3)-2)
	    MXAREA(2)=FAREA(2)                          !MAX. AREA
	    MXAREA(3)=FAREA(3)
	    CALL NMADAR(1,J0,FAREA,0,MXAREA,TREA,PREA,
	1               RTEAR,PPREA)                    !GET AREA
	    IF (J0.LE.0) GOTO 20                        !NO AREA GIVEN
	  END IF
C
C GET COORDINATE TYPE
C
 26       CONTINUE
	  IF (CRD.LT.-6.OR.CRD.GT.6) CRD=0		!FIT IN RANGE
	  IF (.NOT.WNDPAR('COORD',LOPT,LEN(LOPT),
	1	J0,CRDNAM(CRD))) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20           !RETRY FILE
	    GOTO 26
	  END IF
	  IF (J0.LE.0) LOPT='NONE'
	  I1=1						!PLOT GRIDS COORDS
	  IF (LOPT(1:1).EQ.'O') THEN			!NO GRIDS
	     I1= -1
	     LOPT=LOPT(2:)				!STRIP THE 'O'
	  END IF
	  IF (LOPT(:3).EQ.'DLM') THEN                   !SET COORD. TYPE
	    CRD=1
	  ELSE IF (LOPT(:3).EQ.'LM') THEN
	    CRD=2
	  ELSE IF (LOPT(:3).EQ.'DRA') THEN
	    CRD=3
	  ELSE IF (LOPT(:3).EQ.'RAD') THEN
	    CRD=4
	  ELSE IF (LOPT(:3).EQ.'DDE') THEN
	    CRD=5
	  ELSE IF (LOPT(:3).EQ.'DEG') THEN
	    CRD=6
	  ELSE
	    CRD=0
	  END IF
	  CRD=I1*CRD					!MERGE WITH GRIDS
	  IF (CRD.NE.0) THEN                            !COORD. WANTED
	    IF (CRDTYP.GT.3.OR.CRDTYP.LT.1) CRDTYP=1	!FIT IN RANGE
	    IF (.NOT.WNDPAR('COORD_TYPE',LOPT,LEN(LOPT),
	1		J0,TYPNAM(CRDTYP))) THEN
	      IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20         !RETRY FILE
	      GOTO 26
	    END IF
	    IF (J0.LE.0) LOPT='TICK'
	    IF (LOPT(:3).EQ.'DOT') THEN                 !SET COORD. TYPE
	      CRDTYP=2
	    ELSE IF (LOPT(:3).EQ.'FUL') THEN
	      CRDTYP=3
	    ELSE
	      CRDTYP=1
	    END IF
	  END IF
C
C GET CONTOURING FOR COORD PRECISION
C
	  IF (.NOT.WNDPAR('COORD_PREC',I,LB_J,J0)) THEN
	    I=256
	  ELSE IF (J0.LE.0) THEN
	    I=256
	  END IF
	  HAINT=I
C
C GET PLOT POSITIONS
C
 27       CONTINUE
	  IF (PLTSRC.LT.0.OR.PLTSRC.GT.3) PLTSRC=0	!FIT IN RANGE
	  IF (.NOT.WNDPAR('PLOT_POSITIONS',LOPT,LEN(LOPT),
	1		J0,SRCNAM(PLTSRC))) THEN
	    IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 20           !RETRY FILE
	    GOTO 27
	  END IF
	  IF (J0.LE.0) LOPT='NO'
	  IF (LOPT(1:2).EQ.'NO') PLTSRC=0		!NO   POSITIONS
	  IF (LOPT(1:3).EQ.'YES') PLTSRC=1		!PLOT POSITIONS
	  IF (LOPT(1:3).EQ.'NAM') PLTSRC=2		!ALSO WRITE NAME
	  IF (LOPT(1:3).EQ.'EDI') PLTSRC=3		!AND WANTS TO EDIT
	  IF (PLTSRC.GT.0) THEN				!WANT SOURCES, SO...
	    CALL NMODAX(NSRC)                           !GET SOURCES
	    IF (PLTSRC.GT.1) THEN
		JS=NMONM1()				!INITIALISE NAMELIST
	        IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 27 	!RETRY
	    END IF
	  END IF
C
C GET PLOT POSITIONS
C
 35       CONTINUE
	  IF (.NOT.WNDPAR('PLOT_HEADING',LOPT,LEN(LOPT),
	1		J0,'YES')) THEN
	     LOPT='YES'
	  ELSE IF (J0.LE.0) THEN
	     LOPT='YES'
	  END IF
	  IF (LOPT(1:2).EQ.'NO') PLTHDR=.FALSE.		!NO   HEADING
	  IF (LOPT(1:3).EQ.'YES') PLTHDR=.TRUE.		!PLOT HEADING
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
