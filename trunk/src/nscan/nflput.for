C+ NFLPUT.FOR
C  JEN 931111
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NFLPUT (OPER,USERFLAG,DFAR,
     1                     SHOW_CNT,TRACE)
C
C  PUT flags from the internal flag-list (FLF) into the Scan headers
C  and/or the uv-data inside (a sub-cube of) the specified hypercube. 
C
C  Result:
C
C	CALL NFLPUT (OPER_C(*):I,USERFLAG_J:I,DFAR_J:IO,
C                    SHOW_CNT_L:I,TRACE_L:I)
C
C PIN references:
C
C	PUT_RANGE
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
	INCLUDE 'SCH_O_DEF'		!SCAN HEADER
	INCLUDE 'FLH_O_DEF'		!DELETE FILE HEADER
	INCLUDE 'FLF_O_DEF'		!ENTRY HEADER
C
C  Parameters:
C
	INTEGER XX,XY,YX,YY		!POL. POINTERS
	  PARAMETER (XX=0, XY=1, YX=2, YY=3)
C
	INTEGER NPUTR			!# IN PUT RANGE (chan,ha,ifr,pol)
	  PARAMETER (NPUTR=4)
	INTEGER MXRANG			!MAX. RANGE VALUE
	  PARAMETER (MXRANG=65536*16)
C
C  Arguments:
C  (NB: NODIN, FCAIN and SETS are in common block)
C
        CHARACTER       OPER*(*)    	!SELECTED OPERATION
C
        INTEGER         USERFLAG         	!FLAGBYTE (FOR USER OVERRIDE)
	INTEGER	        DFAR		!FLAG FILE AREA CONTROL PAR
C
	LOGICAL 	SHOW_CNT      	!SHOW FLAG-COUNT AFTER OPS
	LOGICAL		TRACE		!TRACE/DEBUG FLAGGING OPERATION
C   
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
	LOGICAL WNFRD			!READ DATA
	LOGICAL WNFWR			!WRITE DATA
	CHARACTER*32 WNTTSG		!SET NAME
	LOGICAL WNDSTQ			!GET SETS
	INTEGER WNCAJ			!GET INTEGER FROM TEXT
	LOGICAL NSCSTG			!GET A SET
	LOGICAL NSCSCH,NSCSCW		!READ/WRITE SCAN HEADER
	LOGICAL NSCSIF			!READ INTERFEROMETER TABLE
	LOGICAL NSCIFS			!IFR SELECTION
	LOGICAL NSCPLS			!POL SELECTION
	LOGICAL NSCHAS			!HA SELECTION
	LOGICAL NFLFL0,NFLFL1,NFLFL2	!FLAG FILE HANDLING
	LOGICAL NFLFL5,NFLFL6
	LOGICAL NFLFL7,NFLFL8,NFLFL9
	LOGICAL NFLFLS,NFLFLR
	LOGICAL NFLCNT                  !FLAG COUNTING
	LOGICAL NFLCUB                  !UV-DATA HYPER/SUB-CUBE
C
C  Data declarations:
C
C-----------------------------------------------------------------------
C
C  "Static variables"
C
C  Look-up table to find existence and offset for polarisations
C  depending on the number of polarisations present in the data
C
	INTEGER PPOL(XX:YY,1:4,0:1)	!POL. SELECT XX,XY,YX,YY FOR 
					! NPOL=1:4:
C OLD:	  DATA  PPOL/1,0,0,0, 1,0,0,8, 0,0,0,0, 1,2,4,8, !BITS
 	  DATA  PPOL/XX_P,0,0,0, XX_P,0,0,YY_P, 0,0,0,0,
     1	             XX_P,XY_P,YX_P,YY_P,                !BITS
     1	             0,0,0,0, 0,0,0,1, 0,0,0,0, 0,1,2,3/ !OFFSETS
C
C-----------------------------------------------------------------------
C
C   Variables with user-input, defaults and direct derivatives
C
        INTEGER         I6,I7,I8,I9     !LOOP VARIABLES
	INTEGER		NOPER		!CURRENT OPERATION NR (E.G. N_PUT)
        REAL            HA(0:1)         !EXTRA HA-RANGE 
	CHARACTER*8	PUTR(NPUTR)	!PUT RANGE DEFINED BY USER
	INTEGER		JPUTR(0:1,NPUTR) !PUT RANGE (LOW,HIGH)
        LOGICAL         TYPIFR          !ENTRY TYPE IS IFR-TYPE
        INTEGER         IFRMIN,IFRMAX   !MIN, MAX IFR NR (PUT)
        INTEGER         IPOLMIN,IPOLMAX !MIN, MAX POL NR (PUT)
        INTEGER         RTW,RTE         !WEST, EAST TEL NR (PUT)
        INTEGER         RTW1,RTW2       !WEST TEL NRS (PUT)
        INTEGER         RTE1,RTE2       !EAST TEL NRS (PUT)
        INTEGER         RTWMIN,RTWMAX   !MIN, MAX WEST TEL NR (PUT)
        INTEGER         RTEMIN,RTEMAX   !MIN, MAX EAST TEL NR (PUT)
        INTEGER         ICHMIN,ICHMAX   !MIN, MAX FREQU CHANN NR (PUT)
        REAL            HAMIN,HAMAX     !MIN, MAX HA (PUT)
        INTEGER         BAS,BASMIN,BASMAX !BASELINE LENGTH (PUT, M)
	INTEGER         NENT            !ENTRY COUNTER
C
C   Flow control
C
	INTEGER	 	SELFLAG		!SELECTED FLAG(S) TO BE PUT
        INTEGER         FLAG            !FLAGBYTE FROM LIST ENTRY
        INTEGER         FLAGH           !FLAGBYTE FOR SCAN HEADER
        INTEGER         FLAGD           !FLAGBYTE FOR UV-DATUM
C
	LOGICAL		SETFLAG		!FLAG/UNFLAG
        LOGICAL         SETFH
        LOGICAL         SETFD
C
        LOGICAL         MODFH           !MODIFY FLAG(S) IN HEADER
        LOGICAL         MODFD           !MODIFY FLAG(S) IN UV-DATUM
C
	LOGICAL		WRSCH		!REWRITE (MODIFIED) SCAN HEADER
	LOGICAL		WRSCN		!REWRITE (MODIFIED) SCAN DATA
C
C-----------------------------------------------------------------------
C
C  Storage areas, buffer arrays
C
        CHARACTER*80 TXT80              !TEXT BUFFER
C
        REAL SELHA(0:1)                 !SELECTED HA-RANGE (NSCHAS)
        LOGICAL SELPOL(0:3)             !SELECTED POLS 
        BYTE SELIFR(0:STHTEL-1,0:STHTEL-1)  !SELECTED IFRS PER TEL PAIR 
	LOGICAL SELIFR1(0:STHIFR-1)     !SELECTED IFRS PER IFR
C
	INTEGER IFRNR(0:STHTEL-1,0:STHTEL-1)  !IFR NR LOOKUP TABLE
C
        INTEGER FLACC(0:STHIFR-1,0:3)   !FLAG COUNTS 
        INTEGER MASK(0:STHIFR-1,0:3)    !FLAGBYTES USED
C
	INTEGER SNAM(0:7)		!SET NAME
	INTEGER STHP			!SET HEADER POINTER
        BYTE STH(0:STH__L-1)            !SET HEADER
          INTEGER*2 STHI(0:STH__L/LB_I-1)
          INTEGER STHJ(0:STH__L/LB_J-1)
          REAL STHE(0:STH__L/LB_E-1)
          DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
          EQUIVALENCE (STH,STHJ,STHI,STHE,STHD)
        BYTE SCH(0:SCH__L-1)            !SCAN HEADER
          INTEGER*2 SCHI(0:SCH__L/LB_I-1)
          INTEGER SCHJ(0:SCH__L/LB_J-1)
          REAL SCHE(0:SCH__L/LB_E-1)
          EQUIVALENCE (SCH,SCHJ,SCHI,SCHE)
C
	INTEGER*2 IFRT(0:STHIFR-1)	!INTERFEROMETER TABLE
	INTEGER IFRA(0:1,0:STHIFR-1)    !RTWEST(0), RTEAST(1)
        REAL BASEL(0:STHIFR-1)          !BASELINE TABLE (M)
        REAL ANG(0:2,STHIFR-1)          !DIPOLE ANGLE INFORMATION
C
	INTEGER		CHCUR		!CURRENT CHANNEL NR
        REAL		HACUR		!CURRENT HA
C
	INTEGER*2 LDAT(0:2,0:4*STHIFR-1) !DATA
C
        BYTE FLH(0:FLH__L-1)            !FLAG FILE HEADER
          INTEGER FLHJ(0:FLH__L/LB_J-1)
          REAL FLHE(0:FLH__L/LB_E-1)
          EQUIVALENCE (FLH,FLHJ,FLHE)
        BYTE FLF(0:FLF__L-1,2)          !FLAG FILE ENTRIES (RANGE)
          INTEGER*2 FLFI(0:FLF__L/LB_I-1,2)
          INTEGER FLFJ(0:FLF__L/LB_J-1,2)
          REAL FLFE(0:FLF__L/LB_E-1,2)
          EQUIVALENCE (FLF,FLFI,FLFJ,FLFE)
C-
C******************************************************************************
C*****************************************************************************
C*****************************************************************************
C
C Initialise:
C
	SETFLAG = .TRUE.                        !ALWAYS
C
C Make sure that there is a flag-list to be PUT:
C
	  IF (DFAR.EQ.0) THEN    
	    CALL WNCTXT(F_TP,
     1		'No entries in list (use GET/LOAD/READ)')
	    GOTO 700                            !ESCAPE
	  ELSE
	    IF (.NOT.NFLFLS(DFAR,FLH)) GOTO 700	   !GET FLAG-LIST HEADER (FLH)
	  END IF
C
C Only specified flag types are GOT. The default is all flag-types,
C unless the flag-types are explicitly given (USERFLAG>0).
C The user may locally override the flag-types.
C
        CALL WNCTXT (F_TP,'Only PUT flags of the following type(s):')
	SELFLAG=FL_ALL		       !DEFAULT FLAG TYPE(S): ALL TYPES
	IF (USERFLAG.NE.0) SELFLAG=USERFLAG     !OVERRIDE BY USER-DEFINED FLAG TYPE(S)
	CALL WNDDA3 ('SELECT_FLAG',SELFLAG) !ASK THE USER
	IF (SELFLAG.EQ.0) THEN
          CALL WNCTXT (F_TP,'No flag type(s) to be PUT')
          GOTO 700                     !NONE: ESCAPE
        END IF
C
C Get put-range (i.e expand-range) JPUTR, in NPUTR dimensions.
C This allows the user to `convolve' the flags in the flag-list
C with a range in any of the 4 expansion directions. 
C Succession of dimension-indices: chan(1), HA(2), ifr(3), pol(4) 
C For each expansion dimension I, a `left' (JPUTR(0,I)) and `right'
C (JPUTR(1,I)) expansion renge is given. 
C
	DO I=1,NPUTR       !FOR ALL EXPANSION DIMENSIONS
          JPUTR(0,I)=0     !DEFAULT: NO EXPANSION TO THE `LEFT'
	  JPUTR(1,I)=0     !         NOR TO THE `RIGHT'
	END DO
C
C NB: The only meaning-full expansions are in the dimensions:
C     - channel (it may be desirable to expand the flagging of a bad
C       interferometer in one channel to all channels, in order to 
C       have the same uv-coverage for all cannels.    
C Since the old expansion keyword (PUT_RANGE) was not easy to understand,
C it has been replaced with separate keuwords for each expansion dimension.
C
	I4 = 1                                 !DIMENSION IS CHANNELS
	IF (.NOT.WNDPAR('PUT_EXPAND_CH',JPUTR(1,I4),LB_J,
     1		         J0,A_B(-A_OB),JPUTR(1,I4),1)) GOTO 700	        !ESCAPE
	IF (J0.EQ.0) GOTO 700                  !EMPTY STRING: ESCAPE
	IF (J0.LT.0) THEN                      !WILDCARD (*): PUT ALL
          JPUTR(0,I4)=-MXRANG
	  JPUTR(1,I4)=+MXRANG
	ELSE IF (JPUTR(1,I4).GE.0) THEN        !VALID NR OF CHANNELS
          JPUTR(0,I4)=-JPUTR(1,I4)             !SYMMETRIC LEFT/RIGHT
        ELSE                                   !NEGATIVE NR (?)
        END IF
C
        CALL WNCTXT (F_TP,
     1       'NFLPUT: expansion (channels):'
     1       //' !SJ+ich <-> ich+!SJ'
     1       ,JPUTR(0,I4),JPUTR(1,I4))
C
C Select sub-cube of the uv-data hypercube (specified before):
C
	IF (.NOT.NFLCUB('SELECT','HYPERCUBE',0,
     1                  SELHA,SELPOL,SELIFR)) GOTO 700
	IF (.NOT.NFLCUB('SPECIFY','SUBCUBE',0,
     1                  SELHA,SELPOL,SELIFR)) GOTO 700
C
C Adjust the selected HA-range (SELHA) according to the range of HA's
C of all the entries in the flag-list, enlarged with a possible expansion
C PUT-range in the HA-direction (JPUTR). 
C This HA-range is stored in the  flag-list header (FLH)). 
C NB: This can only decrease the selected HA-range, to save processing time.
C     It will NOT extend the selected range beyond the (sub-cube of the) 
C     specified data hypercube.
C 
	HAMIN=-179.99/360                !MINIMUM START HA (CIRCLES)
	HAMAX=+179.99/360                !MAXIMUM STOP HA (CIRCLES)
	IF (FLHJ(FLH_HA_J).EQ.-1) THEN   !ALL HA's AVAILABLE
          CONTINUE
        ELSE
          HAMIN = FLHE(FLH_RHA_E+0)+
     1                     (JPUTR(0,2)-0.5)*STHE(STH_HAI_E)
          HAMAX = FLHE(FLH_RHA_E+1)+
     1                     (JPUTR(1,2)+0.5)*STHE(STH_HAI_E)
        END IF
        SELHA(0) = MAX(HAMIN,SELHA(0))   !MIN SELECTED HA (CIRCLES)
        SELHA(1) = MIN(HAMAX,SELHA(1))   !MAX SELECTED HA (CIRCLES)
C
C*****************************************************************************
C*****************************************************************************
C*****************************************************************************
C ACT ON HYPERCUBE
C
 300    CONTINUE
C
C Reset the flag-count buffers:
C
        JS = NFLCNT ('RESET',' ',0,0,0,0,0)
C   
C Flow control:
C
        SETFLAG = .TRUE.                  !GLOBAL SET/CLEAR SWITCH
C
C Read Set(s) of Sectors:
C  
	DO WHILE (NSCSTG(FCAIN,SETS,STH,STHP,SNAM))          !ALL SECTORS
	  IF (.NOT.NSCSIF(FCAIN,STH,IFRT,IFRA,ANG)) THEN     !READ IFR TABLE
	    CALL WNCTXT(F_TP,'Error reading interferometer table')
	    GOTO 30				             !NEXT SECTOR
	  END IF
C
C Adjust the polarisation selection, to ignore polarisations that may
C not be present in this SCN Sector, using info from the Sector Header:
C
	  IF (.NOT.NFLCUB('ADJUST','SELPOL',STHI(STH_PLN_I),
     1         SELHA,SELPOL,SELIFR)) GOTO 30         !PROBLEM, NEXT SECTOR
C
C Make a quick-lookup table (IFRNR) for ifr-numbers.
C Make a quicker ifr-selection table (SELIFR1).
C
          DO I1=0,STHJ(STH_NIFR_J)-1
            RTW = IFRA(0,I1)                    !WEST TEL NR
            RTE = IFRA(1,I1)                    !EAST TEL NR
            IFRNR(RTW,RTE) = I1                 !IFR NR
            SELIFR1(I1) = SELIFR(RTW,RTE)       
          END DO
C 
!  Check if the channel (CHCUR) of the current Sector is needed (i.e
C  whether it is in the channel range of any of the flag-list entries. 
!  (enlarged with the user-defined put-range).
C  This channel-range is stored in the flag-list header.
C
	  CHCUR=STHI(STH_CHAN_I)    !FREQU CHANNEL NR OF THE CURRENT SECTOR
          ICHMIN = FLHJ(FLH_RCHAN_J+0)+JPUTR(0,1)   !MIN CHANNEL IN LIST
          ICHMAX = FLHJ(FLH_RCHAN_J+1)+JPUTR(1,1)   !MAX CHANNEL IN LIST
	  IF ((FLHJ(FLH_CHAN_J).NE.-1) .AND.
     1		(CHCUR.LT.ICHMIN .OR. CHCUR.GT.ICHMAX)) THEN
            GOTO 30                 !CHANNEL OUT OF RANGE, SKIP SECTOR
          END IF
C
C Get baseline lengths (for baseline-type flag-list entries, if any):
C
	  CALL NSCMBL(STHE(STH_RTP_E),STHJ(STH_NIFR_J),IFRT,
     1		       SELIFR,BASEL)		!GET BASELINES
C
C
C
C******************************************************************************
C ACT ON HA-SCANS:
C
 400      CONTINUE
C
	  DO I=0,STHJ(STH_SCN_J)-1		                !ALL SCANS
	    HACUR=STHE(STH_HAB_E)+I*STHE(STH_HAI_E)             !HA OF SCAN
            IF (HACUR.GE.(SELHA(0)-STHE(STH_HAI_E)/2+1E-5) .AND.
     1		HACUR.LE.(SELHA(1)+STHE(STH_HAI_E)/2-1E-5)) THEN  !SELECTED
C
	      IF (.NOT.NSCSCH(FCAIN,STH,IFRT,I,0,0,SCH)) THEN !READ HEADER
		CALL WNCTXT(F_TP,'Error reading scan header !UJ',I)
		GOTO 30
              END IF
              WRSCH = .FALSE.                  !NO REWRITE SCAN HEADER
C
              IF (.NOT.WNFRD(FCAIN,STHJ(STH_SCNL_J)-SCH__L,
     1            LDAT,STHJ(STH_SCNP_J)+
     1            I*STHJ(STH_SCNL_J)+SCH__L)) THEN             !READ SCAN DATA
		CALL WNCTXT(F_TP,'Error reading scan !UJ',I)
		GOTO 700
              END IF
              WRSCN = .FALSE.                  !NO REWRITE SCAN DATA
C
C Flag-count accumulator for newly PUT uv-data. To save time, all new
C flags (i.e. from all flag-list entries) PUT to the uv-data in this 
C Scan are accumulated by means of licical OR-operations, and counted
C before going to the next Scan. 
C
              DO I1=0,STHIFR-1
                DO I3=0,3
                  FLACC(I1,I3) = 0     !FLAG-COUNT 
                  MASK(I1,I3) = 0
                END DO
              END DO
C
C*****************************************************************************
C*****************************************************************************
! Go through entries of the flag-list. 
C
              JS=NFLFLR(DFAR)			!MAKE SURE BEGIN LIST
              NENT = 0                              !COUNTER
              DO WHILE(NFLFL2(DFAR,FLF(0,1),FLF(0,2))) !READ ALL ENTRIES
                NENT = NENT+1
C
C Check whether the entry's flag contains any of the selected flag-types:
C
	        FLAG = IAND(FL_ALL,FLFJ(FLF_FLAG_J,1))  !COPY ENTRY FLAG
                IF (IAND(FLAG,SELFLAG).EQ.0) GOTO 80  !SELECTED FLAG TYPES ONLY
                FLAGH = FLAG                        !HEADER FLAG (if required) 
                FLAGD = FLAG                        !DATA FLAG (if required)
C
C Check whether the entry's channel range (extended with the user-defined
C PUT-range) covers the current channel:
C
                ICHMIN = FLFJ(FLF_CHAN_J,1)+JPUTR(0,1) !MIN CHANNEL NR
                ICHMAX = FLFJ(FLF_CHAN_J,2)+JPUTR(1,1) !MAX CHANNEL NR 
		IF (FLFJ(FLF_CHAN_J,1).NE.-1) THEN     !NOT ALL CHANNELS
                  IF (CHCUR.LT.ICHMIN) GOTO 80         !OUT OF RANGE
                  IF (CHCUR.GT.ICHMAX) GOTO 80         !OUT OF RANGE
		END IF
C
C Check whether the entry's HA-range (extended with the user-defined
C PUT-range) covers the current HA:
C
                HAMIN = FLFE(FLF_HA_E,1)+
     1                       (JPUTR(0,2)-0.5)*STHE(STH_HAI_E) !MIN HA 
                HAMAX = FLFE(FLF_HA_E,2)+
     1                       (JPUTR(1,2)+0.5)*STHE(STH_HAI_E) !MAX HA
		IF (FLFE(FLF_HA_E,1).NE.-1) THEN       !NOT ALL HA's
                  IF (HACUR.LT.HAMIN) GOTO 80          !OUT OF RANGE
                  IF (HACUR.GT.HAMAX) GOTO 80          !OUT OF RANGE
		END IF
C
C Debugging message (if required):
C
                IF (TRACE) THEN
                    CALL WNCTXT (F_T,'  HA=!8$EAF6.2:'
     1                      //'  ENTRY NR !UJ'
     1                      ,HACUR,NENT)
                END IF
C
C******************************************************************************
C******************************************************************************
C ACT ON SCAN HEADER (if required):
C
C  If the entry specifies all ifrs (*) and all pols (*), then put flag(s) 
C  in the Scan header:
C
                  IF ((FLFI(FLF_IFR_I,1).EQ.-1) .AND.
     1                (FLFI(FLF_POL_I,1).EQ.-1)) THEN    !ALL POLS AND IFRS
                    MODFH = .TRUE.
C
C MODIFY SCAN HEADER FLAGS (IF REQUIRED):
C
	            IF (MODFH) THEN
                      SETFH = .TRUE.                     !ALWAYS FOR PUT....
                      IF (SETFH) THEN	                 !SET FLAG
	                SCHJ(SCH_BITS_J)=
     1                          IOR(SCHJ(SCH_BITS_J),FLAGH)
	              ELSE               !CLEAR FLAG(S) (relevant for PUT?)
	                SCHJ(SCH_BITS_J)=
     1                          IAND(SCHJ(SCH_BITS_J),NOT(FLAGH))
	              END IF
                      WRSCH = .TRUE.    !WRITE BACK THE MODIFIED SCAN HEADER
C
C COUNT THE PUT FLAGS (i.e. those from the flag-list only, and not the ones 
C that were already set in the Scan header!):
C
                      JS = NFLCNT ('ACC','HEAD',FLAGH,SELFLAG,
     1                                 IFRA,CHCUR,HACUR) 
	            END IF
C
C NB: It would not be correct to escape from the loop here and to go to the
C     next Scan, since there might be other entries in the flag-list that
C     set other flags in the Scan header (and the uv-data!!!??). 
C     This issue requires a little thought.....
C 
C
C
C
C******************************************************************************
C******************************************************************************
C ELSE, ACT ON UV-DATA:
C
                  ELSE
C
! To save time, determine the range of ifrs (IFRMIN-IFRMAX) and pols 
! (IPOLMIN-IPOLMAX) of the uv-data in which flags are to be set according 
! to the current flag-list entry.
! NB: The entry can be of `ifr-type' (TYPIFR=.TRUE.) or `baseline-type'
!     (TYPIFR=.FALSE.). In the latter case, its `ifr-range' indicates a range 
!     of baseline lengths (m) for which flags have to be set.  
C
                    TYPIFR = .TRUE.                    !IFR-TYPE ENTRY
                    IF (IAND(FLFJ(FLF_FLAG_J,1),'01000000'X).NE.0) 
     1                         TYPIFR = .FALSE.        !BASEL-TYPE ENTRY
C
                    IFRMIN = 0                         !MIN IFR NR
                    IFRMAX = STHIFR-1                  !MAX 
                    BASMIN = 0                         !MIN BASLENGTH (M)
                    BASMAX = 10000                     !MAX 
                    IF (FLFI(FLF_IFR_I,1).EQ.-1) THEN  !ALL IFRS/BAS
                      CONTINUE
                    ELSE
                      IF (TYPIFR) THEN                 !IFR-TYPE ENTRY
     	                RTW = MOD(FLFI(FLF_IFR_I,1),256) !WEST TEL
     			RTE = FLFI(FLF_IFR_I,1)/256      !EAST TEL
                        IFRMIN = MAX(IFRMIN,IFRNR(RTW,RTE)+JPUTR(0,3))
     	                RTW = MOD(FLFI(FLF_IFR_I,2),256) !WEST TEL
     			RTE = FLFI(FLF_IFR_I,2)/256      !EAST TEL
                        IFRMAX = MIN(IFRMAX,IFRNR(RTW,RTE)+JPUTR(1,3))
                      ELSE                             !BASEL-TYPE ENTRY
                        BASMIN = MAX(BASMIN,
     1                               FLFI(FLF_IFR_I,1)+JPUTR(0,3))
                        BASMAX = MIN(BASMAX,
     1                               FLFI(FLF_IFR_I,2)+JPUTR(1,3))
                      END IF                           !
                    END IF
C
                    IPOLMIN = 0                        !MIN POL NR
                    IPOLMAX = 3                        !MAX
                    IF (FLFI(FLF_POL_I,1).EQ.-1) THEN  !ALL POLS
                      CONTINUE
                    ELSE
                      IPOLMIN = MAX(0,FLFI(FLF_POL_I,1)+JPUTR(0,4))
                      IPOLMAX = MIN(3,FLFI(FLF_POL_I,2)+JPUTR(1,4))
                    END IF
C
                    IF (TRACE) THEN
	              CALL WNCTXT (F_T,'   '
     1                     //' IFR12=!SJ:!SJ  POL12=!SJ:!SJ'
     1                     //' TYPIFR=!LJ'
     1                     ,IFRMIN,IFRMAX,IPOLMIN,IPOLMAX,TYPIFR)
                    END IF
C
C GO THROUGH THE RELEVANT UV-DATA FOR THIS FLAG-LIST ENTRY:
C
		    DO I1=IFRMIN,IFRMAX	               !IFRS
                      IF (SELIFR1(I1)) THEN            !SELECTED IFR
                        MODFD = .FALSE.
                        IF (TYPIFR) THEN               !IFR-TYPE ENTRY
                          MODFD = .TRUE.               !ALWAYS
                        ELSE                           !BASEL-TYPE ENTRY
                          BAS = NINT(BASEL(I1))        !BASELINE LENGTH (M)
                          IF (BAS.GE.BASMIN .AND.
     1                        BAS.LE.BASMAX) MODFD=.TRUE. !BAS IN RANGE
                        END IF
C                            
                        IF (MODFD) THEN                 !MODIFY FLAG
		          I2=STHI(STH_PLN_I)*I1	        !DATA POINTER
		          DO I3=IPOLMIN,IPOLMAX	        !POLS
                            IF (SELPOL(I3)) THEN        !SELECTED POL
C
		              I4=I2+PPOL(I3,STHI(STH_PLN_I),1)   !OFFSET
		              I5=LDAT(0,I4)             !WEIGHT/FLAGS
		              I5=IAND('0000ffff'X,I5)   !WEIGHT/FLAGS 
		              IF (I5.NE.0) THEN   !DATA PRESENT (WEIGHT<>0)
C
                                SETFD = .TRUE.          !ALWAYS FOR PUT.....
			        IF (SETFD) THEN         !SET FLAG 
			          I5=IOR(I5,FLAGD)
			        ELSE	                !CLEAR FLAG (relevant?)
			          I5=IAND(I5,NOT(FLAGD))
			        END IF
		                IF (IAND(I5,'00008000'X).NE.0) THEN
    			           I5=IOR(I5,'ffff0000'X)
                                END IF
		                LDAT(0,I4)=I5   !MODIFIED DATA FLAGBYTE
		                WRSCN=.TRUE.    !REWRITE SCAN DATA (below)
C
                                FLACC(I1,I3) = IOR(FLAGD,FLACC(I1,I3))
                                MASK(I1,I3) = IOR(FLAGD,MASK(I1,I3))  !?
C
			      END IF		        !DATA PRESENT
		            END IF			!SELPOL
		          END DO			!POLS (I3)
                        END IF                          !MODFD
C
		      END IF			        !SELIFR1
		    END DO				!IFRS (I1)
C
                  END IF                         !HEADER OR DATA
 80		CONTINUE                         !IF OUT OF HA/CHAN RANGE 
              END DO                             !FLAG-LIST ENTRIES
C
C REWRITE SCAN DATA (IF REQUIRED):
C
	      IF (WRSCN) THEN			!REWRITE SCAN
	        IF (.NOT.WNFWR(FCAIN,STHJ(STH_SCNL_J)-SCH__L,
     1                LDAT,STHJ(STH_SCNP_J)+I*STHJ(STH_SCNL_J)+
     1                SCH__L)) THEN !WRITE DATA
	          CALL WNCTXT(F_TP,'Error writing scan !UJ',I)
		  GOTO 30			!NEXT SET
	        END IF
	      END IF
C
C WRITE BACK THE SCAN HEADER (If required):
C
              IF (WRSCH) THEN
	        IF (.NOT.NSCSCW(FCAIN,STH,IFRT,I,0,0,SCH)) THEN 
	          CALL WNCTXT(F_TP,'Error writing scan header !UJ',I)
	 	  GOTO 30
	        END IF
              END IF
C
C ADD THE NEWLY PUT DATA-FLAGS TO THE ACCUMULATOR-BUFFER:
C
              JS = NFLCNT ('ACC','DATA',FLACC,MASK,IFRA,CHCUR,HACUR)
C
C NEXT SCAN
C
	    END IF				!SELHA
	  END DO				!NEXT SCAN
C
C****************************************************************************
C****************************************************************************
C
C NEXT SECTOR (if any):
C
 30	  CONTINUE
	END DO					!NEXT SECTOR
C
C**************************************************************************
C**************************************************************************
C END OF (OR ESCAPE FROM) OPERATION: 
C
 700    CONTINUE
C
C  Display a summary of flags, if required:
C
	SHOW_CNT = .TRUE.                       !Temporary
        IF (SHOW_CNT) THEN
          JS = NFLCNT ('SHOW','FTYP',0,SELFLAG,0,0,0)
          CALL WNCTXT (F_T,'NB: Tested and counted are ONLY'
     1       //' those flags that were PUT to the data/headers') 
          CALL WNCTXT (F_T,'by the last operation.' 
     1       //' Use INSPECT for a closer look.')
	  CALL WNCTXT (F_T,' ')
        END IF
C
        GOTO 800                                !BACK TO OPS_FLIST            
C
C**************************************************************************
C**************************************************************************
C READY
C
 800	CONTINUE
	RETURN                                  !BACK TO FLAG_OPTION 
	END
C
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  Pieces of controversial code, kept stored here pending a discussion
C  with CMV about the precise meaning of PUTting ifr-type entries,
C  i.e. if TYPIFR = .TRUE.
C
C  Calculation of IFRMIN,IFRMAX:
C
C     	                    RTW1 = MOD(FLFI(FLF_IFR_I,1),256) !WEST TEL
C     	                    RTW2 = MOD(FLFI(FLF_IFR_I,2),256) !WEST TEL
C     			    RTE1 = FLFI(FLF_IFR_I,1)/256      !EAST TEL
C     			    RTE2 = FLFI(FLF_IFR_I,2)/256      !EAST TEL
C     	                    RTWMIN = MIN(RTW1,RTW2)+JPUTR(0,3) 
C     	                    RTWMAX = MAX(RTW1,RTW2)+JPUTR(1,3) 
C     			    RTEMIN = RTE1+JPUTR(0,3) 
C                            RTEMAX = RTE2+JPUTR(1,3) 
C                            IFRMIN = STHJ(STH_NIFR_J)-1    !
C                            IFRMAX = 0                     !
C                            DO I1 = 0,STHJ(STH_NIFR_J)-1   
C                              RTW = IFRA(0,I1)             !WEST TEL
C                              RTE = IFRA(1,I1)             !EAST TEL
C                              IF (RTW.GE.RTWMIN .AND.
C     1                            RTW.LE.RTWMAX .AND.
C     1                            RTE.GE.RTEMIN .AND.
C     1                            RTE.LE.RTEMAX) THEN
C                                IFRMIN = MIN(IFRMIN,I1)    !MIN IFR NR
C                                IFRMAX = MAX(IFRMAX,I1)    !MAX IFR NR
C                              END IF
C                            END DO 
C
C-----------------------------------------------------------------------------
C inside ifr-loop (I1)
C                              RTW = IFRA(0,I1)             !WEST TEL
C                              RTE = IFRA(1,I1)             !EAST TEL
C                              IF (RTW.GE.RTWMIN .AND.
C     1                            RTW.LE.RTWMAX .AND. 
C     1                            RTE.GE.RTEMIN .AND. 
C     1                            RTE.LE.RTEMAX) MODFD=.TRUE. 
C
C-----------------------------------------------------------------------------
C Old PUT-RANGE code, kept in case we would want to revive it (unlikely).
C
C Get put-range (JPUTR), in NPUTR dimensions.
C Succession of dimension-indices: chan(1), HA(2), ifr(3), pol(4) 
C
CC	    IF (.NOT.WNDPAR('PUT_RANGE',PUTR,LEN(PUTR(1))*NPUTR,
CC     1		             J0,'.,.,.,.')) GOTO 700	!ESCAPE
CC	    IF (J0.EQ.0) GOTO 700                  !EMPTY STRING: ESCAPE
CC	    IF (J0.LT.0) THEN                      !WILDCARD (*): PUT ALL
CC	      DO I=1,NPUTR
CC		PUTR(I)='.'                        !PUT-RANGE IS 1 ??
C NB: This use is inconsistent with the usual meaning of the wildcard (*)!!!
CC	      END DO
CC	    END IF
C
CC	    DO I=1,NPUTR
CC	      IF (PUTR(I).EQ.'.') THEN
CC		JPUTR(0,I)=0
CC		JPUTR(1,I)=0
CC	      ELSE IF (PUTR(I).EQ.'*') THEN
CC		JPUTR(0,I)=-MXRANG
CC		JPUTR(1,I)=+MXRANG
CC	      ELSE
CC		I1=0					!POINTER
CC		J=WNCAJ(PUTR(I),LEN(PUTR(I)),I1)	!GET VALUE
CC		IF (J.LE.0) THEN
CC		  JPUTR(0,I)=0
CC		  JPUTR(1,I)=-1
CC		ELSE IF (PUTR(I)(I1+1:).EQ.' ' .OR.
CC     1		    PUTR(I)(I1+1:I1+1).EQ.'C' .OR.
CC     1		    PUTR(I)(I1+1:I1+1).EQ.'c') THEN
CC		  JPUTR(0,I)=-(J/2)
CC		  JPUTR(1,I)=(J-1)/2
CC		ELSE IF (PUTR(I)(I1+1:I1+1).EQ.'L' .OR.
CC     1		    PUTR(I)(I1+1:I1+1).EQ.'l') THEN
CC		  JPUTR(0,I)=-J+1
CC		  JPUTR(1,I)=0
CC		ELSE IF (PUTR(I)(I1+1:I1+1).EQ.'R' .OR.
CC     1		    PUTR(I)(I1+1:I1+1).EQ.'r') THEN
CC		  JPUTR(0,I)=0
CC		  JPUTR(1,I)=J-1
CC		ELSE
CC		  CALL WNCTXT(F_TP,'PUT_RANGE format error: !AS',
CC     1			PUTR(I))
CC		  GOTO 700
CC		END IF
CC	      END IF
C
C------------------------------------------------------------------------

