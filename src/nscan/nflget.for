C+ NFLGET.FOR
C  JEN 931111
C
C    Updates:
C	940415 CMV	Correct bug on HP (L = IAND does not work)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NFLGET (OPER,USERFLAG,DFAR,
     1                     SHOW_CNT,TRACE)
C
C  Make an internal flag list (FLF) from the (specified, USERFLAG) flags that
C  are set in the Scan headers and/or the uv-data in the specified hypercube.  
C
C  Result:
C
C	CALL NFLGET (OPER_C(*):I,USERFLAG_J:I,DFAR_J:IO
C                    SHOW_CNT_L:I,TRACE_L:I)
C
C PIN references:
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
	REAL VERYLARGE			!VERY LARGE VALUE
	  PARAMETER (VERYLARGE = 1.E38)
C
	INTEGER MXNFLTYP
          PARAMETER (MXNFLTYP=8)
C
C  Arguments:
C  (NB: NODIN, FCAIN and SETS are in common block)
C
        CHARACTER       OPER*(*)      	!SELECTED OPERATION
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
	LOGICAL NFLFL0,NFLFL1	        !FLAG FILE HANDLING
	LOGICAL NFLCUB                  !DATA SUB/HYPERCUBE
	LOGICAL NFLCNT                  !FLAG COUNTING
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
	INTEGER FLAGTYPE(0:MXNFLTYP-1)
          DATA FLAGTYPE /FL_MAN,FL_CLIP,FL_NOIS,FL_ADD,
     1                   FL_SHAD,FL_3,FL_2,FL_1/
C-----------------------------------------------------------------------
C
C   Variables with user-input, defaults and direct derivatives
C
        INTEGER         I6,I7,I8,I9     !LOOP VARIABLES
	INTEGER		NOPER		!CURRENT OPERATION NR 
	INTEGER		CHCUR		!CURRENT CHANNEL
        REAL            HAMIN,HAMAX     !HA-range of list-entry
        REAL		HACUR,HANEXT	!CURRENT AND NEXT HA
	REAL            HAMARGIN        !HA MARGIN
        LOGICAL         TYPIFR          !ENTRY TYPE IS IFR-TYPE
	INTEGER	 	SELFLAG		!GLOBAL FLAGBYTE TO BE USED
	INTEGER		FLAGH,FLAGD     !HEADER/DATA FLAGBYTE
	INTEGER		EFLAG		!FLAGBYTE FOR LIST ENTRY
	INTEGER         RTW,RTE         !WEST,EAST TEL NR
        LOGICAL         LASTSCAN        !INDICATES LAST SCAN IN SECTOR
	INTEGER         NENT            !NR OF FLF ENTRIES (1 OR 2)
	INTEGER*2       NPOLS           !Pol counter
C
	LOGICAL         SELPOL(0:3)     !POL. SELECTION  
	BYTE  SELIFR(0:STHTEL-1,0:STHTEL-1) !IFR SELECTION 
	REAL            SELHA(0:1)	!HA-RANGE SELECTION
C
C-----------------------------------------------------------------------
C
C  Storage areas, buffer arrays
C
	REAL HASERH (0:1,0:MXNFLTYP-1)   !GET HA-series accum. array (headers)
	REAL HASERD (0:1,0:MXNFLTYP-1,-1:3,0:STHIFR-1)  !idem for data
	LOGICAL FLAGGED(0:3,0:MXNFLTYP-1)            !Indicator switches
	LOGICAL NODATA(0:3)                          !Indicator switches
C
	INTEGER FLACC(0:STHIFR-1,0:3)   !FLAG COUNTS
	INTEGER MASK(0:STHIFR-1,0:3)    !FLAG COUNT MASKS
C
        CHARACTER*80 TXT80              !TEXT BUFFER
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
        REAL ANG(0:2,STHIFR-1)          !DIPOLE ANGLE INFORMATION
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
C Only specified flag types are GOT. The default is all flag-types,
C unless the flag-types are explicitly given (USERFLAG>0).
C The user may locally override the flag-types.
C
	CALL WNCTXT (F_TP,'Only GET flags of the following type(s):')
	IF (TRACE) CALL WNCTXT (F_T,'NFLGET:  USERFLAG=!SJ',USERFLAG)
	SELFLAG=FL_ALL		       !DEFAULT FLAG TYPE(S): ALL TYPES
	IF (USERFLAG.NE.0) SELFLAG=USERFLAG !OVERRIDE BY USER-DEFINED FLAG TYPE(S)
	CALL WNDDA3 ('USER_FLAG',SELFLAG) !ASK THE USER
	IF (SELFLAG.EQ.0) THEN            !NONE: ESCAPE
	  CALL WNCTXT (F_TP,'No flag type(s) to be GOT.')
	  GOTO 700                     !ESCAPE
	END IF
C
C Select sub-cube of the uv-data hypercube (specified before):
C
	IF (.NOT.NFLCUB('SELECT','HYPERCUBE',0,
     1                  SELHA,SELPOL,SELIFR)) GOTO 700
	IF (.NOT.NFLCUB('SPECIFY','SUBCUBE',0,
     1                  SELHA,SELPOL,SELIFR)) GOTO 700
C
C Reset the flag count buffers:
C
	JS = NFLCNT ('RESET',' ',0,0,0,0)
C
C*****************************************************************************
C Read Set(s) of Sectors:
C  
	DO WHILE (NSCSTG(FCAIN,SETS,STH,STHP,SNAM)) !ALL SETS
	  IF (.NOT.NSCSIF(FCAIN,STH,IFRT,IFRA,ANG)) THEN !READ IFR TABLE
	    CALL WNCTXT(F_TP,'Error reading interferometer table')
	    GOTO 30				!PROBLEM, NEXT SECTOR
	  END IF
C
	  CHCUR=STHI(STH_CHAN_I)		!CURRENT CHANNEL NR
C
	  IF (.NOT.NFLCUB('ADJUST','SELPOL',STHI(STH_PLN_I),
     1         SELHA,SELPOL,SELIFR)) GOTO 30    !PROBLEM, NEXT SECTOR
C
C Initialise the accumulator arrays for HA-series:
C
          DO I4=0,MXNFLTYP-1                      !FLAG-TYPES
            HASERH(0,I4) = VERYLARGE              !DISABLE HAMIN
            HASERH(1,I4) = VERYLARGE              !DISABLE HAMAX
            DO I1=0,STHIFR-1                      !IFRS
              DO I3=-1,3                          !POLS
                HASERD(0,I4,I3,I1) = VERYLARGE    !DISABLE HAMIN
                HASERD(1,I4,I3,I1) = VERYLARGE    !DISABLE HAMAX
              END DO
            END DO
          END DO
C
C******************************************************************************
C ACT ON HA-SCANS (if required):
C
          LASTSCAN = .FALSE.                    !IF TRUE, FLUSH ACCUMULATOR
C
	  DO I=0,STHJ(STH_SCN_J)-1		           !ALL SCANS
	    HACUR=STHE(STH_HAB_E)+I*STHE(STH_HAI_E)        !HA OF SCAN
	    HANEXT=HACUR+STHE(STH_HAI_E)                   !HA OF NEXT SCAN
            HAMARGIN = STHE(STH_HAI_E)/2-1E-5
            IF ((HANEXT-HAMARGIN).GT.SELHA(1)) LASTSCAN=.TRUE. 
	    IF (I.GE.STHJ(STH_SCN_J)-1) LASTSCAN = .TRUE.
	    IF (HACUR.GE.(SELHA(0)-HAMARGIN).AND. 
     1          HACUR.LE.(SELHA(1)+HAMARGIN)) THEN         !SCAN IN HYPERCUBE 
C
              IF (.NOT.NSCSCH(FCAIN,STH,IFRT,I,0,0,SCH)) THEN !READ HEADER
		CALL WNCTXT(F_TP,'Error reading scan header !UJ',I)
		GOTO 30
              END IF
C
	      DO I1=0,STHIFR-1
	        DO I3=0,3
                  FLACC(I1,I3) = 0                   !FLAG COUNTERS
	          MASK(I1,I3) = 0
                END DO
              END DO
C
C=======================================================================
C
! If the specified flags types are set in the Scan header, 
! write an entry in the flag-list with ifrs=* (all) and pols=* (all).
C
		FLAGH = SCHJ(SCH_BITS_J)                !HEADER FLAGBYTE
    		EFLAG = IAND(SELFLAG,FLAGH)             !SELECTED FLAG(S) ONLY
		IF (EFLAG.NE.0) THEN                    !HEADER FLAG(S) SET
C
                  DO I4=0,MXNFLTYP-1                      !ALL FLAG-TYPES
                    IF (IAND(EFLAG,FLAGTYPE(I4)).NE.0) THEN !FLAG-TYPE PRESENT
		      HASERH(1,I4) = HACUR                !UPDATE HAMAX
                      IF (HASERH(0,I4).GT.HACUR) THEN     !NEW SERIES
                        HASERH(0,I4) = HACUR              !HAMIN OF NEW SERIES
		      END IF
                    END IF
                  END DO
C
	          JS = NFLCNT ('ACC','HEAD',EFLAG,SELFLAG,
     1                         IFRA,CHCUR,HACUR)        !COUNT ENTRY FLAGS
C
C  If a Scan is flagged, all its data are flagged. This means that
C  it is not necessary to interrupt any `active' flag-series for individual
C  ifrs. This keeps the nr of flag-list entries to a minimum.
C
		  DO I4=0,MXNFLTYP-1                      !ALL FLAG_TYPES
	            IF (IAND(FLAGTYPE(I4),EFLAG).NE.0) THEN !ENTRY-TYPES ONLY!?
		      DO I1=0,STHIFR-1                    !ALL IFRS
		        DO I3=-1,3                        !ALL POLS (INCL -1)
                          HAMAX = HASERD(1,I4,I3,I1)      !LAST HA OF SERIES 
		          IF (HAMAX.LE.HACUR) THEN        !ACTIVE SERIES
                            HASERD(1,I4,I3,I1) = HACUR    !CONTINUE IT
			  END IF
		        END DO
		      END DO
                    END IF
		  END DO
C
C=======================================================================
C
! Else, go through the (hypercube) uv-data in this Scan, and generate
! a separate list-entry for each data-point for which flags of
! the specified type(s) have been set:
C
                ELSE
		  IF (.NOT.WNFRD(FCAIN,STHJ(STH_SCNL_J)-SCH__L,
     1                LDAT,STHJ(STH_SCNP_J)+I*STHJ(STH_SCNL_J)+
     1                SCH__L)) THEN                      !READ SCAN DATA
		    CALL WNCTXT(F_TP,'Error reading scan !UJ',I)
		    GOTO 700
		  END IF
C
		  DO I1=0,STHJ(STH_NIFR_J)-1	         !ALL IFRS
		    RTW = MOD(IFRT(I1),256)              !WEST TEL
                    RTE = IFRT(I1)/256                   !EAST TEL
		    IF (SELIFR(RTW,RTE)) THEN            !SELECTED IFR
		      I2=STHI(STH_PLN_I)*I1	         !DATA POINTER
		      DO I3=0,3			         !ALL POLS
                        NODATA(I3) = .FALSE.             !RESET INDICATORS
                        DO I4=0,MXNFLTYP-1
                          FLAGGED(I3,I4) = .FALSE.       !RESET INDICATORS
                        END DO
                        IF (SELPOL(I3)) THEN             !SELECTED POL
		          I4=I2+PPOL(I3,STHI(STH_PLN_I),1)   !OFFSET
		          I5=LDAT(0,I4)                      !WEIGHT/FLAGS
		          I5=IAND('0000ffff'X,I5)            !WEIGHT/FLAGS 
		          IF (I5.NE.0) THEN   !DATA PRESENT (NON-ZERO WEIGHT)
C
                            I5 = IAND(I5,SELFLAG)            !SPECIFIED FLAGS ONLY
			    IF (I5.NE.0) THEN             !SPEC. FLAG(S) SET
                              DO I4=0,MXNFLTYP-1          !ALL FLAG-TYPES
                                FLAGGED(I3,I4) = 
	1			  (IAND(I5,FLAGTYPE(I4)).NE.0)
                              END DO                      
			      FLACC(I1,I3) = IAND(SELFLAG,I5)  !COUNT ENTRY FLAGS
			      MASK(I1,I3) = SELFLAG            !MASK USED FOR THIS
                            END IF
C
			  ELSE
                            NODATA(I3) = .TRUE.               !NO DATA PRESENT
			  END IF		              !
		        END IF		                      !POL SELECTED
		      END DO			              !POLS (I3)
C
C  If no data, consider the point flagged for `active' flag-series.
C  This will prevent an active series to be broken by absent data.
C  NB: This works for flag-series for individual ifrs/pols, but not yet
C      for ifrs for which all pols (*) are flagged.
C
		      DO I3=0,3                         !ALL POLS
                        IF (NODATA(I3)) THEN            !NO DATA PRESENT
		          DO I4=0,MXNFLTYP-1            !ALL FLAG_TYPES
                            HAMAX = HASERD(1,I4,I3,I1)     !LAST HA IN SERIES 
		            IF (HAMAX.LE.HACUR) THEN    !IF `ACTIVE' THEN 
                              FLAGGED(I3,I4) = .TRUE.   !CONSIDER IT FLAGGED
			    END IF
		          END DO
                        END IF
		      END DO
C
C If, for the current ifr (I1), one of the specified flagtypes is set for 
C a particular pol (I3) and flagtype(I4), indicate this by setting the
C maximum of the HA-range to the HA of the current Scan (HACUR).
C If the minimum of this HA-range is greater than HACUR, this indicates that
C this is a new HA-range: So the minimum HA is set equal to HACUR also.
C NB: If all 4 pols are flagged (NPOLS=4), use the special slot I3=-1
C
                      DO I4=0,MXNFLTYP-1               !FLAG-TYPES
                        NPOLS = 0
                        DO I3=0,3
                          IF (FLAGGED(I3,I4)) NPOLS=NPOLS+1
                        END DO
                        IF (NPOLS.EQ.STHI(STH_PLN_I)) THEN    !ALL POLS FLAGGED
                          HASERD(1,I4,-1,I1) = HACUR      !NEW HAMAX for POL=-1
                          IF (HASERD(0,I4,-1,I1).GT.HACUR) THEN
                            HASERD(0,I4,-1,I1) = HACUR    !NEW HAMIN for POL=-1
                          END IF
                        ELSE
                          DO I3=0,3
                            IF (FLAGGED(I3,I4)) THEN
                              HASERD(1,I4,I3,I1) = HACUR   !NEW HAMAX FOR POL=I3 
                              IF (HASERD(0,I4,I3,I1).GT.HACUR) THEN
                                HASERD(0,I4,I3,I1) = HACUR !NEW HAMIN FOR POL=I3
                              END IF
                            END IF
                          END DO
                        ENDIF
                      END DO
C
C NEXT IFR
C
		    END IF			       !IFR SELECTED
		  END DO			       !IFRS (I1)

C
C*****************************************************************************
C
C FINISH THE SCAN
C
                END IF                                 !HEADER/DATA
C
C For each flagtype (I4), check whether a continuously flagged HA-series 
C is `broken' by an unflagged Scan or uv-point. 
C This is the case if the maximum HA (HAMAX) of such a HA-series 
C is smaller than the HA of the current Scan (HACUR).
C If so, generate new FLF entry in the flag-list for this HA-range.
C NB: A flagged uv-area is described by two FLF entries (NENT=2),
C     while a single flagged uv-`point' (which may contain wildcards)
C     is described by a single FLF entry (NENT=1). 
C
C First the Scan headers:
C
                DO I4=0,MXNFLTYP-1
                  HAMAX = HASERH(1,I4)                !MAX HA OF SERIES
                  IF (HAMAX.LE.HACUR) THEN            !VALID SERIES
                    IF (LASTSCAN.OR.(HAMAX.LT.HACUR)) THEN  !SERIES ENDED
C
		      FLFJ(FLF_FLAG_J,1)=FLAGTYPE(I4) !FLAGBYTE
		      FLFJ(FLF_FLAG_J,2)=FLAGTYPE(I4) !FLAGBYTE
		      FLFE(FLF_HA_E,1)=HASERH(0,I4)   !MIN HA
		      FLFE(FLF_HA_E,2)=HASERH(1,I4)   !MAX HA
	              FLFJ(FLF_CHAN_J,1)=CHCUR	  !MIN CHANNEL NR
	              FLFJ(FLF_CHAN_J,2)=CHCUR	  !MAX CHANNEL NR
	              FLFI(FLF_IFR_I,1)=-1		  !ALL IFRS
	              FLFI(FLF_POL_I,1)=-1		  !ALL POLS
C
                      NENT = 1                        !1 FLF ENTRY
                      HAMIN = HASERH(0,I4)            !MIN HA OF SERIES
                      IF (HAMAX.GT.HAMIN) NENT=2      !2 FLF ENTRIES
                      FLFJ(FLF_FLAG_J,1) = 
     1                     IOR(FLFJ(FLF_FLAG_J,1),NENT-1) 
                      FLFJ(FLF_FLAG_J,2) = 
     1                     IOR(FLFJ(FLF_FLAG_J,2),2) 
                      DO I7=1,NENT                     !1 OR 2 FLF ENTRIES
                        IF (TRACE) THEN
		          CALL WNCTXT (F_T,' NFLGET: FLF scan: '
     1                       //'flag=!UJ  HA=!F7.2 - !F7.2'
     1                       ,FLFJ(FLF_FLAG_J,1)
     1                       ,FLFE(FLF_HA_E,1)*360
     1                       ,FLFE(FLF_HA_E,2)*360)
                        END IF
		      	IF (.NOT.NFLFL1(DFAR,FLF(0,I7))) THEN
		          CALL WNCTXT(F_TP,'Error writing FLF entry')
		          GOTO 700	                   !PROBLEM, ESCAPE
                        END IF
                      END DO
C
	              HASERH(0,I4) = VERYLARGE        !DEACTIVATE SERIES
		      HASERH(1,I4) = VERYLARGE        !
		    END IF
                  END IF
                END DO                                  !FLAG-TYPES
C
C Then the uv-data:
C
                DO I1=0,STHIFR-1                             !ALL IFRS
                  DO I3=-1,3                                 !POLS
                    DO I4=0,MXNFLTYP-1                       !FLAG-TYPES
		      HAMAX = HASERD(1,I4,I3,I1)                !MAX HA       
                      IF (HAMAX.LE.HACUR) THEN
                        IF (LASTSCAN.OR.(HAMAX.LT.HACUR)) THEN
C
		      	  FLFJ(FLF_FLAG_J,1)=FLAGTYPE(I4)    !FLAGBYTE
		      	  FLFJ(FLF_FLAG_J,2)=FLAGTYPE(I4)    !FLAGBYTE
C
		          FLFE(FLF_HA_E,1)=HASERD(0,I4,I3,I1)   !MIN HA       
		          FLFE(FLF_HA_E,2)=HASERD(1,I4,I3,I1)   !MAX HA       
C
	                  FLFJ(FLF_CHAN_J,1)=CHCUR           !MIN CHANN NR
	                  FLFJ(FLF_CHAN_J,2)=CHCUR           !MAX CHANN NR
		          FLFI(FLF_IFR_I,1)=IFRT(I1)         !MIN IFR NR
		          FLFI(FLF_IFR_I,2)=IFRT(I1)         !MAX IFR NR
C                         .... What about the TEL type of entry? ....
C
		      	  FLFI(FLF_POL_I,1)=I3               !MIN POL NR
		      	  FLFI(FLF_POL_I,2)=I3               !MIN POL NR
                          IF (I3.EQ.-1) FLFI(FLF_POL_I,1)=-1 !WILDCARD (ALL)
C
                          NENT = 1                           !1 FLF ENTRY
		          HAMIN = HASERD(0,I4,I3,I1)            !MIN HA       
	                  IF (HAMAX.GT.HAMIN) NENT = 2       !2 FLF ENTRIES
                          FLFJ(FLF_FLAG_J,1) = 
     1                              IOR(FLFJ(FLF_FLAG_J,1),NENT-1) 
                          FLFJ(FLF_FLAG_J,2) = 
     1                              IOR(FLFJ(FLF_FLAG_J,2),2) 
                          DO I7=1,NENT                     !1 OR 2 FLF ENTRIES
                            IF (TRACE) THEN
		              CALL WNCTXT (F_T,' NFLGET: FLF data: '
     1                           //'flag=!UJ  ifr=!UI  pol=!UI ' 
     1                           //' HA=!F7.2 - !F7.2'
     1                           ,FLFJ(FLF_FLAG_J,1)
     1                           ,FLFI(FLF_IFR_I,1)
     1                           ,FLFI(FLF_POL_I,1)
     1                           ,FLFE(FLF_HA_E,1)*360
     1                           ,FLFE(FLF_HA_E,2)*360)
                              END IF
		      	    IF (.NOT.NFLFL1(DFAR,FLF(0,I7))) THEN
		              CALL WNCTXT(F_TP,'Error writing FLF entry')
		              GOTO 700	                   !PROBLEM, ESCAPE
                            END IF
                          END DO
C
                          HASERD(0,I4,I3,I1) = VERYLARGE     !INITIALISE HAMIN
                          HASERD(1,I4,I3,I1) = VERYLARGE     !INITIALISE HAMAX
                        END IF
                      END IF
                    END DO                                !FLAG TYPES
                  END DO                                  !POLS
                END DO                                    !IFRS
C
C----------------------------------------------------------------------------
C
C NEXT SCAN:
C
	      JS = NFLCNT ('ACC','DATA',FLACC,MASK,
     1                     IFRA,CHCUR,HACUR)          !COUNT ENTRY FLAGS
C
	    END IF				!WITHIN HYP HA-RANGE
	  END DO				!NEXT SCAN
C
C****************************************************************************
C****************************************************************************
C
C NEXT SECTOR (if any):
C
 30       CONTINUE
	END DO					!NEXT SECTOR
C
C**************************************************************************
C FINISHED NORMALLY:
C  
C  Display a summary of flags, if required:
C
        IF (SHOW_CNT) THEN
          JS = NFLCNT ('SHOW','FTYP',0,0,0,0,0)
          CALL WNCTXT (F_T,
     1      'NB: Tested and counted are ONLY'
     1      //' those flags that were GOT by the last') 
          CALL WNCTXT (F_T,
     1      'GET operation, and added to the flag-list.'
     1      //' Use INSPECT for a closer look.')
	  CALL WNCTXT (F_T,' ')
        END IF
C
C**************************************************************************
C END OF (OR ESCAPE FROM) OPERATION: 
C
 700    CONTINUE
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





