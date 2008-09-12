C+ NFLIST.FOR
C  JEN 930916
C
C  Revisions:
C	CMV 940203	Added nodename and sets to NFLFL7 call
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NFLIST (OPER,USERFLAG,DFAR,
     1                     SHOW_CNT,TRACE)
C
C  Interactions with the internal flag-list (FLF), incl. GET/PUT operations.
C
C  Result:
C
C	CALL NFLIST (OPER_C(*):I,USERFLAG_J:I,DFAR_J:IO,
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
	INTEGER MXNFLTYP                !NR OF FLAG TYPES
	  PARAMETER (MXNFLTYP=8)
C
C  Arguments:
C  (NB: NODIN, FCAIN and SETS are in common block)
C
        CHARACTER       OPER*(*)        !SELECTED OPERATION
C
        INTEGER         USERFLAG           !FLAGBYTE (FOR USER OVERRIDE)
	INTEGER	        DFAR		!FLAG-LIST AREA CONTROL PAR
C
	LOGICAL		SHOW_CNT        !SHOW FLAG-COUNT AFTER OPS
	LOGICAL	        TRACE           !TRACE THE FLAGGING OPS
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
	LOGICAL NFLFL0,NFLFL1,NFLFL2	!FLAG FILE HANDLING
	LOGICAL NFLFL5,NFLFL6
	LOGICAL NFLFL7,NFLFL8,NFLFL9
	LOGICAL NFLFLS,NFLFLR
	LOGICAL NFLCNT                  !FLAG COUNTING 
C
C  Data declarations:
C
        INTEGER         I6,I7,I8,I9     !LOOP VARIABLES
        LOGICAL         TYPIFR          !ENTRY TYPE IS IFR-TYPE
	INTEGER		NIFRTYP,NBASTYP !NR OF EITHER TYPE
        INTEGER         IFRMIN,IFRMAX   !MIN, MAX IFR NR 
        INTEGER         IPOLMIN,IPOLMAX !MIN, MAX POL NR 
        INTEGER         RTW,RTE         !WEST, EAST TEL NR 
        INTEGER         RTW1,RTW2       !WEST TEL NRS 
        INTEGER         RTE1,RTE2       !EAST TEL NRS 
        INTEGER         RTWMIN,RTWMAX   !MIN, MAX WEST TEL NR 
        INTEGER         RTEMIN,RTEMAX   !MIN, MAX EAST TEL NR 
        INTEGER         ICHMIN,ICHMAX   !MIN, MAX FREQU CHANN NR 
        REAL            HAMIN,HAMAX     !MIN, MAX HA (CIRCLES)
        REAL            HA1,HA2,HA3     !HA LOOP PARAMETERS (CIRCLES)
        INTEGER         BAS,BASMIN,BASMAX !BASELINE LENGTH (PUT, M)
	INTEGER		CHCUR		!CURRENT CHANNEL
        REAL		HACUR		!CURRENT HA
	INTEGER		LLENGTH		!CURRENT LIST LENGTH
C
C  Names of things etc:
C
	CHARACTER*1 TELNAM(0:STHTEL-1)       !TELESCOPE NAMES (WSRT)
	  DATA TELNAM/'0','1','2','3','4','5','6','7',
     1                '8','9','A','B','C','D'/
C
	CHARACTER*2 POLNAM(0:3)              !POLARISATION NAMES (WSRT)
	  DATA POLNAM/'XX','XY','YX','YY'/
C
	CHARACTER*4 FLAGNAM(0:MXNFLTYP-1)    !FLAG NAMES (WSRT)
	  DATA FLAGNAM/' MAN','CLIP','NOIS',' ADD',
     1                 'SHAD','  U3','  U2','  U1'/
C
	INTEGER FLAGTYP(0:MXNFLTYP-1)        !FLAG TYPES (WSRT)
	  DATA FLAGTYP/FL_MAN,FL_CLIP,FL_NOIS,FL_ADD,
     1                 FL_SHAD,FL_3,FL_2,FL_1/
C

C   Flow control
C
	INTEGER	 	FLAG		!FLAGBYTE 
	INTEGER	 	SELFLAG		!SELECTED FLAG TYPE(S)
        INTEGER         FLAGH           !FLAGBYTE FOR SCAN HEADER
        INTEGER         FLAGD           !FLAGBYTE FOR UV-DATUM
C
C  Storage areas, buffer arrays
C
        CHARACTER*80 TXT80              !TEXT BUFFER
	INTEGER FLACC(0:STHIFR-1,0:3)	!FLAG COUNTS
	INTEGER MASK(0:STHIFR-1,0:3)	!FLAGBYTES USED
C
	INTEGER IFRA(0:1,0:STHIFR-1)    !RTWEST(0), RTEAST(1)
        REAL BASEL(0:STHIFR-1)          !BASELINE TABLE (M)
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
C  Always: Create the flag list area if it does not exist already,
C  i.e. if DFAR=0:
C
	IF (.NOT.NFLFL0(DFAR)) THEN
	  CALL WNCTXT(F_TP,'Error getting flag file/area')
	  GOTO 800
	END IF
        JS = NFLFLS (DFAR,FLH)             !READ FLAG-LIST HEADER
        LLENGTH = FLHJ(FLH_FLFN_J)         !NR OF SINGLE ENTRIES
C
C Take action according to the requested OPERation:
C
!*** DELETE the internal flag-list:
C    (NB: This option used to be called CLEAR, but there is a clash with
C         clearing flags in NFLFLG). 
C
	  IF (OPER(:3).EQ.'DEL') THEN
	    JS = NFLFL9(DFAR)		       !MAKE SURE FLAG FILE GONE
	    IF (.NOT.NFLFL0(DFAR)) THEN
	      CALL WNCTXT(F_TP,'Error getting flag file/area')
	      GOTO 800
	    END IF

            JS = NFLFLS (DFAR,FLH)             !READ FLAG-LIST HEADER
            LLENGTH = FLHJ(FLH_FLFN_J)         !NR OF SINGLE ENTRIES
	    IF (LLENGTH.EQ.0) THEN
	      CALL WNCTXT (F_TP,'  The flag-list is empty.')
	    ELSE
	      CALL WNCTXT (F_TP,'  Something has gone wrong:'
     1          //' List length is !SJ',LLENGTH)
	    END IF
C
!*** LOAD (FLF-file)
C
	  ELSE IF (OPER(:3).EQ.'LOA') THEN
	    IF (LLENGTH.GT.0) THEN
              CALL WNCTXT (F_T,' ')
	      CALL WNCTXT (F_T,'The contents of the FLF-file will'
     1           //' be ADDED to the current flag-list!') 
            END IF
	    JS=NFLFL6(DFAR)			!LOAD
	    GOTO 200                          !Show list header
C
!*** UNLOAD (FLF-file)
C
	  ELSE IF (OPER(:3).EQ.'UNL') THEN
	    JS=NFLFL5(DFAR)			!UNLOAD
	    GOTO 200                          !Show list header
C
!*** WRITE (Flag-file, ASCII version):
C
	  ELSE IF (OPER(:3).EQ.'WRI') THEN
	    JS=NFLFL7(DFAR,0,NODIN,SETS)	      !WRITE ASCII FILE
	    GOTO 200                          !Show list header
C
!*** READ (Flag-file, ASCII version):
C
	  ELSE IF (OPER(:3).EQ.'REA') THEN
	    IF (LLENGTH.GT.0) THEN
              CALL WNCTXT (F_T,' ')
	      CALL WNCTXT (F_T,'The contents of the ASCII-file will'
     1           //' be ADDED to the current flag-list!') 
            END IF
	    JS=NFLFL8(DFAR)			!READ ASCII
	    GOTO 200                          !Show list header
C
!*** LIST the internal flag-list:
C
	  ELSE IF (OPER(:3).EQ.'LIS') THEN
	    CALL WNCTXT (F_TP,' ')
            JS = NFLFL7 (DFAR,F_T)              !Type flag list on screen
	    GOTO 200                            !Show list header
C
!*** GET flags from headers/data into flag-list:
C
	  ELSE IF (OPER(:3).EQ.'GET') THEN
	    IF (LLENGTH.GT.0) THEN
              CALL WNCTXT (F_T,' ')
	      CALL WNCTXT (F_T,'The result of the GET operation will'
     1           //' be ADDED to the current flag-list!') 
            END IF
            CALL NFLGET (OPER,USERFLAG,DFAR,
     1                   SHOW_CNT,TRACE)
	    GOTO 200                            !Show list header
C
!*** PUT flags from flag-list into headers/data:
C
	  ELSE IF (OPER(:3).EQ.'PUT') THEN
            CALL NFLPUT (OPER,USERFLAG,DFAR,
     1                   SHOW_CNT,TRACE)
C
C----------------------------------------------------------------------------
!*** HEADER: Show the contents of the flag-list header:
C            The header of the flag-list contains the range of HA-s and
C            frequ channels that are present in the list. 
C
	  ELSE IF (OPER(:3).EQ.'HEA') THEN
 200        CONTINUE                           !USED BY OTHER OPTIONS
	    CALL WNCTXT (F_TP,' ')
	    CALL WNCTXT (F_TP,
     1        'Summary of the contents of the current flag-list:')
C
	    JS = NFLFLS (DFAR,FLH)             !READ THE FLAG-LIST HEADER
C
            LLENGTH = FLHJ(FLH_FLFN_J)         !LIST LENGTH (SINGLE ENTRIES)
	    IF (LLENGTH.LE.0) THEN
	      CALL WNCTXT (F_TP,'  The flag-list is empty.')
	      CALL WNCTXT (F_TP,' ')
	      GOTO 800                         !ESCAPE
	    END IF
C
C Go through the flag-list itself:
C
            JS=NFLFLR(DFAR)		       !MAKE SURE BEGIN LIST
            I1 = 0                             !COUNTER (OF LIST ENTRIES)
	    FLAG = 0                           !COMPOSITE FLAG-BYTE
	    NIFRTYP = 0                        !NR OF IFR-TYPE ENTRIES
	    NBASTYP = 0                        !NR OF BASEL-TYPE ENTRIES
            DO WHILE(NFLFL2(DFAR,FLF(0,1),FLF(0,2))) !READ ALL ENTRIES
              I1 = I1+1                        !COUNT NR OF ACTUAL ENTRIES
              FLAG = IOR(FLAG,FLFJ(FLF_FLAG_J,1))    !FLAG-TYPE(S) IN LIST
              IF (IAND(FLFJ(FLF_FLAG_J,1),'01000000'X).EQ.0) THEN
                NIFRTYP = NIFRTYP+1
              ELSE
                NBASTYP = NBASTYP+1
              END IF    
            END DO
C
            CALL WNCTXT (F_TP,
     1        '  Total nr of single and double (range) entries:'
     1        //' !SJ (list length=!SJ)'
     1        ,I1,FLHJ(FLH_FLFN_J))
            IF (NBASTYP.GT.0) THEN
              CALL WNCTXT (F_TP,
     1          '  Nr of baseline-type entries: !SJ (ifrtype=!SJ)'
     1        ,NBASTYP,NIFRTYP)
            END IF
C
C Summary of flag-types:

	    TXT80 = ' '
	    I=1
	    DO I4=0,MXNFLTYP-1
              IF (IAND(FLAGTYP(I4),FLAG).NE.0) THEN
                I5 = FLAGTYP(I4)
                I5 = IAND('000000ff'X,ISHFT(I5,-8))     !FLAG TYPE CODE
		CALL WNCTXS (TXT80(I:),'!4$AS(!2$XJ)'
     1                      ,FLAGNAM(I4),I5)
		I=I+8
              END IF
            END DO 
            CALL WNCTXT (F_TP,'  Flag-types: '//TXT80)
C
C Range of frequ channels:
C
            ICHMIN = FLHJ(FLH_RCHAN_J+0)       !MIN CHANNEL NR IN LIST
            ICHMAX = FLHJ(FLH_RCHAN_J+1)       !MAX CHANNEL NR IN LIST
	    IF (FLHJ(FLH_CHAN_J).EQ.-1) THEN   !WILDCARD: ALL CHANNELS
              CALL WNCTXT (F_TP,
     1          '  Range of frequ channels:  All (*)')
	    ELSE
              CALL WNCTXT (F_TP,
     1          '  Range of frequ channels:  !SJ:!SJ'
     1          ,ICHMIN,ICHMAX)
            END IF
C
C Range of ifrs/baselines:
C
            IFRMIN = FLHJ(FLH_RIFR_J+0)        !MIN IFR NR IN LIST
	    RTW1 = MOD(IFRMIN,256)
	    RTE1 = IFRMIN/256
            IFRMAX = FLHJ(FLH_RIFR_J+1)        !MAX IFR NR IN LIST
	    RTW2 = MOD(IFRMAX,256)
	    RTE2 = IFRMAX/256
	    IF (FLHJ(FLH_IFR_J).EQ.-1) THEN    !WILDCARD: ALL IFR'S
              CALL WNCTXT (F_TP,
     1          '  Range of interferometers: All (*)')
	    ELSE
              CALL WNCTXT (F_TP,
     1          '  Range of interferometers: !AS!AS:!AS!AS'		
     1          ,TELNAM(RTW1),TELNAM(RTE1)
     1          ,TELNAM(RTW2),TELNAM(RTE2))
            END IF
C
C Range of polarisations:
C
            IPOLMIN = FLHJ(FLH_RPOL_J+0)       !MIN POL NR IN LIST
            IPOLMAX = FLHJ(FLH_RPOL_J+1)       !MAX POL NR IN LIST
	    IF (FLHJ(FLH_POL_J).EQ.-1) THEN    !WILDCARD: ALL POL'S
              CALL WNCTXT (F_TP,
     1          '  Range of polarisations:   All (*)')
	    ELSE
              CALL WNCTXT (F_TP,
     1          '  Range of polarisations:   !AS:!AS (!SJ:!SJ)'
     1          ,POLNAM(IPOLMIN),POLNAM(IPOLMAX)
     1          ,IPOLMIN,IPOLMAX)
            END IF
C
C Range of HA's
C
            HAMIN = FLHE(FLH_RHA_E+0)          !MIN HA IN LIST
            HAMAX = FLHE(FLH_RHA_E+1)          !MAX HA IN LIST
	    IF (FLHJ(FLH_HA_J).EQ.-1) THEN     !WILDCARD: ALL HA'S
              CALL WNCTXT (F_TP,
     1          '  Range of hour-angles:    All (*)')
	    ELSE
              CALL WNCTXT (F_TP,
     1          '  Range of hour-angles:    !EAF10.2:!EAF10.2'
     1          //' degr',HAMIN,HAMAX)
            END IF
C
	    CALL WNCTXT (F_TP,' ')
C
C------------------------------------------------------------------------------
!*** COMPACT the internal flag-list:
C
	  ELSE IF (OPER(:3).EQ.'COM') THEN
	    CALL WNCTXT(F_TP,'NFLIST: COMPACT not implemented yet.')
C
!*** LCOUNT/COUNT flags in the LIST as various 1D projections:
C
	  ELSE IF ((OPER(:3).EQ.'LCO') .OR.
     1             (OPER(:3).EQ.'COU')) THEN
C
	    CALL WNCTXT (F_TP,'NFLIST: LCOUNT not implemented yet')
	    GOTO 800
C
	    I1 = 0                               
	    DO RTW=0,STHTEL-1                     !WEST TEL
              DO RTE=RTW,STHTEL-1                 !EAST TEL
                IFRA(0,I1) = RTW                  !Home-made IFRA
                IFRA(1,I1) = RTE                  !(i.e. not from header)
                I1 = I1+1                         !INCREMENT IFR NR
              END DO
            END DO
C                
	    JS = NFLFLS (DFAR,FLH)             !READ FLAG-LIST HEADER
            HA1 = FLHE(FLH_RHA_E+0)            !MIN HA IN LIST
            HA2 = FLHE(FLH_RHA_E+1)            !MAX HA IN LIST
	    HA3 = 0.25/360.                    !HA INCREMENT ???????
C
	    SELFLAG = FL_ALL                   !Count ALL flag-type(s)
C
	    JS = NFLCNT ('RESET',' ',0,0,0,0)  !RESET FLAG-COUNT BUFFERS
C
C-------------------------------------------------------------------------
	    DO HACUR=HA1,HA2,HA3               !ALL HA-BINS IN LIST 
              DO I1=0,STHIFR-1
                DO I3=0,3
                  FLACC(I1,I3) = 0             !SET TO ZERO
                  MASK(I1,I3) = 0              !SET TO ZERO
                END DO
              END DO
C
              JS = NFLFLR(DFAR)		       !MAKE SURE BEGIN FLAG-LIST
              DO WHILE(NFLFL2(DFAR,FLF(0,1),FLF(0,2)))   !READ ALL ENTRIES
                FLAG = FLFJ(FLF_FLAG_J,1)	    !FLAGBYTE OF ENTRY
C 
                HAMIN = FLFE(FLF_HA_E,1)            !MIN HA
                HAMAX = FLFE(FLF_HA_E,2)            !MAX HA
                IF (HAMIN.EQ.-1) THEN               !ALL HA-SCANS 
                  HAMIN = HA1                       !MIN HA IN LIST
                  HAMAX = HA2                       !MAX HA IN LIST
	        END IF
                IF (HACUR.GE.HAMIN.AND.HACUR.LE.HAMAX) THEN  !IN RANGE
C
                  IPOLMIN = FLFI(FLF_POL_I,1)
                  IPOLMAX = FLFI(FLF_POL_I,2)
                  IF (IPOLMIN.EQ.-1) THEN               !ALL IFRS
                    IPOLMIN = 0
                    IPOLMAX = 3
                  END IF
C
                  IF (IAND(FLAG,'01000000'X).EQ.0) THEN 
                    TYPIFR = .TRUE.                    !IFR-TYPE ENTRY
                    IFRMIN = FLFI(FLF_IFR_I,1)          
                    IFRMAX = FLFI(FLF_IFR_I,2)
                    IF (IFRMIN.EQ.-1) THEN             !ALL IFRS
                      IFRMIN = 0
                      IFRMAX = STHIFR-1
                    ELSE
                    END IF
                    DO I1=IFRMIN,IFRMAX                  !IFRS
                      DO I3=IPOLMIN,IPOLMAX              !POLS
                        FLACC(I1,I3) = FLAG        
                        MASK(I1,I3) = SELFLAG
                      END DO
                    END DO
C
                  ELSE                                 
                    TYPIFR = .FALSE.                   !BASEL-TYPE ENTRY
                    BASMIN = FLFI(FLF_IFR_I,1)          
                    BASMAX = FLFI(FLF_IFR_I,2)
                    IF (BASMIN.EQ.-1) THEN             !ALL IFRS
                      BASMIN = 0
                      BASMAX = 3000
                    ELSE
                    END IF
                  END IF                               !TYPIFR
C
                  ICHMIN = FLFJ(FLF_CHAN_J,1)         !MIN CHANNEL NR
                  ICHMAX = FLFJ(FLF_CHAN_J,2)         !MAX CHANNEL NR 
                  IF (ICHMIN.EQ.-1) THEN              !ALL CHANNELS 
                    ICHMIN = FLHJ(FLH_RCHAN_J+0)      !MIN CHANNEL NR IN LIST
                    ICHMAX = FLHJ(FLH_RCHAN_J+1)      !MAX CHANNEL NR IN LIST
                    IF (ICHMIN.EQ.-1) THEN            !ALL CHANNELS 
                      ICHMIN = 0                      !??
                      ICHMAX = 10                     !??
                    END IF
	          END IF
C
                  DO CHCUR=ICHMIN,ICHMAX              !ALL FREQU CHANNELS
	            IF ((IPOLMIN.EQ.-1).AND.(IFRMIN.EQ.-1)) THEN
                      JS = NFLCNT ('ACC','HEAD',FLAG,SELFLAG,
     1                            IFRA,CHCUR,HACUR)
	            ELSE
                      JS = NFLCNT ('ACC','DATA',FLACC,MASK,
     1                            IFRA,CHCUR,HACUR)
	            END IF
                  END DO                             !CHCUR
C
                END IF                               !HACUR IN RANGE
	      END DO                                 !LIST ENTRIES
            END DO                                   !HACUR
C
	    JS = NFLCNT ('SHOW','FTYP',0,MASK,0,0,0)
	    CALL WNCTXT (F_T,'Counted are the flags in the flag-list,'
     1                     //' NOT in the data/headers!')
	    CALL WNCTXT (F_T,' ')
C
C**************************************************************************
C
!*** Operation not recognised:
C
          ELSE
	    CALL WNCTXT(F_TP,'FLIST operation not recognised')
          END IF
C
C**************************************************************************
C**************************************************************************
C READY
C
 800	CONTINUE
	RETURN                                  !BACK TO FLAG_OPTION 
	END






