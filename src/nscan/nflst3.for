C+ NFLST3.FOR
C  JEN940419
C
C  Revisions:

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NFLST3 (ACTION,NAME,OPTIONAL)
C
C  Display statistics of accumulation group etc:
C
C  Result:
C
C	CALL NFLST3 (ACTION_C(*):I,NAME_C(*):I,OPTIONAL_C(*):I)
C
C       CALL NFLST3 ('GROUP',<groupname>,' ')
C       CALL NFLST3 ('LISTS',<polarisations>,' ')
C       CALL NFLST3 ('DCOFF',<polarisations>,' ')
C
C PIN references:
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'	
C
C  Parameters:
C
	INTEGER MAXQ                    !MAX NR OF PRINTED FIELDS
	  PARAMETER (MAXQ=20)

	INTEGER XX,XY,YX,YY		!POL. POINTERS
	  PARAMETER (XX=0, XY=1, YX=2, YY=3)
C
	INTEGER NCSLIN                  !Nr of chars per line
	  PARAMETER (NCSLIN=79)
        CHARACTER*79 SEPAR              !Separator string (see SHOW)
          PARAMETER (SEPAR=
     1          '########################################'//     
     1          '#######################################')     
C
C  Arguments:
C
        CHARACTER ACTION*(*)            !ACTION TO BE PERFORMED
        CHARACTER NAME*(*)              !CLOSER SPECIFICATION OF ACTION
        CHARACTER OPTIONAL*(*)          !CLOSER SPECIFICATION OF ACTION
	CHARACTER*80 ARGSTR
C
C  Function references:
C
	REAL NFLST1                     !STATISTICS ACCUM
C
C  Data declarations:
C
        CHARACTER*2 POLNAME(0:3)              !POL NAMES (XX, XY ETC)
          DATA POLNAME /'XX','XY','YX','YY'/    !
C
        CHARACTER*1 TELNAME(0:STHTEL-1)       !TEL NAMES (0,1,2,A, ETC)
          DATA TELNAME /'0','1','2','3','4','5','6',
     1                '7','8','9','A','B','C','D'/
C
C  Variables:
C
        INTEGER      N,NW0
	INTEGER	     IPR,NPR              !HEADER REPEAT COUNTER
	INTEGER	     IVAL,NVAL            !
        INTEGER      RTW,RTE              !WEST,EAST TELESCOPE NR
	REAL	     MEAN,RMS,RMSVAR,RMSMS,MINVAL,MAXVAL,WTOT,DCOFF
        LOGICAL      SELPOL(-1:3)         !POL SELECTION (SHOW)
	LOGICAL      PRINTLINE            !SWITCH 
	LOGICAL      IFRGROUP		  !GROUP OF IFRS
C
	INTEGER      IQ,NQ                !NR OF STATISTICAL QTY's
	CHARACTER*16 QNAM(0:20)           !QTY NAMES (FOR NFLST1)
	CHARACTER*16 QHED(0:20)           !QTY NAMES (FOR HEADERS)
	REAL         QVAL(0:20)           !QTY VALUES
	INTEGER      QPOS(0:20)           !QTY start columns
	INTEGER      QWID(0:20)           !QTY field size (chars)
	INTEGER      QDEC(0:20)           !QTY nr of decimals
	CHARACTER*24 GROUP(0:20)          !GROUP NAMES (NFLST3)
C
        INTEGER      IFRA(0:1,0:STHIFR-1) !TELESCOPE TABLE (W,E)
        REAL         BASEL(0:STHIFR-1)    !BASELINES (M)
	INTEGER	     IFRSUCC(STHIFR)      !IFR SUCCESSION
        REAL         HARAN(0:1)           !HA-RANGE OF TESTED SCANS
        INTEGER      CHRAN(0:1)           !CHANNEL-RANGE OF TESTED SECTORS
C
        CHARACTER*80 TXT80                !GENERAL TEXT BUFFER
        CHARACTER*80 HEADER               !HEADER TEXT
        CHARACTER*80 APPRAISAL            !APPRAISAL TEXT (FROM NFLST1)
        CHARACTER*24 WTOT_GROUP           !NAME OF GROUP TO BE TESTED FOR WTOT
        CHARACTER*24 APPRAISAL_GROUP      !NAME OF GROUP TO BE APPRAISED

C
C Common:
C
C-
C******************************************************************************
C******************************************************************************
C INITIALISE:
C
	NPR = 25                            !HEADER REPEAT COUNTER
	IFRGROUP = .FALSE.                  !Assume no ifrs
	DO I=1,STHIFR
	  IFRSUCC(I) = I                    !required succession of ifrs
	END DO
C
	NQ = 1                              !7 QUANTITIES (QTY) PRINTED
	QPOS(0) = 1                         !DEFAULT START COLUMN 
	QWID(0) = 0                         !DEFAULT 
        DO IQ=1,NQ                          !FOR ALL PRINTED QTY's
	  GROUP(IQ) = NAME                  !GROUP NAME FOR NFLST1
	  QWID(IQ) = 12                     !DEFAULT FIELD WIDTH (CHARS)
	  QDEC(IQ) = 4                      !DEFAULT NR OF DECIMALS
	END DO
C
	APPRAISAL_GROUP = NAME
	WTOT_GROUP = NAME
C
C******************************************************************************
C******************************************************************************
C CHECK ACTION:
C
	IF (ACTION(:5).EQ.'GROUP') THEN
	  R0 = NFLST1('GET',NAME,'LENGTH',NVAL,0.,0.) !nr of slots in group
	  ARGSTR=ACTION(:3)//' NFLST3: NVAL=!UJ '//NAME
	  CALL WNCTXT (F_TP,ARGSTR,NVAL)
	  IF (NVAL.LE.0) THEN
	    ARGSTR=' NFLST3: '//ACTION(:3)//' slots = !UJ (<=0) '//NAME
	    CALL WNCTXT (F_TP,ARGSTR,NVAL)
	    GOTO 900                                  !escape
	  ELSE IF (NVAL.EQ.STHIFR) THEN
	    IFRGROUP = .TRUE.                         !ifrs
	  END IF
C
C------------------------------------------------------------------------
C
	ELSE IF (ACTION(:5).EQ.'DCOFF') THEN
	  IFRGROUP = .TRUE.                            !ifrs
C
C------------------------------------------------------------------------
C
	ELSE IF (ACTION(:5).EQ.'LISTS') THEN
	  IFRGROUP = .TRUE.                            !ifrs
C
C------------------------------------------------------------------------
C
	ELSE
	  ARGSTR=' NFLST3: Action not recognised: '//ACTION
	  CALL WNCTXT (F_TP,ARGSTR)
          GOTO 900                            !escape
	END IF
C
C------------------------------------------------------------------------
C
	CALL NFLST0 ('GET','IFRTABLE',2*STHIFR,IFRA,0.)
	CALL NFLST0 ('GET','BASEL',STHIFR,0,BASEL)
C
C******************************************************************************
C******************************************************************************
C DEFINE LAYOUT AND MAKE HEADER TEXT:
C
        IF (ACTION(:5).EQ.'GROUP') THEN
          CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
          CALL WNCTXT (F_TP,
     1                 '# Statistics of group: !AS !#C!AS'
     1                 ,NAME,NCSLIN,'#')  
	  CALL NFLST0 ('SHOW','RANGES',NCSLIN,0,0.)    !HA-RANGE ETC
C
	  APPRAISAL_GROUP = NAME
	  WTOT_GROUP = NAME
          DO IQ=1,MAXQ                        !FOR ALL PRINTED QTY's
	    GROUP(IQ) = NAME                  !GROUP NAME FOR NFLST1
	    QWID(IQ) = 7                      !DEFAULT FIELD WIDTH (CHARS)
	    QDEC(IQ) = 0                      !DEFAULT NR OF DECIMALS
	  END DO
C
	  NQ = 0
	  QPOS(0) = 12                        !START COLUMN (1st QTY)
C
	  NQ = NQ+1
	  QNAM(NQ) = 'WTOT'
	  NQ = NQ+1
	  QNAM(NQ) = 'MEAN'
	  NQ = NQ+1
	  QNAM(NQ) = 'RMS'
CCCC	  NQ = NQ+1
CCCC	  QNAM(NQ) = 'RMSMS'
	  NQ = NQ+1
	  QNAM(NQ) = 'RMSVAR'
	  NQ = NQ+1
	  QNAM(NQ) = 'MINVAL'
	  NQ = NQ+1
	  QNAM(NQ) = 'MAXVAL'
	  NQ = NQ+1
	  QNAM(NQ) = 'DCOFF'
C
	  HEADER = '#'
          IF (IFRGROUP) THEN
            HEADER = '# ifr (m)'
	  END IF
C
          DO IQ=1,NQ                               !FOR ALL QTY
            QPOS(IQ) = QPOS(IQ-1) + QWID(IQ-1)     !START COLUMN PER QTY
	    QHED(IQ) = QNAM(IQ)                    !QTY NAME FOR NFLST1
	    CALL WNCALC (QHED(IQ))                 !CONVERT TO LOWER CASE
            CALL WNCTXS (HEADER(QPOS(IQ):),'!#$AS'
     1                   ,QWID(IQ),QHED(IQ))
	  END DO
          HEADER(QPOS(NQ)+QWID(NQ)+2:) = '...'      !closing text
C
C-------------------------------------------------------------------------
C
        ELSE IF (ACTION(:5).EQ.'LISTS') THEN
          CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
          CALL WNCTXT (F_TP,
     1         '# Amplitude and Phase summary: !#C!AS'
     1         ,NCSLIN,'#')  
          CALL WNCTXT (F_TP,
     1         '# **** XX only (experimental) **** !#C!AS'
     1         ,NCSLIN,'#')  
	  CALL NFLST0 ('SHOW','RANGES',NCSLIN,0,0.)    !HA-RANGE ETC
C
          DO IQ=1,MAXQ                        !FOR ALL PRINTED QTY's
	    GROUP(IQ) = NAME                  !GROUP NAME FOR NFLST1
	    QWID(IQ) = 6                      !DEFAULT FIELD WIDTH (CHARS)
	    QDEC(IQ) = 0                      !DEFAULT NR OF DECIMALS
	  END DO
C
	  WTOT_GROUP = 'DAT_A_XX'
	  R0 = NFLST1('GET',WTOT_GROUP,'LENGTH',NVAL,0.,0.) !nr of slots/group
	  APPRAISAL_GROUP = 'DAT_A_XX'
C
	  NQ = 0                              !7 QUANTITIES (QTY) PRINTED
	  QPOS(0) = 12                        !START COLUMN (1st QTY)
C
C Ampl:
C
	  NQ = NQ + 1
	  GROUP(NQ) = 'DAT_A_XX'
	  QNAM(NQ) = 'WTOT'
	  QHED(NQ) = 'npts'
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'RMSMS'
	  GROUP(NQ) = 'DAT_A_XX'
	  QHED(NQ) = 'rmsms' 
	  QWID(NQ) = QWID(NQ) + 3                !HORIZONTAL SEPARATION
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'MINVAL'
	  GROUP(NQ) = 'DAT_A_XX'
	  QHED(NQ) = 'min' 
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'MAXVAL'
	  GROUP(NQ) = 'DAT_A_XX'
	  QHED(NQ) = 'max' 
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'RMSVAR'
	  GROUP(NQ) = 'DAT_A_XX'
	  QHED(NQ) = 'm.e.' 
C
C Phase:
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'RMSMS'
	  GROUP(NQ) = 'DAT_P_XX'
	  QHED(NQ) = 'rmsms' 
	  QWID(NQ) = QWID(NQ) + 3                !HORIZONTAL SEPARATION
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'MINVAL'
	  GROUP(NQ) = 'DAT_P_XX'
	  QHED(NQ) = 'min' 
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'MAXVAL'
	  GROUP(NQ) = 'DAT_P_XX'
	  QHED(NQ) = 'max' 
C
	  NQ = NQ + 1
	  QNAM(NQ) = 'RMSVAR'
	  GROUP(NQ) = 'DAT_P_XX'
	  QHED(NQ) = 'm.e.' 
C
C
          HEADER = '# ifr (m)'
          DO IQ=1,NQ                               !FOR ALL QTY
            QPOS(IQ) = QPOS(IQ-1) + QWID(IQ-1)     !START COLUMN PER QTY
            CALL WNCTXS (HEADER(QPOS(IQ):),'!#$AS'
     1                   ,QWID(IQ),QHED(IQ))
	  END DO
          HEADER(QPOS(NQ)+QWID(NQ)+2:) = ' '      !closing text
C
	  TXT80 = ' '
	  TXT80(QPOS(2)+4:) = 'Amplitude (W.U)'
	  TXT80(QPOS(6)+4:) = 'Phase (degr)'
          CALL WNCTXT (F_TP,'# !AS !#C!AS',TXT80,NCSLIN,'#')  
C
C-------------------------------------------------------------------------
C
        ELSE IF (ACTION(:5).EQ.'DCOFF') THEN
          CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
          CALL WNCTXT (F_TP,
     1                 '# DC-offset: !#C!AS'
     1                 ,NCSLIN,'#')  
	  CALL NFLST0 ('SHOW','RANGES',NCSLIN,0,0.)    !HA-RANGE ETC
C
          DO IQ=1,MAXQ                        !FOR ALL PRINTED QTY's
	    GROUP(IQ) = NAME                  !GROUP NAME FOR NFLST1
	    QWID(IQ) = 7                      !DEFAULT FIELD WIDTH (CHARS)
	    QDEC(IQ) = 0                      !DEFAULT NR OF DECIMALS
	  END DO
C
	  WTOT_GROUP = 'DAT_C_XX'
	  R0 = NFLST1('GET',WTOT_GROUP,'LENGTH',NVAL,0.,0.) !nr of slots/group
	  APPRAISAL_GROUP = 'DAT_C_XX'
C
	  NQ = 0
C
	  HEADER = '#'
          DO IQ=1,NQ                               !FOR ALL QTY
            QPOS(IQ) = QPOS(IQ-1) + QWID(IQ-1)     !START COLUMN PER QTY
            CALL WNCTXS (HEADER(QPOS(IQ):),'!#$AS'
     1                   ,QWID(IQ),QHED(IQ))
	  END DO
C
          HEADER(QPOS(NQ)+QWID(NQ)+2:) = '...'      !closing text
C
C-------------------------------------------------------------------------
C
	ELSE
	END IF
C
C-------------------------------------------------------------------------
C GENERAL FINISH:
C 
	HEADER(NCSLIN:) = '#'                       !closing hash
C
C******************************************************************************
C******************************************************************************
C PRINT DATA ITSELF:
C
	  IPR = 0                                     !HEADER REPEAT COUNTER
          NW0 = 0
	  DO I=1,NVAL
	    IVAL = I                                  !
	    TXT80 = '#'                               !OPENING HASH (#)
C
	    IF (IFRGROUP) THEN                        !IFRS
	      IVAL = IFRSUCC(I)                       !ifr succession
	      RTW = IFRA(0,IVAL-1)                    !WEST TEL
	      RTE = IFRA(1,IVAL-1)                    !EAST TEL
	      CALL WNCTXS (TXT80,
     1             '# !AS!AS (!4$UJ): '
     1             ,TELNAME(RTW),TELNAME(RTE)
     1             ,NINT(BASEL(IVAL-1)))
	    END IF
C
	    WTOT = 1                                  !Default: print
	    IF (WTOT_GROUP.NE.' ') THEN
	      WTOT = NFLST1('CALC',WTOT_GROUP,'WTOT',-IVAL,0.,0.)
	    END IF
C
	    IF (WTOT.GT.0) THEN
              DO IQ=1,NQ
                QVAL(IQ) = 
     1            NFLST1 ('CALC',GROUP(IQ),QNAM(IQ),-IVAL,0.,0.)
                CALL WNCTXS (TXT80(QPOS(IQ):),'!#$F#.#'
     1               ,QWID(IQ),QWID(IQ),QDEC(IQ),QVAL(IQ)) 
	      END DO
C
	      IF (APPRAISAL_GROUP.NE.' ') THEN
	        R0 = NFLST1('APPRAISE',APPRAISAL_GROUP,
     1                       APPRAISAL,IVAL,0.,0.)        !get appraisal text
                TXT80(QPOS(NQ)+QWID(NQ)+2:) = APPRAISAL   !closing text
	      END IF
C
	      IF (IPR.EQ.0) CALL WNCTXT (F_TP,HEADER)  !repeat header
	      IPR = MOD(IPR+1,NPR)                     !increment counter
              CALL WNCTXT (F_TP,'!AS !#C!AS',TXT80,NCSLIN,'#') !print line
C
	    ELSE
	      NW0 = NW0+1                   !Skipped, increment counter
            END IF          
	  END DO
C
C Finish off:
C
	  IF (NW0.GT.0) THEN
            CALL WNCTXT (F_TP,
     1         '# Nr of empty slots (wtot=0, not shown): '
     1         //' !UJ out of !UJ total !#C!AS'
     1         ,NW0,NVAL,NCSLIN,'#') 
	  END IF
C
          CALL WNCTXT (F_TP,'!AS',SEPAR)               !SEPARATOR
C
CCC	  DO IQ=1,NQ
CCC	    R0 = NFLST1 ('EXPLAIN',QNAM(IQ),TXT80,0,0.,0.) !Explain quantities
CCC       END DO
C
C******************************************************************************
C
 900	CONTINUE
C
C******************************************************************************
	RETURN
	END


