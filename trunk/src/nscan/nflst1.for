C+ NFLST1.FOR
C  JEN 930922
C
C  Revisions:
C	CMV 940517	Define DWARF symbols MEAN,RMS etc
C	HjV 940728	Use real values (0.) in MAX functions
C	CMV 940926	Re-installed above two changes
C	JPH 951017	Put RMS behind RMSxx 
C	CMV 960122	More explanation in header of GROUPS option
C
C------------------------------------------------------------------------
C  PLEASE INDICATE **ALL** CHANGES MADE TO A FILE IN THE REVISION HISTORY 
C------------------------------------------------------------------------
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	REAL FUNCTION NFLST1 (ACTION,NAME,TEXTIO,NVALS,VAL,WGT)
C  
C  Deal with statistics (of arrays) for NFLAG (but can be used more
C  generally too).
C  A general program to deal with accumulating and making available
C  statistics of values (it unburdens the source code a bit).
C
C  Result:
C
C	NFLST1_R:O = NFLST1 (ACTION_C(*):I, NAME_C(*):I,TEXTIO_C(*):IO,
C                            NVALS_J:IO,VAL_R(1:*):IO,WGT_R(1:*):IO)
C
C  The statistics are ACCumulated for named `groups' of NVALS `slots' each.
C                   (NB: The internal name for NVALS is NVAL)
C  Each slot consists of a small number (7) of accumulation buffers,
C  in which wtot, sum, sum of squares, minval, maxval etc are accumulated.
C 
C    CALL NFLST1 ('INIT',' ',' ',0,0.,0.)        de-assign all groups
C         NB: This is compulsory at the beginning of the program.
C    CALL NFLST1 ('DEL', name,' ',0,0.,0.)       de-assign named group
C    CALL NFLST1 ('DEASS', name,' ',0,0.,0.)     de-assign named group
C
C    CALL NFLST1 ('ASSIGN',name,'UNIT=..',nslots,0.,0.)   new group (nslots)  
C         NB: A group of slots will be assigned automatically if necessary.
C
C    CALL NFLST1 ('RESET',name,' ',0,0.,0.)      reset accumulator slots to 0
C         NB: A group of slots will be assigned automatically if necessary.
C    CALL NFLST1 ('RESET','#ALLGROUPS',' ',0,0.,0.)  reset all groups 
C
C  The action 'ACC' accumulates given values into the relevant slots:
C  If a group with the specified 'name' does not exist yet, a new group 
C             of nval slots will be assigned automatically.
C  If nval=nslots, the values given in the arrays val(nval) and wgt(nval)
C             will be accumulated separately into the nval slots of the
C             specified group.
C  If nslots=1 and nval>1, the given values will all be accumulated 
C             into the one slot of the specified group.
C  If nval<0, only one given value (val(1),wgt(1)) will be accumulated 
C             in the specified slot nr abs(nval) of the group. 
C
C    CALL NFLST1 ('ACC', name,' ',nvals,val,wgt)   accumulate value(s)
C
C  The action 'CALC' (calculate) returns statistical results:
C  If nval=1, only the overall result of all slots of the group
C             will be returned in scalar values (e.g. mean and wtot),
C             and as the function value. 
C  If nval<0, only the result of the ABS(nval)th slot of group 
C             will be returned in scalar values (e.g. mean and wtot). 
C  If nval=nslots, the individual results the slots of the group 
C             will be returned in arrays (e.g. mean(nval), wtot(nval)), 
C             and the overall result as the function value.
C  
C    mean   = NFLST1 ('CALC',name,'MEAN'  ,nval,mean,wtot)  return mean
C    rms    = NFLST1 ('CALC',name,'RMS'   ,nval,rms,wtot)          rms
C    rmsms  = NFLST1 ('CALC',name,'RMSMS' ,nval,rmsms,wtot)        rmsms
C    rmsms  = NFLST1 ('CALC',name,'RMSVAR',nval,rmsms,wtot)        rmsvar
C    dcoff  = NFLST1 ('CALC',name,'DCOFF' ,nval,rmsms,wtot)        dc offset
C    minval = NFLST1 ('CALC',name,'MIN'   ,nval,minval,wtot)       miniumum
C    maxval = NFLST1 ('CALC',name,'MAX'   ,nval,maxval,wtot)       maximum
C
C    CALL NFLST1 ('APPRAISE',name,textout,ival,0,wtot) return appraisal
C         NB: Appraisal-text, e.g. smooth, or flat, or spike(s) etc
C    CALL NFLST1 ('EXPLAIN','RMSVAR',textout,0,0,0)   explain RMSVAR etc
C
C    CALL NFLST1 ('SET',name,'UNIT=textin',0,0,0)  set `unit'-text

C    CALL NFLST1 ('GET',' ','NGROUPS',ngroups,0,0) get nr of defined group
C    CALL NFLST1 ('GET',' ','FREESL',nfree,0,0)    get nr of free slots  
C    CALL NFLST1 ('GET',' ','FREEGR',nfree,0,0)    get nr of free groups  
C    CALL NFLST1 ('GET',name,'LENGTH',nslots,0,0)  get nr of slots of group
C    CALL NFLST1 ('GET',name,'FIRST',slot1,0,0)    get first slot of group  
C    CALL NFLST1 ('GET',name,'LAST',slot2,0,0)     get last slot of group  
C    CALL NFLST1 ('GET_UNIT,name,textout,0,0,0)    get unit-text of group
C    CALL NFLST1 ('GET',textout,'NAME',igroup,0,0) get name of group (igroup)
C    CALL NFLST1 ('GET','#ISLOT',' ',islot,maxval,wtot)  (for instance)
C             NB: Directly addressing slots by slot nr is dangerous, 
C             because slots are moved (compacted) when a group is deleted.
C
C  The action 'SHOW' displays statistical results in a standard way.
C  Each call produces a single result (mean,rms,min,max etc), on one line.
C  If nval=1, the overall result over all slots of the group is shown.
C  Otherwise, the result of the ABS(nval)th slot of group is shown.
C  NB: The possibility nval<0 is included for consistency with
C      the use of nval in the actions ACC and CALC (see above).       
C  
C    CALL NFLST1 ('SHOW',name,textin,ival,0.,0.)   print result of named group
C    CALL NFLST1 ('SHOW','#GROUPDEFS',' ',0,0.,0.) print group definitions
C    CALL NFLST1 ('SHOW','#HEADER',' ',0,0.,0.)    print header line
C    CALL NFLST1 ('SHOW','#SEPAR',' ',0,0.,0.)     print separator line (####)
C    CALL NFLST1 ('SHOW','#TEXT',textin,0.,0.)     print text line
C    CALL NFLST1 ('SHOW','#SINGLES',' ',0,0.,0.)   results of 1-slot groups
C
C
C NB: If the first character of the argument `name' is a hash (#), it has
C     special meaning for the program. 
C
C PIN references:
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
        INTEGER MXNGRP                !Max nr of accumulation `groups' 
          PARAMETER (MXNGRP=100)
        INTEGER NCSNAM                !NR OF CHARCTERS PER GRP-NAME
          PARAMETER (NCSNAM=12)
        INTEGER NCSTXT                !NR OF CHARCTERS PER GRP `UNIT' TXT
          PARAMETER (NCSTXT=10)
C
	INTEGER MXNSLOT	              !total nr of slots in accu array
          PARAMETER (MXNSLOT=MXNGRP*40) !Average nr of slots per group
C
        INTEGER MXNSTPAR                  !# of statistics pars per `slot' 
          PARAMETER (MXNSTPAR=7)
	INTEGER AC_WSUM,AC_WSS,AC_WTOT,AC_MIN,AC_MAX
	INTEGER AC_LAST,AC_WSSV
	  PARAMETER (AC_WSUM=1,AC_WSS=2)
	  PARAMETER (AC_WTOT=3,AC_MIN=4, AC_MAX=5)
	  PARAMETER (AC_LAST=6,AC_WSSV=7)
C
C  Codes for the statistics pars accumulated per slot:
C    AC_WSUM = Weighted sum
C    AC_WSS  = Weighted square sum
C    AC_WTOT = Total number of samples
C    AC_MIN  = Minimum value
C    AC_MAX  = Maximum value
C    AC_LAST = Last value (for comparison)
C    AC_WSSV = Weighted sum square variation; i.e. (val-lastval)**2
C
	INTEGER CALC_MEAN,CALC_RMS,CALC_RMSMS
	INTEGER CALC_MIN,CALC_MAX,CALC_WTOT
	INTEGER CALC_RMSVAR,CALC_DCOFF
	  PARAMETER (CALC_MEAN=1, CALC_RMS=2, CALC_RMSMS=3)
	  PARAMETER (CALC_MIN=4, CALC_MAX=5, CALC_WTOT=6)
	  PARAMETER (CALC_RMSVAR=7, CALC_DCOFF=8)
C
        CHARACTER*(NCSNAM) C_UNDEF,C_UNDEF_UNIT
          PARAMETER (C_UNDEF='<undefined>')
          PARAMETER (C_UNDEF_UNIT=' <unit>')
        INTEGER J_UNDEF
          PARAMETER (J_UNDEF=-9798)
        REAL R_UNDEF 
          PARAMETER (R_UNDEF=-9876.12345)
C
        REAL VERYLARGE
          PARAMETER (VERYLARGE=1.E38)
C
        INTEGER NCSLIN                !NR OF CHARS PER LINE
          PARAMETER (NCSLIN=79)
	CHARACTER*79 SEPAR
	  PARAMETER (SEPAR=
	1	'########################################'//
	1	'#######################################' )
C
C  Arguments:
C
        CHARACTER*(*)  ACTION          !ACTION TO BE PERFORMED
        CHARACTER*(*)  NAME            !FURTHER SPECIFICATION
        CHARACTER*(*)  TEXTIO          !I/O TEXT STRING
        INTEGER        NVALS           !NR OF INPUT/OUTPUT VALUES
        REAL           VAL(*)          !INPUT/OUTPUT VALUE(S)
        REAL           WGT(*)          !INPUT/OUTPUT WEIGHT(S)
C
C  Data declarations:
C
        CHARACTER*80 TXT80                      !TEXT BUFFER
	CHARACTER*10 DWARFBUF			!BUFFER FOR WNDPAR
	CHARACTER*(NCSNAM) ACT,NAM		!LOCAL BUFFERS
        CHARACTER*(NCSNAM) GRPNAME(0:MXNGRP)    !NAMES OF ACCUM. GRPS
        CHARACTER*(NCSTXT) UNITXT(0:MXNGRP)     !'UNIT' OF ACCUMULATED QTY
        INTEGER NONBLANK                        !NON-BLANK PART OF GRPNAME
C
	INTEGER NVAL                            !local version of NVALS
	REAL WEIGHT,VALIN,WVALIN		       !AUX VARIABLES
        REAL MEAN,MEANSQ,RMS,RMSMS,WTOT,MINVAL,MAXVAL  !STATISTICS RESULTS
	REAL MSVAR,RMSVAR                              !IDEM
        REAL OV_MEAN,OV_MEANSQ,OV_WTOT,OV_MINVAL,OV_MAXVAL  !OVERALL VALUES
	REAL OV_MSVAR                                  !IDEM
C
	INTEGER CALC                            !CALC-CODE
        INTEGER IVAL,IVAL1,IVAL2                !VALUE NR
C
	INTEGER DOGRP(0:MXNGRP)                 !GROUPS TO BE DONE
	INTEGER IDOGRP,NDOGRP                       
        INTEGER IGRP                            !GROUP NR
	INTEGER IGRPFREE			!NEXT FREE GROUP
C
        INTEGER ISLOT,ISLOT1,ISLOT2,NSLOTS      !SLOT NRS
        INTEGER ISLOT12(2,0:MXNGRP)            	!START/STOP SLOTNRS PER GRP
	INTEGER ISLOTFREE			!NEXT FREE SLOT IN ACCU
C
	REAL ACCU (MXNSTPAR,0:MXNSLOT)          !LARGE ACCUMULATOR ARRAY
C
	LOGICAL INITIALISE,INITIALISED			!
	LOGICAL RECOGNISED			!TRUE IF NAME RECOGNISED
	LOGICAL NEWGROUP			!ASSIGN A NEW GROUP
	LOGICAL RESET				!RESET SLOT(S) 
	LOGICAL SHOW_GROUPS                     !Show defined groups (trace)
	INTEGER ITRACE                          !TRACE SWITCH (0-4)
	LOGICAL SELECTED
	CHARACTER*80 ARGSTR
C
C  Common:
C
        COMMON /NFLST1COMMON/ INITIALISED,
     1                        ISLOT12,ISLOTFREE,
     1                        GRPNAME,IGRPFREE,UNITXT,
     1                        ACCU
C-
C******************************************************************************
C******************************************************************************
C
	ITRACE = 0                              !TRACE SWITCH (0=NONE)
	SHOW_GROUPS = .FALSE. 
C
C  Init:
C
	NFLST1 = 0                              !FUNCTION VALUE
	ACT = ACTION				!TRANSFER TO LOCAL BUFFER
	NVAL = NVALS                            !transfer to local variable
	CALL WNCAUC(ACT)                        !Convert to upper-case
	NEWGROUP = .FALSE.			!IF TRUE, CREATE NEW GROUP
	INITIALISE = .NOT.INITIALISED		!INITIALISE AT LEAST ONCE
	RESET = .FALSE.				!IF TRUE: RESET SLOT(S)
C
C  Check NAME-FIELD:
C
	NAM = NAME				!TRANSFER TO LOCAL BUFFER
	CALL WNCAUC(NAM)                         !Convert to upper-case
	NDOGRP = 0                              !NR OF GROUPS TO BE DONE
	RECOGNISED = .FALSE.			!NAME NOT YET RECOGNISED
C
        IF (NAM(:1).EQ.'#') THEN                !SPECIAL NAME
	  IF (NAM(:6).EQ.'#ISLOT') THEN	!SLOT NR SPECIFIED EXPLOBICITLY 
	    RECOGNISED = .TRUE.
	    ISLOT1 = NVAL			!INPUT VALUE
	    ISLOT2 = NVAL			!INPUT VALUE
C
          ELSE IF (NAM(:6).EQ.'#ALLGR') THEN    !DO ALL DEFINED GROUPS 
	    RECOGNISED = .TRUE.
	    DO IGRP=1,MXNGRP
	      IF (GRPNAME(IGRP).NE.C_UNDEF) THEN
	        NDOGRP = NDOGRP + 1
	        DOGRP(NDOGRP) = IGRP
	      END IF
            END DO
C
          ELSE IF (NAM(:7).EQ.'#SINGLE') THEN    !ALL SINGLE-SLOT GROUPS
	    RECOGNISED = .TRUE.
	    NVAL = 1                             !local version of NVALS
	    DO IGRP=1,MXNGRP
	      IF (GRPNAME(IGRP).NE.C_UNDEF) THEN
	        NSLOTS = ISLOT12(2,IGRP)-ISLOT12(1,IGRP)+1
	        IF (NSLOTS.EQ.1) THEN
	          NDOGRP = NDOGRP + 1
	          DOGRP(NDOGRP) = IGRP
                END IF
	      END IF
            END DO
C
          ELSE IF (NAM(:2).EQ.'##') THEN         !ALL GROUPS WITH ##SUB-STRING
	    RECOGNISED = .TRUE.
	    NAM = NAM(3:)                        !STRIP OFF ##
            NONBLANK = 0
            DO I=1,NCSNAM
              IF (NAM(I:I).NE.' ') NONBLANK = I  !NR OF NON-BLANK CHARS
            END DO
	    DO IGRP=1,MXNGRP
              IF (INDEX(GRPNAME(IGRP),NAM(:NONBLANK)).GT.0) THEN 
	        NDOGRP = NDOGRP + 1
	        DOGRP(NDOGRP) = IGRP
              END IF
            END DO
C
          ELSE IF (NAM(:6).EQ.'#ISLOT') THEN     !SLOT NR ADDRESSED DIRECTLY
	    CALL WNCTXT (F_TP,'NFLST1: #ISLOT not implemented')
	    GOTO 900
C
	  ELSE
	    CONTINUE                            !........?
          END IF
C
        ELSE
          DO IGRP = 1,MXNGRP
            IF (GRPNAME(IGRP).EQ.NAM) THEN      !INPUT NAME FOUND IN GROUP LIST
	      RECOGNISED = .TRUE.
	      NDOGRP = 1
	      DOGRP(NDOGRP) = IGRP
              GOTO 10                           !OK, ESCAPE
            END IF
          END DO
 10       CONTINUE     
        END IF
C
C  Take appropratie action if name is not recognised:
C
        IF (.NOT.RECOGNISED) THEN		!NAME NOT RECOGNISED
          IF (ACT(:3).EQ.'ACC') THEN          
	    NEWGROUP = .TRUE.                       !ASSIGN A NEW GROUP
          ELSE IF (ACT(:3).EQ.'ASS') THEN       
	    NEWGROUP = .TRUE.                       !ASSIGN A NEW GROUP 
          ELSE IF (ACT(:4).EQ.'INIT') THEN          !INITIALISE ALL GRPS
            INITIALISE = .TRUE.
          ELSE
	    CONTINUE                            !........?
          END IF      
C
C  or if name is recognised:
C
	ELSE                                        !NAME RECOGNISED 
          IF (ACT(:5).EQ.'RESET') THEN
	    RESET = .TRUE.	
	  ELSE
	    CONTINUE                            !........?
          END IF      
        END IF                                      !(RECOGNISED)
C
C Initialise, if necessary:
C
	IF (INITIALISE) THEN
          DO IGRP=1,MXNGRP
            GRPNAME(IGRP) = C_UNDEF                 !GRP DE-ASSIGNED
	    UNITXT(IGRP) = C_UNDEF_UNIT		    !UNIT OF ACCUM. QTY
            ISLOT12(1,IGRP) = 0                     !FIRST SLOT  
            ISLOT12(2,IGRP) = 0                     !LAST SLOT
          END DO
	  ISLOTFREE = 1                             !FIRST FREE SLOT
	  IGRPFREE = 1                              !FIRST FREE GROUP
	  INITIALISED = .TRUE.
	  IF (ITRACE.GT.3) THEN                     !TRACING
            CALL WNCTXT (F_TP,'NFLST1 (!UJ): '//ACT(:3)
     1           //' [!UJ] initialised '
     1           //NAM,ITRACE,NVAL)
	    SHOW_GROUPS = .TRUE.
	  END IF
	END IF
C
C Assign a new group (if required):
C
	IF (NEWGROUP) THEN
	  IF (NVAL.LE.0) THEN
            CALL WNCTXT (F_TP,'NFLST1 (newgroup): '//ACT(:3)
     1           //' Invalid NVAL=!SJ for !AS',NVAL,NAME)
	  ELSE IF (IGRPFREE.GE.MXNGRP) THEN		!GROUP TABLE FULL
            CALL WNCTXT (F_TP,'NFLST1 (newgroup): '//ACT(:3)
     1           //' Group table full (!UJ,!UJ), name=!AS'
     1           ,IGRPFREE,MXNGRP,NAME)
	  ELSE IF (ISLOTFREE+NVAL-1.GT.MXNSLOT) THEN	!NOT ENOUGH SLOTS LEFT
            CALL WNCTXT (F_TP,'NFLST1 (newgroup): '//ACT(:3)
     1           //' !UJ+!UJ>!UJ: max slots exceeded, !AS'
     1           ,ISLOTFREE-1,NVAL,MXNSLOT,NAME)
	  ELSE
	    IGRP = IGRPFREE				!NEXT FREE GROUP NR
            GRPNAME(IGRP) = NAME                        !ASSIGN NAME TO GROUP
	    UNITXT(IGRP) = C_UNDEF_UNIT
	    I = INDEX(TEXTIO,'UNIT=')                   !Search for optional =
	    IF (I.GT.0) UNITXT(IGRP) = TEXTIO(I+5:)	!Unit text given
            ISLOT12(1,IGRP) = ISLOTFREE			!FIRST SLOT IN ACCU
            ISLOT12(2,IGRP) = ISLOT12(1,IGRP)+NVAL-1	!LAST SLOT IN ACCU
	    IGRPFREE = IGRPFREE + 1			!
            ISLOTFREE = ISLOT12(2,IGRP)+1               !NEXT FREE SLOT
	    NDOGRP = NDOGRP+1
	    DOGRP(NDOGRP) = IGRP
            RESET = .TRUE.                              !RESET SLOTS
	    IF (ITRACE.GT.3) THEN                       !TRACING
	      ISLOT1 = ISLOT12(1,IGRP)			!FIRST SLOT IN GROUP
	      ISLOT2 = ISLOT12(2,IGRP)			!LAST SLOT IN GROUP
              CALL WNCTXT (F_TP,'NFLST1 (!UJ): '//ACT(:3)
     1           //' [!UJ] new group: !UJ, slot nrs !UJ-!UJ '
     1           //NAM,ITRACE,NVAL,IGRP,ISLOT1,ISLOT2)
	      IF (ITRACE.GT.4) SHOW_GROUPS = .TRUE.
	    END IF
	  END IF
	END IF
C
C  Reset slots to initial values, if required:
C
	IF (RESET) THEN
	  DO IDOGRP=1,NDOGRP
	    IGRP = DOGRP(IDOGRP)                       !GROUP NR
	    ISLOT1 = ISLOT12(1,IGRP)                   !FIRST SLOT
	    ISLOT2 = ISLOT12(2,IGRP)                   !LAST SLOT
	    NSLOTS = ISLOT2-ISLOT1+1                   !NR OF SLOTS
            DO ISLOT=ISLOT1,ISLOT2                     !ALL VALUES
	      DO I=1,MXNSTPAR
                ACCU(I,ISLOT) = 0                      !RESET TO ZERO
              END DO
              ACCU(AC_MIN,ISLOT) = VERYLARGE           !RESET  
              ACCU(AC_MAX,ISLOT) = -VERYLARGE          !RESET
   	    END DO
	    IF (ITRACE.GT.3) THEN                      !TRACING
              CALL WNCTXT (F_TP,'NFLST1 (!UJ): '//ACT(:3)
     1           //' [!UJ] reset group: !UJ, slot nrs !UJ-!UJ '
     1           //NAM,ITRACE,NVAL,IGRP,ISLOT1,ISLOT2)
	    END IF
	  END DO
	END IF
C
C Default settings (saves code lower down):
C
	IF (NDOGRP.GT.0) THEN
	  IDOGRP = NDOGRP                         !LAST GROUP 
	  IGRP = DOGRP(IDOGRP)                    !GROUP NR
	  ISLOT1 = ISLOT12(1,IGRP)                !FIRST SLOT
	  ISLOT2 = ISLOT12(2,IGRP)                !LAST SLOT
	  NSLOTS = ISLOT2-ISLOT1+1                !NR OF SLOTS
	END IF
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C
!*** ACCumulate the given value(s) (VAL), with the given weight(s) (WGT):
C
        IF (ACT(:3).EQ.'ACC') THEN
C
	  DO IDOGRP=1,NDOGRP
	    IGRP = DOGRP(IDOGRP)                    !GROUP NR
	    ISLOT1 = ISLOT12(1,IGRP)                !FIRST SLOT
	    ISLOT2 = ISLOT12(2,IGRP)                !LAST SLOT
	    NSLOTS = ISLOT2-ISLOT1+1                !NR OF SLOTS
	    IVAL1 = 1                               !FIRST I/O VALUE
	    IVAL2 = NVAL                            !LAST I/O VALUE
	    IF ((NVAL.GT.0).AND.(NVAL.EQ.NSLOTS)) THEN  !ALL SLOTS OF GROUP
	      IVAL1 = 1                             !
	      IVAL2 = 1 + NSLOTS - 1 
	    ELSE IF ((NVAL.LT.0).AND.(ABS(NVAL).LE.NSLOTS)) THEN 
	      ISLOT1 = ISLOT1 + ABS(NVAL) - 1       !SPECIFIED SLOT NR ONLY
	      ISLOT2 = ISLOT1
	    ELSE IF ((NSLOTS.EQ.1).AND.(NVAL.GT.NSLOTS)) THEN
	      IVAL1 = 1                             !
	      IVAL2 = NVAL                          !ALL INTO ONE SLOT 
	    ELSE
              CALL WNCTXT (F_TP,'NFLST1: '//ACT(:3)
     1           //' acc: inadmissible nval=!SJ '
     1           //' (nslots=!UJ) '
     1           //NAM
     1           ,NVAL,NSLOTS)
	      GOTO 900
            END IF
C
	    I = 0                                     !COUNTER
	    DO IVAL=IVAL1,IVAL2                       !INPUT VALUE(S)
              IF (WGT(IVAL).GT.0.) THEN                !VALID INPUT VALUE
                ISLOT=MIN(ISLOT1+IVAL-1,ISLOT2)       !SLOT NR
		WEIGHT = WGT(IVAL)                    
		VALIN = VAL(IVAL)                      
		WVALIN = WEIGHT*VALIN 
                ACCU(AC_MIN,ISLOT) = 
     1               MIN(ACCU(AC_MIN,ISLOT),VALIN)     !MINIMUM VALUE
                ACCU(AC_MAX,ISLOT) = 
     1               MAX(ACCU(AC_MAX,ISLOT),VALIN)     !MAXIMUM VALUE
                ACCU(AC_WSUM,ISLOT) = 
     1               ACCU(AC_WSUM,ISLOT) + WVALIN      !WEIGHTED SUM
                ACCU(AC_WSS,ISLOT) = 
     1               ACCU(AC_WSS,ISLOT) + WVALIN*VALIN !WGT SUM SQU
                IF (ACCU(AC_WTOT,ISLOT).GT.0) THEN     !EXCLUDE FIRST TIME
                  ACCU(AC_WSSV,ISLOT) = ACCU(AC_WSSV,ISLOT) + 
     1                 WEIGHT*((VALIN-ACCU(AC_LAST,ISLOT))**2) !VARIATION
		END IF 
                ACCU(AC_WTOT,ISLOT) = 
     1               ACCU(AC_WTOT,ISLOT) + WEIGHT      !TOTAL WEIGHT
                ACCU(AC_LAST,ISLOT) = VALIN            !KEEP LAST VALUE
C
	        I = I+1                                !INCREMENT COUNTER
	        IF (ITRACE.GT.1) THEN                  !TRACING
                  CALL WNCTXT (F_TP,'NFLST1 (!UJ): '//ACT(:3)
     1               //' [!UJ] acc: slot !4$UJ (!4$UJ) wgt=!8$F8.2 '
     1               //NAM,IGRP,NVAL,ISLOT,IVAL,WGT(IVAL))
	        END IF
              END IF
	    END DO                                     !NEXT IVAL
C
	    IF (ITRACE.EQ.1) THEN                       !TRACING
              CALL WNCTXT (F_TP,'NFLST1 (!UJ): '//ACT(:3)
     1           //' [!UJ] acc: slot nrs !UJ-!UJ '
     1           //' (nonzero=!UJ) '
     1           //NAM,ITRACE,NVAL,ISLOT1,ISLOT2,I)
	    END IF
C
          END DO                                       !NEXT IDOGRP
C
C  INITialise: Release (de-assign) all GRPs
C
        ELSE IF (ACT(:4).EQ.'INIT') THEN          !INITIALISE ALL GRPS
	  CONTINUE                                !DONE ALREADY
C
!*** DELete (de-assign) an existing GRP:
C
        ELSE IF (ACT(:3).EQ.'DEL') THEN
	  IGRP = DOGRP(NDOGRP)
	  NSLOTS = ISLOT12(2,IGRP)-ISLOT12(1,IGRP)+1    !NR OF DEASS. SLOTS
	    DO IGRP = DOGRP(NDOGRP),IGRPFREE-1          !GROUPS TO BE MOVED
	      ISLOT1 = ISLOT12(1,IGRP)
	      ISLOT2 = ISLOT12(2,IGRP)
	      DO ISLOT = ISLOT1,ISLOT2                  !SLOT NR
	        I1 = ISLOT-NSLOTS			!DESTINATION SLOT NR
	          DO I=1,MXNSTPAR			!
	          ACCU(I,I1) = ACCU(I,ISLOT) 		!MOVE SLOT CONTENTS
	        END DO
	      END DO
	      ISLOT12(1,IGRP-1) = ISLOT1-NSLOTS		!MOVE GROUP DOWN
	      ISLOT12(2,IGRP-1) = ISLOT2-NSLOTS		!MOVE GROUP DOWN
	      GRPNAME(IGRP-1) = GRPNAME(IGRP)		!MOVE GROUP DOWN
	      UNITXT(IGRP-1) = UNITXT(IGRP)		!MOVE GROUP DOWN
	    END DO
	    IGRPFREE = IGRPFREE - 1                     !FIRST FREE GROUP
	    GRPNAME(IGRPFREE) = C_UNDEF
	    UNITXT(IGRPFREE) = C_UNDEF_UNIT
	    ISLOT12(1,IGRPFREE) = 0			!
	    ISLOT12(2,IGRPFREE) = 0			!
	    ISLOTFREE = ISLOTFREE - NSLOTS		!FIRST FREE SLOT
C
	  IF (ITRACE.GT.3) THEN                         !TRACING
            CALL WNCTXT (F_TP,'NFLST1 (!UJ): '//ACT(:3)
     1           //' [!UJ] delete group: '
     1           //NAM,ITRACE,NVAL)
	    IF (ITRACE.GT.4) SHOW_GROUPS = .TRUE.
	  END IF
C
!*** RESET the accumulator buffer(s) of a specified GRP to initial values:
C
        ELSE IF (ACT(:5).EQ.'RESET') THEN
	  CONTINUE                                  !DONE ABOVE ALREADY
C
!*** ASSIGN: Assign an accumulator GRP with the givem name: 
C
        ELSE IF (ACT(:3).EQ.'ASS') THEN
          CONTINUE                                  !DONE ABOVE ALREADY
C
C***************************************************************************
C GET some specified quantities:
C
        ELSE IF (ACT(:3).EQ.'GET') THEN
C
          IF (TEXTIO(:4).EQ.'LENG') THEN
	    NVALS = NSLOTS                           !Return nr of slots
C
          ELSE IF (TEXTIO(:5).EQ.'FIRST') THEN
	    NVALS = ISLOT1                           !Return first slot nr
C
          ELSE IF (TEXTIO(:4).EQ.'LAST') THEN
	    NVALS = ISLOT2                           !Return last slot nr
C
          ELSE IF (TEXTIO(:4).EQ.'NGRO') THEN
	    NVALS = IGRPFREE-1                       !Return nr of defined grps
C
          ELSE IF (TEXTIO(:6).EQ.'FREEGR') THEN
	    NVALS = MXNGRP - IGRPFREE + 1            !Return nr of free groups
C
          ELSE IF (TEXTIO(:6).EQ.'FREESL') THEN
	    NVALS = MXNSLOT - ISLOTFREE + 1          !Return nr of free slots
C
          ELSE IF (TEXTIO(:4).EQ.'NAME') THEN
	    IGRP = NVAL                             !Input group nr
	    IF (IGRP.GT.0.AND.IGRP.LT.IGRPFREE) THEN
	      NAME = GRPNAME(IGRP)                  !Return group name (NAME)
	    ELSE
              CALL WNCTXT (F_TP,'NFLST1: (GET NAME): '
     1                    //' IGRP=!UJ, out of range',IGRP)
	    END IF
C
          ELSE IF (ACT(:8).EQ.'GET_UNIT') THEN
	    IGRP = DOGRP(NDOGRP)
            TEXTIO = UNITXT(IGRP)                    !Retrun unit-text 
C
	  ELSE
	    ARGSTR='NFLST1 ('//ACT(:3)//'): '//' not recognised '//TEXTIO
            CALL WNCTXT (F_TP,ARGSTR)
	    GOTO 900
	  END IF
C
C***************************************************************************
!*** SET some values:
C
        ELSE IF (ACT(:3).EQ.'SET') THEN          !.......
	  I = INDEX(TEXTIO,'UNIT=')
	  IF (I.GT.0) THEN
	    DO IDOGRP=1,NDOGRP
	      IGRP = DOGRP(IDOGRP)
              UNITXT(IGRP) = TEXTIO(I+5:)            !`UNIT' OF GRP QTY 
	    END DO
	  END IF
C
C******************************************************************************
C
C******************************************************************************
!return statistical result(s):
C 
        ELSE IF (ACT(:7).EQ.'EXPLAIN') THEN
          IF (NAM(:4).EQ.'MEAN') THEN
            CALL WNCTXS (TXT80,'   mean: average value')
          ELSEIF (NAM(:5).EQ.'RMSMS') THEN
            CALL WNCTXS (TXT80,'  rmsms: rms w.r.t. the mean')
          ELSEIF (NAM(:6).EQ.'RMSVAR') THEN
            CALL WNCTXS (TXT80,' rmsvar: rms variation '
     1                        //' (of successive values)')
          ELSEIF (NAM(:3).EQ.'RMS') THEN
            CALL WNCTXS (TXT80,'    rms: rms value')
          ELSEIF (NAM(:5).EQ.'DCOFF') THEN
            CALL WNCTXS (TXT80,'  dcoff: dc offset indicator'
     1                        //' (=mean/max(rmsms,rmsvar))')
          ELSEIF (NAM(:3).EQ.'MIN') THEN
            CALL WNCTXS (TXT80,'    min: minimum value')
          ELSEIF (NAM(:3).EQ.'MAX') THEN
            CALL WNCTXS (TXT80,'    max: maximum value')
          ELSEIF (NAM(:4).EQ.'WTOT') THEN
            CALL WNCTXS (TXT80,'   wtot: total weight '
     1                        //' (often: nr of values)')
	  ELSE
	    ARGSTR='Explain: name not recognised '//NAME
            CALL WNCTXT (F_TP,ARGSTR)
	    GOTO 900
	  END IF
C
	  CALL WNCTXT (F_TP,'# !AS !#C!AS',TXT80,NCSLIN,'#')
	  CALL WNCTXS (TEXTIO,'# !AS !#C!AS',TXT80,NCSLIN,'#')
C
C*************************************************************************
C CALCULATE STATISTICAL QUANTITIES:
C
        ELSE IF (ACT(:4).EQ.'CALC') THEN
          IF (NDOGRP.EQ.1) THEN
	    IGRP = DOGRP(NDOGRP)                    !GROUP NR
	    ISLOT1 = ISLOT12(1,IGRP)                !FIRST SLOT
	    ISLOT2 = ISLOT12(2,IGRP)                !LAST SLOT
	    NSLOTS = ISLOT2-ISLOT1+1                !NR OF SLOTS
	    IF (NVAL.EQ.1) THEN                     !OVER ALL SLOTS OF GROUP
	      IVAL1 = 1                             !
	      IVAL2 = 1                             !SINGLE OUTPUT VALUE
	    ELSE IF (NVAL.EQ.NSLOTS) THEN           !INDIVIDUALLY, ARRAYS
	      IVAL1 = 1
	      IVAL2 = NVAL                          !OUTPUT ARRAY
	    ELSE IF ((NVAL.LT.0).AND.(ABS(NVAL).LE.NSLOTS)) THEN      !
	      ISLOT1 = ISLOT1 + ABS(NVAL)-1         !SPECIFIC SLOT OF GROUP
	      ISLOT2 = ISLOT1
	      IVAL1 = 1
	      IVAL2 = 1                             !SINGLE OUTPUT VALUE
	    ELSE
              CALL WNCTXT (F_TP,'NFLST1 (!UJ): '//ACT(:3)
     1           //' acc: inadmissible nval=!SJ '
     1           //' (nslots=!UJ) '
     1           //NAM,NVAL,NSLOTS)
	      GOTO 900                              !........?
            END IF
	  ELSE
	    ARGSTR='NFLST1 ('//ACT(:3)//'): '//' NDOGRP<>1 '//TEXTIO 
            CALL WNCTXT (F_TP,ARGSTR)
	    GOTO 900                                 !........!!
	  END IF
C
	  CALC = 0
          IF (TEXTIO(:4).EQ.'MEAN')   CALC = CALC_MEAN
          IF (TEXTIO(:3).EQ.'RMS')    CALC = CALC_RMS
          IF (TEXTIO(:5).EQ.'RMSMS')  CALC = CALC_RMSMS
          IF (TEXTIO(:6).EQ.'RMSVAR') CALC = CALC_RMSVAR
          IF (TEXTIO(:3).EQ.'MIN')    CALC = CALC_MIN
          IF (TEXTIO(:3).EQ.'MAX')    CALC = CALC_MAX
          IF (TEXTIO(:5).EQ.'DCOFF')  CALC = CALC_DCOFF
          IF (TEXTIO(:4).EQ.'WTOT')   CALC = CALC_WTOT
	  IF (CALC.EQ.0) THEN
	    ARGSTR='NFLST1 ('//ACT(:3)//'): ' //' not recognised '//TEXTIO
            CALL WNCTXT (F_TP,ARGSTR)
	    GOTO 900
	  END IF
C
	  OV_MEAN = 0                              !OVERALL MEAN          
	  OV_WTOT = 0				   !OVERALL WTOT
          OV_MEANSQ = 0
          OV_MSVAR = 0
          OV_MINVAL = VERYLARGE
          OV_MAXVAL = -VERYLARGE
C
	  DO ISLOT=ISLOT1,ISLOT2                     !SLOT NRS
            WTOT = ACCU(AC_WTOT,ISLOT) 
            I = MIN(ISLOT-ISLOT1+1,IVAL2)            !I/O ADDRESS IN VAL/WGT
            WGT(I) = WTOT                            !return total weight
            VAL(I) = 0                               !default value (?)
            IF (WTOT.GT.0) THEN                      !wtot>0
              MEAN   = ACCU(AC_WSUM,ISLOT)/WTOT      !mean
              MEANSQ = ACCU(AC_WSS,ISLOT)/WTOT       !mean square
              RMS    = SQRT(MAX(0.,MEANSQ))          !rms
              RMSMS  = SQRT(MAX(0.,MEANSQ-MEAN**2))  !rmsms
              MSVAR  = ACCU(AC_WSSV,ISLOT)/WTOT      !mean square variation
              RMSVAR = SQRT(MAX(0.,MSVAR))           !rms variation
              MINVAL = ACCU(AC_MIN,ISLOT)            !maximum value
              MAXVAL = ACCU(AC_MAX,ISLOT)            !maximum value
	      IF (CALC.EQ.CALC_MEAN) THEN
                VAL(I) = MEAN                        !return mean
	      ELSE IF (CALC.EQ.CALC_RMS) THEN
                VAL(I) = RMS                         !return rms
	      ELSE IF (CALC.EQ.CALC_RMSMS) THEN
                VAL(I) = RMSMS                       !return rmsms
	      ELSE IF (CALC.EQ.CALC_RMSVAR) THEN
                VAL(I) = RMSVAR                      !return rmsvar
	      ELSE IF (CALC.EQ.CALC_MIN) THEN
                VAL(I) = MINVAL                      !return minval
	      ELSE IF (CALC.EQ.CALC_MAX) THEN
                VAL(I) = MAXVAL                      !return maxval
	      ELSE IF (CALC.EQ.CALC_WTOT) THEN
                VAL(I) = WTOT                        !return wtot
	      ELSE IF (CALC.EQ.CALC_DCOFF) THEN
                R0 = MAX(RMSVAR,RMSMS)
	        IF (R0.GT.0) VAL(I) = 100*(MEAN/R0)  !`dc offset' indicator
	        IF (R0.LE.0) WGT(I) = -1             !something wrong..... 
	      END IF
	      OV_MINVAL = MIN(OV_MINVAL,MINVAL)	     !overall
	      OV_MAXVAL = MAX(OV_MAXVAL,MAXVAL)	     !overall
	      OV_WTOT   = OV_WTOT + WTOT             !overall wtot
	      OV_MEAN   = OV_MEAN + MEAN*WTOT	     !overall
	      OV_MEANSQ = OV_MEANSQ + MEANSQ*WTOT    !overall
	      OV_MSVAR  = OV_MSVAR + MSVAR*WTOT	     !overall
            END IF
          END DO
C
          NFLST1 = OV_WTOT                           !Function value (?)
          WTOT   = OV_WTOT
	  IF (WTOT.GT.0) THEN
            MEAN   = OV_MEAN/WTOT
            MEANSQ = OV_MEANSQ/WTOT
            RMSMS  = SQRT(MAX(0.,MEANSQ-MEAN**2))   
            RMS    = SQRT(MAX(0.,MEANSQ))   
            MSVAR  = OV_MSVAR/WTOT
            RMSVAR = SQRT(MAX(0.,MSVAR))
	    MINVAL = OV_MINVAL   
	    MAXVAL = OV_MAXVAL   
	    IF (CALC.EQ.CALC_MEAN) THEN
                NFLST1 = MEAN                        !return mean
	        DWARFBUF='MEAN'
	    ELSE IF (CALC.EQ.CALC_RMS) THEN
                NFLST1 = RMS                         !return rms
	        DWARFBUF='RMS'
	    ELSE IF (CALC.EQ.CALC_RMSMS) THEN
                NFLST1 = RMSMS                       !return rmsms
	        DWARFBUF='RMSMS'
	    ELSE IF (CALC.EQ.CALC_RMSVAR) THEN
                NFLST1 = RMSVAR                      !return rmsvar
	        DWARFBUF='RMSVAR'
	    ELSE IF (CALC.EQ.CALC_MIN) THEN
                NFLST1 = MINVAL                      !return minval
	        DWARFBUF='MINVAL'
	    ELSE IF (CALC.EQ.CALC_MAX) THEN
                NFLST1 = MAXVAL                      !return maxval
	        DWARFBUF='MAXVAL'
	    ELSE IF (CALC.EQ.CALC_WTOT) THEN
                NFLST1 = WTOT                        !return wtot
	        DWARFBUF='WTOT'
	    ELSE IF (CALC.EQ.CALC_DCOFF) THEN
                R0 = MAX(RMSVAR,RMSMS)
	        IF (R0.GT.0) NFLST1 = 100*(MEAN/R0)  !`dc offset' indicator
	        IF (R0.LE.0) NFLST1 = -1             !something wrong..... 
	        DWARFBUF='DCOFF'
	    END IF
	    CALL WNCTXS(TXT80,'!E',NFLST1)	     !and save as symbol
	    CALL WNDPAG(DWARFBUF,TXT80)
	    CALL WNCTXT(F_T,
	1	'You may use !AS in respons to prompts '//
	1	'(e.g. CLIPLEVEL=3*!AS)',DWARFBUF,DWARFBUF)
	  END IF
          IF (IVAL2.EQ.1) THEN
            VAL(1) = NFLST1                        ! single output value
            WGT(1) = WTOT                          ! single output value
          END IF
C
C Appraise the nature of the statistics and put it in a string:
C
        ELSE IF (ACT(:7).EQ.'APPRAIS') THEN
	  DO ISLOT=ISLOT1,ISLOT2                     !SLOT NRS
            WTOT = ACCU(AC_WTOT,ISLOT) 
            IF (WTOT.GT.0) THEN                      !wtot>0
              MEAN = ACCU(AC_WSUM,ISLOT)/WTOT        !mean
              MEANSQ = ACCU(AC_WSS,ISLOT)/WTOT       !mean square
              RMSMS  = SQRT(MAX(0.,MEANSQ-MEAN**2))  !rmsms
              MSVAR  = ACCU(AC_WSSV,ISLOT)/WTOT      !mean square variation
              RMSVAR = SQRT(MAX(0.,MSVAR))           !rms variation
              MINVAL = ACCU(AC_MIN,ISLOT)               !maximum value
              MAXVAL = ACCU(AC_MAX,ISLOT)               !maximum value
	      OV_MINVAL = MIN(OV_MINVAL,MINVAL)	     !overall
	      OV_MAXVAL = MAX(OV_MAXVAL,MAXVAL)	     !overall
	      OV_WTOT   = OV_WTOT + WTOT             !overall wtot
	      OV_MEAN   = OV_MEAN + MEAN*WTOT	     !overall
	      OV_MEANSQ = OV_MEANSQ + MEANSQ*WTOT    !overall
	      OV_MSVAR  = OV_MSVAR + MSVAR*WTOT	     !overall
            END IF
          END DO
          NFLST1 = OV_WTOT                           !Function value
	  TEXTIO = ' '                               !appraisal text  
	  IF (OV_WTOT.GT.0) THEN
            MEAN   = OV_MEAN/OV_WTOT
            MEANSQ = OV_MEANSQ/OV_WTOT
            RMSMS  = SQRT(MAX(0.,OV_MEANSQ-OV_MEAN**2))   
            RMS    = SQRT(MAX(0.,OV_MEANSQ))   
            MSVAR  = OV_MSVAR/OV_WTOT
            RMSVAR = SQRT(MAX(0.,OV_MSVAR/OV_WTOT))
	    MINVAL = OV_MINVAL   
	    MAXVAL = OV_MAXVAL   
          ELSE
	    TEXTIO = ' wtot=0'  
	  END IF
C
C******************************************************************************
C******************************************************************************
C  SHOW
C
        ELSE IF (ACT(:4).EQ.'SHOW') THEN
          J1 = 7                                  !NR OF CHARS PER FIELD
          J2 = 14                                 !START CHAR IN TXT80
          J3 = J2+7*J1 + 2                        !START CHAR IN TXT80
C          
C---------------------------------------------------------------------------
          IF (NAM(:1).EQ.'#') THEN               !SPECIAL NAME-STRING
C
            IF (NAM(:7).EQ.'#GROUPS') THEN            
	      CALL WNCTXT(F_TP,'Statistical data is collected '//
	1		'into so-called accumulation groups')
	      CALL WNCTXT(F_TP,'This option gives a list of the '//
	1		'currently defined accumulation groups')
	      CALL WNCTXT(F_TP,'It is mainly useful for '//
	1		'debugging purposes.')
	      CALL WNCTXT (F_TP,
     1             ' Defined accumulation groups: '
     1             //' (max groups=!UJ, max slots=!UJ) '
     1             ,MXNGRP,MXNSLOT) 
	      DO IGRP=1,MXNGRP
	        IF (GRPNAME(IGRP).NE.C_UNDEF) THEN
                  ISLOT1 = ISLOT12(1,IGRP)
                  ISLOT2 = ISLOT12(2,IGRP)
		  NSLOTS = ISLOT2-ISLOT1+1
		  I=0
		  DO ISLOT=ISLOT1,ISLOT2
		    IF (ACCU(AC_WTOT,ISLOT).GT.0) I=I+1   !ACTIVE SLOTS
		  END DO
                  CALL WNCTXT (F_TP,' group !2$UJ:'
     1                //' !4$UJ slots (!4$UJ-!4$UJ) '
     1                //' acc=!4$UJ  name='//GRPNAME(IGRP)
     1                //' '//UNITXT(IGRP)     
     1                ,IGRP,NSLOTS,ISLOT1,ISLOT2,I)
                END IF	    
	      END DO
              GOTO 900
C
            ELSE IF (NAM(:4).EQ.'#HEA') THEN              !PRINT HEADER STRING
              TXT80 = '# <name>'
              TXT80(J2+0*J1:) = '   wtot'
              TXT80(J2+1*J1:) = '   mean'
              TXT80(J2+2*J1:) = '    rms'
              TXT80(J2+3*J1:) = '  rmsms'
              TXT80(J2+4*J1:) = ' rmsvar'
              TXT80(J2+5*J1:) = ' minval'
              TXT80(J2+6*J1:) = ' maxval'
              TXT80(J3:) = C_UNDEF_UNIT
              TXT80(NCSLIN:) = '#'                    !CLOSING HASH (#)
              CALL WNCTXT (F_TP,'!AS',TXT80)          !PRINT LINE
              GOTO 900                                !ESCAPE
C
            ELSE IF (NAM(:4).EQ.'#SEP') THEN          !PRINT SEPARATOR
              CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
              GOTO 900                                !ESCAPE
C
            ELSE IF (NAM(:5).EQ.'#TEXT') THEN         !PRINT GIVEN TEXT LINE
              CALL WNCTXS (TXT80,'# !AS: ',TEXTIO)    !
              TXT80(NCSLIN:) = '#'                    !CLOSING HASH (#)
              CALL WNCTXT (F_TP,'!AS',TXT80)          !PRINT LINE
              GOTO 900                                !ESCAPE
C
	    ELSE
              GOTO 900                                !ERROR, ESCAPE
            END IF
C
          END IF
C------------------------------------------------------------------------
C
          DO IDOGRP = 1,NDOGRP                         !Groups to be done
	    IGRP = DOGRP(IDOGRP)                       !Group nr
            ISLOT1 = ISLOT12(1,IGRP)
	    ISLOT2 = ISLOT12(2,IGRP)
            NSLOTS = ISLOT12(2,IGRP)-ISLOT12(1,IGRP)+1
C
            CALL WNCTXS (TXT80,'# !AS: ',GRPNAME(IGRP))
	    IF (NVAL.EQ.1) THEN                        !Show overall values
              CONTINUE
	    ELSE IF ((NVAL.NE.0).AND.(ABS(NVAL).LE.NSLOTS)) THEN  !single slot
	      ISLOT1 = ISLOT1 + ABS(NVAL) - 1          !Specified slot nr
	      ISLOT2 = ISLOT1
            ELSE
              CALL WNCTXT (F_TP,'NFLST1 (SHOW): '
     1                      //'NVAL=!SJ, out of range: !AS '
     1                      ,NVAL,GRPNAME(IGRP)) 
	      GOTO 900
	    END IF
C
	      WTOT = 0
	      MEAN = 0
	      MEANSQ = 0
	      MSVAR = 0
	      MINVAL = VERYLARGE
	      MAXVAL = -VERYLARGE
	      DO ISLOT=ISLOT1,ISLOT2                      !All specified slots
                IF (ACCU(AC_WTOT,ISLOT).GT.0) THEN	    !wtot>0 
                  WTOT = WTOT + ACCU(AC_WTOT,ISLOT)         !wtot
                  MEAN   = MEAN + ACCU(AC_WSUM,ISLOT)       !mean
                  MEANSQ = MEANSQ + ACCU(AC_WSS,ISLOT)      !mean square
                  MINVAL = MIN(MINVAL,ACCU(AC_MIN,ISLOT))   !minimum value
                  MAXVAL = MAX(MAXVAL,ACCU(AC_MAX,ISLOT))   !maximum value
                  MSVAR  = MSVAR + ACCU(AC_WSSV,ISLOT)      !mean sq variation
		END IF        
	      END DO                                    !ISLOT
	      IF (WTOT.GT.0) THEN
		MEAN = MEAN/WTOT                        !mean
		MEANSQ = MEANSQ/WTOT			!mean square
		MSVAR = MSVAR/WTOT			!mean sq variation
                RMSMS  = SQRT(MAX(0.,MEANSQ-MEAN**2))   !rmsms
                RMS    = SQRT(MAX(0.,MEANSQ))           !rms
                RMSVAR = SQRT(MAX(0.,MSVAR))            !rms variation
                CALL WNCTXS (TXT80(J2+0*J1:),'!#$F#.0',J1,J1,WTOT)
                CALL WNCTXS (TXT80(J2+1*J1:),'!#$F#.0',J1,J1,MEAN) 
                CALL WNCTXS (TXT80(J2+2*J1:),'!#$F#.0',J1,J1,RMS)
                CALL WNCTXS (TXT80(J2+3*J1:),'!#$F#.0',J1,J1,RMSMS)
                CALL WNCTXS (TXT80(J2+4*J1:),'!#$F#.0',J1,J1,RMSVAR)
                CALL WNCTXS (TXT80(J2+5*J1:),'!#$F#.0',J1,J1,MINVAL)
                CALL WNCTXS (TXT80(J2+6*J1:),'!#$F#.0',J1,J1,MAXVAL)
                CALL WNCTXS (TXT80(J3+1:),UNITXT(IGRP)) !`unit' text
              ELSE
                CALL WNCTXS (TXT80(J2+0*J1:),
     1                     ' No values accumulated (wtot=0)') 
              END IF          
              TXT80(NCSLIN:) = '#'                     !CLOSING HASH (#)
              CALL WNCTXT (F_TP,'!AS',TXT80)           !PRINT LINE
          END DO                                       !IGRP
C
C****************************************************************************
C
        ELSE                                             !
	  ARGSTR='NFLST1: '//' Action not recognised: '//ACTION
          CALL WNCTXT (F_TP,ARGSTR)
          GOTO 900                                       !EXIT
        END IF                                           !OF ACTIONS
C
C****************************************************************************
 900    CONTINUE   
C
	IF (SHOW_GROUPS) THEN
	  CALL WNCTXT (F_TP,'NFLST1 (defined groups): '
     1         //' IGRPFREE=!UJ  ISLOTFREE=!UJ'
     1         ,IGRPFREE,ISLOTFREE) 
	  DO IGRP=1,MXNGRP
	    IF (GRPNAME(IGRP).NE.C_UNDEF) THEN
              ISLOT1 = ISLOT12(1,IGRP)
              ISLOT2 = ISLOT12(2,IGRP)
              CALL WNCTXT (F_TP,' !2$UJ: slots !4$UJ-!4$UJ '
     1            //GRPNAME(IGRP)//' '//UNITXT(IGRP)     
     1            ,IGRP,ISLOT1,ISLOT2)
            END IF	    
	  END DO
          CALL WNCTXT (F_TP,' ')
	END IF
C
	RETURN
	END












