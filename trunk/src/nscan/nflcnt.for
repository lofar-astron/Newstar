C+ NFLCNT.FOR
C  JEN930918
C
C  Revisions:
C	JPH 940929	SHOW: Give message only when no counts available 
C	CMV 941023	Properly terminate line (J3:J3) -> (J3:)
C	CMV 960122	Change backslash to space in IFR output
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	SUBROUTINE NFLCNT (ACTION,NAME,FLAG,MASK,IFRA,ICHAN,HA)
C
C  Count flags that have been set:
C
C  Result:
C
C	CALL NFLCNT (ACTION_C(*):I,NAME_C(*):I,
C                    FLAG_J(0:*):I,MASK_J(0:*):I,
C                    IFRA_J(0:1,0:STHIFR-1):I,
C                    ICHAN_J:I,HA_R:I)
C
C       CALL NFLCNT ('RESET',' ',0,0,0,0,0)
C
C       CALL NFLCNT ('ACC','DATA',flacc(0:STHIFR-1,0:3),
C                                  mask(0:STHIFR-1,0:3),
C                                  IFRA(0:1,0:STHIFR),ICHAN,HA)
C       CALL NFLCNT ('ACC','HEAD',flacc(0),mask(0),0,ICHAN,HA)
C
C       CALL NFLCNT ('SHOW','FTYP',0,0,0,0,0)
C       CALL NFLCNT ('SHOW','TEL',0,0,0,0,0)
C       CALL NFLCNT ('SHOW','IFR',0,0,0,0,0)
C       CALL NFLCNT ('SHOW','CHA',0,0,0,0,0)
C
C       CALL NFLCNT ('SHOW','HA',0,0,0,0,0)         !ALL POLS
C       CALL NFLCNT ('SHOW','HA_XX',0,0,0,0,0)      !XX 
C       CALL NFLCNT ('SHOW','HA_YY',0,0,0,0,0)      !YY 
C       CALL NFLCNT ('SHOW','HA_XY',0,0,0,0,0)      !XX,YY
C       CALL NFLCNT ('SHOW','HA_YX',0,0,0,0,0)      !XY.YX
C
C PIN references:
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
	INCLUDE 'CBITS_DEF'
	INCLUDE 'STH_O_DEF'		!SET HEADER
C
C  Parameters:
C
	INTEGER XX,XY,YX,YY		!POL. POINTERS
	  PARAMETER (XX=0, XY=1, YX=2, YY=3)
C
        INTEGER IHAMIN,IHAMAX           !HA-BIN NRS (1 degr wide)
          PARAMETER (IHAMIN=-180, IHAMAX=180)
C
        INTEGER ICHMIN,ICHMAX           !CHANNEL NRS 
          PARAMETER (ICHMIN=0, ICHMAX=256)
C
        INTEGER MXNFLTYP                !# of flag types 
          PARAMETER (MXNFLTYP=8)
C
        INTEGER NCPL                    !Nr of chars printed per line
          PARAMETER (NCPL=79)
C
        CHARACTER*1 C_NOTEST,C_NOFLAG,C_ALLSET
          PARAMETER (C_NOTEST='.')      !Data item not tested
          PARAMETER (C_NOFLAG='-')      !No flags set
          PARAMETER (C_ALLSET='*')      !All (100%) flags set
C
        CHARACTER*1 C_VER,C_HOR
          PARAMETER (C_VER='#')         !Vertical edge of display frame
          PARAMETER (C_HOR='#')         !Horizontal edge of display frame
        CHARACTER*79 SEPAR              !Separator string (see SHOW)
          PARAMETER (SEPAR=
     1          '########################################'//     
     1          '#######################################')     
C
C  Arguments:
C
        CHARACTER ACTION*(*)            !ACTION TO BE PERFORMED
        CHARACTER NAME*(*)              !CLOSER SPECIFICATION OF ACTION
        INTEGER   FLAG(0:*)             !HEADER/DATA FLAG(S)
C                                       !  HEADER: FLAG(0:0)
C                                       !  DATA  : FLAG(0:STHIFR-1,0:3)
        INTEGER   MASK(0:*)             !FLAGBYTE MASK(S) USED
C                                       !  CONFORMAL WITH FLAG(0:*)
        INTEGER   IFRA(0:1,0:STHIFR-1)  !TELESCOPE TABLE (W,E)
        INTEGER   ICHAN                 !FREQUENCY CHANNEL NR
        REAL      HA                    !HA OF SCAN
C
C  Function references:
C
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
C  Array with flag-types and flag-codes
C
        CHARACTER*4 FLAGNAME(0:MXNFLTYP-1)
        DATA FLAGNAME /'MAN','CLIP','NOIS','ADD','SHAD',
     1                 'U3','U2','U1'/          ! exclude ,'ALL'/
        INTEGER FLAGTYPE(0:MXNFLTYP-1)
        DATA FLAGTYPE /FL_MAN,FL_CLIP,FL_NOIS,FL_ADD,FL_SHAD,
     1                 FL_3,FL_2,FL_1/          ! exclude ,FL_ALL/
C
C  Variables:
C
        INTEGER      N
        INTEGER      IHA                !NR OF HA-BIN
	REAL 	     HA1, HA2		! HA range counted
        INTEGER      RTW,RTE            !WEST,EAST TELESCOPE NR
        INTEGER      LIN,COL            !LINE, COLUMN NR
        INTEGER      HEADMASK,DATAMASK  !UTILISED MASKS
        INTEGER      NND,NFD,NNH,NFH    !HELP VARIABLES
        LOGICAL      SELPOL(-1:3)       !POL SELECTION (SHOW)
	LOGICAL      PRINTLINE          !SWITCH 
C
C  Storage areas, buffer arrays
C
        CHARACTER*80 TXT80                    !GENERAL TEXT BUFFER
        CHARACTER*80 TEXT(-5:STHTEL+5)        !TEL-TEL MATRIX 
        CHARACTER*80 TXT_LEGEND               !LEGEND STRING
        CHARACTER*80 TXT_SCANS                !SCANS TESTED (HA,CH)
        CHARACTER*80 TXT_DATA                 !DATA TESTED (POL,IFRS)
        CHARACTER*80 TXT_FLAGS                !FLAGS TESTED (HEADERS,DATA)
        CHARACTER*80 TXT_CALC                 !
        CHARACTER*80 ARGSTR
C
C HA-range and Channel-range of counted Scans:
C
        REAL    HARANH(0:1)             !HA-RANGE OF TESTED SCAN HEADERS
        REAL    HARAND(0:1)             !HA-RANGE OF TESTED SCAN DATA
        INTEGER CHRANH(0:1)             !CHANNEL-RANGE OF TESTED SCAN HEADERS
        INTEGER CHRAND(0:1)             !CHANNEL-RANGE OF TESTED SCAN DATA
C
C  Flag count buffers:
C
        INTEGER NFPTH(0:MXNFLTYP-1)     !# OF SET FLAGS PER TYPE (IN HEADERS)
        INTEGER NNPTH(0:MXNFLTYP-1)     !# OF TESTED SCAN HEADERS PER FLAGTYPE 
        INTEGER NFPTD(0:3,0:MXNFLTYP-1) !# OF FLAGS PER TYPE PER POL (IN DATA)
        INTEGER NNPTD(0:3,0:MXNFLTYP-1) !# OF TESTED DATA PER TYPE,POL(IN DATA)
C
        INTEGER NFPHH(IHAMIN:IHAMAX)    !# OF SET FLAGS PER HA-BIN (IN HEADERS)
        INTEGER NNPHH(IHAMIN:IHAMAX)    !# OF TESTED SCAN HEADERS PER HA-BIN 
        INTEGER NFPHD(0:3,IHAMIN:IHAMAX)!# OF FLAGS PER HA-BIN,POL (IN DATA)
        INTEGER NNPHD(0:3,IHAMIN:IHAMAX)!# OF TESTED DATA PER HA-BIN,POL(DATA)
C
        INTEGER NFPCH(ICHMIN:ICHMAX)    !# OF SET FLAGS PER CHAN (IN HEADERS)
        INTEGER NNPCH(ICHMIN:ICHMAX)    !# OF TESTED SCAN HEADERS PER CHAN 
        INTEGER NFPCD(0:3,ICHMIN:ICHMAX)!# OF FLAGS PER CHAN,POL (IN DATA)
        INTEGER NNPCD(0:3,ICHMIN:ICHMAX)!# OF TESTED DATA PER CHAN,POL(DATA)
C
        INTEGER NPI(0:3,0:STHTEL-1,0:STHTEL-1) !# FLAGS/TESTED PER IFR PER POL
C
        INTEGER NFPP(0:3)               !# OF SET FLAGS PER POL
        INTEGER NNPP(0:3)               !# OF TESTED DATA PER POL
C
        INTEGER NFPT(0:3,0:STHTEL-1)    !# OF SET FLAGS PER TEL (DIP), PER POL
        INTEGER NNPT(0:3,0:STHTEL-1)    !# OF TESTED DATA PER TEL, PER POL
C-
C******************************************************************************
C******************************************************************************
C******************************************************************************
!*** RESET flag-count-buffers:
C
        IF (ACTION(:5).EQ.'RESET') THEN
C
          HEADMASK = 0                         !UTILISED MASK
          DATAMASK = 0                         !UTILISED MASK
C
          DO I=0,MXNFLTYP-1                    !PER FLAG TYPE
            NFPTH(I) = 0                       !SCAN HEADERS
            NNPTH(I) = 0 
            DO I3=0,3
              NFPTD(I3,I) = 0                  !UV-DATA
              NNPTD(I3,I) = 0   
            END DO
          END DO
C
          DO I=IHAMIN,IHAMAX                   !PER HA
            NFPHH(I) = 0                       !SCAN HEADERS
            NNPHH(I) = 0   
            DO I3=0,3
              NFPHD(I3,I) = 0                  !UV-DATA
              NNPHD(I3,I) = 0   
            END DO
          END DO
          HARANH(0) = +181                     !DEGR
          HARANH(1) = -181
          HARAND(0) = +181
          HARAND(1) = -181
C
          DO I=ICHMIN,ICHMAX                   !PER FREQU CHANNEL
            NFPCH(I) = 0                       !SCAN HEADERS
            NNPCH(I) = 0   
            DO I3=0,3
              NFPCD(I3,I) = 0                  !UV-DATA
              NNPCD(I3,I) = 0   
            END DO
          END DO
          CHRANH(0) = ICHMAX+1
          CHRANH(1) = ICHMIN-1
          CHRAND(0) = ICHMAX+1
          CHRAND(1) = ICHMIN-1
C
          DO I3=0,3                            !PER POL
            NFPP(I3) = 0 
            NNPP(I3) = 0  
            DO I=0,STHTEL-1                    !PER TEL
              NFPT(I3,I) = 0   
              NNPT(I3,I) = 0   
              DO I1=0,STHTEL-1
                NPI(I3,I,I1) = 0               !PER IFR
              END DO
            END DO
          END DO
C
C
C******************************************************************************
!*** ACCUMULATE flag-counts:
C
        ELSE IF (ACTION(:3).EQ.'ACC') THEN
C
          IHA = NINT(HA*360)               !HA-BIN NR, convert from circles
C
          IF (ICHAN.LT.ICHMIN .OR. ICHAN.GT.ICHMAX) THEN
            CALL WNCTXT (F_TP,'NFLCNT ACC '//NAME(:3)
     1              //': Channel nr out of range: !UJ',ICHAN)
C
          ELSE IF (IHA.LT.IHAMIN .OR. IHA.GT.IHAMAX) THEN
            CALL WNCTXT (F_TP,'NFLCNT ACC '//NAME(:3)
     1              //': IHA out of range: !UJ',IHA)
C
          ELSE IF (NAME(:3).EQ.'DAT') THEN
C
            HARAND(0) = MIN(HARAND(0),HA*360)          !HA-RANGE (DEGR)
            HARAND(1) = MAX(HARAND(1),HA*360)          !HA-RANGE
            CHRAND(0) = MIN(CHRAND(0),ICHAN)           !CHAN-RANGE (NR)
            CHRAND(1) = MAX(CHRAND(1),ICHAN)           !CHAN-RANGE
C
            DO I1=0,STHIFR-1                           !ALL IFRS
              RTW = IFRA(0,I1)                         !WEST TEL NR
              RTE = IFRA(1,I1)                         !EAST TEL NR
              DO I3=0,3                                !ALL POLS
                I2 = I3*STHIFR + I1                    !INDEX IN FLAG/MASK
                IF (MASK(I2).NE.0) THEN                !.......?
                  DATAMASK = MASK(I2)                  !KEEP `THE' MASK
C
                  DO I4=0,MXNFLTYP-1                      !ALL FLAG TYPES
                    IF (IAND(MASK(I2),FLAGTYPE(I4)).NE.0) THEN
                      NNPTD(I3,I4) = NNPTD(I3,I4) + 1     !# TESTED/TYPE
                      IF (IAND(FLAGTYPE(I4),FLAG(I2)).NE.0) THEN 
                        NFPTD(I3,I4) = NFPTD(I3,I4) + 1   !FLAGS PER TYPE 
                      END IF
                    END IF
                  END DO
C
                  NNPP(I3) = NNPP(I3) + 1              !TESTED DATA PER POL
                  NNPHD(I3,IHA) = NNPHD(I3,IHA) + 1    !TESTED DATA PER HA-BIN,
                  NNPCD(I3,ICHAN) = NNPCD(I3,ICHAN) + 1!TESTED DATA PER CHAN,
                  NPI(I3,RTE,RTW) = NPI(I3,RTE,RTW) + 1!TESTED DATA PER IFR,POL
                  NNPT(I3,RTW) = NNPT(I3,RTW) + 1      !TESTED DATA PER TEL,POL
                  NNPT(I3,RTE) = NNPT(I3,RTE) + 1      !TESTED DATA PER TEL,POL
C
                  IF (IAND(FLAG(I2),MASK(I2)).NE.0) THEN  !SELECTED FLAGS ONLY
                    NFPP(I3) = NFPP(I3) + 1               !FLAGS PER POL
                    NFPHD(I3,IHA) = NFPHD(I3,IHA) + 1     !FLAGS PER HA-BIN,POL
                    NFPCD(I3,ICHAN) = NFPCD(I3,ICHAN) + 1 !FLAGS PER CHAN,POL
                    NPI(I3,RTW,RTE) = NPI(I3,RTW,RTE) + 1 !FLAGS PER IFR.POL
                    NFPT(I3,RTW) = NFPT(I3,RTW) + 1       !FLAGS PER TEL.POL
                    NFPT(I3,RTE) = NFPT(I3,RTE) + 1       !FLAGS PER TEL,POL
                  END IF
C
                END IF                             !MASK<>0
              END DO                               !POLS (I3)
            END DO                                 !IFRS (I1)
C
C=============================================================================
C ACCUMULATE Header flags:
C
          ELSE IF (NAME(:3).EQ.'HEA') THEN
C
            HEADMASK = MASK(0)                   !KEEP `THE' UTILISED MASK
C
            HARANH(0) = MIN(HARANH(0),HA*360)    !HA-RANGE
            HARANH(1) = MAX(HARANH(1),HA*360)    !HA-RANGE
            CHRANH(0) = MIN(CHRANH(0),ICHAN)     !CHAN-RANGE
            CHRANH(1) = MAX(CHRANH(1),ICHAN)     !CHAN-RANGE
C
            DO I4=0,MXNFLTYP-1                   !ALL FLAG TYPES
              IF (IAND(MASK(0),FLAGTYPE(I4)).NE.0) THEN
                NNPTH(I4) = NNPTH(I4) + 1        !# OF TESTED SCANS PER TYPE  
                IF (IAND(FLAGTYPE(I4),FLAG(0)).NE.0) THEN
                  NFPTH(I4) = NFPTH(I4) + 1      !# OF SET FLAGS PER TYPE
                END IF 
              END IF
            END DO
C
            NNPHH(IHA) = NNPHH(IHA) + 1          !TESTED HEADERS PER HA-BIN
            NNPCH(ICHAN) = NNPCH(ICHAN) + 1      !TESTED HEADERS PER CHAN
C
            IF (IAND(FLAG(0),MASK(0)).NE.0) THEN !SELECTED FLAGS ONLY
              NFPHH(IHA) = NFPHH(IHA) + 1        !FLAGGED HEADERS PER HA-BIN
              NFPCH(ICHAN) = NFPCH(ICHAN) + 1    !FLAGGED HEADERS PER CHANNEL
            END IF
C
C=============================================================================
          ELSE
	    ARGSTR='NFLCNT ACC'//': Name not recognised: '//NAME
            CALL WNCTXT (F_TP,ARGSTR)
     1           
          END IF                                   !DATA/HEADER
C
C
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
C******************************************************************************
!*** SHOW flag-counts:
C
	ELSE IF (ACTION(:4).EQ.'SHOW') THEN
C
C  Check if anything has been counted at all
C
	  HA1 = MIN(HARANH(0),HARAND(0)) 
	  HA2 = MAX(HARANH(1),HARAND(1))
	  IF (HA1.EQ.0 .AND. HA2.EQ.0) THEN
	    CALL WNCTXT(F_T,'No counts available: Do COUNT first')
	  ELSE
C
C  Check specific instructions embedded in the input string(s): 
C
	    TXT_CALC = ' '
            DO I3=0,3                               !POLARISATIONS
              SELPOL(I3) = .FALSE.                  !POL NOT SELECTED
            END DO
            IF (INDEX(NAME,'_XY').GT.0) THEN        !XX,YY
	      TXT_CALC = C_VER//' Calculated for pols XX,YY only'
	      TXT_CALC(NCPL:) = C_VER
              SELPOL(XX) = .TRUE.               
              SELPOL(YY) = .TRUE.              
            ELSE IF (INDEX(NAME,'_YX').GT.0) THEN   !XY,YX
	      TXT_CALC = C_VER//' Calculated for pols XY,YX only'
	      TXT_CALC(NCPL:) = C_VER
              SELPOL(YX) = .TRUE. 
              SELPOL(XY) = .TRUE. 
            ELSE IF (INDEX(NAME,'_XX').GT.0) THEN    !XX ONLY
	      TXT_CALC = C_VER//' Calculated for pol XX only'
	      TXT_CALC(NCPL:) = C_VER
              SELPOL(XX) = .TRUE.               
            ELSE IF (INDEX(NAME,'_YY').GT.0) THEN    !YY ONLY
	      TXT_CALC = C_VER//' Calculated for pol YY only'
	      TXT_CALC(NCPL:) = C_VER
              SELPOL(YY) = .TRUE.               
            ELSE
              DO I3=0,3
                SELPOL(I3) = .TRUE.                 !ALL POLS SELECTED
              END DO
            END IF
C
C  Prepare some general explanatory text-strings for later use:
C
            CALL WNCTXS (TXT_LEGEND,C_VER//
	1	' (!AS)=zero flags, (!AS)=100% flagged,  (!AS)=not tested)',
	1       C_NOFLAG,C_ALLSET,C_NOTEST)     
            TXT_LEGEND(NCPL:) = C_VER
C
            CALL WNCTXS (TXT_SCANS,
     1         C_VER//' Tested Scans:' 
     1         //'  channels=!UJ:!UJ'
     1         //'  HA-range=!F4.0:!F4.0'
     1         ,MIN(CHRANH(0),CHRAND(0))
     1         ,MAX(CHRANH(1),CHRAND(1))
     1         ,HA1,HA2)
            TXT_SCANS(NCPL:) = C_VER
C
            TXT80 = ' '
            J1 = 7                                      !CHARS PER POL
            DO I3=0,3
              IF (NNPP(I3).GT.0) THEN                   !POL TESTED
                I2 = 0
                I4 = 0
                DO RTW=0,STHTEL-1
	          DO RTE=RTW,STHTEL-1
                    IF (NPI(I3,RTE,RTW).GT.0) THEN      !IFR TESTED
	              IF (RTW.EQ.RTE) I4 = I4 + 1	      !AUTOCORRS
	              IF (RTW.NE.RTE) I2 = I2 + 1	      !CROSSCORRS
                    END IF
                  END DO
                END DO
                CALL WNCTXS (TXT80(2+J1*I3:),
     1             '!AS=!SJ',POLNAME(I3),I2)
              ELSE
                CALL WNCTXS (TXT80(2+J1*I3:),
     1             '!AS=--',POLNAME(I3))
              END IF
            END DO 
            CALL WNCTXS (TXT_DATA,
     1         C_VER//' Tested nr of ifrs: !AS',TXT80)
            TXT_DATA(NCPL:) = C_VER
C
            TXT_FLAGS = C_VER//' Headers/data tested for flag-types: '
            I=37                                       !START CHAR
            DO I4=0,MXNFLTYP-1
              IF ((IAND(HEADMASK,FLAGTYPE(I4)).NE.0) .OR.
     1          (IAND(DATAMASK,FLAGTYPE(I4)).NE.0)) THEN
                CALL WNCTXS (TXT_FLAGS(I:),' !4$AS',FLAGNAME(I4)) 
                I=I+5
              END IF
            END DO
            TXT_FLAGS(NCPL:) = C_VER
C
C==============================================================================
C SHOW flags per polarisation and per flag-type:
C
            IF (NAME(:3).EQ.'FTY') THEN
C
              CALL WNCTXT (F_TP,'!/!AS',SEPAR)          !SEPARATOR
C
              TXT80 = C_VER//' Percentage of SET flags per flag-type.'
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,'!AS',TXT80)
C
              CALL WNCTXT (F_TP,'!AS',TXT_LEGEND)
              IF (TXT_SCANS.NE.' ') CALL WNCTXT (F_TP,TXT_SCANS)
              IF (TXT_DATA.NE.' ') CALL WNCTXT (F_TP,TXT_DATA)
              CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
C
              J1 = 5                                  !NR OF CHARS PER FIELD
              J2 = 12                                 !START OF FIELDS
              J3 = J2+MXNFLTYP*J1 + 1                 !START AFTER FIELDS
              TXT80 = C_VER//' flagtype:'
              DO I4=0,MXNFLTYP-1
                CALL WNCTXS (TXT80(J2+I4*J1:),'!#$AS',J1,FLAGNAME(I4))
              END DO
              TXT80(J3:) = '          tested:'
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,'!AS',TXT80)
C
              TXT80 = C_VER//' headers:'
              PRINTLINE = .FALSE.
              J0 = 0
              DO I4=0,MXNFLTYP-1
                J0 = MAX(J0,NNPTH(I4))                      !COUNT SCANS
                COL = J2+I4*J1                              !COLUMN NR
                R0 = (100.*NFPTH(I4))/MAX(1,NNPTH(I4))      !PERCENTAGE
                IF (NNPTH(I4).EQ.0) THEN
                  CALL WNCTXS (TXT80(COL:),'!#$AS',J1,C_NOTEST) !NOT TESTED
                ELSE IF (R0.GT.0) THEN
                  CALL WNCTXS (TXT80(COL:),'!#$UJ',J1,NINT(R0))
                  PRINTLINE = .TRUE.
                ELSE
                  CALL WNCTXS (TXT80(COL:),'!#$AS',J1,C_NOFLAG) !NO FLAGS
                  PRINTLINE = .TRUE.
                END IF
              END DO
              CALL WNCTXS (TXT80(J3:),'% of   !6$UJ Scans',J0)
              TXT80(NCPL:) = C_VER
              IF (PRINTLINE) CALL WNCTXT (F_TP,'!AS',TXT80)
C
              DO I3=0,3                                     !ALL POLS
                  PRINTLINE = .FALSE.
                  CALL WNCTXS (TXT80,C_VER//' data !AS:',POLNAME(I3))
                  DO I4=0,MXNFLTYP-1
                    COL = J2+I4*J1                          !COLUMN NR
                    R0 = (100.*NFPTD(I3,I4))/MAX(1,NNPP(I3))
                    IF (NNPTD(I3,I4).EQ.0) THEN
                      CALL WNCTXS (TXT80(COL:),'!#$AS',J1,C_NOTEST) !NOT TESTED
                    ELSE IF (R0.GT.0) THEN
                      PRINTLINE = .TRUE.
                      CALL WNCTXS (TXT80(COL:),'!#$UJ',J1,NINT(R0))
                    ELSE
                      PRINTLINE = .TRUE.
                      CALL WNCTXS (TXT80(COL:),'!#$AS',J1,C_NOFLAG) !NO FLAGS
                    END IF
                  END DO
                  CALL WNCTXS (TXT80(J3:),'% of !8$UJ uv-pts',NNPP(I3))
                  TXT80(NCPL:) = C_VER
                  IF (PRINTLINE) CALL WNCTXT (F_TP,'!AS',TXT80)
              END DO                                       !NEXT POL
C
              TXT80 = C_VER//' flagcode:'
              DO I4=0,MXNFLTYP-1
                COL = J2+I4*J1                             !COLUMN NR
                I5 = FLAGTYPE(I4)                          !E.G. FL_MAN
                I5 = IAND('000000ff'X,ISHFT(I5,-8))        !FLAG CODE (?)
                CALL WNCTXS (TXT80(COL:),' (!2$XJ)',I5)
              END DO
              TXT80(NCPL:) = C_VER
              PRINTLINE = .FALSE.                          !INHIBIT OUTPUT...
              IF (PRINTLINE) CALL WNCTXT (F_TP,'!AS',TXT80)
C
              CALL WNCTXT (F_TP,'!AS',SEPAR)               !SEPARATOR
C
C============================================================================
C  SHOW flags per polarisation and per telescope:
C
            ELSE IF (NAME(:3).EQ.'TEL') THEN
C
              CALL WNCTXT (F_TP,' ')
              CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
C
              TXT80 = C_VER//
     1              ' Percentage of flagged uv-pnts, per telescope.'
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,'!AS',TXT80)
C
              CALL WNCTXT (F_TP,'!AS',TXT_LEGEND)
              IF (TXT_SCANS.NE.' ') CALL WNCTXT (F_TP,TXT_SCANS)
              IF (TXT_DATA.NE.' ') CALL WNCTXT (F_TP,TXT_DATA)
              IF (TXT_FLAGS.NE.' ') CALL WNCTXT (F_TP,TXT_FLAGS)
              CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR

              J1 = 4                                  !NR OF CHARS PER TYPE
              J2 = 7                                  !START CHAR IN TXT80
              J3 = J2+STHTEL*J1 + 2                   !START CHAR IN TXT80
              TXT80 = C_VER//' tel:'
              DO I4=0,STHTEL-1
                CALL WNCTXS (TXT80(J2+I4*J1:),'!#$AS',
     1                     J1,'RT'//TELNAME(I4))
              END DO
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,'!AS',TXT80)
C
              DO I3=0,3                                     !ALL POLS
                CALL WNCTXS (TXT80,C_VER//' !AS:',POLNAME(I3))
                DO I4=0,STHTEL-1
                  COL = J2+I4*J1                          !COLUMN NR
                  R0 = (100.*NFPT(I3,I4))/MAX(1,NNPT(I3,I4))      !PERCENTAGE
                  IF (R0.GT.0) THEN
                    CALL WNCTXS (TXT80(COL:),'!#$UJ',J1,NINT(R0))
                  ELSE IF (NNPT(I3,I4).EQ.0) THEN
                    CALL WNCTXS (TXT80(COL:),'!#$AS',J1,C_NOTEST) !NOT TESTED
                  ELSE
                    CALL WNCTXS (TXT80(COL:),'!#$AS',J1,C_NOFLAG) !NO FLAGS 
                  END IF
                END DO
                CALL WNCTXS (TXT80(J3:),'% of pts/tel')
                TXT80(NCPL:) = C_VER
                CALL WNCTXT (F_TP,'!AS',TXT80)
              END DO                                  !NEXT POL
C
              CALL WNCTXT (F_TP,'!AS!/',SEPAR)          !SEPARATOR
C
C============================================================================
C  SHOW flags per frequency channel:
C
            ELSE IF (NAME(:4).EQ.'CHAN') THEN
C
              CALL WNCTXT (F_TP,'NFLCNT: Not implemented yet.')
C
C============================================================================
C  SHOW flags per HA-bin:
C
            ELSE IF (NAME(:2).EQ.'HA') THEN
C
              CALL WNCTXT (F_TP,'!/!AS',SEPAR)          !SEPARATOR
C
              TXT80 = C_VER//' Flags per HA-bin (1 degree wide).'
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,'!AS',TXT80)
              TXT80 = C_VER//' Format: 85;25 means: '
     1            //'85% of uv-data '
     1            //'and 25% of Scan headers flagged.'
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,'!AS',TXT80)

              CALL WNCTXT (F_TP,'!AS',TXT_LEGEND)
              IF (TXT_SCANS.NE.' ') CALL WNCTXT (F_TP,TXT_SCANS)
              IF (TXT_DATA.NE.' ') CALL WNCTXT (F_TP,TXT_DATA)
              IF (TXT_FLAGS.NE.' ') CALL WNCTXT (F_TP,TXT_FLAGS)
              IF (TXT_CALC.NE.' ') CALL WNCTXT (F_TP,TXT_CALC)
              CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
C
              J1 = 7                                  !NR OF CHARS PER FIELD
              J2 = 7                                  !START OF FIELDS
              J3 = J2+10*J1 + 2                       !START AFTER FIELDS
              TXT80 = C_VER//'  HA'                   !COLUMN DESCRIPTION
              DO I1=0,9
                COL = J2+I1*J1+4
                CALL WNCTXS (TXT80(COL:),'+!1$UJ',I1)
              END DO
              TXT80(J3:) = 'deg'
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,TXT80)
C
              DO I=IHAMIN,IHAMAX,10                   !10 DEGR PER LINE
                PRINTLINE = .FALSE.
                CALL WNCTXS (TXT80,C_VER//' !3$SJ:',I)
                DO I1=0,9
                  IHA = I+I1
                  IF (IHA.GT.IHAMAX) GOTO 601             !ESCAPE
C
                  COL = J2+I1*J1
                  NND = 0                               
                  NFD = 0  
                  DO I3=0,3
                    IF (SELPOL(I3)) THEN                 !SELECTED POL
                      NND = NND + NNPHD(I3,IHA)          !TOTAL TESTED PER POL
                      NFD = NFD + NFPHD(I3,IHA)          !TOTAL FLAGGED PER POL
                    END IF
                  END DO
                  R0 = (100.*NFD)/MAX(1,NND)             !% OF DATA FLAGGED
                  IF (NND.EQ.0) THEN                     !NOT TESTED
                    CALL WNCTXS (TXT80(COL:),'!#$AS;',J1-3,C_NOTEST) 
                  ELSE IF (R0.GT.0) THEN                 !SOME FLAGS SET
                    PRINTLINE = .TRUE.
                    IF (R0.LT.100) THEN                    !<100% FLAGGED
                      CALL WNCTXS (TXT80(COL:),'!#$UJ;',J1-3,NINT(R0))
                    ELSE                                   !100% FLAGGED
                      CALL WNCTXS (TXT80(COL:),'!#$AS',J1-3,C_ALLSET)  
                    END IF
                  ELSE                                   !NO FLAGS SET
                    PRINTLINE = .TRUE.
                    CALL WNCTXS (TXT80(COL:),'!#$AS;',J1-3,C_NOFLAG)  
                  END IF
C
                  COL = COL+J1-3+1
                  R0 = (100.*NFPHH(IHA))/MAX(1,NNPHH(IHA)) !% OF HEADERS FLAGGED
                  IF (NNPHH(IHA).EQ.0) THEN                !NOT TESTED
                    CALL WNCTXS (TXT80(COL:),'!2$AS',C_NOTEST)  
                  ELSE IF (R0.GT.0) THEN                   !SOME FLAGS SET
                    PRINTLINE = .TRUE.
                    IF (R0.LT.100) THEN                    !<100% FLAGGED
                      CALL WNCTXS (TXT80(COL:),'!2$UJ',NINT(R0))
                    ELSE                                   !100% FLAGGED
                      CALL WNCTXS (TXT80(COL:),'!2$AS',C_ALLSET)  
                    END IF
                  ELSE                                     !NO FLAGS SET
                    PRINTLINE = .TRUE.
                    CALL WNCTXS (TXT80(COL:),'!2$AS',C_NOFLAG)  
                  END IF
C
                END DO                         
 601            CONTINUE         
                TXT80(J3:) = ' '          	!CLOSING TEXT PER LINE
                TXT80(NCPL:) = C_VER
                IF (PRINTLINE) CALL WNCTXT (F_TP,'!AS',TXT80)
              END DO
C
              CALL WNCTXT (F_TP,'!AS!/',SEPAR)          !SEPARATOR
C
C==============================================================================
C SHOW flags per ifr (and per polarisation):
C The information is displayed in two `squares' side by side.
C The 1st (left) square contains XX in the upper right triangle (URT),
C and YY in the lower left triangle (LLT).
C The second (right) square has XY in URT, and YX in LLT.
C
            ELSE IF (NAME(:3).EQ.'IFR') THEN
C
              CALL WNCTXT (F_TP,'!/!AS',SEPAR)          !SEPARATOR
C
              TXT80 = C_VER//' Percentage of flagged uv-pnts, per ifr.'
              TXT80(NCPL:) = C_VER
              CALL WNCTXT (F_TP,'!AS',TXT80)
C
              CALL WNCTXT (F_TP,'!AS',TXT_LEGEND)
              IF (TXT_SCANS.NE.' ') CALL WNCTXT (F_TP,TXT_SCANS)
              IF (TXT_DATA.NE.' ') CALL WNCTXT (F_TP,TXT_DATA)
              IF (TXT_FLAGS.NE.' ') CALL WNCTXT (F_TP,TXT_FLAGS)
              IF (TXT_CALC.NE.' ') CALL WNCTXT (F_TP,TXT_CALC)
              CALL WNCTXT (F_TP,'!AS',SEPAR)          !SEPARATOR
C
	      DO I=-3,STHTEL+3                         !ALL LINES
                TEXT(I)= ' '                           !OPENING HASH
	      END DO
C
              J1 = 2                              !NR OF CHARS PER FIELD
              J2 = 6                              !RT0 COL OF FIRST SQUARE
              J3 = J2+40                          !RT0 COL OF SECOND SQUARE
              TEXT(-3) = SEPAR
	      TEXT(-2)(J2+20:) = 'XX'             !EXPL. FOR URT OF 1ST SQUARE
	      TEXT(-2)(J3+20:) = 'XY'             !EXPL. FOR URT OF 2ND SQUARE
C
	      DO RTE=0,STHTEL-1                   !ROWS OF TEL NAMES
                I = RTE*J1 + J1-1
	        TEXT(-1)(J2+I:J2+I) = TELNAME(RTE)      !TOP ROW
	        TEXT(-1)(J3+I:J3+I) = TELNAME(RTE)
	        TEXT(STHTEL)(J2+I:J2+I) = TELNAME(RTE)  !BOTTOM 
	        TEXT(STHTEL)(J3+I:J3+I) = TELNAME(RTE)
	      END DO
C
	      DO RTW=0,STHTEL-1		        !COLUMNS OF TEL NAMES
                I = -2                            !TO THE LEFT OF SQUARE
                TEXT(RTW)(J2+I:J2+I) = TELNAME(RTW)
                TEXT(RTW)(J3+I:J3+I) = TELNAME(RTW)
                I = J1*STHTEL + 2                 !TO THE RIGHT OF SQUARE
                TEXT(RTW)(J2+I:J2+I) = TELNAME(RTW)
                TEXT(RTW)(J3+I:J3+I) = TELNAME(RTW)
	      END DO
C
              TEXT(STHTEL+1)(J2+3:) = 'YY'        !EXPL. FOR LLT OF 1ST SQUARE
              TEXT(STHTEL+1)(J3+3:) = 'YX'        !EXPL. FOR LLT OF 2ND SQUARE
              TEXT(STHTEL+2) = SEPAR            !CLOSING SEPARATOR
              TEXT(STHTEL+3) = ' '              !BLANK LINE
C
              DO RTW = 0,STHTEL-1                           !WEST TEL
                DO RTE = RTW,STHTEL-1                       !EAST TEL
                  DO I3=0,3                                 !ALL POLS
                    IF (I3.EQ.XX .OR. I3.EQ.XY) THEN        !URT's
                      LIN = RTW
                      COL = J2+J1*RTE                       !XX
                      IF (I3.EQ.XY) COL = J3+J1*RTE         !XY
                    ELSE                                    !LLT's
                      LIN = RTE
                      COL = J2+J1*RTW                       !YY
                      IF (I3.EQ.YX) COL = J3+J1*RTW         !YX
                    END IF
                    NFD = NPI(I3,RTW,RTE)                    !# OF FLAGS
                    NND = NPI(I3,RTE,RTW)                    !# OF DATA TESTED
                    R0 = (100.*NFD)/MAX(1,NND)               !PERCENTAGE
                    IF (RTW.EQ.RTE) THEN                     !AUTOCORR (?)
		      CALL WNCTXS(TEXT(LIN)(COL:COL+J1-1),
	1                    '!#$AS',J1,' ')               !BACKSLASH ->SPACE
                    ELSE IF (R0.GT.0) THEN                   !NON-ZERO FLAGS
                      IF (NINT(R0).LT.100) THEN              !<100% 
		        CALL WNCTXS(TEXT(LIN)(COL:COL+J1-1),
	1                    '!#$UJ',J1,NINT(R0))          !GIVE PERCENTAGE
                      ELSE
		        CALL WNCTXS(TEXT(LIN)(COL:COL+J1-1),
	1                    '!#$AS',J1,C_ALLSET)          !100% FLAGS SET
                      END IF
                    ELSE IF (NND.EQ.0) THEN                  !UV-PNT NOT TESTED
		      CALL WNCTXS(TEXT(LIN)(COL:COL+J1-1),
	1                    '!#$AS',J1,C_NOTEST)
                    ELSE                                     !# OF FLAGS IS ZERO
		      CALL WNCTXS(TEXT(LIN)(COL:COL+J1-1),
	1                    '!#$AS',J1,C_NOFLAG)          
                    END IF
	          END DO                                     !POLS(I3)
                END DO                                       !RTE
	      END DO                                         !RTW
C
	      DO I=-3,STHTEL+2                               !ALL LINES
                TEXT(I)(1:1) = C_VER                           !OPENING HASH
                TEXT(I)(NCPL:) = C_VER                         !CLOSING HASH
	        CALL WNCTXT(F_TP,'!AS',TEXT(I))              !PRINT/TYPE LINE
	      END DO
              CALL WNCTXT (F_TP,' ')
C
C*****************************************************************************
C
            ELSE
              ARGSTR='NFLCNT Action '//ACTION(:3)//
     1           ': Name not recognised: '//NAME
              CALL WNCTXT (F_TP,ARGSTR)
            END IF
	  ENDIF ! counts available
C******************************************************************************
C
        ELSE
            CALL WNCTXT (F_TP,'NFLCNT Action '//ACTION(:3)//
     1           ': not recognised')
        END IF ! actions
C
C******************************************************************************
	RETURN
	END
