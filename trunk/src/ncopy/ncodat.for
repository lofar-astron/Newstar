C+ NCODAT.FOR
C  JPH 930317
C
C  Revisions:
C        JPH 930610        Restrict polsn selection to XX/XY/XYX
C        JPH 930831        Suppress OUTPUT_SECTORS prompt
C        JPH 931012        Add SIM option. - Remove 'XYX' default from
C                         WNDPAR('SELECT_XYX'...) call.
C        JPH 931102        Add KEEP_MODEL. SELECT_XYX --> POLARISATION
C        CMV 931220        Pass FCA of input file to WNDXLP and WNDSTA/Q
C        JPH 940107        Remove 'SIM' option. Correct previous: FCAOUT --> FCAIN
C        CMV 940518        Add options to select copy of model and IF-data 
C        CMV 940926        Close old file before asking new one
C        JPH 941006        APPLY, DE_APPLY prompts
C        JPH 960220        NSCIFS
C        JPH 960625        Message on MIFR corrections
C        JPH 960725        Add YX polarisation mode
C        JPH 961213        SHORTCOPY option, SCANS parameter
C        JPH 961218        Call NSCHAS with type=1: '*' prompt
C        JPH 970403        Init SCANS to 0,65536 for standard COPY option
C        AXC 010709        Linux port - TABS
C
C
        SUBROUTINE NCODAT
C
C  Get NCOPY program parameters
C
C  Result:
C
C        CALL NCODAT        will ask and set all program parameters
C
C PIN references:
C
C        OPTION
C        INPUT_SCN_NODE
C        OUTPUT_SCN_NODE
C        INPUT_SECTORS
C        HA_RANGE
C        POLARISATION
C        COPY_MODEL
C        COPY_IFDATA
C
C  Include files:
C
        INCLUDE 'WNG_DEF'
        INCLUDE 'NCO_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
        INTEGER*4        PUT_PARM
        LOGICAL WNDPAR                        !GET DWARF PARAMETER
        LOGICAL WNDNOD                        !GET NODE NAME
        LOGICAL WNDDA1, WNDDA2                ! get APPLY, DE_APPLY
        LOGICAL WNDPAP                        ! set interface-parameter value
        LOGICAL WNFMOU                        !MOUNT TAPE
        LOGICAL WNFOP,WNFOPF                !OPEN FILE
        LOGICAL WNDSTA                        !GET SCTS TO DO
        LOGICAL NSCHAS                        !GET HA-RANGE
        LOGICAL NSCIFS                        ! select ifrs
C
C  Data declarations:
C
        CHARACTER*4         POLC                !POLARISATION CODE
        CHARACTER*80        FILOUT
        LOGICAL BB1
C-
C
C GET OPTION
C
 100        CONTINUE
        IF (.NOT.WNDPAR('OPTION',OPTION,
        1        LEN(OPTION),J0,'QUIT')) THEN
          OPTION='QUIT'                                !ASSUME END
        ELSE IF (J0.LE.0) THEN
          OPTION='QUIT'                                !ASSUME END
        END IF
        IF (OPT.EQ.'QUI') GOTO 900

C******************************************************************************
C COPY OR SIMULOATE DATA: SELECT IN- AND OUTPUTS
C******************************************************************************
C Open input SCN file
C
        IF (OPT.EQ.'COP' .OR. OPT.EQ.'SHO') THEN
 110          CONTINUE
          IF (.NOT.WNDNOD('INPUT_SCN_NODE',NODIN,
        1        'SCN','R',NODIN,IFILE)) THEN
            IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100        !RETRY OUTPUT
            GOTO 110                                !REPEAT
          ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
            GOTO 100                                !RETRY OUTPUT
          ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
            GOTO 110                                !MUST SPECIFY
          END IF
          IF (.NOT.WNFOP(FCAIN,IFILE,'R'))        !OPEN INPUT SCAN FILE
        1        GOTO 110                        !RETRY
C
C Open output SCN file
C
 120          CONTINUE
          CALL WNFCL(FCAOUT)
          IF (.NOT.WNDNOD('OUTPUT_SCN_NODE',NODOUT,'SCN',
        1                'U',NODOUT,FILOUT)) THEN
            IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 110
            GOTO 120                                !error
          ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
            GOTO 110
          ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
            GOTO 120                                !MUST SPECIFY
          END IF
          IF (.NOT.WNFOP(FCAOUT,FILOUT,'U'))        !OPEN OUTPUT SCAN FILE
        1        GOTO 120                        !RETRY
C
C Open SCN file for update
C
        ELSEIF (OPT.EQ.'REV') THEN
 125          CONTINUE
          IF (.NOT.WNDNOD('SCN_NODE',NODOUT,'SCN',
        1                'R',NODOUT,FILOUT)) THEN
            IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 100        !RETRY OPTION
            GOTO 125                                !REPEAT
          ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
            GOTO 100                                !RETRY OPTION
          ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
            GOTO 125                                !MUST SPECIFY
          END IF
          IF (.NOT.WNFOP(FCAOUT,FILOUT,'U'))        !OPEN OUTPUT SCAN FILE
        1            GOTO 125                        !RETRY
        ENDIF! copy, simulate or reverse
C
C Sectors specification
C
 130        CONTINUE
        IF (OPT.NE.'OVE') THEN
          IF (.NOT.WNDSTA('SCN_SETS',MXNSCT,INSCTS,FCAIN))
        1        GOTO 120
          IF (INSCTS(0,0).EQ.0) GOTO 120        !NO SCTS SPECIFIED
C
 131          CONTINUE        
CC          IF (.NOT.WNDSTQ('OUTPUT_SECTORS',1,OUTSCTS,0))
CC        1        GOTO 120                         !GET SCTS TO MAKE
CC          IF (OUTSCTS(0,0).EQ.0) GOTO 120        !NONE
        ENDIF
C
C Hour angle specification
C
        IF (OPT.EQ.'COP' .OR. OPT.EQ.'SHO') THEN
 140          CONTINUE
          IF (.NOT.NSCHAS(1,HARAN)) GOTO 130
          IF (OPT.EQ.'SHO') THEN
            IF (.NOT.WNDPAR('SCANS',SCANS,2*LB_J,J0)) GOTO 130
          ELSE
            SCANS(0)=0
            SCANS(1)=65536
          ENDIF
          IF (.NOT.NSCIFS(1,SIFRS)) GOTO 130
C
C Polarisation specification
C
 150          CONTINUE
          IF (.NOT.WNDPAR('POLARISATION',POLC,LEN(POLC),J0)) THEN
            IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 140
            GOTO 150                                ! error
          ENDIF
          IF (J0.EQ.0) GOTO 140                        ! RETRY HA range
          IF (J0.LT.0) POLC='XYX'                ! ALL
          IF (POLC.EQ.'XYX') THEN                ! SET CODE
            ONPOL=4
          ELSEIF (POLC.EQ.'XY') THEN                ! XX,YY
            ONPOL=2
          ELSEIF (POLC.EQ.'YX') THEN                ! copy XY,YX to XX,YY
            ONPOL=-2
          ELSE                                        ! XX only
            ONPOL=1
          ENDIF
C
C Copy model and/or IF data?
C
          DO_MDL=.TRUE.                                !COPY MODEL
          DO_IFH=.TRUE.                                !COPY IFH
C
          IF (.NOT.WNDPAR('COPY_MODEL',BB1,LB_L,J0,'YES')) THEN
             IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 150        !RETRY
             GOTO 100                                !RETRY OPTION
          END IF
          IF (.NOT.BB1) DO_MDL=.FALSE.                !DO NOT COPY
C
          IF (.NOT.WNDPAR('COPY_IFDATA',BB1,LB_L,J0,'YES')) THEN
             IF (E_C.EQ.DWC_ENDOFLOOP) GOTO 150        !RETRY
             GOTO 100                                !RETRY OPTION
          END IF
          IF (.NOT.BB1) DO_IFH=.FALSE.                !DO NOT COPY
C
          CALL WNCTXT(F_T,
        1'!/NOTES:
        1 !/!4C\The corrections that you specify below will be applied resp.
        1 !/!4C\de-applied to the visibilities that will be written to the 
        1 !/!4C\output file. This modification of the visibilities is 
        1 !/!4C\IRREVERSIBLE, so the only path back to the uncorrected data
        1 !/!4C\will be via the input file.
        1 !/!4C\The corresponding correction parameters must be set to zero,
        1 !/!4C\lest they be applied for a second time later. This is not         
        1 !/!4C\implemented yet, so you must "manually" zero them using 
        1 !/!4C\NCALIB.!/
        1 !/
        1 !/!4C\Multiplicative and additive interferometer corrections are
        1 !/!4C\currently NOT copied!! Use NCALIB SET ICOPY instead.!!')
C 
          IF (.NOT.WNDPAP('X_APPLY',' NONE /ASK')) CALL WNGEX! force prompting
          IF (.NOT.WNDPAP('X_DE_APPLY',' NONE /ASK')) CALL WNGEX
          IF (.NOT.WNDDA1('X_APPLY',CAP)) CALL WNGEX        ! get APPLY bits
          IF (.NOT.WNDDA2('X_DE_APPLY',CDAP)) CALL WNGEX! get DE_APPLY bits
C        
        ENDIF! copy
C
 900        CONTINUE
        RETURN                                        !READY
C
C
        END
