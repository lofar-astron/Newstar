C+ NSCDMP.FOR
C  WNB 900219
C
C  Revisions:
C	HjV 920520	HP does not allow extended source lines
C       HjV 941107	Add (when tape) label to MEDIAD
C			If volume does not exist, also add it.
C       		Therefore extract several fields:
C				FD-26  (record length in bytes)
C				FD-28  (# of records per block)
C       			FD-100 (# of blocks)
C				OH-40  (Sequence-number)
C       HjV 941125	Typo, WNCALN declared twice
C	CMV 950120	Always stop if not completely copied,
C			Defensize rounding for size
C	CMV 950125	Inform MEDIAD if label partially copied
C	HjV 970407	Give error-message in case WNFRD return an error
C
C
	SUBROUTINE NSCDMP
C
C  Dump WSRT tape to disk or vice versa
C
C  Result:
C
C	CALL NSCDMP	will dump a WSRT tape to/from disk
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NSC_DEF'
	INCLUDE 'FDW_O_DEF'
	INCLUDE 'FDW_T_DEF'
	INCLUDE 'OHW_O_DEF'
	INCLUDE 'OHW_T_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
        LOGICAL WNDPAR                          !Get DWARF parameter
	LOGICAL WNFOP,WNFOPF			!OPEN FILE
	LOGICAL WNFRD				!READ DATA
	LOGICAL WNFWR				!WRITE DATA
	REAL    WNFTLN				!TAPE LENGTH WRITTEN
	INTEGER WNFTLB				!CURRENT TAPE LABEL
	INTEGER WNCALN				!STRING LENGTH
        INTEGER WNFSCI                          !Talk to Scissor qed deqmon
        INTEGER WNFSCS                          !GET RETURN STRING
	LOGICAL WNGMED				!Handle medium administration
C
C  Data declarations:
C
	CHARACTER*6 VOLUME			!OUTPUT VOLUME
	CHARACTER*6 LTXT			!LABEL NAME
	BYTE      RWBUF(SRTRCL)			!I/O BUFFER
	BYTE      TBUF(SRTRCL)			!I/O BUFFER
	CHARACTER*1 TBUFC(SRTRCL)		!I/O BUFFER
	INTEGER   TBUFJ(SRTRCL/4)		!I/O BUFFER
	INTEGER*2 TBUFI(SRTRCL/2)		!I/O BUFFER
	LOGICAL   FDDONE,OHDONE			!SWITCH TO SEE IF FD/OH DONE
	LOGICAL   OUTOPEN			!SWITCH TO SEE IF OMCA OPEN
	INTEGER*2 LRCRD				!RECORD LENGTH IN BYTES (FD-26)
	INTEGER*2 PHBLL				!# RECODS PER BLOCK (FD-28)
	INTEGER   NBL				!# OF DATA-BLOCKS (FD-100)
	INTEGER   VOLGNR			!SEQUENCENR. OF OBS. (OH-40)
	REAL      MBYT				!LENGTH IN MBYTES
	REAL      FREE				!Free space in Mbytes
	REAL      NEEDS				!Needed space in Mbytes
        BYTE LG1
        LOGICAL LG4
	CHARACTER*128 SCIBUF			!I/O BUFFER FOR SCI-ROUTINES
	CHARACTER*1024 COMMAND			!Command to send

	EQUIVALENCE (TBUF,TBUFC,TBUFI,TBUFJ)
C-
C
C INIT
C
	J=0					!START LABEL INPUT
	J1=OLAB-1				!START LABEL OUTPUT
C
C DO A LABEL
C
 10	CONTINUE
	J1=J1+1					!NEXT OUTPUT LABEL
	J=J+1					!COUNT INPUT LABEL
	IF (NLAB(1).LT.0) THEN			!ALL LABELS ON TAPE
	  J0=J					!NEXT INPUT LABEL
	ELSE IF (J.LE.NLAB(1)) THEN
	  J0=ILAB(J,1)				!NEXT INPUT LABEL
	ELSE
	  GOTO 900				!READY
	END IF
C
C OPEN INPUT
C
	IF (UNIT.EQ.'D') THEN			!DISK INPUT
	  CALL WNCTXS(LTXT,'!6$ZJ',J0)		!MAKE LABEL NAME
	  IF (.NOT.WNFOP(IMCA,IFILE(1:WNCALN(IFILE))//'.'//LTXT,'R')) THEN
	     IF (NLAB(1).GT.0)
	1	  CALL WNCTXT(F_TP,'Cannot find file !AS\.!AS',IFILE,LTXT)
	     GOTO 900				!STOP
	  END IF
	ELSE					!TAPE INPUT
	  IF (.NOT.WNFOPF(IMCA,' ','R',0,0,0,J0)) THEN
	    CALL WNCTXT(F_TP,'Cannot find label !UJ',J0)
	    GOTO 900
	  END IF
	END IF
C
C COPY DATA, OPEN INPUT ONCE EVERYTHING HAS BEEN CHECKED
C
	OUTOPEN=.FALSE.
	FDDONE=.FALSE.
	OHDONE=.FALSE.
	VOLGNR=-1
	J2=0					!DATA POINTER
C
C READ
C
 20	CONTINUE
	IF (.NOT.WNFRD(IMCA,SRTRCL,RWBUF,J2)) THEN	!EOD
	  IF (E_C .NE. '00000870'X .AND. E_C .NE. '00000000'X) THEN
	     CALL WNCTXT(F_TP,'ERROR: ')
	     CALL WNCTXT(F_TP,'ERROR: Program halted with error !XJ',E_C)
	     CALL WNCTXT(F_TP,'ERROR: ')
	  ENDIF
	  CALL WNFCL(IMCA)			!CLOSE INPUT
C
C Output not open, so nothing written, generate error
C
	  IF (.NOT.OUTOPEN) THEN
	     CALL WNCTXT(F_TP,'Nothing copied from input...')
	     GOTO 900
	  END IF
C
C Flush last buffer to get proper output size 
C
	  IF (OUNIT.NE.'D') J1=WNFTLB(OMCA)	!CURRENT OUTPUT LABEL
	  CALL WNCTXT(F_TP,'Label !UJ copied to label !UJ',J0,J1)
 21	  CONTINUE
	  CALL WNFPUR(OMCA)			!WRITE REMAINING BYTES
	  IF (OUNIT.NE.'D') THEN
	    MBYT=WNFTLN(OMCA,3)			!LENGTH IN MBYTES
	    CALL WNCTXT(F_TP,'!F8.3 Mbytes written',MBYT+0.00005)
	    IF (MBYT.EQ.0) THEN			!NOTHING WRITTEN AT ALL...
	       CALL WNCTXT(F_TP,
	1	'Aborting dump, possibly open error on label')
	       GOTO 900
	    ENDIF
	  END IF
C
C Now close the output and inform Scissor if necessary
C
	  CALL WNFCL(OMCA)			!CLOSE OUTPUT
	  IF (OUNIT.NE.'D') THEN		!SEND TO SCISERV
            IF (.NOT.WNGMED(OFILE(1:6),J1,MBYT,VOLGNR)) THEN
	       CALL WNCTXT(F_TP,
	1	'The dump will be aborted since the administration '//
	1	'is incorrect')
	       CALL WNCTXT(F_TP,
	1	'Please inform the Scissor manager before continuing')
	       GOTO 900
	    ENDIF
	  END IF
	  GOTO 10				!NEXT LABEL
	END IF
C
C EXTRACT INFO
C
	IF (.NOT.FDDONE) THEN			!TEST FORMAT
	  DO J3=1,SRTRCL
	    TBUF(J3)=RWBUF(J3)
	  END DO
          IBMSW=.FALSE.                         !ASSUME NON-IBM
          DECSW=.FALSE.                         !ASSUME LOCAL
          IF (TBUFC(3).NE.'F' .OR. TBUFC(4).NE.'D') THEN
            IBMSW=.TRUE.                        !ASSUME IBM
            CALL WNTTIL(SRTRCL,TBUF,FDW_T)      !TRANSLATE
            IF (TBUFC(3).NE.'F' .OR. TBUFC(4).NE.'D') THEN
 23           CONTINUE
	      CALL WNCTXT(F_TP,'Not a WSRT tape, could not find FD')
              GOTO 901                          !RETURN
            END IF
          ELSE IF (TBUFI(1).NE.32767) THEN
            DECSW=.TRUE.                        !ASSUME FROM DEC
            CALL WNTTDL(SRTRCL,TBUF,FDW_T)      !TRANSLATE
            IF (TBUFI(1).NE.32767) GOTO 23
C
C       DECStation/Alpha has the same swapping sequence as VAX D/G,
C       but uses IEEE floating point format. The test on BUFi2(1) is
C       therefore not sufficient. Since raw data is assumed to be in
C       IBM (type -1) or VAX D (type 1) format, the following test is
C       safe and sufficient. 
C
          ELSE IF (PRGDAT.EQ.6) THEN
            DECSW=.TRUE.                        !ASSUME FROM DEC
            CALL WNTTDL(SRTRCL,TBUF,FDW_T)      !TRANSLATE
            IF (TBUFI(1).NE.32767) GOTO 23
          END IF
	  FDDONE=.TRUE.
	  LRCRD=TBUFI(FDW_LRCRD_I+1)
	  PHBLL=TBUFI(FDW_PHBLL_I+1)
	  NBL=TBUFJ(FDW_NBL_J+1)
C
C CALCULATE IF THIS LABEL FITS ON OUTPUT VOLUME
C
	  IF (OUNIT.NE.'D') THEN		!TAPE OUTPUT
	    VOLUME=OFILE(1:6)
C
C  Check if label already exist (only if not append)
C
	    IF (J1.NE.0) THEN
 	      CALL WNCTXS (COMMAND,
	1	    'SELECT=MEDIAD LABEL=!UJ VOLUME=!AS',
	2	    J1,VOLUME)
	      J3=WNFSCI(COMMAND)			!Send command
	      IF (MOD(J3,100).EQ.0) THEN		!Label already exist
	        CALL WNCTXT(F_TP,'Label !UJ already exist on volume !AS',
	1	    J1,VOLUME)
  80	        CONTINUE
                IF (.NOT.WNDPAR('OVERWRITE',LG1,1,J3,'Y')) GOTO 80
	        LG4=LG1
                IF (J3.EQ.1 .AND. LG4) THEN       !YES
 	          CALL WNCTXS (COMMAND,
	1	      'DELETE=MEDIAD LABEL=!UJ VOLUME=!AS',
	2	      J1,VOLUME)
	          J3=WNFSCI(COMMAND)			!Send command
	        ENDIF
	        IF ((MOD(J3,100).NE.0).OR.(.NOT.LG4)) THEN  !Failed or NO
	          GOTO 901
	        ENDIF
	        J3=WNFSCS(SCIBUF)
	        CALL WNCTXT(F_TP,'!AS',SCIBUF(1:WNCALN(SCIBUF)))
	      ENDIF 
	    ENDIF
C
	    J3=WNFSCI('CHECK=VOLUMES VOLUME='//VOLUME)
	    IF (MOD(J3,100).NE.0) THEN
	      CALL WNCTXT(F_TP,'Could not get free space for volume !AS',
	1	  VOLUME)
	    ELSE
	      J3=WNFSCS(SCIBUF)
	      READ (SCIBUF,*) FREE
	      NEEDS=1.+((NBL*PHBLL*LRCRD)/1024.**2.)
	      IF (FREE.LE.NEEDS) THEN		!Does not fit
	        CALL WNCTXT(F_TP,'Not enough free space on volume !AS',
	1	  VOLUME)
	        CALL WNCTXT(F_TP,'Available !F9.3 Mb, need about: !F9.3 Mb',
	1	  FREE,NEEDS)
	        GOTO 901
	      END IF
	    END IF
	  END IF
	ELSE
	  IF (.NOT.OHDONE) THEN			!GET SEQUENCE NR.
	    DO J3=1,SRTRCL
	      TBUF(J3)=RWBUF(J3)
	    END DO
C
C READ OH
C
            IF (IBMSW) CALL WNTTIL(SRTRCL,TBUF,OHW_T) !TRANSLATE
            IF (DECSW) CALL WNTTDL(SRTRCL,TBUF,OHW_T)
            IF (TBUFI(1).NE.32767 .OR.
	1	 TBUFC(3).NE.'O' .OR. TBUFC(4).NE.'H') THEN
               GOTO 200				!MORE
            END IF
	    OHDONE=.TRUE.
	    VOLGNR=TBUFJ(OHW_VOLGNR_J+1)
	  END IF
	END IF
C
C OPEN OUTPUT IF NOT YET DONE
C
	IF (.NOT.OUTOPEN) THEN
	  IF (OUNIT.EQ.'D') THEN			!DISK OUTPUT
	    CALL WNCTXS(LTXT,'!6$ZJ',J1)		!MAKE LABEL NAME
	    IF (.NOT.WNFOP(OMCA,
	1	  OFILE(1:WNCALN(OFILE))//'.'//LTXT,'W')) THEN
	      CALL WNCTXT(F_TP,'Cannot open file !AS\.!AS',OFILE,LTXT)
	      GOTO 900				!STOP
	    END IF
	  ELSE					!TAPE OUTPUT
	    IF (.NOT.WNFOPF(OMCA,' ','W',0,0,0,J1)) THEN
	      CALL WNCTXT(F_TP,'Cannot write to label !UJ',J1)
	      GOTO 900
	    END IF
	  END IF
	  OUTOPEN=.TRUE.
	END IF
C
C WRITE
C
 200	IF (.NOT.WNFWR(OMCA,SRTRCL,RWBUF,J2)) THEN !COPY DATA
	  IF (OUNIT.NE.'D') J1=WNFTLB(OMCA)	!CURRENT OUTPUT LABEL
	  CALL WNCTXT(F_TP,'Write error on output')
	  CALL WNCTXT(F_TP,'Label !UJ not fully copied'//
	1		' to label !UJ',J0,J1)
	  VOLGNR=0				!DUMMY SEQNUMBER
	  GOTO 21				!STOP
	END IF
	J2=J2+SRTRCL				!NEXT POINTER
	GOTO 20					!MORE
C
C READY
C
 901	CALL WNFCL(IMCA)			!CLOSE INPUT
	CALL WNFCL(OMCA)			!CLOSE OUTPUT
 900	CALL WNFDMO(IMCA)			!DISMOUNT INPUT
	CALL WNFDMO(OMCA)			!DISMOUNT OUTPUT
C
	RETURN					!READY
C
C
	END
