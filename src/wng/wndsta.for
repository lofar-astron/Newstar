C+ WNDSTA.FOR
C  WNB 910327
C
C  Revisions:
C	WNB 910809	Allow ..
C	WNB 910826	Add STQ
C	WNB 910923	Delete trailing .
C	HjV 930309	Add call to WNDSTA_X
C	JPH 930513	Comments. For WNDSTQ SETS is :IO
C	JPH 930602	Comments
C	WNB 931015	Use SSH
C	CMV 931220	Option for LAYOUT and OVERVIEW
C	CMV 931220	Correction for * answer
C	WNB 931222	Correct FCAIN test
C	CMV 940103	Add test on LOOPS, split decoding in WNDSTM
C	JPH 940901	Add MXS=0 option. Comments
C	JPH 940906	Error if no sector found
C	JPH 940907	Call WNDPOHC at all RETURNs, inhibit WNDPOHC calls from
C			 WNDPAR through A_J flag
C	CMV 940929	Do not check on sectors found if called by WNDXLP
C	JPH 940929	Add comment to explain this
C	JPH 941214	Call WNDPOHC before executing Layout or Overview
C
C
	LOGICAL FUNCTION WNDSTA(KW,MXS,SETS,FCAIN)
C
C  Ask SET values
C
C  Result:
C
C	WNDSTA_L = WNDSTA( KW_C*:I, MXS_J:I, SETS_J(0:*,0:*):IO, FCAIN_J:I)
C				Ask the user with keyword KW for the sets to do.
C				MXS indicate the maximum number of
C				specifications. The specifications will be
C				put in SETS. If FCAIN correct disk descriptor
C				to describe an opened file, and the layout
C				and overview options can be used (currently
C				for SCN and WMP only).
C				Call with MXS=1 if only one set allowed.
C				Call with MXS=0 if this one set may represent 
C				only one sector,
C				
C	WNDSTQ_L = WNDSTQ( KW_C*:I, MXS_J:I, SETS_J(0:*,0:*):IO, FCAIN_J:I)
C				As STA, but will prompt with present value of 
C				SETS i.s.o. ""
C
C	WNDSTM_L = WNDSTM( SETS_J(0:*,0:*):I, STR_C(*):O)
C				Return string representation of sets in SETS
C
C  NOTES:
C	WNDSTA is also used to perform the prompt for WNDXLP. This is possible
C  because a LOOPS specification looks formally like a sets specification. It
C  should not be interpreted as such, however, so the checks on existence of 
C  sets are suppressed.
C
C	The second dimension of SETS must be (0:1) at least, to hold the number
C  of valid spec lines in row 0 and one spec in row 1. If loop specs are to be
C  expected, one additional line must be reserved for every single one.
C	The 'loops' in this routine represent set specifications of the form
C  <i>-<j>:<k> and have nothing to do with the LOOPS specification associated
C  with the SETS being specified here. 
C
C******************************************************************************
C Organisation of array SETS[0:7,0:*] (inferred by JPH from the code)
C******************************************************************************
C
C header line SETS[0:7,0]
C =======================
C	SETS[0,0] = nr of valid lines; first valid line is SETS[0:7,1]
C 
C valid lines SETS[0:7, ]
C =======================
C simple specification:
C	SETS[0:4, ]	group, observation etc.; bit 29 clear
C
C primary line of loop specification:
C	SETS[i, ]	bit 29 set; bits 0-28 point to the line number in SETS 
C			 where the loop parameters are stored
C
C specials:
C	SETS[1, ] = -2: # set number; value in SETS[0, ]; SETS[2-7, ] ignored
C	SETS[i, ] = -1: * wildcard: all sets
C
C loop parameters:
C	SETS[0, ]	reserved
C	SETS[1, ]	bit 30 set; bits 0-29 are back ptr to primary line
C	SETS[2, ]	start value
C	SETS[3, ]	end value, -1 = "*" (= all remaining)
C	SETS[4, ]	increment 
C
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SSH_O_DEF'		!SET INFO
C
C  Parameters:
C
	INTEGER MXLSET			!LOCAL SETS
	  PARAMETER (MXLSET=64)
C
C  Entry points:
C
	LOGICAL WNDSTQ
	LOGICAL WNDSTM
C
C  Arguments:
C
	CHARACTER*(*) KW		!KEYWORD TO USE
	INTEGER MXS			!MAX. # OF SETS
	INTEGER SETS(0:SOF__N-1,0:*)	!SETS TO DO
	INTEGER FCAIN			!FCA of input file
	CHARACTER*(*) STROUT		!Output string for WNDSTM
C
C  Function references:
C
	LOGICAL WNDPAR			!GET USER DATA
	LOGICAL WNDSTA_X		!EXTRA KEYWORD LAYER
	INTEGER WNCAJ			!GET INTEGER VALUE
	INTEGER WNCAL0			!STRING LENGTH
	LOGICAL WNCATD			!TEST DIGIT
	LOGICAL WNCASC,WNCATC		!TEST/SKIP CHARACTER
	INTEGER WNFTFC			!TEST PROPER FCA
	LOGICAL WNDSTH			! get sector header
C
C  Data declarations:
C
	CHARACTER*32  CSET(MXLSET)	!LOCAL SETS SPECIFIED
	CHARACTER*128 TXT		!PROMPT TEXT
	LOGICAL       ASK		!ASK QUESTION OR JUST DECODE
	INTEGER	      LOCMXS		! actual max nr of sets
	INTEGER       SNAM(0:7)		! sector/image/cut name
	CHARACTER*6   TYPE		! 
C-
	WNDSTA=.TRUE.					!ASSUME OK
	ASK=.TRUE.					!ASK QUESTION
	TXT='""'					!NO PROMPT
	J=3
	GOTO 10

	ENTRY WNDSTM(SETS,STROUT)
C
	WNDSTM=.TRUE.					!ASSUME OK
	ASK=.FALSE.					!JUST DECODE
	STROUT='???'					!Cannot decode
	GOTO 11
C
	ENTRY WNDSTQ(KW,MXS,SETS,FCAIN)
C
	WNDSTQ=.TRUE.					!ASSUME OK
	ASK=.TRUE.					!ASK QUESTION

C*******************************************************************************
C Decode the existing values of SETS
C*******************************************************************************
C
  11	CONTINUE
	A_J(0)=1				! hold local prompt etc.
	IF (SETS(SOF_0_NLINE,0).LE.0) THEN	!NO PROMPT
	  TXT='""'
	  J=3					!first free char. in TXT
	ELSE
	  TXT=' '				!START EASY
	  J=1					!first free char. in TXT
	  DO I=1,SETS(SOF_0_NLINE,0)		!loop all valid lines in SETS
	    IF (I.GT.1) THEN			!add ","
	      TXT(J:J)=','
	      J=J+1
	    END IF
	    IF (SETS(SOF_SPEC,I).EQ.SOF_M_SPEC) THEN !absolute set nr, add "#"
	      TXT(J:J)='#'
	      J=J+1
	      IF (SETS(0,I).EQ.SOF_M_ALL) THEN	!wildcard set nr, add "*"
		TXT(J:)='*'
	      ELSE IF (IAND(SETS(0,I),
	1		SOF_M_LOOP).NE.0) THEN 	!LOOP specification
		J0=IAND(SETS(0,I),SOF_M_LO)	!line nr WHERE DEFINED
		IF (SETS(SOF_L_END,J0).NE.SOF_M_ALL) THEN !complete,
		  CALL WNCTXS(TXT(J:),		! add <start>-<end>:<increment>
	1		'!UJ\-!UJ\:!UJ',
	1		SETS(SOF_L_START,J0),SETS(SOF_L_END,J0),
	1		SETS(SOF_L_INC,J0))
		ELSE				!open-ended,
		  CALL WNCTXS(TXT(J:),		! add <start>-*:<increment>
	1		'!UJ\-*:!UJ',
	1	  	SETS(SOF_L_START,J0),SETS(SOF_L_INC,J0))
		END IF
	      ELSE
		CALL WNCTXS(TXT(J:),
	1		'!UJ',SETS(0,I))	!single VALUE
	      END IF
	      J=WNCAL0(TXT)+1			!NEW LENGTH
	    ELSE				!"." specification
	      DO I1=0,SOF__N-1			!ALL LEVELS
		IF (SETS(I1,I).EQ.SOF_M_ALL) THEN !wildcard, add "*"
		  TXT(J:)='*'
		ELSE IF (IAND(SETS(I1,I),
	1		SOF_M_LOOP).NE.0) THEN 	!LOOP, see above
		  J0=IAND(SETS(I1,I),SOF_M_LO)
		  IF (SETS(SOF_L_END,J0).NE.SOF_M_ALL) THEN
		    CALL WNCTXS(TXT(J:),'!UJ\-!UJ\:!UJ',
	1		SETS(SOF_L_START,J0),SETS(SOF_L_END,J0),
	1		SETS(SOF_L_INC,J0))
		  ELSE
		    CALL WNCTXS(TXT(J:),'!UJ\-*:!UJ',
	1		SETS(SOF_L_START,J0),SETS(SOF_L_INC,J0))
		  END IF
		ELSE				!value, see above
		  CALL WNCTXS(TXT(J:),'!UJ',SETS(I1,I))
		END IF
		J=WNCAL0(TXT)+1
		TXT(J:J)='.'			!add "."
		J=J+1
		DO I2=I1+1,SOF__N-1		!check remaining levels
		  IF (SETS(I2,I).NE.SOF_M_ALL) GOTO 30 ! any non-wildcard left
		END DO
		GOTO 31				!no, this specification done
 30		CONTINUE
	      END DO
 31	      CONTINUE
	      J=J-1				!DELETE TRAILING "."
	    END IF
	    IF (J.GT.LEN(TXT)-28) GOTO 32	!STOP INTERPRETING
	  END DO
 32	  CONTINUE
	END IF
C
C	For entry WNDSTM we just return the decoded string
C
	IF (.NOT.ASK) THEN
	   J=MAX(1,J-1)				!Strip trailing dot
	   STROUT=TXT(1:J)
	   CALL WNDPOHC
	   RETURN
	END IF
	GOTO 10

C*******************************************************************************
C prompt for SETS
C*******************************************************************************
 10	CONTINUE
	IF (KW(1:3).EQ.'SCN') TYPE='sector'
	IF (KW(1:3).EQ.'WMP') TYPE='image'
	IF (KW(1:3).EQ.'NGF') TYPE='cut'
	LOCMXS=MXS				! max nr of sets
	IF (MXS.EQ.0) LOCMXS=1			! if 0: 1 set AND 1 sector
	J=MAX(1,J-1)				!SET LENGTH
	IF (.NOT.WNDPAR(KW,CSET,
	1	MXLSET*LEN(CSET(1)),SETS(SOF_0_NLINE,0),
	1	TXT(1:J))) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) THEN	!NONE
	    GOTO 900				!NOT SPECIFIED
	  ELSE
	    GOTO 10				!RETRY
	  END IF
	END IF
C
C SETS(0,0) is the number of specifications the user has given
C
	IF (SETS(SOF_0_NLINE,0).EQ.0) GOTO 800	!READY, go set init. values
C
C The user may reply 'L' or 'O' in which case (for SCN and WMP) a layout
C or overview of the input file is given on the screen.
C
	IF (SETS(SOF_0_NLINE,0).GT.0.AND.
	1	(INDEX(CSET(1),'L').GT.0.OR.
	1	 INDEX(CSET(1),'O').GT.0) ) THEN	!O or L and not *
	   CALL WNDPOHC
	   IF (WNFTFC(FCAIN).NE.1) THEN
	      CALL WNCTXT(F_T,'No input file is available.')
	   ELSE IF (INDEX(KW,'SCN_SET')  .GT.0 .OR.
	1	    INDEX(KW,'SCN_LOOPS').GT.0) THEN
	      CALL NSCPFL(F_T,FCAIN,' ',(INDEX(CSET(1),'O').GT.0))
	   ELSE IF (INDEX(KW,'WMP_SET')  .GT.0 .OR.
	1	    INDEX(KW,'WMP_LOOPS').GT.0) THEN
	      CALL NMAPFL(F_T,FCAIN,' ',(INDEX(CSET(1),'O').GT.0))
	   ELSE IF (INDEX(KW,'NGF_SET')  .GT.0 .OR.
	1	    INDEX(KW,'NGF_LOOPS').GT.0) THEN
	      CALL NGCPFL(F_T,FCAIN,' ',(INDEX(CSET(1),'O').GT.0))
	   ELSE
	      CALL WNCTXT(F_T,
	1      'No layout/overview can be given for this filetype')
	   END IF
	   GOTO 10
C
C The user may reply '@' or '>', in which case we continue to prompt for the
C  individual components of the sets specification
C
	ELSE IF (SETS(SOF_0_NLINE,0).GT.0.AND.
	1        ((INDEX(CSET(1),'@').GT.0).OR.
	1		(INDEX(CSET(1),'>').GT.0))) THEN
	  JS=WNDSTA_X(KW,TXT,CSET(1),
	1		SETS(SOF_0_NLINE,0))	!go prompt for each level
	  IF (.NOT.JS) GOTO 10			!error: TRY WNDPAR AGAIN
	  TXT=CSET(1)				!SUCCESS: copy result to prompt
	  J=WNCAL0(TXT)+1			! default string
	  GOTO 10				!go get user's confirmation
	END IF
C
C Start interpreting
C
	IF (SETS(SOF_0_NLINE,0).LT.0) THEN	!*
	  SETS(SOF_0_NLINE,0)=1			!1 LINE
	  DO I=0,SOF__N-1
	    SETS(I,1)=SOF_M_ALL			!all wildcards
	  END DO
	  GOTO 700				!done
	END IF
	IF (SETS(SOF_0_NLINE,0).GT.LOCMXS) THEN	!TOO MANY, reprompt
	  CALL WNCTXT(F_TP,'Too many sets defined')
	  GOTO 10
	END IF
	J0=SETS(SOF_0_NLINE,0)			!reserve a line in SETS
						! for each spec in CSETS
	DO I=1,SETS(SOF_0_NLINE,0)		!loop over specs
	  J=1					!pointer in CSET(I) text
	  CALL WNCASB(CSET(I),J)		!SKIP BLANKs if any
	  IF (WNCASC(CSET(I),J,'#')) THEN	!#
	    SETS(SOF_SPEC,I)=SOF_M_SPEC		!INDICATE #
	    SETS(0,I)=SOF_M_ALL			!assume *
	    IF (WNCASC(CSET(I),J,'*')) THEN	!#*
	      SETS(0,I)=SOF_M_ALL
	    ELSE IF (WNCATD(CSET(I),J)) THEN	!if digit, decode
	      J=J-1
	      SETS(0,I)=WNCAJ(CSET(I),
	1	LEN(CSET(I)),J)			!decode VALUE
	      J=J+1
	      IF (WNCATC(CSET(I),J,'-') .OR.	!if at "-" or ":",
	1	WNCATC(CSET(I),J,':')) THEN	! loop spec.
		IF (J0.GE.LOCMXS) THEN		!if no room left,
		  CALL WNCTXT(F_TP,
	1		'Too many loop definitions')
		  GOTO 10			! reprompt
		END IF
		J0=J0+1				!LOOP DEFINITION LINE
		SETS(0,J0)=0			!LOCAL USE
		SETS(SOF_L_DEF,J0)=SOF_M_SLOOP+I !back ptr to primary line
		SETS(SOF_L_START,J0)=SETS(0,I)	!START VALUE
		SETS(SOF_L_END,J0)=SOF_M_ALL	!ASSUME no end value
		SETS(SOF_L_INC,J0)=1		!assume INCREM. OF 1
		SETS(0,I)=SOF_M_LOOP+J0		!primary line points to loop 
						! spec
		IF (WNCASC(CSET(I),J,'-')) THEN	!if "-" look for end value
		  IF (WNCASC(CSET(I),J,'*'))
	1		THEN			! "-*" was assumed
		  ELSE IF (WNCATD(CSET(I),J))
	1		THEN			!if DIGIT: end value, decode
		    J=J-1
		    SETS(SOF_L_END,J0)=WNCAJ(CSET(I),
	1		LEN(CSET(I)),J)
		    J=J+1
		  END IF
		END IF
		IF (WNCASC(CSET(I),J,':')) THEN	!if ":" look for increment
		  IF (WNCASC(CSET(I),J,'*'))
	1		THEN			!"*" means 1 as assumed
		  ELSE IF (WNCATD(CSET(I),J))
	1		 THEN			!if DIGIT: increment, decode
		    J=J-1
		    SETS(SOF_L_INC,J0)=MAX(1,WNCAJ(CSET(I),
	1		LEN(CSET(I)),J))
		    J=J+1
		  END IF
		END IF
	      END IF
	    END IF
	    IF (CSET(I)(J:).NE.' ') THEN	!error if any non-blank left
 20	      CONTINUE
	      CALL WNCTXT(F_TP,'Format error in !AS',CSET(I))
	      GOTO 10
	    END IF
	  ELSE					!"." spec
	    DO I1=0,SOF__N-1			!ALL LEVELS
	      SETS(I1,I)=SOF_M_ALL		!decode as above
	      IF (WNCASC(CSET(I),J,'*')) THEN
	      ELSE IF (CSET(I)(J:).EQ.' ') THEN
	      ELSE IF (WNCATD(CSET(I),J)) THEN
	        J=J-1
	        SETS(I1,I)=WNCAJ(CSET(I),LEN(CSET(I)),J)
	        J=J+1
	        IF (WNCATC(CSET(I),J,'-') .OR.
	1		WNCATC(CSET(I),J,':')) THEN
		  IF (J0.GE.LOCMXS) THEN
		    CALL WNCTXT(F_TP,'Too many loop definitions')
		    GOTO 10
		  END IF
		  J0=J0+1
		  SETS(0,J0)=0
		  SETS(SOF_L_DEF,J0)=SOF_M_SLOOP+I
		  SETS(SOF_L_START,J0)=SETS(I1,I)
		  SETS(SOF_L_END,J0)=SOF_M_ALL
		  SETS(SOF_L_INC,J0)=1
		  SETS(I1,I)=SOF_M_LOOP+J0
		  IF (WNCASC(CSET(I),J,'-')) THEN
		    IF (WNCASC(CSET(I),J,'*')) THEN
		    ELSE IF (WNCATD(CSET(I),J)) THEN
		      J=J-1
		      SETS(SOF_L_END,J0)=WNCAJ(CSET(I),LEN(CSET(I)),J)
		      J=J+1
		    END IF
		  END IF
		  IF (WNCASC(CSET(I),J,':')) THEN
		    IF (WNCASC(CSET(I),J,'*')) THEN
		    ELSE IF (WNCATD(CSET(I),J)) THEN
		      J=J-1
		      SETS(SOF_L_INC,J0)=
	1			MAX(1,WNCAJ(CSET(I),LEN(CSET(I)),J))
		      J=J+1
		    END IF
		  END IF
	        END IF
	      END IF
	      IF (WNCASC(CSET(I),J,'.')) THEN	!. OK
	      ELSE IF (CSET(I)(J:).EQ.' ') THEN	!OK
	      ELSE				!ERROR
	        GOTO 20				!go report and reprompt
	      END IF
	    END DO
	  END IF
	END DO
C
C Check for any sector at all
C
 700	CONTINUE
	IF (INDEX(KW,'LOOPS').EQ.0) THEN
	  DO I=1,SOF__N-1				!INIT SEARCH
	    SETS(I,0)=0
	  END DO
	  IF (.NOT.WNDSTH(FCAIN,SETS,I1,I2,SNAM)) THEN
	    CALL WNCTXT(F_T,
	1	'Error: None of the requested !AS\s exist',TYPE)
	    GOTO 10
	  ENDIF
	END IF
C
C Check for one sector only
C
	IF (MXS.EQ.0) THEN
	  IF (WNDSTH(FCAIN,SETS,I1,I2,SNAM)) THEN
	    CALL WNCTXT(F_T,
	1	'Error: You may specify only a single !AS here',TYPE)
	    GOTO 10
	  ENDIF
	ENDIF
C
C Ready
C
800	CONTINUE
	DO I=1,SOF__N-1				!INIT SEARCH
	  SETS(I,0)=0
	END DO
C
	CALL WNDPOHC				! clear local prompt etc.	
	RETURN					!READY
C
C ERROR
C
 900	CONTINUE
	DO I=0,SOF__N-1
	  SETS(I,0)=0				!SET NONE
	END DO
	WNDSTA=.FALSE.				!ERROR
C
	CALL WNDPOHC				! clear local prompt etc.
	RETURN
C
C
	END
