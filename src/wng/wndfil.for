C+ WNDFIL.FOR
C  WNB 900130
C
C  Revisions:
C	WNB 910909	Add save of Infix and Datab
C	CMV 940223      DATAB not used if set to "*"
C	CMV 940422	If NODIN ends in PFX, assume user typed PFX himself
C	CMV 940808	Option to select names from list of matching names
C	CMV 940822	Message if DATAB/INFIX change (previously in DWARF)
C
	LOGICAL FUNCTION WNDFIL(NODIN,PFX,NODOUT,FILOUT)
C
C  Convert given node name to file name and full node name
C
C  Result:
C
C	WNDFIL_J = WNDFIL( NODIN_C*:I, PFX_C*:I, NODOUT_C*:O,
C				FILOUT_C*:O )
C			Convert node NODIN into file name FILOUT, using
C			postfix PFX. NODOUT gives the translated input
C			node name.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WND_DEF'
C
C  PIN:
C
C	DATAB
C	INFIX
C
C  Parameters:
C
C
C  Arguments:
C
	CHARACTER*(*) NODIN			!INPUT NODE NAME
	CHARACTER*(*) PFX			!NAME TRAILER
	CHARACTER*(*) NODOUT			!NODE NAME RETURNED
	CHARACTER*(*) FILOUT			!OUTPUT FILE NAME
C
C  Function references:
C
	INTEGER WNCALN				!STRING LENGTH
	INTEGER WNCAJ				!GET VALUE FROM STRING
	LOGICAL WNGSDL				!LIST MATCHING NODES
	LOGICAL WNDPAP				!SAVE USER PARAMETER
C
C  Data declarations:
C
	CHARACTER*160 STRIN,STRIN1		!COPY INPUT
	LOGICAL NODATAB				!DO NOT UPDATE DATAB
	LOGICAL DO_NEXT				!USED IN FILE SEARCHING
	LOGICAL DO_DATAB			!FLAG DATAB CHANGED
C-
C
C GET/SET DATABASE
C
	WNDFIL=.TRUE.				!ASSUME OK
	STRIN=NODIN
	NODATAB=(DATAB.EQ.'*')			!Use datab?
C
C HANDLE PURE FILENAMES
C
	IF (INDEX(STRIN,'@').NE.0) THEN		!PURE FILE NAME
	  FILOUT=STRIN(INDEX(STRIN,'@')+1:)
	  NODOUT='UNKNOWN'			!NO NODE GIVEN
	  RETURN
	END IF
C
C GET Nth FILE FROM LIST
C
	IF (STRIN(1:1).EQ.'#'.AND.
	1	STRIN(2:2).GE.'0'.AND.STRIN(2:2).LE.'9') THEN
	   I1=1						!OFFSET FOR WNCAJ
	   I3=WNCAJ(STRIN,WNCALN(STRIN),I1)		!GET VALUE
	   I1=1						!INIT SEARCH
	   I2=0						!COUNT MATCHES
	   DO_NEXT=.TRUE.				!FIND FILES
	   DO WHILE (DO_NEXT)
	      IF (WNGSDL(STRIN,DATAB,PFX,I1)) THEN	!FIND NEXT
	         I2=I2+1				!COUNT THIS ONE
	         DO_NEXT=(I2.NE.I3)
	      ELSE
	         DO_NEXT=.FALSE.			!NO MORE FILES
	      END IF
	   END DO
	   IF (I2.EQ.I3) THEN				!FOUND Nth
	      CALL WNCTXT(F_T,'Selected node !AS',STRIN)
	   ELSE						!NOT FOUND
	      CALL WNCTXT(F_T,'Could not find node #!UJ',I3)
	      WNDFIL=.FALSE.				!ERROR RETURN
	      RETURN
	   END IF
	END IF
C
C EXTRACT DATABASE NAME
C
	DO_DATAB=.FALSE.			!ASSUME NOT CHANGED
	IF (INDEX(STRIN,']').NE.0) THEN		!DATABASE GIVEN
	  DATAB=STRIN(1:INDEX(STRIN,']'))	!SAVE DATABASE
	  STRIN1=STRIN(INDEX(STRIN,']')+1:)
	  STRIN=STRIN1
	  DO_DATAB=.TRUE.
	ELSE IF (INDEX(STRIN,':').NE.0) THEN
	  DATAB=STRIN(1:INDEX(STRIN,':'))	!SAVE DATABASE
	  STRIN1=STRIN(INDEX(STRIN,':')+1:)
	  STRIN=STRIN1
	  DO_DATAB=.TRUE.
	ELSE IF (INDEX(STRIN,'/') .NE.0) THEN
	  J=INDEX(STRIN,'/')
	  DO WHILE (INDEX(STRIN(J+1:),'/').NE.0) !SEARCH END
	    J=J+INDEX(STRIN(J+1:),'/')
	  END DO
	  DATAB=STRIN(1:J)			!SAVE DATABASE
	  STRIN1=STRIN(J+1:)
	  STRIN=STRIN1
	  DO_DATAB=.TRUE.
	END IF
C
	IF (DO_DATAB) THEN
	  CALL WNCALC(DATAB)			!MAKE SURE LC
	  IF (.NOT.NODATAB) THEN
	     JS=WNDPAP('NGEN$DATAB',DATAB)	!SAVE DATA BASE GENERAL
	     CALL WNCTXT(F_T,
	1	'Your default directory (DATAB) is now !AS',
	1	DATAB(:WNCALN(DATAB)))
	  END IF
	END IF
C
C FILL PREFIX
C
 10	CONTINUE
	J=INDEX(STRIN,'#')
	IF (J.NE.0) THEN
	  IF (PREFIX.NE.' ') THEN
	    STRIN1=STRIN(1:J-1)//PREFIX(1:WNCALN(PREFIX))//STRIN(J+1:) !COPY
	  ELSE
	    STRIN1=STRIN(1:J-1)//STRIN(J+1:)	!COPY
	  END IF
	  STRIN=STRIN1
	  GOTO 10				!RETRY
	END IF
C
C FIND PREFIX
C
	J=INDEX(STRIN,'(')
	IF (J.NE.0) THEN
	  J1=INDEX(STRIN(J+1:),')')
	  IF (J1.EQ.0) THEN			!NO CLOSING )
	    FILOUT=' '
	    NODOUT=' '
	    WNDFIL=.FALSE.
	    RETURN
	  END IF
	  PREFIX=STRIN(J+1:J+J1-1)		!SAVE PREFIX
	  JS=WNDPAP('NGEN$INFIX',PREFIX)	!SAVE PREFIX GENERAL
	  CALL WNCTXT(F_T,
	1	'INFIX is now !AS (replaces # in nodenames)',
	1	PREFIX(:WNCALN(PREFIX)))
C
	  IF (J.EQ.1) THEN			!DELETE ()
	    STRIN1=STRIN(J+1:J+J1-1)//STRIN(J+J1+1:)
	  ELSE
	    STRIN1=STRIN(1:J-1)//STRIN(J+1:J+J1-1)//STRIN(J+J1+1:)
	  END IF
	  STRIN=STRIN1
	END IF
C
C STRIP PFX IF ACCIDENTALLY GIVEN BY USER
C
	J0=WNCALN(STRIN)
	J=WNCALN(PFX)
	IF (J0.GT.J+1) THEN
	   IF (STRIN(J0-J:J0).EQ.'.'//PFX(1:3)) STRIN=STRIN(:J0-J-1)
	END IF
C
C SET CONVERTED NODE NAME
C
	NODOUT=STRIN
	J0=WNCALN(STRIN)+1			!APPEND .
	STRIN(J0:J0)='.'
C
C CONVERT TO FILE NAME
C
 20	CONTINUE
	J=INDEX(STRIN,'.')			!POSITION .
	IF (J.NE.0) THEN			!MAKE _
	  STRIN(J:J)='_'
	  GOTO 20
	END IF
	J1=1
 21	CONTINUE
	J2=INDEX(STRIN(J1+1:),'_')		!SET .
	IF (J1+J2.GT.36 .OR. J2.EQ.0) THEN
	  STRIN(J1:J1)='.'
	ELSE
	  J1=J1+J2
	  GOTO 21
	END IF
	STRIN(J0+1:)=PFX			!ADD POSTFIX
	CALL WNCAUC(STRIN)			!MAKE SURE UC
	IF (DATAB.EQ.' '.OR.DATAB.EQ.'*') THEN	!RETURN FILE NAME
	  FILOUT=STRIN
	ELSE
	  FILOUT=DATAB(1:WNCALN(DATAB))//STRIN
	END IF
C
	IF (NODATAB) DATAB='*'			!Reset datab
C
	RETURN					!READY
C
C
	END
