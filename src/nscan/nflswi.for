C+ NFLSWI.FOR
C  JEN 940215
C
C  Revisions:
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
	LOGICAL FUNCTION NFLSWI (ACTION,NAME,ILVAL)
C
C  Routine for transfer of `flagging-mode' switches between 
C  sub-routines.
C
C  Result:
C
C	JS = NFLSWI (ACTION_C(*):I,NAME_C(*):I,ILVAL_J:IO)
C		NB: ILVAL may also be a logical TRUE/FALSE .....
C
C  JS = NFLSWI ('SET','<name>',<ILVAL>)
C  JS = NFLSWI ('GET','<name>',<ILVAL>)
C  JS = NFLSWI ('SHOW','<name>',0)
C  JS = NFLSWI ('SHOW','ALL',0)
C
C PIN references:
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NFL_DEF'
C
C  Parameters:
C
	INTEGER MAXSWI			!MAX NR OF STORED SWITCHES
	  PARAMETER (MAXSWI=5)		
C
C  Arguments:
C
	CHARACTER ACTION*(*)		!ACTION TO BE PERFORMED
	CHARACTER NAME*(*)		!EXTRA INFORMATION
	INTEGER ILVAL                    !INTEGER (or logical) I/O VALUE
C
C  Function references:
C
	LOGICAL WNDPAR			!GET DWARF PARAMETER
C
C  Data declarations:
C
	INTEGER INDEX			!INDEX IN STORAGE ARRAY
	INTEGER SWITCH(-1:MAXSWI)	!STORAGE ARRAY
	CHARACTER*12 SWINAME(MAXSWI)	!SWITCH NAMES
C
        CHARACTER*80 TXT80              !TEXT BUFFER
	CHARACTER*80 ARGSTR
C
C  Commons:
C
	COMMON /NFLSWITCH/ SWITCH
C-
C******************************************************************************
C******************************************************************************
C
	NFLSWI = .TRUE.                         !ASSUME OK
CCCC	CALL WNCTXT (F_T,'NFLSWI: '//ACTION(:5)//NAME)
C
C***************************************************************************
C
	DO INDEX=-1,MAXSWI
	  SWINAME(INDEX) = 'undefined'
	END DO
	SWINAME(1) = 'USERFLAG'
	SWINAME(2) = 'CORRDAT'
	SWINAME(3) = 'TRACE'
	SWINAME(4) = 'SHOW_CNT'
	SWINAME(5) = 'ESTIM_DFLT'
C
	INDEX = 0                               !STORAGE INDEX
	IF (NAME(:7).EQ.'ALL') THEN
          INDEX = -1	                        !SPECIAL INDEX
	ELSE
	  DO I=1,MAXSWI
	    DO I1=LEN(NAME),5,-1
	      IF (NAME(:I1).EQ.SWINAME(I)(:I1)) THEN
		INDEX = I			!FOUND
		GOTO 10				!ESCAPE
	      END IF
	    END DO
	  END DO
	  ARGSTR='NFLSWI: '//ACTION(:5)//'Name not recognised: '//NAME
          CALL WNCTXT (F_T,ARGSTR)
          NFLSWI = .FALSE.
          GOTO 900                               !ESCAPE             
	END IF
 10	CONTINUE
C
C===========================================================================
C
	IF (ACTION(:3).EQ.'SET') THEN
	  IF (INDEX.GT.0) THEN
	    SWITCH(INDEX) = ILVAL
	  END IF
	  
	ELSE IF (ACTION(:3).EQ.'GET') THEN
	  IF (INDEX.GT.0) THEN
	    ILVAL = SWITCH(INDEX)
	  END IF
C
	ELSE IF (ACTION(:4).EQ.'SHOW') THEN
	  IF (INDEX.GT.0) THEN
            I1=INDEX
	    I2=INDEX
	  ELSE IF (INDEX.EQ.-1) THEN		!SHOW ALL
	    I1=1
	    I2=MAXSWI
	  END IF
          DO I=I1,I2
	    CALL WNCTXS (TXT80,'  '//SWINAME(I)//'= '
     1                   //'= !SJ '
     1                   ,SWITCH(I))
          END DO
C
	ELSE
	END IF
C
C===========================================================================
 900	CONTINUE
	RETURN
	END






