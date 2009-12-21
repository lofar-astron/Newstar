C+ NSCXES.FOR
C  WNB 910211
C
C  Revisions:
C	WNB 931214	Correct for string data; add P:, S:
C	WNB 931215	Add specified formats
C
	SUBROUTINE NSCXES(PTYPE,DAT,EDL,EDC,EDJ,PLIST,
	1		PNXT,PEPTR,PHP,PSZ)
C
C  Edit an area in detail
C
C  Result:
C
C	CALL NSCXES ( PTYPE_J:I, DAT_B(*):I, EDL_J:I, EDC_C*(4,*):I,
C				EDJ_J(4,*):I, PLIST_C*(*):I, PNXT_J:O,
C				PEPTR_J:O, PHP_J:O, PSZ_J(0:1)_O)
C					Edit the area DAT and show on PTYPE
C					with EDL edit lines given in
C					EDC and EDJ.
C					PLIST contains a list (last ' ') of
C					known P:/S: values.
C					PNXT will return 0 if ready, number
C					in list if P:/S: recognised; then
C					PEPTR points to line in ED; PHP to
C					disk address. PSZ returns the P: ()
C					and / values.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  PIN references:
C
C	EDIT
C
C  Parameters:
C
	INTEGER LENFLD			!LENGTH INPUT FIELD
	  PARAMETER (LENFLD=80)
	INTEGER MXNFLD			!MAX. # OF INPUT FIELDS
	  PARAMETER (MXNFLD=16)
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (f_p, f_t ETC)
	BYTE DAT(0:*)			!DATA AREA
	INTEGER EDL			!LENGTH EDIT ARRAYS
	CHARACTER*(*) EDC(4,*)		!EDIT DATA
	INTEGER EDJ(4,*)		!EDIT DATA
	CHARACTER*(*) PLIST(*)		!LIST OF P:NAMES
	INTEGER PNXT			!RETURN VALUE
	INTEGER PEPTR			!WHERE FOUND IN EDIT LIST
	INTEGER PHP			!WHERE ON DISK
	INTEGER PSZ(0:1)		!OFFSET AND NUMBER OF P:
C
C  Function references:
C
	LOGICAL WNDPAR			!GET USER DATA
	INTEGER WNCAFU			!MINI-MAX FIT
	LOGICAL WNCACJ			!GET J FROM STRING
	LOGICAL WNCAFN			!GET FIELD
	LOGICAL WNCASC			!TEST CHARACTER
	INTEGER WNCALN			!STRING LENGTH
	INTEGER WNGGJ			!GET J
C
C  Data declarations:
C
	CHARACTER*(LENFLD) TXT(MXNFLD)	!INPUT DATA
	CHARACTER*16 TXT1		!FORMAT
	CHARACTER*16 LPED		!LOCAL EDIT
	CHARACTER*16 LPFORM		!LOCAL P: NAME
	CHARACTER*10 LEDC(4)		!LOCAL COPY EDIT INFO
	INTEGER LEDJ(4)
	INTEGER LIDX			!LOCAL INDEX
	CHARACTER*8 LNAM,LPNAM		!LOCAL NAME
	INTEGER LSIZ			!LOCAL SIZE
	INTEGER LFLD			!OFFSET IF NO NAME
	LOGICAL LOVER			!OVERWRITE PROTECTION
	LOGICAL LREL			!RELATIVE ADDRESS
C-
C
C GET EDIT INFO
C
 10	CONTINUE
	PNXT=0						!ASSUME LAST IN P: TREE
	IF (.NOT.WNDPAR('EDIT',TXT,MXNFLD*LENFLD,J0,'""')) THEN
	  IF (E_C.EQ.DWC_ENDOFLOOP) RETURN		!READ
	  GOTO 10					!RETRY
	ELSE IF (J0.EQ.0) THEN				!NO MORE
	  RETURN
	ELSE IF (J0.LT.0) THEN				!DO SHOW
	  CALL WNCTXT(PTYPE,' ')
	  CALL NSCXXS(PTYPE,DAT,EDL,EDC,EDJ)
	  GOTO 10					!MUST SPECIFY
	END IF
	CALL WNCAUC(TXT(1))				!MAKE UC
C
C SET LOCAL FORMATS
C
	I=INDEX(TXT(1),':')				!FIND :
	IF (I.GT.0) THEN				!LOCAL FORMAT GIVEN
	  LPED=TXT(1)(I+1:)
	  TXT(1)(I:)=' '
	  IF (LPED.EQ.' ') LPED='-'			!ASK QUESTION
	ELSE
	  LPED=' '
	END IF
	IF (LPED(1:1).EQ.':') THEN			!P: ASKED
	  LPFORM=LPED
	  LPED=' '
	ELSE
	  LPFORM=' '
	END IF
	IF (LPED.NE.' ' .AND. LPED.NE.'-') THEN
	  IF (INDEX(LPED,'$').LE.0) THEN		!NO FIELDWIDTH
	    LPED='26$'//LPED
	  END IF
	END IF
	I=1						!ANALYSE NAME
	CALL WNCASB(TXT(1),I)				!SKIP BLANK
	JS=WNCAFN(TXT(1),I,LNAM)
	IF (.NOT.JS) THEN				!NO NAME
	  IF (WNCASC(TXT(1),I,'.')) THEN		!RELATIVE ADDRESS
	    LREL=.TRUE.
	  ELSE
	    LREL=.FALSE.
	  END IF
	  CALL WNCASB(TXT(1),I)
	  IF (.NOT.WNCACJ(TXT(1),I,10,LFLD)) THEN	!NO VALUE
	    IF (LPFORM.EQ.' ') THEN
	      CALL WNCTXT(PTYPE,'Known names:')
	      DO I=1,EDL
	        CALL WNCTXT(PTYPE,'!_!8$4AS; !5$4UJ',
	1		EDC(1,I),EDJ(1,I))		!SHOW NAMES
	      END DO
	      GOTO 10
	    END IF
	  END IF
	END IF
	CALL WNCASB(TXT(1),I)
	IF (WNCASC(TXT(1),I,'(')) THEN			!INDEX
	  JS=WNCACJ(TXT(1),I,10,LIDX)			!GET INDEX
	ELSE
	  LIDX=0
	END IF
	LIDX=MAX(0,LIDX)
	CALL WNCASB(TXT(1),I)
	JS=WNCASC(TXT(1),I,')')				!POSSIBLE )
	IF (WNCASC(TXT(1),I,'/')) THEN			!SIZE
	  CALL WNCASB(TXT(1),I)
	  IF (WNCASC(TXT(1),I,'*')) THEN		!S: SPECIAL
	    LSIZ=-1
	    LIDX=0
	  ELSE
	    JS=WNCACJ(TXT(1),I,10,LSIZ)
	  END IF
	ELSE
	  LSIZ=0
	END IF
	LOVER=(WNCASC(TXT(1),I,'=') .AND. WNCASC(TXT(1),I,'=')) !UNPROTECT
	IF (LPFORM(1:1).EQ.':') THEN
	  I=2						!ANALYSE NAME P:
	  CALL WNCASB(LPFORM,I)				!SKIP BLANK
	  IF (.NOT.WNCAFN(LPFORM,I,LPNAM)) THEN		!NO NAME
	    I1=0
	    DO WHILE (PLIST(I1+1).NE.' ')
	      I1=I1+1
	    END DO
	    CALL WNCTXT(PTYPE,'!80$8Q\Known P: types: !#AS',I1,PLIST)
	    GOTO 10					!NEXT QUESTION
	  END IF
	  CALL WNCASB(LPFORM,I)
	  IF (WNCASC(LPFORM,I,'(')) THEN		!INDEX
	    JS=WNCACJ(LPFORM,I,10,PSZ(0))		!GET INDEX
	  ELSE
	    PSZ(0)=0
	  END IF
	  PSZ(0)=MAX(0,PSZ(0))
	  CALL WNCASB(LPFORM,I)
	  JS=WNCASC(LPFORM,I,')')			!POSSIBLE )
	  IF (WNCASC(LPFORM,I,'/')) THEN		!SIZE
	    JS=WNCACJ(LPFORM,I,10,PSZ(1))
	  ELSE
	    PSZ(1)=0
	  END IF
	ELSE
	  LPNAM=' '
	  PSZ(0)=0
	  PSZ(1)=0
	END IF
C
C FIND FIELD
C
	DO I=1,EDL
	  IF (EDC(1,I).EQ.LNAM) THEN
	    PEPTR=I
	    DO I1=1,4
	      LEDJ(I1)=EDJ(I1,I)			!COPY EDIT INFO
	      LEDC(I1)=EDC(I1,I)
	    END DO
	    IF (LPED.NE.' ') THEN
	      IF (LPED.EQ.'-') THEN			!FORMAT ASKED
		CALL WNCTXT(PTYPE,'Edit data: !4AS; !4UJ',
	1		LEDC,LEDJ)
		GOTO 10					!CONTINUE
	      END IF
	      LEDC(2)=LPED				!SET LOCAL FORMAT
	    END IF
	    LIDX=MIN(LIDX,LEDJ(2)-1)			!LIMIT OFFSET
	    LEDJ(1)=LEDJ(1)+LIDX*LEDJ(4)		!PROPER OFFSET
	    LEDJ(2)=LEDJ(2)-LIDX			!MAX. NUMBER
	    IF (LSIZ.GT.0) LEDJ(2)=MIN(LEDJ(2),LSIZ)	!LIMIT SIZE
	    IF (LPNAM.NE.' ') THEN
	      LEDC(4)='P:'//LPNAM			!SET NAME
	      PEPTR=-PEPTR				!INDICATE USER GIVEN
	    END IF
	    GOTO 20					!FOUND
	  END IF
	END DO
	IF (LNAM.NE.' ') THEN
	  CALL WNCTXT(PTYPE,'Unknown fieldname')
	  GOTO 10					!RETRY
	END IF
	IF (LPNAM.EQ.' ') THEN
	  CALL WNCTXT(PTYPE,'Must have :: format for numeric address')
	  GOTO 10
	ELSE
	  LEDC(4)='P:'//LPNAM				!MAKE SURE FILLED
	END IF
C
C EDIT
C
 20	CONTINUE
	IF (J0.EQ.1) THEN				!READY
	  IF (LEDC(4)(1:2).EQ.'P:') THEN		!POINTER FIELD
	    PNXT=WNCAFU(LEDC(4)(3:),PLIST)		!FIND P: AREA
	    IF (PNXT.GT.0) THEN				!FOUND P:
	      IF (LNAM.EQ.' ') THEN			!NUMERIC ADDRESS
		PHP=LFLD
		IF (LREL) PNXT=PNXT+1000
	      ELSE IF (WNCALN(PLIST(PNXT)).EQ.1) THEN
	        PHP=LEDJ(1)				!SET OFFSET
	        PNXT=PNXT+1000
	      ELSE
	        PHP=WNGGJ(DAT(LEDJ(1)))			!DISK POINTER
	      END IF
	      RETURN
	    END IF
	  ELSE IF (LEDC(4)(1:2).EQ.'S:') THEN		!SUB-STRUCTURE
	    PNXT=WNCAFU(LEDC(4),PLIST)			!FIND S: AREA
	    IF (PNXT.GT.0) THEN
	      PHP=LEDJ(1)				!OFFSET
	      PSZ(0)=LSIZ				!GET SPECIAL *
	      IF (PSZ(0).NE.-1) PNXT=PNXT+1000		!OFFSET
	      RETURN
	    END IF
	  END IF
	ELSE IF ((IAND(LEDJ(3),1).NE.0 .AND. .NOT.LOVER) .OR.
	1		LEDC(4)(1:2).EQ.'P:' .OR.
	1		LEDC(4)(1:2).EQ.'S:') THEN
	  CALL WNCTXT(PTYPE,'Edit of field !AS not allowed',
	1		LEDC(1))
	ELSE
	  DO J3=0,MIN(J0-2,LEDJ(2)-1)			!DO ALL FIELDS
	    TXT1='!'//LEDC(2)				!FORMAT
	    IF (INDEX(LEDC(2),'AL').GT.0)		!AL
	1		CALL WNCTXS(TXT1(2:),'AL!UJ',LEDJ(4))
	    CALL WNCTXI(TXT(J3+2),TXT1,
	1		DAT(LEDJ(1)+J3*LEDJ(4)))	!GET VALUE
	  END DO
	END IF
C
C SHOW RESULT
C
 30	CONTINUE
	CALL NSCXXS(PTYPE,DAT,1,LEDC,LEDJ)		!SHOW LINE ITEM
C
	GOTO 10						!TRY AGAIN
C
C
	END