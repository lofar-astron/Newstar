C+ WNTIRL.FOR
C  WNB 930501
C
C  Revisions:
C
	LOGICAL FUNCTION WNTIRL(LUN,ISTR,IDAT,ICOM,CONTL)
C
C  Read a single line from input
C
C  Result:
C
C	WNTIRL_L = WNTIRL( LUN_J:I, ISTR_C*:O, IDAT_C*:O, ICOM_C*:O, CONTL_L:O)
C			Read a line (ISTR) from LUN, and separate it in
C			data (IDAT) and comment (ICOM). CONTL indicates
C			that a continuation line follows, or EOF detected.
C			WNTIRL returns .false. if error or EOF.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER LUN		!FILE TO READ FROM
	CHARACTER*(*) ISTR	!LINE
	CHARACTER*(*) IDAT	!DATA ON LINE
	CHARACTER*(*) ICOM	!COMMENT ON LINE
	LOGICAL CONTL		!CONTINUATION INDICATOR
C
C  Function references:
C
	INTEGER WNCALN		!LENGTH STRING
	LOGICAL WNCASC,WNCATC	!SKIP/TEST GIVEN CHARACTER
C
C  Data declarations:
C
C-
C
C PRELIMINARY
C
	WNTIRL=.TRUE.				!ASSUME OK
	ISTR=' '				!PREPARE
	IDAT=' '
	ICOM=' '
	CONTL=.FALSE.				!NO CONTINUATION LINE
C
C READ LINE
C
	READ (UNIT=LUN,FMT=1000,END=900,ERR=910) ISTR
 1000	FORMAT(A)
C
C SPLIT IN COMMENT FIELD
C
	I=1					!START BEGIN OF STRING
	CALL WNCAFX(ISTR,I,IDAT)		!GET DATA PART
	IF (WNCASC(ISTR,I,'!')) THEN		!COMMENT FOLLOWS
	  ICOM=ISTR(I:)				!SET COMMENT
	END IF
C
C LOOK FOR CONTINUATION
C
	I=WNCALN(IDAT)				!END DATA
	IF (WNCATC(IDAT,I,CHAR(92))) THEN	!CONTINUATION
	  IDAT(I:I)=' '				!DELETE CONTINUATION
	  CONTL=.TRUE.				!INDICATE CONTINUATION
	END IF
C
	RETURN
C
C ERRORS
C
 900	CONTINUE				!EOF
	CONTL=.TRUE.				!SHOW EOF
 910	CONTINUE				!READ ERROR
	WNTIRL=.FALSE.
C
	RETURN
C
C
	END
