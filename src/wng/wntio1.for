C+ WNTIO1.FOR
C  WNB 930501
C
C  Revisions:
C
	SUBROUTINE WNTIO1(TP,FENTB,CNAM,UC,LC)
C
C  Get name of data
C
C  Result:
C
C	CALL WNTIO1( TP_J:I, FENTB_S:I, CNAM_C*:I, UC_C*:O, LC_C:*:O)
C			Get Upper and LowerCase name from data structure
C			FENTB. (TP irrelevant)
C			CNAM is used to generate a unique name
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'WNT_O_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER TP		!FORTRAN(0)/C
	BYTE FENTB(0:*)		!DATA STRUCTURE
	CHARACTER*(*) CNAM	!COMMON NAME
	CHARACTER*(*) UC	!UPPER CASE NAME
	CHARACTER*(*) LC	!LOWER CASE NAME
C
C  Function references:
C
C
C  Data declarations:
C
C-
	CALL WNGMTS(WNTF_NAM_N,FENTB(WNTF_NAM_1),UC)	!NAME
	IF (UC(1:1).EQ.'-') THEN			!NO NAME
	  CALL WNCTXS(LC,'!AS!AS',CNAM,UC(2:))		!MAKE NAME
	  UC=LC
	END IF
	LC=UC						!LC
	CALL WNCALC(LC)
C
	RETURN
C
C
	END
