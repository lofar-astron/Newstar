C+ WNFTVL.FOR
C  HjV 931202      Splitted because of HP-UX 09.01 problem 
C                       with character entry
C
C  Revisions:
C
	CHARACTER*(*) FUNCTION WNFTH1(FCA)
C
C  Get tape header info
C
C  Result:
C	WNFTH1_C80 = WNFTH1( FCA_J:I)
C			Get current tape HDR1
C
C  PIN references:
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'MCA_O_DEF'			!MCA
	INCLUDE 'FCA_O_DEF'			!FCA
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA				!DYNAMIC FILE AREA
C
C  Entry points:
C
C
C  Function references:
C
	INTEGER WNFTFC				!TEST FCA PRESENT
C
C  Data declarations:
C
C
C  Equivalences:
C
C
C  Commons:
C
C-
	WNFTH1=' '					!ASSUME ERROR
	I0=WNFTFC(FCA)					!TYPE OF BLOCK
	IF (I0.GE.0) RETURN				!CANNOT DO
	J=FCA
	J1=(J-A_OB)					!DUMMY ARRAY OFFSET
	CALL WNGMTS(80,A_B(J1+MCA_HD1_1),WNFTH1)	!SET DATA
C
	RETURN
C
C
	END
