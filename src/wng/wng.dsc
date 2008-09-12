!+ WNG.DSC
!  WNB 890427
!
!  Revisions:
!
%REVISION=HjV=940217="Change data type T (remove blank line)"
%REVISION=WNB=931130="Add BKJY, DBKJY"
%REVISION=JPH=930825="Comment"
%REVISION=WNB=930803="Use new WNTINC options"
%REVISION=WNB=930727="Add T_S, LB_S, T_ALL"
%REVISION=WNB=930527="Add A data type"
%REVISION=WNB=921222="Add WSRT/ATNF longitude and latitude"
%REVISION=HJV=920626="Add comment for prgdat (8 = HP station)"
%REVISION=HJV=920501="Type of JS changed to L from J"
%REVISION=WNB=890427="Original version"
!
!	Layout of overall include file (WNG_DEF)
!
%COMMENT="  WNG.DSC is a general include file. WNG_DEF (wng_inc) should be"
%COMMENT="	included as the first executable statement after the"
%COMMENT="	routine definition. In WNGLOGIN.COM an assignment to"
%COMMENT="	WNG_DEF is present, so use it as:"
%COMMENT="	INCLUDE 'WNG_DEF'"
%COMMENT="	Initialisation is done in WNGIN (via NLINK)"
!
%LOCAL=NFILE=16			!# OF ASCII FILES, CHANGE ALSO IN WNC.DSC
%LOCAL=FYES=1			!DISPOSITION, CHANGE ALSO IN WNC.DSC
%VERSION=1
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
%FORTRAN=IMPLICIT NONE
!-
.DEFINE
  .PARAMETER
!
!  Mathematical
!
	DPI	D	/3.1415926535897932385/		!PI
	PI	E	/DPI/
	DEE	D	/2.7182818284590452353/		!E
	EE	E	/DEE/
	DPI2	D	/6.2831853071795864769/		!2*PI
	PI2	E	/DPI2/
	DRAD	D	/0.0174532925199432958/		!PI/360
	RAD	E	/DRAD/
	DDEG	D	/57.2957795130823208768/	!360/PI
	DEG	E	/DDEG/
	DCRTSC	D	/240.*360./			!CIRCLES TO SECONDS
	RCRTSC	E	/DCRTSC/
	DCL	D	/2.997925D8/			!C IN M/S
	CL	E	/DCL/
	DBKJY	D	/1380.54/			!K IN JY.M^2/K
	BKJY	E	/DBKJY/
	LATW	E	/52.9169/			!LAT WSRT (DEG)
	LONGW	E	/6.604167/			!LONG WSRT (DEG)
	SLATW	E	/0.797762/			!SIN(LAT) WSRT
	CLATW	E	/0.602973/			!COS(LAT) WSRT
	LATA	E	/-30.31445/			!LAT ATNF (DEG)
	LONGA	E	/149.566928/			!LONG ATNF (DEG)
	SLATA	E	/-0.504745/			!SIN(LAT) ATNF
	CLATA	E	/0.863268/			!COS(LAT) ATNF
	E2T8	E	/2.**8/				!2**8
	E2T16	E	/2.**16/			!2**16
	E2T32	E	/2.**32/			!2**32
	D2T16	D	/2D0**16/			!2**16
	D2T32	D	/2D0**32/			!2**32
!
!  ASCII files:
!
	F_FILN	J	/NFILE/				!# OF FILES
	F	M:	/T,P,,,,,,P1,0,1,2,3,4,5,6,7,8, \
				9,10,11,12,13,14,15/	!Type BIT
							!Print BIT
							!Prefix BIT: puts a ">"
!					in column 1 (which by default is blank)
!					of output text 

							!FILE 0-15 BITS
	F_TP	J	/F_T+F_P/			!TYPE/PRINT BITS
	F_ALL	J	/-F_0+F_15+F_15+F_TP/		!ALL FILE BITS
!
	F_NO	J	/0/				!NO DISPOSITION
	F_YES	J	/FYES/				!KEEP FILE
	F_SP	J	/2/				!SPOOL FILE
	F_CAT	J	/3/				!CONCATENATE FILE
!
	F_LC	J	/1/				!LINE COUNT CODE
	F_PC	J	/2/				!PAGE COUNT CODE
	F_LL	J	/3/				!LINE LENGTH CODE
	F_PL	J	/4/				!PAGE LENGTH CODE
	F_DIS	J	/5/				!DISPOSITION CODE
!
!  Special:
!
	IUND	I	/-32768/			!UNDEFINED WSRT VALUE
!
!  Explicitly used DWARF error codes
!
	DWC_ENDOFLOOP J	/134448144/			!^Z PARAMETER REPLY
	DWC_NULLVALUE J /134448161/			!"" PARAMETER REPLY
	DWC_WILDCARD  J /134448169/			!*  PARAMETER REPLY
!
!  Data types:
!
	L	N:(8,8,32,16,32,32,32,64,64,128,16,8) \	!LENGTH IN BITS OF
			/B,C,L,I,J,K,E,D,X,Y,A,S/	!  DATA TYPES
	LB	N:(1,1,4,2,4,4,4,8,8,16,2,1) \		!LENGTH IN BYTES OF
			/B,C,L,I,J,K,E,D,X,Y,A,S/	!  DATA TYPES
	T	A:	/B,C,L,I,J,K,E,D,X,Y,A,S/	!TYPE CODES OF 
!							   DATA TYPES
	T_ALL	C12	/BCLIJKEDXYAS/			!KNOWN DATA TYPES
!
  .DATA
!
!  Local variables:
!
	J	J					!POINTERS
	J0	J
	J1	J
	J2	J
	J3	J
	J4	J
	J5	J
	I	J					!LOOPS
	I0	J
	I1	J
	I2	J
	I3	J
	I4	J
	I5	J
	JS	L					!ERROR
	R0	E					!SOME HELP
	R1	E
	D0	D
	D1	D
	L0	L
	L1	L
	B0	B
	B1	B
  .COMMON
	A_Y	Y(0:0)					!DUMMY ARRAYS
	A_B=A_Y	B(0:0)
	A_I=A_Y	I(0:0)
	A_J=A_Y	J(0:0)
	A_K=A_Y	K(0:0)
	A_L=A_Y	L(0:0)
	A_E=A_Y	E(0:0)
	A_D=A_Y	D(0:0)
	A_X=A_Y	X(0:0)
	E_C	J					!MOST RECENT ERROR CODE
	A_OB	J					!ARRAY OFFSETS
	A_OI	J
	A_OJ	J
	A_OK	J
	A_OL	J
	A_OE	J
	A_OD	J
	A_OX	J
	A_OY	J
	LOGCD	J					!LOG CODE
	PRGDAT	J					!DATA TYPE
							! 1= VAX, D_FORMAT
							! 2= VAX, G_FORMAT
							! 3= ALLIANT
							! 4= CONVEX
							! 5= IEEE
							! 6= DEC station
							! 7= SUN station
							! 8= HP  station
	PRGNAM	C9					!PROGRAM NAME
	PRGVER	C6					!PROGRAM VERSION
.END
