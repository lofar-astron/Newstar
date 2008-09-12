!+ GFH.DSC
!  WNB 900131
!
!  Revisions:
!
%REVISION=WNB=931215="Add some edit formats"
%REVISION=JPH=930903="Comments"
%REVISION=WNB=930803="Include gfh_eqv.def"
%REVISION=WNB=900131="Original version"
!
!	Define general DWARF-redundancy file header
!
%COMMENT="GFH.DSC defines the general file header redundancy files"
%COMMENT=" "
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!-
.PARAMETER
.BEGIN=GFH
	ID	C4	<,1>	!Type of block (e.g. .SCN)
	LEN	J	<,1>	!Length header
	VER	J	<,1>	!Version
	CDAT	C11		!Creation date (dd-mmm-yyyy)
	CTIM	C5		!Creation time (hh:mm)
	RDAT	C11		!Revision date
	RTIM	C5		!Revision time
	RCNT	J		!Revison count
	NAME	C80	<,1>	!Node name used
	DATTP	B	<,1>	!Data type (1,2=VAX, 3=Alliant, 4=Convex)
	-	-(0:22)		!Reserved
	LINK	J(0:1)	<XJ,1>	!Link to data
	  ALHD=LINK J(0:1)  <XJ,1> !absolute listhead
	NLINK	J	<,1>	!Count linkage
	  ALLEN=NLINK J <,1>	!absolute-list length
	LINKG	J(0:1)	<XJ,1,,P:SGH> !Secondary link
	  LHD=LINKG J(0:1) <XJ,1,,P:SGH> !(subgroup) listhead
	NLINKG	J	<,1>	
	  LLEN=NLINKG J <,1>	!(subgroup) list length
	IDMDL	J	<,1>	!Model unique identification (not used)
	ID1	J	<,1>	!Other id
	ID2	J	<,1>	!Other id
	USER	-		!Start user area
.OFFSET=512			!Reserve with binary zeroes
.END					!END DEFINITION
!-
