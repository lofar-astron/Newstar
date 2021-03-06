!+ WQ_PP2.DSC
!  HjV 950710
!
!  Revisions:
!
!
!	Define PP2 portrait plotter. Layout should match WQD.DSC
!
!
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=HjV
%%DATE
%%NAME
%REVISION=HjV=950710="Original version"
%COMMENT="WQ_PP2.DSC defines the PP2 plotter lay-out"
%COMMENT=" "
%LOCAL=NUSED=15				!Max. user data index
%LOCAL=NPLIX=3				!Max. polyline index
%LOCAL=NPMIX=4				!Max. polymarker index
%LOCAL=NTXIX=2				!Max. text index
%LOCAL=NFAIX=3				!Max. fill area index
%LOCAL=NCLIX=1				!Max. colour index
!-
.DEFINE
.PARAMETER
.DATA
.COMMON
	PP2_QUE		J	/0/		!LINK OPEN DEVICES
	PP2_LEN		J			!LENGTH OF AREA
	PP2_BID		C4	/DQID/		!ID OF AREA
	PP2_ACT		J	/0/		!BIT0=1 ACTIVE
	PP2_TYP		J	/1/		!BIT0=1 OUTPUT, 1 INPUT, 2 DISS, 3 META
	PP2_DEFER	J	/0/		!DEFER TYPE
	PP2_REGEN	J	/0/		!REGENERATION
	PP2_NEWFR	J	/0/		!NEW FRAME
	PP2_PEND	J	/0/		!PENDING
	PP2_DEV		C8	/PP2/		!DEVICE TYPE
	PP2_FILE	C80	/PP2.PLT/	!DEVICE FILE
	PP2_XHI		E	/2339/		!HIGHEST X
	PP2_YHI		E	/3167/		!HIGHEST Y
	PP2_XM		E	/0.436/		!X SIZE METERS
	PP2_YM		E	/0.512/		!Y SIZE METERS
	PP2_MGL		J	/132/		!MAX. MESSAGE LENGTH
	PP2_DVRT	J			!DEVICE ROUTINE ADDRESS
	PP2_CHAN	J	/0/		!DEVICE CHANNEL
	PP2_NTR		E(0:3,0:2) /2339,0,2339,0,0,0,1,1,0,0,2339,3167/ !DEVICE TRANSFORM
	PP2_NMPLS	E	/1.2/		!NOMINAL LINE SIZE
	PP2_MXPLS	E	/12/		!MAX. LINE SIZE
	PP2_MNPLS	E	/0.4/		!MIN. LINE SIZE
	PP2_NMPMS	E	/8/		!NOMINAL POLYMARKER SIZE
	PP2_MXPMS	E	/0/		!MAX. POLYMARKER SIZE
	PP2_MNPMS	E	/0/		!MIN. POLYMARKER SIZE
	PP2_NPLT	J	/4/		!# OF LINE TYPES
	PP2_NPMT	J	/5/		!# OF POLYMARKER TYPES
	PP2_EFN		J	/0/		!EFN TO USE
	PP2_BFL		J	/0/		!BUFFER LENGTH
	PP2_BFA		J	/0/		!BUFFER ADDRESSES
	PP2_USE		J(0:NUSED)		!USER DATA
	PP2_NPLIX	J	/NPLIX/		!LENGTH POLYLINE TABLE
	PP2_NPMIX	J	/NPMIX/		!LENGTH POLYMARKER TABLE
	PP2_NTXIX	J	/NTXIX/		!LENGTH TEXT TABLE
	PP2_NFAIX	J	/NFAIX/		!LENGTH FILL AREA TABLE
	PP2_NCLIX	J	/NCLIX/		!LENGTH COLOUR TABLE
	PP2_OPLI	J			!ADDRESS POLYLINE TABLE
	PP2_OPMI	J			!ADDRESS POLYMARKER TABLE
	PP2_OTXI	J			!ADDRESS TEXT TABLE
	PP2_OFAI	J			!ADDRESS FILL AREA TABLE
	PP2_OCLI	J			!ADDRESS COLOUR TABLE
!
	PP2_SVP		J			!START VARIABLE PART
	PP2_PLIXE	E(0:2,0:NPLIX) /1,1,0,2,1,0,3,1,0,4,1,0/ !POLYLINE (4)
	PP2_PMIXE	E(0:2,0:NPMIX) /1,1,0,2,1,0,3,1,0,4,1,0,5,1,0/ !POLYMARKER (5)
	PP2_TXIXE	E(0:2,0:NTXIX) /1,2,0,1,3,0,1,4,0/ !TEXT (3)
	PP2_FAIXE	E(0:2,0:NFAIX) /3,1,0,4,1,0,5,1,0,6,1,0/ !FILL AREA (4)
	PP2_CLIXE	E(0:2,0:NCLIX) /1,0,0,1,1,0/ !COLOUR (2)
!
	PP2_EOL		J			!END OF LIST
.END					!END DEFINITION
!-
