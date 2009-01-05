!+ NGF.DSC
!  WNB 920818
!
!  Revisions:
!
%REVISION=CMV=940805="Add TRTYP=2 (comments only)"
%REVISION=WNB=931216="Add some edit formats"
%REVISION=WNB=931015="Use SSH_DSF"
%REVISION=WNB=930630="Change TRVAL in TRHAI; add TRHA, TRFB, TRFI, FRHACV"
%REVISION=WNB=930628="Add BLN"
%REVISION=WNB=920902="Add TRTYP, TRVAL"
%REVISION=WNB=920819="Original version NGF"
!
!
!	Define layout of general calculation/plot file plot header
!
%COMMENT="NGF.DSC defines the NGCALC file header"
%COMMENT=" "
!
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!-
.PARAMETER
	FRHACV	E		/0.125/		!EACH FREQ. CHANNEL 0.125 DEGREE
.BEGIN=NGF
%INCLUDE=SSH_DSF				!STANDARD AREA
	NAM	C12				!FIELD NAME
	RA	E		<EAF12.7>	!RA (CIRCLES)
	DEC	E		<EAF12.7>	!DEC (CIRCLES)
	FRQ	E		<E12.6>		!FREQUENCY (MHZ)
	BDW	E		<E12.6>		!BANDWIDTH (MHZ)
						!
						! Number refers to TRTYPE
						!
	HAB	E		<EAF12.2>	! 0: START HA (CIRCLES)
						! 1: START BANDNO*FRHACV/360
						! 2: START BLN/360 (DECAM)
						!
	HAI	E		<EAF12.2>	! 0: INCREMENT HA (CIRCLES)
						! 1: FRHACV/360
						! 2: INCREMENT BLN/360 (DECAM)
						!
	HAV	E		<EAF12.2>	!AVERAGE HA (CIRCLES)
	UTB	E		<EHF4>		!START UT (CIRCLES)
	UTE	E		<EHF4>		!END UT (CIRCLES)
	DPT	J		<XJ,1>		!DATA POINTER
	SCN	J		<,1>		!NUMBER OF DATAPOINTS
	VNR	J				!VOLGNUMBER
	BDN	J				!BANDNUMBER
	IFR	C4				!INTERFEROMETER/TELESCOPE
						! 2: SET TO 'ALL'
	POL	C4				!POLARISATION
	ODY	I				!OBS. DAY
	OYR	I				!OBS. YEAR (E.G. 1986)
	TYP	C40				!TYPE OF DATA
	DEL	J				!DELETED POINTS
	MAX	E		<E12.4>		!MAXIMUM VALUE
	MIN	E		<E12.4>		!MINIMUM VALUE
						!
	TRTYP	J				!TRANSPOSE TYPE:
						!	0: HA AXIS
						!	1: BAND AXIS
						!	2: BASELINE AXIS
						!
	TRHAI	E		<EAF12.2>	! 0:   0
						! 1,2: HAI (CIRCLES)
						!
	BLN	E		<E12.3>		! 0,1: BASELINE/
						!       TEL. POSITION (M)
						! 2:   MAX. BASL/TEL.POS (M)
						!
	TRHA	E		<EAF12.2>	! 0:   0
						! 1,2: HA (CIRCLES)
	TRFB	E		<E12.6>		! 0,2: 0
						! 1:   BEGIN FREQUENCY 
						!
	TRFI	E		<E12.6>		! 0,2: 0
						! 1:   INCR. FREQUENCY
						!
 .OFF=512					!LENGTH
.END						!END STRUCTURE
!-