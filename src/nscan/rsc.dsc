!+ QSCHD.DSC
!  WNB 880314
!
!  Revisions:
!	HjV 940519	Changed for use in Newstar
!
%REVISION=WNB=880314="Original version"
%REVISION=HJV=940519="Changed for use in Newstar"
!
!	Define layout of R-series SCAN-header and Scan-data.
!
%COMMENT="QFHHD.DSC defines the R-series SCAN header and SCAN data"
%COMMENT=" "
!
!
%VERSION=2					!VERSION
%SYSTEM=2
%USER=WNB
%%DATE
%%NAME
!
!
%LOCAL=QSCHL=440				!REQUIRED LENGTH
%LOCAL=QFNM=120					!OFFSET TO FIELD NAME REQUIRED
!
.PARAMETER
.BEGIN=QSC
!
!
	VER	I2		!VERSION
	LEN	I2		!LENGTH HEADER
	HA	R4		!HA SCAN (CIRCLES)
	CSM	R4		!COS/SIN SCALE MULTIPLIER
	RNP	R4		!REDUNDANCY PHASE NOISE (W.U.)
	ALP	R4		!ALIGN PHASE NOISE (W.U.)
	ALG	R4		!ALIGN GAIN NOISE (W.U.)
	MAX	R4		!MAXIMUM COS/SIN
	RNG	R4		!REDUNDANCY GAIN ERROR (W.U.)
	RIP	I4		!PTR TO REDUNDANT IFR LIST
	RIN	I2		!# OF REDUNDANT SPACINGS
	DEL	-		!DELETE THIS SCAN
	DI1	-(0:8)		!DELETE IFRS PART 1
	EXT	R4		!EXTINCTION FACTOR
	REF	R4		!REFRACTION (MU-1)
	WRP	R4(0:13)	!SRT PHASE ERROR TEL (CIR)
	RDP	R4(0:13)	!SRT RED. PHASE ERROR (CIR)
	CLP	R4(16)		!SRT CELESTIAL PHASE (CIR)
	WRG	R4(0:13)	!SRT GAIN ERROR TEL (LOG)
	RDG	R4(0:13)	!SRT RED. GAIN ERROR (LOG)
	CLG	R4(16)		!SRT CELESTIAL GAIN (LOG)
	FRP	-(0:13)		!WSRT PHASE FREEDOM
	FRG	-(0:13)		!WSRT GAIN FREEDOM
	DI2	-(0:2)		!DELETE IFR PART 2
	R2	-(0:0)		!RESERVED
	DAT	I2(2,0:90)	!SCAN DATA COS,SIN
.END
!-