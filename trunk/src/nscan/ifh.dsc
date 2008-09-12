!+ IFH.DSC
!  CMV 940420
!
!  Revisions:
!
%REVISION=CMV=941003="Add IFHAB (only used at loading)"
%REVISION=CMV=940420="Original version"
!
!	Define IF/TotalPower Header block
!
!  The IF data is organised as follows:
!
!	IFH
!	Total Powers    0..NTP,0..NSTAR_TEL,X..Y,On..Off
!	IF Corrections  not yet used
!
!
%COMMENT="IFH.DSC defines the IF/Total Power header block"
%COMMENT=" "
!
%VERSION=1					!VERSION
%SYSTEM=4
%USER=CMV
%%DATE
%%NAME
!
! Get number of telescopes
!
%INCLUDE=NSTAR_DSF
!-
.BEGIN=IFH
	CHAN	I				!BAND NUMBER
	GCODE	I				!PRINCIPAL GAIN CORR. METHOD
	GNCAL	I(0:NSTAR_TEL-1,0:1)		!ACTUAL GAIN CORR. METHOD
	-	-(4)
	TSYSI	E(0:NSTAR_TEL-1,0:1)		!CONSTANT SYSTEM TEMP.
	-	-(16)
	RGAINI	E(0:NSTAR_TEL-1,0:1)		!CONSTANT RECEIVER GAIN
	-	-(16)
	TNOISEI E(0:NSTAR_TEL-1,0:1)		!CONSTANT NOISE TEMP.
	-	-(16)
	TPINT	J				!Total Power Int.time
	HAB	E		<EAF12.7>	!FIRST HA APP.
	HAI	E		<EAF12.7>	!HA INCREMENT
	NTP	J		<,1>		!# OF TOTAL POWER SCANS
	NIF	J		<,1>		!# OF IF GAIN/PHASE SCANS
	IFHAB	E		<EAF12.7>	!HAB from IH block
	-	-(40)				!RESERVED
.END
!-
