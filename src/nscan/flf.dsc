!+ FLF.DSC
!  WNB 930615
!
!  Revisions:
!
!
!	Define delete file contents
!
!
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
%REVISION=WNB=930615="Original header"
%COMMENT="FLF.DSC defines the delete file contents"
%COMMENT=" "
!-
.PARAMETER
.BEGIN=FLF
	FLAG	J		!Flag seen (bit 0=1: 1st of range; b1=1: 2nd)
	CHAN	J		!Channel (or -1)
	HA	E		!HA (or (J) -1)
	IFR	I		!Interferometer as 'TT'X (or -1)
	POL	I		!Polarisation (or -1)
.END					!END DEFINITION
!-
