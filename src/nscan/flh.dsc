!+ FLH.DSC
!  WNB 930616
!
!  Revisions:
!
!
!	Define flag file header
!
!
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
%REVISION=WNB=930616="Original header"
%COMMENT="FLH.DSC defines the delete file header"
%COMMENT=" "
!-
.PARAMETER
.BEGIN=FLH
	LINK	J(0:1)		!Link files (not used)
	DID	J		!File  ID (not used)
	VER	J		!Version (not used)
	FLFL	J		!Length entry
	FLFN	J		!# of entries in file
	FLFP	J		!Pointer to entries
	TYP	J		!Not used, but e.g. baselines vs interferometers
	FLAG	J		!OR of all entry flags
	FLD	J		!Not used
	CHAN	J		!For channel: -1: all ranges present; 0: range
	HA	J		!For HA: -1/0
	IFR	J		!Interferometer: -1/0
	POL	J		!Polarisation: -1/0
	R1	J		!Reserved: -1/0
	R2	J		!Reserved: -1/0
	-	J(4)		!Fill
	RFLD	J(0:1)		!Range fields (not used)
	RCHAN	J(0:1)		!Range channels if CHAN 0
	RHA	E(0:1)		!Range HA
	RIFR	J(0:1)		!Range interferometers
	RPOL	J(0:1)		!Range polarisation
	RR1	E(0:1)		!Range reserved
	RR2	E(0:1)		!Range reserved
	-	J(8)		!Fill
.END					!END DEFINITION
!-
