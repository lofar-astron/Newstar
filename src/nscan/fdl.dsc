!+ FDL.DSC
!  HjV 950116
!
!  Revisions:
!
%REVISION=HJV=950116="Tape vs -1, system 1"
!
!	Define LEIDEN FD block
!
%COMMENT="FDL.DSC defines the LEIDEN FD block"
!
!
%VERSION=-1					!VERSION
%SYSTEM=1
%USER=HJV
%%DATE
%%NAME
!-
.PARAMETER
.BEGIN=FDL
	CBI	  I		!'32767'
	CBT	  C2		!Type identification (='FD')
	CURRREC	  J		!Recordnr. of this record
	NEXTREC	  J		!Recordnr. of next FD-record
	NBL	  J		!# blocks within dataset
	BYPBL	  I		!# bytes per block (=3840)
	TOTFREQ	  I		!# of requency-bands
	FREQBND	  I		!Current frequency-band
	NRINTF	I		!# of interferometers
	FVERS	  I		!Tape-format version (=-1)
	PROG	  C6		!Program that wrote old Leiden tape
	OLDVOL	  C6		!Volume label of old Leiden tape
	OLDLAB	  C4		!Dataset label on old Leiden tape
	-	  -(0:17)
	OFFINTF	  J(0:159)	!Offset in bytes of start FDB interf.
.END					!END DEFINITION
!-
