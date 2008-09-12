!+ FDX.DSC
!  WNB 900118
!
!  Revisions:
!
%REVISION=WNB=931215="Add some edit formats"
%REVISION=WNB=920812="Changed format of data"
%REVISION=WNB=920812="Changed all fields to H(=I2): Essential to be able"
%REVISION=WNB=920812="... to use some critical data translated properly!"
%REVISION=HJV=920714="Tape vs 7, system 63"
%REVISION=HJV=930714="Extend INFTB from 384 to 512 bytes (H's!!),"
%REVISION=HJV=930714="Remove COM (Westerbork common area first 128 words)"
%REVISION=WNB=900118="Tape vs 7, system 59"
!
!
!	Define WSRT FD extension block
!
!
!
%VERSION=7					!VERSION
%SYSTEM=63
%USER=HJV
%%DATE
%%NAME
%COMMENT="FDX.DSC defines the WSRT FD extension block"
!-
.PARAMETER
.BEGIN=FDX
	INFTB	I(0:511)	!Interferometer table. -1 if not present
	WBOH	I(0:255)	!OH information for Westerbork use
	CHAN0	I(0:24)		!This and following fields provide the
	CHLSP	I(0:24)		!FINDX information to obtain disk location
	FLREC	I(0:49)		!of data in Westerbork.
	RCCNT	I(0:24)		!id.
	ABIT	I		!id.
	VOLGN	I		!id.
	NA	I		!id.
.END					!END DEFINITION
!-
