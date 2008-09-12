!+ WXH.DSC
!  WNB 910828
!
!  Revisions:
!
!	Layout of exit handler include file (WXH.DEF)
!
%VERSION=1
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
%REVISION=WNB=910828="Original version"
%COMMENT="WXH.DEF is an INCLUDE file for exit and other handlers."
%COMMENT=" "
!-
.DEFINE
  .PARAMETER
  .DATA
  .COMMON
	XHED	J		/0/		!HEAD OF EXIT HANDLER LIST
	XHCC	J(0:1)		/0,0/		!CONTROL C: 0: INHIBIT
						!           1: SEEN DURING INHIBIT
	XHRS	J(0:4)		/0,0,0,0,0/	!RESERVED
.END
