!+ DLDM.DSC
!  JPH 960610
!
!  Revisions:
!
!
!	Define Common block for transmitting STH_DLDM to NMOBMV
!	This is a quick and dirty bypass to implement model calculations for
!	 interferometers in which one primary beam is offset 
!
!
%VERSION=0					!VERSION
%SYSTEM=4
%USER=JPH
%%DATE
%%NAME
!
.DEFINE
  .COMMON 	DLDM
DLDM		E(0:1)		! position of fringe-stop centre in beam of
				!  scanning telescope (rad)
.END
!-
