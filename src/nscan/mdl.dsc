!+ MDL.DSC
!  WNB 900327
!
!  Revisions:
!
%REVISION=CMV=950518="Improve comment for TP"
%REVISION=WNB=931215="Add some edit formats"
%REVISION=WNB=931005="Add mask parameters"
%REVISION=JPH=930825="Comments"
%REVISION=WNB=900327="Original definition"
!
!	Define Model line
!
%COMMENT="MDL.DSC defines a model line"
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!-
.PARAMETER
	MDLCLN_M J	/1/	!Clean type (TP)
	MDLBEM_M J	/8/	!Corrected for beam (TP)
	MDLEXT_M J	/1/	!Extended (BITS)
	MDLQUV_M J	/2/	!Q,U,V <> 0 (BITS)
!
.BEGIN=MDL
	I	E	<E12.3>	!Amplitude (Stokes I)
	L	E	<EAF12.7> !l offset
	M	E	<EAF12.7> !m offset
	ID	J		!Identification
	Q	E	<E12.4>	!Q (fraction of I) 
	U	E	<E12.4>	!U
	V	E	<E12.4>	!V
	EXT	E(0:2)	<E12.4>	!Extension parameters:
				! (0:1)= x:y ext, (2)= pos. angle of x (??)
	SI	E	<E12.2>	!Spectral index
	RM	E	<E12.3>	!Rotation measure
	RS	E		!Reserved (used for Update calculations)
	BITS	B	<XB>	!Bits: bit 0= extended; bit 1= Q|U|V <>0
	TP	B	<XB>	!Type: bit 0= clean component; bit 3= beamed
	TP1	B		!Selection type (0..7)
	TP2	B	<XB>
.END					!END DEFINITION
!-
