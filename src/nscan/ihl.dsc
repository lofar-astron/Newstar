!+ IHL.DSC
!  HjV 950116
!
!  Revisions:
!
%REVISION=HJV=950116="Tape vs -1, system 1"
!
!	Define LEIDEN FDB block (=interferometer block)
!
%COMMENT="FDL.DSC defines the LEIDEN FDB block (=interferometer block)"
!
!
%VERSION=-1					!VERSION
%SYSTEM=1
%USER=HJV
%%DATE
%%NAME
!-
.PARAMETER
.BEGIN=IHL
	field	  C16		!Object-name
	ra1	  D		!Right Ascension
	dec1	  D		!Declination
	freq	  D		!Frequency
	band	  D		!Bandwidth
	chinfo	  B(0:1)		!Info on this channel
	-	  -(0:1)
	sday	  J		!Sidereal date (YYDDD)
	stim	  D		!Sidereal starttime
	etim	  D		!Sidereal endtime
	gainl	  I		!Gain level
	prname	  C3		!Project name
	receiv	  B		!Receiver versionnr.
	olsys	  B		!Online computer program versionnr.
	geninfo	  B		!General information
	obstech	  B		!Type of observing technique
	-	  -(0:2)
	reddate	  J		!Reduction date (YYDDD)
	redtime	  J		!Reduction time, MET
	appdecl	  R		!Apparent declination
	rotang	  R		!The angle ofrotation
	-	  -(0:27)
	chnum	  I		!Channelnr.
	ndatp	  I		!# of observed points
	drt	  R		!Baseline
	hab	  R		!Start hour-angle
	dha	  R		!Increment in hour-angle
	volume	  C6		!Volume label
	label	  C4		!Dataset label
	blknr	  I		!Block sequencenr.
	redlev	  B		!Reduction level
	-	  -(0:0)
	ndelp	  I		!# of deleted and/or missing points
.END					!END DEFINITION
!-
