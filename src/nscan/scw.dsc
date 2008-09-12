!+ SCW.DSC
!  WNB 900118
!
!  Revisions:
!
%REVISION=CMV=940414="Add historical notes"
%REVISION=WNB=931215="Add some edit formats"
%REVISION=WNB=930803="Change .RECORD"
%REVISION=WNB=911022="Add mosaicking RA/DEC"
%REVISION=WNB=900118="Tape vs 7, system 59"
!
!
!	Define WSRT SC block
!
!
%COMMENT="SCW.DSC defines the WSRT SC block"
!
%VERSION=7					!VERSION
%SYSTEM=59
%USER=WNB
%%DATE
%%NAME
!-
.PARAMETER
.STRUCTURE=SRC			!Subtracted sources
	SRAI	D	<DAF12.7> !Right ascension subtracted source  nr. I
	SDECI	D	<DAF12.7> !Declination subtracted source nr. I
	SFXXI	E	<E12.3>	!Intensity XX subtracted source nr. I
	SFXYI	E	<E12.3>	!    "     XY	"	   "	 "
	SFYXI	E	<E12.3>	!    "     YX	"	   "	 "
	SFYYI	E	<E12.3>	!    "     YY	"	   "	 "
.END
.STRUCTURE=BCOR			!Band corrections
	PZ	I(32)		!Phase correction band 1
	DOFS	I(32)		!Delay offset band 1
	-	-(0:127)	!		-
	TSYS	E(32)	 	!System temperatures band 1
	GAIN	E(32)	 	!Receiver gainfactors band 1
	NOIS	E(32)	 	!Noise source temperatures band 1
	GNCL	I(32)	 	!Amplitude correction methods band 1
	-	-(0:255)	!		-
.END
.STRUCTURE=MOZP			!Mozaicking positions
	RA0	J		!RA epoch position
	DEC0	J		!DEC epoch position
	RA1	J		!RA apparent
	DEC1	J		!DEC apparent
	SDEC	J		!sin(DEC1)
	CDEC	J		!cos(DEC1)
	MVNPA	J(0:2)		!Apparent position vector
	MDVNPA	J(0:2)		!Differential apparent position vector
	FREQ1	D	<D12.6>	!Apparent band centre frequency (MHz)
				!  ols<62: reserved
	-	-(0:7)
.END
.BEGIN=SCW
				! (*) means use with care, not repaired
	CBI	I		!Unequal data block flag (32767)
	CBT	C2		!Type identificator: System Calibration (SC)
	NSC	J		!Record number of this record
	LSC	J		!Number of SC records
	NSCN	J		!First SC record of next SC group (=-32768 for last)
	SDAY	I		!Local U.T. day number
				!  FVERS<5: ST
	STIM	I		!Start U.T. time in units of 10 sec.
				!  FVERS<5: ST
				!  FVERS<3: units of minutes 
	ETIM	I		!End U.T. time in units of 10 sec. (*)
				!  ols<43: -32768
	-	I 		!Empty (Delete character)
	CFE	J		!Code front end
	CIF	J 		!Code if part
	CBE	J		!Code backend
	CPC	J		!Code pheripheral computers
	CMC	J		!Code main computer
	LRED	I		!Reduction level (Dwingeloo red. code)
	-	-(0:25)		!Reserved for Dwingeloo reduction
	VOLUME	C6		!The volume name of the input
	LABEL	C4		!The label or dataset name of the input
	-	-(0:5)		!               -
	FSYN	E		!Frequency synthesizer (Mhz)
	LO2L	E		!Frequency local oscillator 2 for DLB (Mhz) (*)
				!  FVERS<6: LO2
	LO2C	E		!Frequency local oscillator 2 for DCB (Mhz) (*)
				!  FVERS<6: LO3
	VIDE0	E		!If to video mixing frequency (Mhz)
	FF0	E		!Fringe stopping frequency offset (Mhz)
	AFRATE	E		!Artificial fringe rate (Mhz) (*)
				!  FVERS<6: reserved
	DOFSIJ	I(0:27)		!Delay offsets in nano sec for if lines
				!OX, OY,... to DX, DY. (*)
				!  <04/09/80: in cm
	PREC	D(3,3)	<D12.9>	!Precession matrix
	NUTA	D(3,3)	<D12.9>	!Nutation matrix
	ABER	D(3)	<D12.9>	!Aberration vector
	VNPA	D(3)	<D12.9>	!Apparent position vector
	DVNPA	D(3)	<D12.9>	!Differential apparent position vector (Circle/U.T.day)
				!  FVERS<5: in S.T.
	EQNOX	D	<D12.9>	!Equation of equinoxes
	DEQNOX	D	<D12.9>	!Differential equation of equinoxes (Circle/U.T.day)
				!  FVERS<5: in S.T.
	EPS	D	<D12.9>	!True obliquity
	DEPS	D	<D12.9>	!Nutation in obliquity
	DPSI	D	<D12.9>	!Nutation in length
	-	-(0:3)		!	-
	GLAT	E	<EAF12.7> !Geocentric latitude (Circles)
				!  ols<43: different field (never used)
	WLAT	E	<EAF12.7> !Geographic latitude of the delay switching point
	WLON	E	<EAF12.7> !Geographic longtitude used for the siderial clock point
	CENTRE	E	<E12.4>	!Position of the delay switching point
				!W.R.T. old coordinate system (= 3000)
	PHI	E	<EAF12.7> !Rotation angle
	FREQ	D	<D12.6>	!Fringe stopping frequency for middle observation
	ONCLM	E	<E12.6>	!Total online clock correction for app coord moment
	CSHAD	E(3)		!Telescope 'shadowing' coefficients
	BL1	J		!Code broadband long term (phase)
	BL2	J		!Code broadband long term (amplitude)
	-	-(0:7)		!            -
	BGS	J		!Code broadband short term
	BGT	J		!Code telescope coefficients
	-	-(0:7)		!	    -
	BET	J		!Code if corrections
	BED	J		!Code delay corrections
	-	-(0:7)		!	    -
	SEV	J		!Code video corrections
	BF	J		!Code field dependent corrections
	-	-(0:7)		!	    -
				! GCODE...DPLE: FVERS<3: reserved
	GCODE	I		!Main amplitude correction method (1-3)
	-	-(0:1)		!	    -
	GNCAL	B(0:27)		!Final ampl. correction method per IF -DLB- (Per byte)
	TSYSI	E(0:27)	<E12.2>	!System temperatures (K)
	RGAINI	E(0:27)	<E12.4>	!Receiver gain factors per IF
	TNOISI	E(0:27)	<E12.2>	!Noise source temperature (K)  0X,0Y,...DY
	JDCP	E		!Julian day for last known bih data
	YEAR	I		!Civil year of JDCP
	MONTH	I		!Civil month of JDCP
	DAY	I		!Civil day of JDCP
	-	-(0:1)		!	 -
	CLCOFF	E		!Fixed clock offset
	CLOCK	E		!Total online clock correction for JDCP moment
	DCLOCK	E		!Differential clock correction
	CUTST	D	<D12.9>	!(Length u.t.day / length s.t.day) -1
				!  FVERS<5: reserved
	-	-(0:11)		!	 -
	POLEOFF	E		!Fixed declination of the baseline pole
	POLE	E		!Total baseline pole correction for JDCP moment
	DPLE	E		!Differential baseline pole correction
	LO3C	E(8)		!DCB local oscillators band 1 to 8 (Mhz) (*)
				!  FVERS<6: FB0..7
	LO3G	E		!Average DCB local oscillator used for fringe (Mhz) (*)
				!  FVERS<6: reserved
	FCODE	I		!Code IF switches
	DELTA	E(0:27)		!Real part dipole corr. factors (0X,0Y,...DY) in % (*)
				!  ols<43: reserved
				!  ols>46: different units
	THETA	E(0:27)		!Imaginary part dipole corr. factors (0X,0Y,...DY) in % (*)
				!  ols<43: reserved
				!  ols>46: different units
	FENRS	I(0:27)		!Front end nrs (0X,0Y,1X,...,DY) (*)
				!  ols<57: reserved
	-	-(0:3)		!-
	FROFS	E		!RF frequency step size (System 49)
	SYNF	I		!Synthesizer multiplication factor
				!  ols<43: reserved
	LODEL	I(0:27)		!Delay values of the lo-cables (Nsec)
	FRO	D		!Observing freq. for IF phase zero values (Mhz)
				!  ols<26: reserved
	CCOR	E		!Offline clock correction (Dwingeloo use)
	DPOLM	E		!Total baseline pole corr. for app. coord moment
	CFREQ	E		!Offline frequency correction
	CQI	I(0:13)		!Position corrections per tel. in Q direction
				!(10**(-4) M)
	CNI	I(0:13)		!Position corrections per tel. in N direction
				!(10**(-4) M)
	CPI	I(0:13)		!Position corrections per tel. in P direction
				!(10**(-4) M)
	AD0	E		!Average telescope axis difference (M)
	CPSI	I(0:13)		!Polar axis correction per tel. (2**(-24) circles)
	CPWI	I(0:13)		!Polar axis correction per tel. (2**(-24) circles)
	PZIJ	I(0:27)		!If phase zero corrections for primary fringe
				!(2**(-16) circles)
				! C?X? FVERS<3: incorrect values
	C2X2	E(4)		!4 coefficients for v. Vleck corr. 2x2 mode
	C2X3	E(4)		! 	"	"	"    "    2x3  "
	C2X4	E(4)		!	"	"	"    "    2x4  "
	C4X3	E(4)		!	"	"	"    "    4x3  "
	C4X4	E(4)		!	"	"	"    "    4x4  "
	CFGJI	E(112)		!Coefficients for video band gain  corrections
	CFFJI	E(112)		!Coefficients for video band phase corrections
	TEMP	I		!Outside temperature (Degrees celcius)
	BAR	I		!Air pressure (Mb)
	HUM	I		!Humidity degree
	-	-(0:1)		!            -
	CMU1	E       	!Refraction coefficient dry air
	H1	E		!Scale height dry air
	CMU2	E     		!Refraction index water vapour
	H2	E		!Scale height water vapour
	CEXT	E		!Extinction coefficient
				!  ols<43: reserved
	RHA	E(24)		!Hour angle table (Circles)
	CFRA	E(24)		!24 rotation angles (Circles)
	CRIF	E(24)		!Refraction corrections (Circles)
	-	-(0:279)	!	            -   
	CMON	C64		!Monitoring information
	-	-(0:19)		!	    -
	NRPNCH	I		!Nr. of information fields in PNCHI (Max =4)
	NRSUBT	I		!Nr. of subtracted sources (Max. is 9)
	-	-(0:7)		!            -
	PNCHI	C320		!General information fields
	SRC	S:SRC(9)	!Subtracted sources
	-	C4		!Gain/phase table name
	TMP	J		!Table number (Code)
	-	-(0:3)		!	 -
				!  
	LENTMP	I		!Nr. of records on disk necessary for this table.
				!0 or 11 means 160 interf. (Only standard)
				!27 means 420 interf. (Standard and non-standard)
	-	-(0:1)		!	 -
	PH0	E(0:159)	!	160 phase zero values, per interferometer
				!in radians  (0XAX - 9YDY)
	GN0	E(0:159)	!	160 gain factors, per interferometer 
				!(0XAX - 9YDY)
	PH10	E(0:259)	!	260 phase zero values, per interferometer
				!in radians  (0X0X - DYDY) (Non-standard)
	GN10	E(0:259)	!	260 gain factors, per interferometer 
				!(0X0X - DYDY) (Non-standard)
	-	-(0:463)	!	-
	BCOR	S:BCOR(8)	!Band corrections
				!  FVERS<6: reserved/not on tape
	MOZP	S:MOZP(120)	!Mozaicking positions
				!  FVERS<7: reserved/not on tape
.END
!-
