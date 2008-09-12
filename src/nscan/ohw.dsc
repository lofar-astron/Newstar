!+ OHW.DSC
!  WNB 900118
!
!  Revisions:
!
%REVISION=JPH=970220="Add PATPO, PATFD"
%REVISION=CMV=960228="Increased max. number of SETS"
%REVISION=CMV=940829="Change max. number of SETS"
%REVISION=CMV=940414="Add historical notes"
%REVISION=WNB=931215="Add some edit formats"
%REVISION=WNB=930803="Change .RECORD"
%REVISION=WNB=920813="Add MSNP"
%REVISION=WNB=911022="Add SPEFU"
%REVISION=WNB=900118="Tape vs 7, system 59"
!
!
!	Define WSRT OH block
!
!
%COMMENT="OHW.DSC defines the WSRT OH block"
!
%VERSION=7					!VERSION
%SYSTEM=59
%USER=WNB
%%DATE
%%NAME
!-
%LOCAL=MAXSET=5000		!Maximum number of sets
.PARAMETER
	OHWSET	J	/MAXSET/
!
.STRUCTURE=SET			!Set definitions
	BFREQ	J		!Set observing frequency (2**(-16).Mhz) (*)
				!  FVERS<2: single float (E)
	DATYP	C2		!Data type (IF, XX, XY, YX, or YY)
	BANDNR	I		!Band number
	NSH	J		!First record number of current set
.END				!End SET definition
.BEGIN=OHW
				! (*) means use with care, not repaired
	CBI	I		!Unequal data block flag (32767)
	CBT	C2		!Type identificator: Observation Header (OH)
	NOH	J		!Record number of this record
	LOH	J		!Number of OH records in this group with 
				!information (*)
				!  FVERS<5: erroneous (always 54)
	NOHN	J		!First OH record number of next group (last:-32768)
	SDAY	I		!Local U.T. day number
				!  FVERS<5: ST
	STIM	I		!Start U.T. time in units of 10 sec.
				!  FVERS<5: ST
				!  FVERS<3: units of minutes 
	ETIM	I		!End U.T. time in units of 10 sec.
				!  ols<43: -32768
	-	I		!Empty (Delete character)
	NSC	I		!First record number of SC group
	PROJECT	I		!Project number
	FIELD	C12		!Fieldname
	VOLGNR	J		!Observation number (yynnnnn)
				!  FVERS<2: no year (yy) in number 
				!  ols=57:  yyyy in stead of yy (00327..00586
	STUURC	I		!Online peripherals control bits (*)
				!  FVERS<3: line (1) or cont (0)
	TYPE	C2		!Observation type
	DATE	I(6)		!Epoche and civil start time of the observation
	PRFLG	I		!Online program flags (*)
				!  FVERS<3: reserved
	OLSYS	I		!Online program system nr.
				!  FVERS<3: reserved
	JDAY	D	<D12.2>	!Time of the middle of the observation in julian days
	BECEN	D	<D12.7>	!Time of the middle of the obs. in bessel centuries
	JUCEN	D	<D12.7>	!Time of the middle of the obs. in julian centuries
	FREQC	I		!Observing frequency code
	CATEG	C2		!Astronomical type of observation
	ALLOC	I 		!Program committee allocation code
	REQUEST	C4		!Date of reception request from by tel. group
	STATBG	J(0:1)		!Status BG corrections
	STATBE	J(0:1)		!Status BE corrections
	STATSE	J(0:1)		!Status SE corrections
	POPER	I		!Period on pointing grid point 
				!(Units 10 sec) (*)
				!  FVERS<5: NOBSN
	DXBIT	B(0:1)		!DXB control bit
	APCS	B(0:1)		!DXB recycling instel code
	RA1	D	<DAF12.7> !Right ascension fieldcentre of epoche
	DEC1	D	<DAF12.7> !Declination fieldcentre of epoche
	MODE	I		!Digital correlator mode
	POLC	I 		!Dipole position code
	BAND	E	<E12.6>	!Total bandwidth (DLB). Sum of all bands (DCB)
	NTOT	I		!Total number of channels (*)
				!  FVERS=(>?)6: 4096 for DCB
	NFREQ	I		!Total number of frequency points or bands
	SFREQ	I		!Spacing of the frequency points in
				!units of 0.1 Khz (Only DLB)
				!DCB --> CDCBD - Bitcode 
				!Bit 0 - 7: Describing used bands (Band: 1 - 8)
				!Bit 8 -15: Describing used bandwith (1 = 5 Mhz,
				!0 = 10 Mhz)
	NRPOL	I		!Number of polarization channels
	NRINT	I		!Number of interferometers (*)
				!  FVERS>=6: number of standard ifrs
	TELWD	I(0:1)		!Receiver in use/not in use code
	BSINT	I		!Basic intergration time of the backend (U.T.)
				!  FVERS<5: S.T.
				!  ols=52:  0.1 U.T. sec
	CONFNR	J		!System configuration number
	BECODE	C4		!DLB ,DCB ,DXB .
				!  FVERS<6: reserved (always DLB)
	RA0	D 	<DAF12.7> !Apparent right ascension field centre at middleof obs.
	DEC0	D	<DAF12.7> !Apparent declination field centre at middle of obs.
	FREQ	D	<D12.6> !Obs. freq. of middle of band for middle of
				!observation (DLB) or primary fringe stopping
				!Frequency (DCB).
	HAST	D	<DAF12.7> !Hour angle middle of first 10 sec U.T. period
	HAEND	D	<DAF12.7> !Hour angle middle of last 10 sec  U.T. period
	LST	D	<DAF12.7> !Local sidereal time of middle of the observation
				! PARALLAX...RDEC2 lots of changes FVERS<4 (*)
	PARALAX	E	<EAF12.7> !Paralax for apparent observations (Epoche= 9)
				!in circles
	RRA1	E	<E12.8>	!Pointing offset or linear rate in R.A. 
				!(Circles per juliaans day or degrees/U.T.day)
	RDEC1	E	<E12.8>	!Pointing offset or linear rate in DECL.
				!(Circles per juliaans day or degrees/U.T.day)
	RRA2	E		!Pointing offset or quadratic rate in R.A.
				!(Circles per juliaans day or degrees/U.T.day**2)
	RDEC2	E		!Pointing offset or quadratic rate in DECL.
				!(Circles per juliaans day or degrees/U.T.day**2)
	VLCTY	E		!Velocity (Km/sec) in system given by velc
				!(DCB - empty)
	VELC	I		!Velocity reference system code (DCB - empty)
				! INX...FDEC3 lots of changes FVERS<4
	INX	I		!Telescope bits for pointing offset
	DRA	E	<EAF12.7> !Pointing offset in RA  (Circles)
	DDEC	E	<EAF12.7> !Pointing offset in DEC (Circles)
	NPC	I		!Period on the centre (Units of 10 sec.)
	NPS1	I		!Period on source 1   (Units of 10 sec.)
	FDRA1	E	<EAF12.7> !Offset in RA  for source 1
	FDEC1	E	<EAF12.7> !Offset in DEC for source 1
	NPS2	I		!Period on source 2   (Units of 10 sec.)
	-	-(0:1)		!            -
	FDRA2	E	<EAF12.7> !Offset in RA  for source 2
	FDEC2	E	<EAF12.7> !Offset in DEC for source 2
	NPS3	I		!Period on source 3   (Units of 10 sec.)
	-	-(0:1)		!            -
	FDRA3	E	<EAF12.7> !Offset in RA  for source 3
	FDEC3	E	<EAF12.7> !Offset in DEC for source 3
	-	-(0:1)		!            -
	SPEFU	C2		!Special observation type code
				!  ols<61: reserved
				!  VOLGNR 9100750..9101671: archive in EBCEDIC 
	POST	J(0:13)		!Position telescopes in 2**(-16) M
	TAPER	I		!Code of weighting function used for the
				!Fourier transform to frequency (DCB - empty).
	DEVC0	B(0:1)		!Device code bits (*)
				!  ols<43: reserved
	FREQ0	D	<D12.6>	!Rest freq. for line observations (Line) or
				!observing freq. (DCB), 0 if FREQC<10
				!  FVERS<6: reserved 
	STOPAR	I		!Data in channel-fluxes (0) or in stokes-
				!parameters (1). (Dwingeloo use)
				!  ols<43: reserved
	MSPAT	I		!Mosaicking pattern number (=0: no mosaicking)
				!  ols<60: reserved
	MPOSN	I		!Position in mosaicking pattern (0,1,...,N-1)
				!  ols<60: reserved
	MSNP	I		!Number of pos. in mosaick pattern (else 1)
				!  ols<62: reserved
	-	-(0:17)		! - to be filled in -
	PATPO	I	<XI>	!Mask of telescopes that scan in position
	PATFD	I	<XI>	!Mask of telesc. that scan in fringe-stop
				! and delay position 
	-	-(0:267)	!	            -
	NRSTS	I		!Total number of sets
	NRFRQ	I		!Total number of frequency points
	LENT	I		!Number of bytes per entry in the following table
	SET	S:SET(0:MAXSET)	!Set definitions
.END					!END DEFINITION
!-
