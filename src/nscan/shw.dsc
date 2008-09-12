!+ SHW.DSC
!  WNB 900118
!
!  Revisions:
!
%REVISION=CMV=940414="Add historical notes"
%REVISION=WNB=931215="Add some edit formats"
%REVISION=WNB=930803="Change .RECORD"
%REVISION=WNB=900118="Tape vs 7, system 59"
!
!	Define WSRT SH block
!
!
!
%VERSION=7					!VERSION
%SYSTEM=59
%USER=WNB
%LOCAL=IFRLEN=12				!Length IFR subblock
%%DATE
%%NAME
%COMMENT="SHW.DSC defines the WSRT SH block"
!-
.PARAMETER
.STRUCTURE=IFR			!IFR table
	INFNR	  I		!Interferometer number
				!  FVERS<2: different codes
	WTEL	  I		!West telescope indicator
	OTEL	  I		!East telescope indicator
	RBAS	  I		!Baseline rounded to nearest meter
	NIH	  J		!Record # of IH block
.END				!End IFR table
.BEGIN=SHW
				! (*) means use with care, not repaired
	CBI	I		!Unequal data block flag (32767)
	CBT	C2		!Identification: Set Header (SH)
	NSH	J		!Record number of this record
	LSH	J 		!Number of SH records with information
				!  FVERS<5: always 5
	MHLNK	J		!First record number of the next SH group
	SDAY	I		!Local U.T. day number
				!  FVERS<5: ST
	STIM	I		!Start U.T. time in units of 10 sec
				!  FVERS<5: ST
				!  FVERS<3: units of minutes 
	BANDNR	I		!Frequentie bandnr.
				!  FVERS<6: set nr
	-	I		!Empty (Delete character) (Internal use Dw'loo:SETNR)
	BFREQ	J		!Set frequency (2**(-16) Mhz)
				!  FVERS<2: single float (E)
	SFREQ	I		!Spacing in frequency (DLB), bandwidth (DCB) (Mhz)
	DATYP	C2		!Data type (IF, XX, XY, YX, or YY)
	POLC	I		!Dipole position code
	NRINT	I 		!Number of interferometers in the set (40,88 or 160)
	PTS	J		!Total number of observed points in the set
	WIDTH	E	<E12.6>	!Bandwidth (Mhz)
				! ols<46: reserved
	CORC0	B(0:1)		!Correctioncode: 
				!   Bit 0 = 1 : IF phase correction applied
				!   Bit 1 = 1 : IF gain correction applied
				!   Zero      : IF correction not applied
				! ols<43: reserved
	-	-(0:109)	!	              -
	NENT	I		!Number of entries in the index table (40,88 or 160)
	LENT	I		!Entry length in bytes
 	IFR	S:IFR(0:159)	!IFR table
.END					!END DEFINITION
!-
