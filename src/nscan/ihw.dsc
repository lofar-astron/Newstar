!+ IHW.DSC
!  WNB 900118
!
!  Revisions:
!
!
!	Define WSRT IH block
!
!
!
%VERSION=7					!VERSION
%SYSTEM=59
%USER=WNB
%%DATE
%%NAME
%REVISION=CMV=940414="Add historical notes"
%REVISION=WNB=900118="Tape vs 7, system 59"
%COMMENT="FDW.DSC defines the WSRT IH block"
!-
.PARAMETER
.BEGIN=IHW
				! (*) means use with care, not repaired
	CBI	I		!Unequal data block flag (32767)
	CBT	C2		!Identification: Interferometer Header (IH)
	NIH	J		!Record number of this record
	LIH 	J		!Number of IH records in this group
	IHLNK	J		!First record of the next IH group
	SDAY	I		!Local U.T. day number
				!  FVERS<5: ST
	STIM	I		!Start U.T. time in units of 10 sec
				!  FVERS<5: ST
				!  FVERS<3: units of minutes 
	BANDNR	I		!Frequentie bandnr. (*)
				!  FVERS<6: Setnr.
	INFNR	I		!Interferometer number of this infr. (*)
				!  See SHW.DSC
	WTEL	I		!West telescope indicator: 0-D is 0...13 (*)
				!  See SHW.DSC
	OTEL	I		!East telescope indicator: 0-D is 0...13 (*)
				!  See SHW.DSC
	BFREQ	J		!Set frequency (2**(-16) Mhz) (*)
				!  FVERS<2: single float (E) 
	DRT	E		!Baseline of the interferometer (M)
	HAB	E		!Start hourangle, middle of the first
				!integration time in U.T. sec
				!  FVERS<5: in S.T. sec
	HAE	E		!End hourangle, middle of the last
				!integration time in U.T. sec
				!  FVERS<5: in S.T. sec
	DHA	E		!Hour angle increment (Integration time in U.T. sec)
				!  FVERS<5: in S.T. sec
	LD	I		!Number of data records in this group (*)
				!  FVERS<6: in S.T. sec
	NDATP	I		!Number of data points, inclusive NDELP
	NDELP	I		!Number of deleted data points
	NEXP	I		!Common exponential scaling factor
				!  ols=42: should be 1
	FSCAL	I		!Common multiplication scaling factor
				!  ols=42: should be 1
	OFFS	I		!Common offset scaling factor
	NOISE	E		!Amplitude noise
				!  FVERS<3: NOISE...MEPHA are empty
	ACOS	E		!Average cosine
	MECOS	E		!Mean error in average cosine
	MACOS	I		!Maximal cosine
	MICOS	I		!Minimal cosine
	ASIN	E		!Average sine
	MESIN	E		!Mean error in average sine
	MASIN	I		!Maximal sine
	MISIN	I		!Minimal sine
	AAMP	E		!Average amplitude
	MEAMP	E 		!Mean error in average amplitude
	MAAMP	I		!Maximal amplitude
	MIAMP	I		!Minimal amplitude
	APHA	E		!Average phase
	MEPHA	E		!Mean error in average phase
	INCT	I		!Increment time in U.T. sec.
				!  ols=52: float
				!  FVERS<5: in S.T. sec.
	DWELT	I		!Time per mosaicking pattern pos. per radial
				!in sec. (=INCT if no mosaicking)
				! ols<60: reserved
	VOLGNR	J		!Observation number (*)
				! FVERS<2: reserved
	INTT	E		!Integration time in U.T sec.
				! ols<53: reserved
	DRADT	I		!Time between 2 successive radials of the same
				!mosaicking pattern in sec (=INCT if no mos.)
	-	-(0:5)		!	    -
.END					!END DEFINITION
!-
