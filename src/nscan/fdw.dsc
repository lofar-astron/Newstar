!+ FDW.DSC
!  WNB 900118
!
!  Revisions:
!
%REVISION=CMV=940414="Add historical notes"
%REVISION=WNB=931215="Add some edit formats"
%REVISION=WNB=900118="Tape vs 7, system 59"
!
!	Define WSRT FD block
!
%COMMENT="FDW.DSC defines the WSRT FD block"
!
!
%VERSION=7					!VERSION
%SYSTEM=59
%USER=WNB
%%DATE
%%NAME
!-
.PARAMETER
.BEGIN=FDW
				! (*) means use with care, not repaired
	CBI	I		!Unequal data block flag (32767)
	CBT	C2		!Type identification: File Description (FD)
	NFD	J		!Record number of this record
	LFD	J		!Number of FD records in this group
	NFDE	J		!First FD record number at end of dataset
	SDAY	I		!Local U.T. day number
				!  FVERS<5: ST
	STIM	I		!Start U.T. time in units of 10 sec.
				!  FVERS<5: ST
				!  FVERS<3: units of minutes 
	ETIM	I		!End U.T. time in units of 10 sec. (*)
				!  ols<43: -32768
	-	I		!Empty (Delete character)
	FVERS	I		!Tape format version
	LRCRD	I		!Record length in bytes
	PHBLL	I		!Total number of records per block
	NOBS	I		!Number of simultaneous observations
	OLSYS	I		!Online program system nr. (*)
				!  ols<46: reserved
	-	-(0:1)
	COH	C2		!'OH'
	LOH	I		!Number of OH records per OH group
	NOH	J		!First record number of first OH group
	MOH	J		!Number of OH groups
				!  FVERS<7: NOHE = recnr backup OH record
	CSC	C2		!'SC'
	LSC	I		!Number of SC records per SC group
	NSC	J		!First record number of first SC group
	MSC	J		!Number of SC groups
	CSH	C2		!'SH'
	LSH	I		!Number of SH records per SH group (*)
				!  FVERS>6: Max length, SH's may differ
	NSH	J		!First record number of first SH group
	MSH	J		!Number of SH groups or number of sets
	CIH	C2		!'IH'
	LIH	I		!Number of IH records per IH group
	NIH	J		!First record number of first IH group
	MIH	J		!Total number of IH groups
	CDB	C2		!'DB'
	LDB	I		!1
	NDB	J		!First record number of first DB group
	MDB	J		!Total number of DB records
	-	-(0:3)
	NBL	J		!Total number of blocks within the dataset
	VOLUME	C6		!Name of magnetic volume or tape
	LABEL	C4		!Name of dataset or label
	LENGTH	I		!Total length in feet of the dataset
	REDDATE	J		!Civil date on which the dataset was made in
				!the format YYDDD (Y : year, D : daynumber)
	REDTIME	I		!Civil time in minutes on which the dataset was made
	-	-(0:5)
.END					!END DEFINITION
!-
