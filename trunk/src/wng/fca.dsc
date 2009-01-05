!+ FCA.DSC
!  WNB 850910
!
!  Revisions:
!
%REVISION=WNB=930811="Add some names; change FIB length; add FAT*"
%REVISION=WNB=930803="Use WNTINC options"
%REVISION=WNB=890724="Original version"
%REVISION=JPH=930415="FCA_x_WRT --> FCA_x_WRTAPE"
!
!	Define FCA (File Control Area)
!
%COMMENT="FCA.DSC defines the FCA (File Control Area)"
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
%ALIGN						!ALIGN STRUCTURES
!
%GLOBAL=FIB__L=80				!FIB LENGTH (SYSTEM DEPENDENT)
%GLOBAL=ATR__L=12				!ATTRIBUTE CONTROL BLK LENGTH
%GLOBAL=RATR__L=32				!RECORD ATTRIBUTE LENGTH
%GLOBAL=FCA__FNL=80				!FILE NAME LENGTH
!-
.PARAMETER
	FCA_M		M:	\		!MASKS FOR:
				/ASSIGN,ACCESS,ACTIVE, \ !CHANNEL ASSIGNED, FILE ACCESSED
				SEQUEN,TMP,MAGTAPE,,OLD,/ !READ/WRITE ACTIVE
							!SEQUENTIAL FILE
							!TEMP. FILE, MAGNETIC TAPE
							!OLD FILE
	FCA_V		A:(0)	\		!BITS FOR:
				/ASSIGN,ACCESS,ACTIVE, \ !CHANNEL ASSIGNED, FILE ACCESSED
				SEQUEN,TMP,MAGTAPE,,OLD,/ !READ/WRITE ACTIVE
							!SEQUENTIAL FILE
							!TEMP. FILE, MAGNETIC TAPE
							!OLD FILE
	FCA_M_WRTAPE	J	/64/		!MAGNETIC TAPE WRITE
	FCA_V_WRTAPE	J	/6/
	FCA_M_WRITE	J	/256/		!WRITE ALLOWED
	FCA_V_WRITE	J	/8/
!
! The following are from $FATDEF, only available for Macro
!	They are the FAT$W_* values
!
%LOCAL=EFBLKL=10
%LOCAL=EFBLKH=8
%LOCAL=HIBLKL=6
%LOCAL=HIBLKH=4
%LOCAL=FFBYTE=12
	FAT_EFBLKL_1	J	/EFBLKL/
	FAT_EFBLKH_1	J	/EFBLKH/
	FAT_HIBLKL_1	J	/HIBLKL/
	FAT_HIBLKH_1	J	/HIBLKH/
	FAT_FFBYTE_1	J	/FFBYTE/
	FAT_EFBLKL_I	J	/EFBLKL/2/
	FAT_EFBLKH_I	J	/EFBLKH/2/
	FAT_HIBLKL_I	J	/HIBLKL/2/
	FAT_HIBLKH_I	J	/HIBLKH/2/
	FAT_FFBYTE_I	J	/FFBYTE/2/
!
.BEGIN=FCA
	LINK	J			!LINK, MUST BE AT 0
	TID	J			!ID. 0=FCA, 1=MCA, MUST BE AT 4
	SIZE	J			!SIZE OF BLOCK
	CHAN	J			!ASSIGNED CHANNEL
	IOSB	J(2)			!IO STATUS BLOCK
	  IOSBI=IOSB I(4)
	BITS	J			!BITS
					!THE ABOVE SHOULD BE SAME FOR FCA & MCA
	FIBDES	J(2)			!FIB DESCRIPTOR
	DID	J(2)			!DIRECTORY ID
.ALIGN=LB_J				!MAKE SURE
	ATRJ	J(ATR__L/LB_J)		!ATTRIBUTE CONTROL BLOCK
	  ATR=ATRJ B(ATR__L)
	BQT	J(2)			!TIME ORDERED BUFFER QUEUE
	BQA	J(2)			!ADDRESS ORDERED BUFFER QUEUE
	BLEN	J			!BUFFER LENGTH
	BCP	J			!BUFFER CONTROL AREA POINTER
	FEA	J(2)			!ACTIVE FILE ELEMENT QUEUE
	FEE	J(2)			!EMPTY FILE ELEMENT QUEUE
	FEP	J			!ELEMENT AREA POINTER
	FEL	J			!CURRENT ELEMENT POINTER
	MCA	J			!PTR TO MCA
	HIBLK	J			!LOWEST NOT ALLOCATED ADDRESS
	EOF	J			!FIRST BYTE BEYOND EOF
	RAD	J			!LAST READ ADDRESS
	DAD	J			!DISK ADDRESS
	BAD	J			!BUFFER ADDRESS
	LEN	J			!LENGTH TO READ/WRITE
	ACLEN	J			!ACTUAL LENGTH READ/WRITTEN
	EF	J			!IO EF
	EFA	J			!ACTIVITY EF
	ERR	J			!FINAL IO ERROR
	MAP	J			!MAG TAPE POINTER
	MAB	J			!MAG TAPE BLOCK
	MAW	J			!MAG TAPE WRITE POSITION
	FNAML	J			!FILE NAME LENGTH
.ALIGN=LB_J				!MAKE SURE
	FIBJ	J(FIB__L/LB_J)		!FIB
	  FIBI=FIBJ I(FIB__L/LB_I)
	  FIB=FIBJ B(FIB__L)
.ALIGN=LB_J				!MAKE SURE
	FNAM	C(FCA__FNL)		!FILE NAME
.ALIGN=LB_J				!MAKE SURE
	RECATRJ	J(RATR__L/LB_J)		!RECORD ATTRIBUTES
	  RECATR=RECATRJ B(RATR__L)
.END					!END DEFINITION
!-