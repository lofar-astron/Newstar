!+ SSH.DSC
!  WNB 931015
!
!  Revisions:
!
%REVISION=WNB=931015="Original version"
!
!	Define SET description offsets, and a standard set start header
!
%COMMENT="SSH.DSC defines the offsets in SET list defintions"
%COMMENT="and the standard first 16 bytes of any set header"
%COMMENT=" "
!
%VERSION=1					!VERSION
%SYSTEM=1
%USER=WNB
%%DATE
%%NAME
!
! Define SET array definitiona
!
.PARAMETER
	SOF__N	J	/8/			!# of elements per row
						!Use as: (0:SOF__N-1)
!
! Define the offsets in row 0 of the SET search descriptor
!
	SOF_0	AF*:(0)	/NLINE,LEVEL,CLINE,CSET,CLH/ !The search history line:
						!NLINE: # of lines following
						!LEVEL: Current level (0,..)
						!CLINE: Current search line
						!CSET:  Current disk set ptr
						!CLH:	Current link header ptr
!
! Define the position for special indicator on search line
!
	SOF	AF*:(1)	/SPEC/			!SPEC:	indicator position #
!
! Define loop parameters
!
	SOF_L	AF*:(1)	/DEF,START,END,INC/	!DEFine as loop 
						!START value
						!END value
						!INCrement value
!
! Define special values and masks
!
	SOF_M_ALL	J	/-1/		!* indicator
	SOF_M_SPEC	J	/-2/		!# indicator
	SOF_M_HI	J	/-65536/	!'FFFF0000'X
	SOF_M_LO	J	/65535/		!'0000FFFF'X
	SOF_M_LOOP	J	/536870912/	!'20000000'X loop indicator
	SOF_M_SLOOP	J	/1073741824/	!'40000000'X loop def. line
!
! Define first part of any set header
!
.BEGIN=SSH
%INCLUDE=SSH_DSF
.END
!-
