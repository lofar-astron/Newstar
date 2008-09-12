C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C.Ident:	GEN_LINKHTON
C.Keywords:	Network Data-conversion
C.Author:	Ger van Diepen (NFRA, Dwingeloo)
C.Language:	DWARF-Fortran
C.Environment:	Any
C.Comments:
C.Version:	920113 GvD - creation
C.Returns:	None
C--------------------------------------------------------------------------
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LINK_HTON (DTYPE,IN,OUT,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type
	BYTE		IN(*)		! (i) input data array
	BYTE		OUT(*)		! (o) output data array
	INTEGER*4	NR		! (i) #elements in data arrays
C
C.Purpose:	Convert data from host to network byte order for
C		any DWARF data type
C.Returns:	None
C--------------------------------------------------------------------------
C
	IF (DTYPE.EQ.'J') THEN
	    CALL LINK_HTONJ (IN,OUT,NR)
	ELSE IF (DTYPE.EQ.'I') THEN
	    CALL LINK_HTONI (IN,OUT,NR)
	ELSE IF (DTYPE.EQ.'R') THEN
	    CALL LINK_HTONR (IN,OUT,NR)
	ELSE IF (DTYPE.EQ.'D') THEN
	    CALL LINK_HTOND (IN,OUT,NR)
	ELSE
	    CALL MOVE_BLB   (IN,OUT,NR)
	ENDIF
C
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LINK_HTONR (IN,OUT,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	REAL*4		IN(*)		! (i) input data array
	INTEGER*4	OUT(2,*)	! (o) output data array
	INTEGER*4	NR		! (i) #elements in data arrays
C
C.Purpose:	Convert real*4 data from host to network byte order
C.Returns:	None
C.Remarks:	Each data point is converted to 2 long integers
C		(one for the sign+exponent and one for the mantissa)
C--------------------------------------------------------------------------
C
	REAL*8		RLOG2
	PARAMETER	(RLOG2 = 1d0 / 0.6931471805599453)
C
	INTEGER*4	EXPO,NUM(2)
	REAL*8		NUMD
C
	DO I = 1,NR
	    NUM(2) = 0
	    NUMD   = IN(I)
	    IF (NUMD.EQ.0) THEN
		NUM(1) = 0
	    ELSE
		IF (NUMD.LT.0) THEN
		    NUM(2) = 1000000
		    NUMD   = -NUMD
		ENDIF
		EXPO   = LOG(NUMD) * RLOG2			! exponent
		NUM(1) = NINT((NUMD / 2d0**EXPO) * 2**29)	! mantissa
		NUM(2) = NUM(2) + EXPO				! sign+exponent
	    ENDIF
	    CALL LINK_HTONJ (NUM, OUT(1,I), 2)
	ENDDO
C
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LINK_HTOND (IN,OUT,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	REAL*8		IN(*)		! (i) input data array
	INTEGER*4	OUT(2,*)	! (o) output data array
	INTEGER*4	NR		! (i) #elements in data arrays
C
C.Purpose:	Convert real*8 data from host to network byte order
C.Returns:	None
C.Remarks:	Each data point is converted to 2 long integers
C		(one for the sign+exponent and one for the mantissa)
C--------------------------------------------------------------------------
C
	REAL*8		RLOG2
	PARAMETER	(RLOG2 = 1d0 / 0.6931471805599453)
C
	INTEGER*4	EXPO,NUM(2)
	REAL*8		NUMD
C
	DO I = 1,NR
	    NUM(2) = 0
	    NUMD   = IN(I)
	    IF (NUMD.EQ.0) THEN
		NUM(1) = 0
	    ELSE
		IF (NUMD.LT.0) THEN
		    NUM(2) = 1000000
		    NUMD   = -NUMD
		ENDIF
		EXPO   = LOG(NUMD) * RLOG2			! exponent
		NUM(1) = NINT((NUMD / 2d0**EXPO) * 2**29)	! mantissa
		NUM(2) = NUM(2) + EXPO				! sign+exponent
	    ENDIF
	    CALL LINK_HTONJ (NUM, OUT(1,I), 2)
	ENDDO
C
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LINK_NTOH (DTYPE,IN,OUT,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	CHARACTER*1	DTYPE		! (i) data type
	BYTE		IN(*)		! (i) input data array
	BYTE		OUT(*)		! (o) output data array
	INTEGER*4	NR		! (i) #elements in data arrays
C
C.Purpose:	Convert data from network to host byte order for
C		any DWARF data type
C.Returns:	None
C--------------------------------------------------------------------------
C
	IF (DTYPE.EQ.'J') THEN
	    CALL LINK_NTOHJ (IN,OUT,NR)
	ELSE IF (DTYPE.EQ.'I') THEN
	    CALL LINK_NTOHI (IN,OUT,NR)
	ELSE IF (DTYPE.EQ.'R') THEN
	    CALL LINK_NTOHR (IN,OUT,NR)
	ELSE IF (DTYPE.EQ.'D') THEN
	    CALL LINK_NTOHD (IN,OUT,NR)
	ELSE
	    CALL MOVE_BLB   (IN,OUT,NR)
	ENDIF
C
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LINK_NTOHR (IN,OUT,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	IN(2,*)		! (o) input data array
	REAL*4		OUT(*)		! (i) output data array
	INTEGER*4	NR		! (i) #elements in data arrays
C
C.Purpose:	Convert real*4 data from network to host byte order
C.Returns:	None
C.Remarks:	Each data point is converted from 2 long integers
C		(one for the sign+exponent and one for the mantissa)
C--------------------------------------------------------------------------
C
	INTEGER*4	SIGN,NUM(2)
	REAL*8		NUMD
C
	DO I = 1,NR
	    CALL LINK_NTOHJ (IN(1,I), NUM, 2)
	    IF (NUM(1) .EQ. 0) THEN
		OUT(I) = 0
	    ELSE
		SIGN = 0
		IF (NUM(2).GE.500000) THEN
		    SIGN   = -1
		    NUM(2) = NUM(2) - 1000000
		ENDIF
		NUMD = NUM(1)
		NUMD = (NUMD / 2**29) * 2d0**NUM(2)
		IF (SIGN.NE.0) NUMD = -NUMD
		OUT(I) = NUMD
	    ENDIF
	ENDDO
C
	RETURN
	END
C+PDOC+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	SUBROUTINE LINK_NTOHD (IN,OUT,NR)
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'DWC_DEF'
C
	INTEGER*4	IN(2,*)		! (o) input data array
	REAL*8		OUT(*)		! (i) output data array
	INTEGER*4	NR		! (i) #elements in data arrays
C
C.Purpose:	Convert real*8 data from network to host byte order
C.Returns:	None
C.Remarks:	Each data point is converted from 2 long integers
C		(one for the sign+exponent and one for the mantissa)
C--------------------------------------------------------------------------
C
	INTEGER*4	SIGN,NUM(2)
	REAL*8		NUMD
C
	DO I = 1,NR
	    CALL LINK_NTOHJ (IN(1,I), NUM, 2)
	    IF (NUM(1) .EQ. 0) THEN
		OUT(I) = 0
	    ELSE
		SIGN = 0
		IF (NUM(2).GE.500000) THEN
		    SIGN   = -1
		    NUM(2) = NUM(2) - 1000000
		ENDIF
		NUMD = NUM(1)
		NUMD = (NUMD / 2**29) * 2d0**NUM(2)
		IF (SIGN.NE.0) NUMD = -NUMD
		OUT(I) = NUMD
	    ENDIF
	ENDDO
C
	RETURN
	END
