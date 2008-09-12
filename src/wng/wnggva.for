C+ WNGGVA.FOR
C  WNB 910327
C
C  Revisions:
C
	LOGICAL FUNCTION WNGGVA(LEN,ADDR)
C
C  Get/free aligned virtual memory
C
C  Result:
C
C	WNGGVA_L = WNGGVA( LEN_J:I, ADDR_J:O)
C					Get virtual memory area of length LEN.
C					Address returned (if ok) in ADDR.
C					The area is aligned in such a way that:
C				A_*( (ADDR-A_OB)/LB_* ) points to identical
C				addresses.
C	WNGFVA_L = WNGFVA( LEN_J:I, ADDR_J:IO)
C					Free virtual area at address ADDR with
C					length LEN.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER LEN			!LENGTH TO GET/FREE
	INTEGER ADDR			!ADDRESS OF AREA RETURNED
C
C  Entry points:
C
	LOGICAL WNGFVA
C
C  Function references:
C
	LOGICAL WNGGVM			!GET AN AREA
	LOGICAL WNGFVM			!FREE AN AREA
C
C  Data declarations:
C
C-
C
C GET AREA
C
	I=LEN+2*LB_J+32				!LENGTH OF FULL BLOCK
	WNGGVA=WNGGVM(I,J)			!GET AREA
	IF (WNGGVA) THEN			!GOT IT
	  ADDR=IAND('ffffffe0'X,J+2*LB_J+31-A_OB)+A_OB !ALIGN
	  J1=(ADDR-A_OB)/LB_J			!POINTER TO AREA
	  A_J(J1-1)=J				!SAVE ADDRESS
	  A_J(J1-2)=I				!SAVE LENGTH
	ELSE					!ERROR
	  ADDR=0				!MAKE SURE
	END IF
C
	RETURN
C
C FREE AREA
C
	ENTRY WNGFVA(LEN,ADDR)
C
	IF (ADDR.EQ.0) THEN			!ERROR
	  WNGFVA=.FALSE.
	ELSE
	  J1=(ADDR-A_OB)/LB_J			!POINTER TO AREA
	  J=A_J(J1-1)				!SAVE ADDRESS
	  I=A_J(J1-2)				!SAVE LENGTH
	  WNGFVA=WNGFVM(I,J)			!FREE AREA
	END IF
	ADDR=0					!AND SET FREE
C
	RETURN
C
C
	END
