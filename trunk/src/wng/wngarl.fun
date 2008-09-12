C+ WNGARL.FUN
C  WNB 890308
C
C  Revisions:
C	WNB 921216	Make FUN
C
#ifdef wn_al__
	INTEGER FUNCTION WNGARL(ADDR,ARGL)
#else
	INTEGER FUNCTION WNGARX(ROUT,ARGL)
#endif
C
C  Get and reset argument list
C
C  Result:
C
#ifdef wn_al__
C	J = WNGARL ( ADDR_J(0:*):I, ARGL_J(0:*):O) Copy the call list
C					pointed to by ADDR to a proper ARGL.
C					The result (if wanted) is # of arguments
#endif
C	VAL =WNGARX ( ROUT_ENT:O, ARGL_J(0:*):IO) Transfer to ROUT with proper
C					call list made from ARGL. VAL, if any,
C					depends on ROUT type
#ifdef wn_al__
C	J = WNGARF ( NA_J:I, ARGL_J(0:*):IO) Make ARGL arg. list for NA
C					arguments, using existing ARGL
C					Note: ARGL must have negative members.
C				The ARGL produced will be:
C					-N	descriptor addr arg 1
C					...	...
C					-1	descriptor addr arg N
C					0	N (# of arguments)
#else
C				The ARGL will contain:
C					-M-2	length Mth string
C					...	...
C					-3	length 1st string
C					-2	M (= # of string lenghts)
C					-1	0 (count for string lengths)
#endif
C					0	N (# of real arguments)
C					1	address argument 1
C					...	...
C					N	address argument N
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
#ifdef wn_al__
	INTEGER ADDR(0:*)			!CALL LIST (-* MEMBERS ALSO)
	INTEGER NA				!# OF ARG.
#endif
	EXTERNAL ROUT				!ROUTINE TO CALL
#ifndef wn_al__
	  INTEGER ROUT
#endif
	INTEGER ARGL(0:*)			!ARGUMENT LIST (-* MEMBERS ALSO)
#ifdef wn_al__
C
C  Entry points:
C
	INTEGER WNGARX
	INTEGER WNGARF
#endif
C
C  Function references:
C
#ifdef wn_al__
	INTEGER WNGARQ				!CALL TRANSFER
#endif
C
C  Data declarations:
C
C-
#ifdef wn_al__
	ARGL(0)=IAND(ADDR(-1),'ff'X)		!MAKE PROPER COUNT
	DO I=1,ARGL(0)
	  ARGL(I)=ADDR(I-1)			!SET ARG. LIST
	  ARGL(-I)=0
C	  IF (IAND(ADDR(-1),'10000'X).NE.0)	!DESCRIPTORS PRESENT
C	1		ARGL(-I)=ADDR(-I-1)
	  ARGL(-I)=ADDR(-I-1)			!ALLIANT IF ERROR
	END DO
	WNGARL=ARGL(0)				!RETURN # OF ARG.
C
	RETURN
C
C WNGARX
C
	ENTRY WNGARX(ROUT,ARGL)
C
	ARGL(0)=IOR(ARGL(0),'10000'X)		!DESCRIPTORS PRESENT
	WNGARX=WNGARQ(ROUT,ARGL(1))		!DO ROUTINE
	ARGL(0)=IAND(ARGL(0),'ff'X)		!RESET
C
	RETURN
C
C WNGARF
C
	ENTRY WNGARF(NA,ARGL)
C
	IF (NA.LT.0) THEN			!CANNOT DO
	ELSE IF (NA.LE.ARGL(0)) THEN
	  DO I=1,NA				!SHIFT DESCRIPTORS
	    ARGL(-I)=ARGL(-ARGL(0)+NA-I)
	  END DO
	  ARGL(0)=NA
	ELSE
	  I1=ARGL(0)+1
	  DO I=I1,NA
	    ARGL(I)=0				!SET EMPTY ARGUMENTS
	  END DO
	  DO I=-ARGL(0),-1			!SHIFT DESCRIPTORS
	    ARGL(I-NA+ARGL(0))=ARGL(I)
	  END DO
	  DO I=1,NA-ARGL(0)			!FILL DESCRIPTORS
	    ARGL(-I)=0
	  END DO
	  ARGL(0)=NA
	END IF
	WNGARF=ARGL(0)				!RETURN # OF ARGUMENTS
#else
	WNGARX=ROUT(ARGL(0))			!DO ROUTINE
#endif
C
	RETURN	
C
C
	END
