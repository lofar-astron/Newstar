C+ WNTTSG.FOR
C  WNB 900320
C
C  Revisions:
C	WNB 910730	Cater for # type set name
C	GvD 920506	Use STR iso WNTTSG to fill string (bug on SUN)
C
	CHARACTER*(*) FUNCTION WNTTSG(SGN,WID)
C
C  Translate sub-group full name to string
C
C  Result:
C
C	WNTTSG_C* = WNTTSG( SGN_J(0:7):I, WID_J:I)
C			Translate the sub-group as defined by SGN to a
C			string a.b.c with each a,b,c at its default width
C			(WID<=0) or at fixed width WID.
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
	INTEGER SGN(0:7)		!SUB-GROUP NAME
	INTEGER WID			!WIDTH EACH FIELD (OR 0)
C
C  Function references:
C
	INTEGER WNCAL0			!STRING LENGTH
C
C  Data declarations:
C
	CHARACTER*32 STR
C
C-
	STR=' '					!ASSUME EMPTY
	IF (SGN(1).EQ.-2) THEN			!# TYPE
	  IF (WID.LE.0) THEN			!DEFAULT WIDTH
	    CALL WNCTXS(STR,'#!UJ',SGN(0))
	  ELSE
	    CALL WNCTXS(STR,'#!#$ZJ',WID,SGN(0))
	  END IF
	ELSE					!NORMAL SET TYPE
	  I=0					!FIELD COUNT
	  DO WHILE (I.LT.8 .AND. SGN(I).NE.-1)	!ALL FIELDS
	    J=WNCAL0(STR)+1			!POINTER
	    IF (I.NE.0) THEN
	      STR(J:J)='.'			!SEPARATOR
	      J=J+1
	    END IF
	    IF (WID.LE.0) THEN			!DEFAULT WIDTH
	      CALL WNCTXS(STR(J:),'!UJ',SGN(I))
	    ELSE				!SPECIFIED WIDTH
	      CALL WNCTXS(STR(J:),'!#$ZJ',WID,SGN(I))
	    END IF
	    I=I+1				!NEXT FIELD
	  END DO
	END IF
C
	WNTTSG=STR
	RETURN
C
C
	END
