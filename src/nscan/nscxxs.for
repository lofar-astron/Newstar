C+ NSCXXS.FOR
C  WNB 910211
C
C  Revisions:
C	CMV 931119	Print character fields correctly
C	WNB 931126	Print character fields correctly
C	WNB 931215	Some formating; prepare P: and S:; limit width
C	JPH 941010	TXTU 8 --> 12 chars, units for scalars 5 __> 8 chars.
C			Comments
C
	SUBROUTINE NSCXXS(PTYPE,DAT,EDL,EDC,EDJ)
C
C  Show an area in detail
C
C	This routine takes the edit descriptors in EDC, EDJ and uses them to 
C format the data in DAT.
C
C	EDC comes directly from <xxx>_E_DEF which in turn is a direct copy of
C the edit parameter arguments in <> in <xxx>.DSC. WNTINC does not interpret
C these parameters except for forcing them into a uniform character length
C defined by the output statements in routine WNTFIO (about line 840); this
C length was raised from 10 to 12 chars on 941010 to allow somewhat more
C informative unit strings.
C
C	This routine does some formatting of its own, such as:
C
C	- Limit field name to 8 characters
C
C	- Limit the units field for single variables to 8 characters
C
C	- Limit the width <w> in such formats as E<w>.<d> to 10. (If you want
C a wider field, use the $ field width directive, e.g. 17$E<w>.<d>.) 
C 
C
C  Result:
C
C	CALL NSCXXS ( PTYPE_J:I, DAT_B(*):I, EDL_J:I, EDC_C*(4,*):I,
C				EDJ_J(4,*):I)
C					Show on output PTYPE the area DAT
C					with EDL edit lines given in
C					EDC and EDJ.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
C
C  Parameters:
C
	INTEGER SLEN			!START TEXT LENGTH
	  PARAMETER (SLEN=8)
	CHARACTER*(SLEN) STXT		!START TEXT FORMAT
	  PARAMETER (STXT='!80$8Q1\')
C
C  Arguments:
C
	INTEGER PTYPE			!PRINT TYPE (f_p, f_t ETC)
	BYTE DAT(0:*)			!DATA AREA
	INTEGER EDL			!LENGTH EDIT ARRAYS
	CHARACTER*(*) EDC(4,*)		!EDIT DATA
	INTEGER EDJ(4,*)		!EDIT DATA
C
C  Function references:
C
	INTEGER WNCALN			!STRING LENGTH
C
C  Data declarations:
C
	CHARACTER*256 TXT		!A TEXT LINE
	CHARACTER*16 TXT1		!FORMAT
	CHARACTER*12 TXTU		!UNITS
	CHARACTER*80 ARGSTR
C-
	TXT=STXT					!NO TEXT YET
	J=SLEN						!TEXT POINTER
	J1=0						!TEXT LENGTH
	DO I=1,EDL					!ALL LINES
	  J1=((J1+25)/26)*26				!CORRECT POINTER
	  IF (EDC(4,I)(1:2).EQ.'P:') THEN		!MAKE UNITS
	    TXTU=':P'
	  ELSE IF (EDC(4,I)(1:2).EQ.'S:') THEN
	    TXTU=':S'
	  ELSE
	    TXTU=EDC(3,I)				!UNITS
	  END IF
	  IF (EDJ(2,I).LE.0) THEN			!NOTHING
	  ELSE IF (EDJ(2,I).EQ.1 .OR. TXTU.EQ.':S') THEN! scalar
	    IF (J1.GT.52) THEN				! SHOULD START NEW LINE
	      CALL WNCTXT(PTYPE,TXT)			! flush
	      TXT=STXT					!  and reset buffer
	      J=SLEN					! TEXT POINTER
	      J1=0					! TEXT LENGTH
	    END IF
	    TXT(J+1:)=EDC(1,I)				! FIELDNAME
	    J=J+8					! POINTER
	    J1=J1+8					! LENGTH
	    IF (TXTU.EQ.':S') THEN			! SUB-STRUCTURE
	      CALL WNCTXS(TXT(J+1:),'!4$UJ !7$AS',
	1		EDJ(2,I),EDC(4,I))
	      J=J+13					! POINTER TXT
	      J1=J1+13					! POINTER LINE
	    ELSE
	      I1=INDEX(EDC(2,I),'$')
	      IF (I1.LE.0) THEN				! NO LENGTH GIVEN
	        TXT1='!12$'//EDC(2,I)			! default length
	        IF (EDC(2,I)(1:2).EQ.'AL') THEN
		  CALL WNCTXS(TXT1(7:9),'!UJ',EDJ(4,I))
	        END IF
	        CALL WNCTXS(TXT(J+1:),TXT1,DAT(EDJ(1,I))) !CONVERT
	        J=J+13					!POINTER TXT
	        J1=J1+13				!POINTER LINE
	      ELSE					!LENGTH GIVEN
	        TXT1='!'//EDC(2,I)			!FORMAT
	        IF (EDC(2,I)(I1+1:I1+2).EQ.'AL') THEN
		  CALL WNCTXS(TXT1(I1+4:I1+6),'!UJ',EDJ(4,I))
	        END IF
	        CALL WNCTXS(TXT(J+1:),TXT1,DAT(EDJ(1,I))) !CONVERT
	        I1=WNCALN(TXT)				!NEW POINTER
	        J1=J1+(I1-J)+1				!POINTER LINE
	        J=I1+1					!POINTER TXT
	      END IF
	    END IF
	    TXT(J+1:)=TXTU				!UNITS
	    J=J+8
	    J1=J1+8
	  ELSE						! array
	    IF (J1.GT.0) THEN
	      CALL WNCTXT(PTYPE,TXT)			! flush
	      TXT=STXT					!  and reset buffer
	      J=SLEN					!TEXT POINTER
	      J1=0					!TEXT LENGTH
	    END IF
	    I1=INDEX(EDC(2,I),'$')			!LENGTH GIVEN?
	    IF (I1.LE.0) THEN				!NO LENGTH GIVEN
	      IF (EDC(2,I)(1:2).EQ.'AL') THEN		!AL
	        CALL WNCTXT(PTYPE,TXT(:J)//'!-7$AS!13$#'//
	1		EDC(2,I)(1:2)//'# !AS',
	1		EDC(1,I),EDJ(2,I),EDJ(4,I),DAT(EDJ(1,I)),
	1		TXTU)
	      ELSE
		I1=WNCALN(EDC(2,I))			!FORMAT LENGTH
		ARGSTR=TXT(:J)//'!-7$AS!13$#'//EDC(2,I)(1:I1)//' !AS'
	        CALL WNCTXT(PTYPE,ARGSTR,
	1		EDC(1,I),EDJ(2,I),DAT(EDJ(1,I)),
	1		TXTU)
	      END IF
	      TXT=STXT					!NO TEXT YET
	      J=SLEN					!TEXT POINTER
	      J1=0					!TEXT LENGTH
	    ELSE					!LENGTH GIVEN
	      IF (EDC(2,I)(I1+1:I1+2).EQ.'AL') THEN	!AL
	        ARGSTR=TXT(:J)//'!-7$AS!'//
	1		EDC(2,I)(1:I1)//'#'//
	1		EDC(2,I)(I1+1:I1+2)//'# !AS'
	        CALL WNCTXT(PTYPE,ARGSTR,
	1		EDC(1,I),EDJ(2,I),EDJ(4,I),DAT(EDJ(1,I)),
	1		TXTU)
	      ELSE
		I2=WNCALN(EDC(2,I))			!FORMAT LENGTH
		ARGSTR=TXT(:J)//'!-7$AS!'//
	1		EDC(2,I)(1:I1)//'#'//
	1		EDC(2,I)(I1+1:I2)//' !AS'
		CALL WNCTXT(PTYPE,ARGSTR,
	1		EDC(1,I),EDJ(2,I),DAT(EDJ(1,I)),
	1		TXTU)
	      END IF
	      TXT=STXT					!NO TEXT YET
	      J=SLEN					!TEXT POINTER
	      J1=0					!TEXT LENGTH
	    END IF
	  END IF
	END DO
C
	IF (J1.GT.0) THEN				!LAST LINE
	  CALL WNCTXT(PTYPE,TXT)
	  CALL WNCTXT(PTYPE,' ')
	END IF
C
	RETURN
C
C
	END
