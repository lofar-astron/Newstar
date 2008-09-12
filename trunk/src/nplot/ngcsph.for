C+ NGCSPH.FOR
C  WNB 920826
C
C  Revisions:
C	WNB 930709	Add file number
C	JPH 940810	Narrow down format to <80 chars.
C			Add NGCSPL
C	JPH 940818	Widen space for index from 24 to 32 chars
C	JPH 940819	Add NGCSPC
C			Leave out VNR to get more space for TYP
C
C
	SUBROUTINE NGCSPH(SNAM,NGF)
C
C  Show brief data in NGF file
C
C  Result:
C
C	CALL NGCSPH( SNAM_J(0:7):I, NGF_B(*):I
C				will show data in NGF file SNAM
C	CALL NGCSPL( SNAM_J(0:7):I, NGF_B(*):I
C				will give a terminal message showing the
C				program is alive
C	CALL NGCSPC( SNAM_J(0:7):I, NGF_B(*):I, COUNTS_J(4:5):I)
C				will show grp.fld.chn with pol, iort and seq
C				counts
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGC_DEF'
	INCLUDE 'NGF_O_DEF'		!PLOT HEADER

C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER SNAM(0:7)		!SET NAME
	BYTE NGF(0:NGFHDL-1)		!DATA
	INTEGER NDONE			!NR OF CUTS DONE
	INTEGER CNT(3:5)		!POL, IFR/TEL AND CUT COUNTS
C
C  Function references:
C
	CHARACTER*32 WNTTSG		!SET NAME
C
C  Data declarations:
	INTEGER S3			!SAVE SNAM(3)
C
C-
C
C SHOW PLOT
C
	SNAM(6)=-1			!ASSURE END
	CALL WNCTXT(F_TP,'!5$UJ !AS!24C !-12$AL12'//
	1	' !-35$AL35'//
	1	' !2$AL2!2$AL2',
	1	NGF(NGF_SETN_1),WNTTSG(SNAM,0),NGF(NGF_NAM_1),
	1	NGF(NGF_TYP_1),
	1	NGF(NGF_IFR_1),NGF(NGF_POL_1))
C
C READY
C
	RETURN
C
C
	ENTRY NGCSPL(SNAM,NGF,NDONE)
C
	S3=SNAM(3)
	SNAM(3)=-1
	CALL WNCTXT(F_T,'At !AS!18C !-12$AL12 !-35$AL35 - !UJ done',
	1		WNTTSG(SNAM,0),NGF(NGF_NAM_1),
	1		NGF(NGF_TYP_1), NDONE)
	SNAM(3)=S3
	RETURN
C
C
	ENTRY NGCSPC(SNAM,NGF,CNT)
C
	S3=SNAM(3)
	SNAM(3)=-1
	CALL WNGMV(4,NGF(NGF_SCN_1),I)
	CALL WNCTXT(F_TP,'!5$UJ !AS!14C !-12$AL12 !-35$AL35'//
	1		' !1$UJ\*!3$UJ\*!4$UJ*!4$UJ',
	1		NGF(NGF_SETN_1),
	1		WNTTSG(SNAM,0),NGF(NGF_NAM_1),
	1		NGF(NGF_TYP_1),
	1	        CNT(3), CNT(4)/CNT(3), CNT(5)/CNT(4),
	1		I)
	SNAM(3)=S3
C
	END
