C+ NGIREC.FOR
C  CMV 931029
C
C  Revisions:
C	CMV 931029	Created
C
	LOGICAL FUNCTION NGIREC(CHECK)
C
C  This file has all subroutines needed for the recording options of NGIDS
C
C
C  Result:
C
C	NGIREC_L = NGIREC(CHECK_L:I)
C	   If CHECK is true, a check will be made if the next plane
C	   can be loaded (return .true. if space left)
C	   If CHECK is false, the plane that has just been loaded is
C	   recorded (return .true. if successful)
C
C	Typical use:   IF (NGIREC(.TRUE.)) THEN
C			   Load_plane
C			   JS=NGIREC(.FALSE.)
C	               END IF
C
C
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NGI_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
	LOGICAL CHECK
C
C  Function references:
C
	INTEGER N_GDI_SEQUENCE,N_GDI_RECORD,N_GDI_RINFO !GDI INTERFACE
	LOGICAL WNDPAR		!GET DWARF KEYWORD
	INTEGER	WNCALN		!LENGTH STRING
C
C  Data declarations:
C
	INTEGER SEQ(MXNSEQ)	! GIDS playback sequence
	INTEGER NREC,MREC 	! GIDS map recording
	CHARACTER*5 NEXT	! DO NEXT MAP (allow for resize etc)
	DATA NEXT/'ALL'/
	SAVE NEXT		! Keep next for next time
C-
	NGIREC=.TRUE.				!ASSUME OK
C
C	If CHECK is true, check wether we can have more planes
C
	IF (CHECK) THEN
C
C	We ask for the NEXT keyword to give the user time to zoom.
C	Zooming is applied to all preceding planes until a GCLEAR is given.
C
	   IF (.NOT.DO_FLAG.AND.(NRMAP.EQ.1.OR.NEXT(1:1).EQ.'Y')) THEN
	     IF (NEXT(1:1).EQ.'N') NEXT='ALL'
	     IF (.NOT.WNDPAR('NEXT',NEXT,LEN(NEXT),J0,NEXT)) GOTO 990
	     IF (NEXT(1:1).EQ.'N') GOTO 990
C
C	If only one map is in memory, record it, since with the map
C	that will be loaded now we have a movie.
C
C	If we are in flagging mode, only the current plane is shown.
C
	     IF (NRMAP.EQ.1)THEN
	        IF (N_GDI_RECORD(GID,1).LT.0) THEN
	          CALL WNCTXT(F_TP,'Could not record first map!/')
	          GOTO 990
	        END IF
	     END IF
	   END IF
C
C               Test if we have space to record the new image.
C
	   IF (.NOT.DO_FLAG.AND.NRMAP.GT.0) THEN
	     IF (N_GDI_RINFO(GID,NREC,MREC).LT.0) THEN
	       CALL WNCTXT(F_TP,'Error in GDI_RINFO!/')
	       GOTO 990
	     END IF
	     IF (NRMAP .GT. MREC) THEN
	       CALL WNCTXT(F_TP,
	1       'The maximum of !SL maps have already been loaded!/',MREC)
	       GOTO 990
	     END IF
	   END IF
C
C	If check is false, record the plane just loaded
C
	ELSE
C
C
C       If we are not flagging, the map is recorded.
C       We do not want to record just one map, so only maps 2,3,...
C	are recorded here, the first map is recorded right before the
C       second map is loaded. 
C
	   NRMAP=NRMAP+1
	   IF (.NOT.DO_FLAG) THEN
	      IF (NRMAP.NE.1) THEN
	        IF (N_GDI_RECORD(GID,NRMAP).LT.0) THEN
	          CALL WNCTXT(F_TP,'Could not record map !SL!/',NRMAP)
	          GOTO 990
	        END IF
	      END IF
C
C                               Define the new playback sequence
C
	      DO I1=1,MIN(MXNSEQ,NRMAP)
	        SEQ(I1)=I1
	      END DO
	      IF (NRMAP.GT.1) THEN
	        IF (N_GDI_SEQUENCE(GID,SEQ,
	1		MIN(MXNSEQ,NRMAP)).LT.0) THEN
 	          CALL WNCTXT(F_TP,'Error in GDI_SEQUENCE!/')
	        END IF
	      END IF
C
	   ENDIF				! NOT DO_FLAG
C
	ENDIF					! if (check)
	GOTO 800
C
C ERRORS: could not record, or no space left to load new map
C
 990	CONTINUE
	NGIREC=.FALSE.				!Cannot load more maps
C
 800	CONTINUE				!Everything all right
C
	RETURN
	END
 
