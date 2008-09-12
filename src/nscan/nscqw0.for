C+ NSCQW0.FOR
C  WNB 940729
C
C  Revisions:
C
	LOGICAL FUNCTION NSCQW0(QUA,FCA,NLINE,QUB)
C
C  Write Qube list elements
C
C  Result:
C
C	ERROR_L = NSCQW0(QUA_J:I, FCA_J:I, NLINE_J:I, QUB_B(*):IO)
C	                         Write NLINE Qube lines
C       ERROR_L = NSCQW1(QUA_J:I, FCA_J:I, NLINE_J:I, QUB_B(*):IO)
C                                Merge complete Qube line set
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'QUB_O_DEF'		!QUBE LINE
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER QUA                     !QUBE CONTROL AREA
	INTEGER FCA                     !OUTPUT FILE
	INTEGER NLINE                   !# OF LINES TO WRITE
	BYTE QUB(0:*)                   !QUBE BUFFER
C
C  Entry points:
C
	LOGICAL NSCQW1
C
C  Function references:
C
	LOGICAL WNGSRT                  !SORT
	LOGICAL WNGGVA                  !GET MEMORY
	INTEGER WNGGJ                   !MAKE J
	LOGICAL WNFWR                   !WRITE
	LOGICAL WNFRD                   !READ
	INTEGER WNFEOF                  !FILE POSITION
	EXTERNAL NSCQC0                 !COMPARISON
	  INTEGER NSCQC0
	INTEGER NSCQC1,NSCQC2		!COMPARISON
C
C  Data declarations:
C
	LOGICAL NEW(0:1)                !FOR MERGING
C-
C
C NSCQW0
C
	NSCQW0=.TRUE.                   !ASSUME OK
	IF (.NOT.WNGSRT(QUB,NLINE,QUB__L,NSCQC0)) THEN
 100	  CONTINUE
	  NSCQW0=.FALSE.
	  RETURN
	END IF
	IF (.NOT.WNFWR(FCA,NLINE*QUB__L,QUB,-1)) GOTO 100
C
	RETURN
C
C NSCQW1
C
	ENTRY NSCQW1(QUA,FCA,NLINE,QUB)
C
C PREPARE
C
	NSCQW1=.TRUE.    		!ASSUME OK
	I0=(NLINE+A_J(QUA+QUA_NLINE_J)-1)/A_J(QUA+QUA_NLINE_J)+1    !# OF PARTS
	IF (.NOT.WNGGVA(2*LB_J*I0,J0)) GOTO 100 !GET BUFFER
	J0=(J0-A_OB)/LB_J
	J1=WNFEOF(FCA)                  !OUTPUT POINTER
	J2=0                            !OUTPUT CNT
	DO I=0,I0-1                     !FILL BUFFER
	  A_J(J0+0*I0+I)=MAX(0,MIN(A_J(QUA+QUA_NLINE_J),
	1                          NLINE-I*A_J(QUA+QUA_NLINE_J))) !LENGTH PART
	  A_J(J0+1*I0+I)=I*A_J(QUA+QUA_NLINE_J)*QUB__L !PART DISK POINTER
	END DO
C
C MERGE
C
	DO I=0,I0-2		        !DO MERGE CYCLES
	  NEW(0)=.TRUE.                 !NEED NEW VALUES
	  NEW(1)=.TRUE.
	  J2=0                          !OUTPUT CNT
	  DO WHILE(NEW(0) .OR. NEW(1))
	    IF (NEW(0) .AND. A_J(J0+0*I0+I).GT.0) THEN !MORE AVAILABLE
	      IF (.NOT.WNFRD(FCA,QUB__L,QUB(0),A_J(J0+1*I0+I))) GOTO 100
	      NEW(0)=.FALSE.
	      A_J(J0+0*I0+I)=A_J(J0+0*I0+I)-1 !COUNT
	      A_J(J0+1*I0+I)=A_J(J0+1*I0+I)+QUB__L
	      IF (NEW(1) .AND. A_J(J0+0*I0+I+1).GT.0) THEN !MORE AVAILABLE
	        IF (.NOT.WNFRD(FCA,QUB__L,QUB(QUB__L),A_J(J0+1*I0+I+1)))
	1	      GOTO 100
	        NEW(1)=.FALSE.
	        A_J(J0+0*I0+I+1)=A_J(J0+0*I0+I+1)-1 !COUNT
	        A_J(J0+1*I0+I+1)=A_J(J0+1*I0+I+1)+QUB__L
	        I1=NSCQC0(QUB(0),QUB(QUB__L)) !COMPARE
	        IF (I1.LE.0) THEN
		  IF (.NOT.WNFWR(FCA,QUB__L,QUB(0),J1+J2*QUB__L)) GOTO 100
		  J2=J2+1                    !COUNT
		  NEW(0)=.TRUE.
		  IF (I1.EQ.0) NEW(1)=.TRUE. !SKIP DOUBLE
	        ELSE
		  IF (.NOT.WNFWR(FCA,QUB__L,QUB(QUB__L),J1+J2*QUB__L)) GOTO 100
		  J2=J2+1                    !COUNT
		  NEW(1)=.TRUE.
	        END IF
	      ELSE                           !COPY FIRST BUFFER
	        NEW(1)=.FALSE.
	        IF (.NOT.NEW(0)) THEN
		  IF (.NOT.WNFWR(FCA,QUB__L,QUB(0),J1+J2*QUB__L)) GOTO 100
		  J2=J2+1                    !COUNT
		  NEW(0)=.TRUE.
	        END IF
	        DO WHILE (NEW(0) .AND. A_J(J0+0*I0+I).GT.0) !MORE AVAILABLE
	          IF (.NOT.WNFRD(FCA,QUB__L,QUB(0),A_J(J0+1*I0+I)))
	1		GOTO 100
	          A_J(J0+0*I0+I)=A_J(J0+0*I0+I)-1 !COUNT
	          A_J(J0+1*I0+I)=A_J(J0+1*I0+I)+QUB__L
		  IF (.NOT.WNFWR(FCA,QUB__L,QUB(0),J1+J2*QUB__L)) GOTO 100
		  J2=J2+1                    !COUNT
	        END DO
	        NEW(0)=.FALSE.
	      END IF
	    ELSE                             !COPY SECOND BUFFER
	      NEW(0)=.FALSE.
	      IF (.NOT.NEW(1)) THEN
	        IF (.NOT.WNFWR(FCA,QUB__L,QUB(QUB__L),J1+J2*QUB__L)) GOTO 100
	        J2=J2+1		           !COUNT
	        NEW(1)=.TRUE.
	      END IF
	      DO WHILE (NEW(1) .AND. A_J(J0+0*I0+I+1).GT.0) !MORE AVAILABLE
	        IF (.NOT.WNFRD(FCA,QUB__L,QUB(QUB__L),A_J(J0+1*I0+I+1)))
	1	      GOTO 100
	        A_J(J0+0*I0+I+1)=A_J(J0+0*I0+I+1)-1 !COUNT
	        A_J(J0+1*I0+I+1)=A_J(J0+1*I0+I+1)+QUB__L
	        IF (.NOT.WNFWR(FCA,QUB__L,QUB(QUB__L),J1+J2*QUB__L)) GOTO 100
	        J2=J2+1                    !COUNT
	      END DO
	      NEW(1)=.FALSE.
	    END IF
	  END DO                           !END MERGE CYCLE
	  DO I1=0,J2-1                     !COPY DATA
	    IF (.NOT.WNFRD(FCA,QUB__L,QUB(0),J1+I1*QUB__L)) GOTO 100
	    IF (.NOT.WNFWR(FCA,QUB__L,QUB(0),I1*QUB__L)) GOTO 100
	  END DO
	  A_J(J0+0*I0)=J2                  !NEW COUNT
	  A_J(J0+1*I0)=0                   !NEW INPUT PTR
	END DO                             !NEXT MERGE CYCLE
C
C GET RID OF DUPLICATES
C
	A_J(QUA+QUA_CNT_J)=J2
	J2=0                               !OUTPUT CNT
	I=0                                !INPUT CNT
	NEW(0)=.TRUE.
	NEW(1)=.TRUE.
	DO WHILE (I.LT.A_J(QUA+QUA_CNT_J)
	1         .OR. (.NOT.NEW(0) .AND. .NOT.(NEW(1))))
	  IF (NEW(0)) THEN
	    IF (.NOT.WNFRD(FCA,QUB__L,QUB(0),I*QUB__L)) GOTO 100
	    I=I+1                          !CNT
	    NEW(0)=.FALSE.
	  ELSE IF (NEW(1)) THEN
	    IF (.NOT.WNFRD(FCA,QUB__L,QUB(QUB__L),I*QUB__L)) GOTO 100
	    I=I+1                          !CNT
	    NEW(1)=.FALSE.
	  ELSE
	    I1=NSCQC0(QUB(0),QUB(QUB__L))  !COMPARE
	    IF (I1.EQ.0) THEN
	      NEW(1)=.TRUE.                !SKIP
	    ELSE IF (I1.LT.0) THEN
	      IF (.NOT.WNFWR(FCA,QUB__L,QUB(0),J2*QUB__L)) GOTO 100 !WRITE
	      NEW(0)=.TRUE.
	      J2=J2+1                      !COUNT
	    ELSE
	      IF (.NOT.WNFWR(FCA,QUB__L,QUB(QUB__L),J2*QUB__L)) GOTO 100 !WRITE
	      NEW(1)=.TRUE.
	      J2=J2+1                      !COUNT
	    END IF
	  END IF
	END DO
	IF (.NOT.NEW(0)) THEN              !WRITE LAST
	  IF (.NOT.WNFWR(FCA,QUB__L,QUB(0),J2*QUB__L)) GOTO 100 !WRITE
	  J2=J2+1
	END IF
	IF (.NOT.NEW(1)) THEN              !WRITE LAST
	  IF (.NOT.WNFWR(FCA,QUB__L,QUB(QUB__L),J2*QUB__L)) GOTO 100 !WRITE
	  J2=J2+1
	END IF
C
C FINALISE
C
	A_J(QUA+QUA_CNT_J)=J2
	A_J(QUA+QUA_NFLD_J)=0           !INIT QUBE DESCRIPTION
	A_J(QUA+QUA_NFRQ_J)=0
	A_J(QUA+QUA_NHA_J)=0
	A_J(QUA+QUA_NIFR_J)=0
	A_J(QUA+QUA_NBLK_J)=0
	J3=0                            !CURRENT FIELD START POINTER
	I=0                             !LINE CNT
	DO WHILE (I.LT.A_J(QUA+QUA_CNT_J))
	  IF (.NOT.WNFRD(FCA,QUB__L,QUB(0),J3)) GOTO 100
	  I=I+1				!CNT LINE
	  J1=J3+QUB__L			!INPUT PTR
	  I1=1				!FREQ. CNT
	  I2=WNGGJ(QUB(QUB_SCN_1)) 	!HA CNT
	  I3=1				!HA BLK COUNT
	  A_J(QUA+QUA_NFLD_J)=A_J(QUA+QUA_NFLD_J)+1 !FIELD ID
	  DO WHILE (I.LT.A_J(QUA+QUA_CNT_J)) !CHECK LINES
	    IF (.NOT.WNFRD(FCA,QUB__L,QUB(QUB__L),J1)) GOTO 100
	    IF (NSCQC1(QUB(0),QUB(QUB__L)).EQ.0) THEN !SAME FIELD
	      IF (NSCQC2(QUB(0),QUB(QUB__L)).EQ.0) THEN !SAME FREQ
		I2=I2+WNGGJ(QUB(QUB__L+QUB_SCN_1)) !ADD HA LENGTH
		I3=I3+1
	      ELSE
		A_J(QUA+QUA_NHA_J)=MAX(A_J(QUA+QUA_NHA_J),I2) !MAX. HA SIZE
		A_J(QUA+QUA_NBLK_J)=MAX(A_J(QUA+QUA_NBLK_J),I3) !MAX. HA BLK
		I2=WNGGJ(QUB__L+QUB_SCN_1) !NEW HA SIZE
		I3=1
	      END IF
	      CALL WNGMV(LB_J,A_J(QUA+QUA_NFLD_J),
	1                QUB(QUB__L+QUB_FID_1)) !SET FIELD ID
	      CALL WNGMVZ(LB_J,QUB(QUB__L+QUB_NFRQ_1))
	      IF (.NOT.WNFWR(FCA,QUB__L,QUB(QUB__L),J1)) GOTO 100 !REWRITE
	      I=I+1			!COUNT
	      I1=I1+1
	      J1=J1+QUB__L
	    ELSE
	      GOTO 200			!TRY MORE
	    END IF
	  END DO
 200	  CONTINUE
	  CALL WNGMV(LB_J,A_J(QUA+QUA_NFLD_J),QUB(QUB_FID_1)) !SET FIELD ID
	  CALL WNGMV(LB_J,I1,QUB(QUB_NFRQ_1)) !SET # OF FREQUENCIES
	  IF (.NOT.WNFWR(FCA,QUB__L,QUB(0),J3)) GOTO 100 !REWRITE
	  J3=J1			!NEXT FIELD START
	  A_J(QUA+QUA_NFRQ_J)=MAX(A_J(QUA+QUA_NFRQ_J),I1) !SET MAX. SIZES
	  A_J(QUA+QUA_NHA_J)=MAX(A_J(QUA+QUA_NHA_J),I2)
	  A_J(QUA+QUA_NBLK_J)=MAX(A_J(QUA+QUA_NBLK_J),I3)
	  A_J(QUA+QUA_NIFR_J)=MAX(A_J(QUA+QUA_NIFR_J),WNGGJ(QUB(QUB_NIFR_1)))
	END DO
C
	CALL WNGFVA(2*LB_J*I0,J0*LB_J+A_OB) !FREE MEMORY
	RETURN
C
C
	END
