C+ NSCQOP.FOR
C  WNB 940216
C
C  Revisions:
C       WNB 940728	General update
C	WNB 940812	Add error output
C
	LOGICAL FUNCTION NSCQOP(QUA,FCA,SETS,LPOFF,INFO)
C
C  Create a Qube list from the user specified sets
C
C  Result:
C
C	NSCQOP_L = NSCQOP( QUA_J:O, FCA_J:I, SETS_J(0:*,0:*):I, LPOFF_J(0:*):I,
C                          INFO_J(QINFO__L:QINFO__H):O)
C				Create a Qube information set from SETS for
C				the current LPOFF loop in file FCA
C                               QUA is the control area pointer, INFO
C                               the (max) # of fields, freq, ha, ifr
C	NSCQCL_L = NSCQCL( QUA_J:I, FCA_J:I, SETS_J(0:*,0:*):I)
C				Remove structure and tmp files
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'SSH_O_DEF'		!SET FIELD DEFINITIONS
	INCLUDE 'CBITS_DEF'             !BIT DEFINITIONS
	INCLUDE 'QUB_O_DEF'		!QUBE DEFINITION
	INCLUDE 'STH_O_DEF'		!SCAN SET HEADER
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER QUA			!QUBE CONTROL AREA
	INTEGER FCA			!FILE
	INTEGER SETS(0:SOF__N-1,0:*)	!INPUT SETS
	INTEGER LPOFF(0:*)		!SET LOOP OFFSET
	INTEGER INFO(QINFO__L:QINFO__H) !QUBE INFORMATION
C
C  Entry points:
C
	LOGICAL NSCQCL
C
C  Function references:
C
	LOGICAL WNGGVA			!GET VIRTUAL MEMORY
	LOGICAL WNGSRT			!SORTING
	CHARACTER*20 WNFFNM             !GET FILENAME
	LOGICAL WNFOP                   !OPEN FILE
	LOGICAL NSCSTL			!GET NEXT SET
	LOGICAL NSCQW0                  !WRITE QUBE PART
	LOGICAL NSCQW1                  !MERGE QUBE PARTS
C
C  Data declarations:
C
	INTEGER SNAM(0:SOF__N-1)	!SET NAME
	CHARACTER*20 LFNM               !LOCAL FILE NAME
	BYTE QUB(0:QUB__L-1)		!QUBE LINE
	  INTEGER*4 QUBJ(0:QUB__L/LB_J-1)
	  REAL QUBE(0:QUB__L/LB_E-1)
	  DOUBLE PRECISION QUBD(0:QUB__L/LB_D-1)
	  EQUIVALENCE (QUB,QUBJ,QUBE,QUBD)
	BYTE STH(0:STH__L-1)		!SCAN SET HEADER
	  INTEGER*2 STHI(0:STH__L/LB_I-1)
	  INTEGER*4 STHJ(0:STH__L/LB_J-1)
	  REAL STHE(0:STH__L/LB_E-1)
	  DOUBLE PRECISION STHD(0:STH__L/LB_D-1)
	  EQUIVALENCE (STH,STHI,STHJ,STHE,STHD)
C-
C
C INIT
C
	NSCQOP=.TRUE.				!ASSUME OK
	IF (.NOT.WNGGVA(QUA__L,QUA)) GOTO 10    !QUBE CONTROL AREA
	QUA=(QUA-A_OB)/LB_J                     !AREA POINTER
	CALL WNGMVZ(QUA__L,A_J(QUA))
	LFNM=WNFFNM('000','TMP')
	IF (.NOT.WNFOP(A_J(QUA+QUA_FCA_J),LFNM,'WT')) THEN
 20	  CONTINUE
	  CALL WNCTXT(F_TP,'Cannot open temporary Qube file')
	  GOTO 800
	END IF
	CALL WNDDAM(A_J(QUA+QUA_MEMSZ_J))	!GET MEMORY CHUNKS
	A_J(QUA+QUA_NLINE_J)=
	1        MAX(128,A_J(QUA+QUA_MEMSZ_J)/QUB__L) !# OF LINES PER BUFFER
	IF (.NOT.WNGGVA(A_J(QUA+QUA_NLINE_J)*QUB__L,
	1        A_J(QUA+QUA_BPT_J))) THEN
 10	  CONTINUE
	  CALL WNCTXT(F_TP,'No memory for Qube tables')
	  A_J(QUA+QUA_BPT_J)=0
	  A_J(QUA+QUA_IBPT_J)=0
	  GOTO 800
	END IF
	A_J(QUA+QUA_BPT_J)=A_J(QUA+QUA_BPT_J)-A_OB
	CALL WNDSTR(FCA,SETS)			!RESET SET SEARCH STATUS
C
C CREATE QUBE LIST
C
	DO WHILE (NSCSTL(FCA,SETS,STH,QUBJ(QUB_STHP_J),
	1		SNAM,LPOFF))		!ALL SETS
	  QUBD(QUB_RA_D)=STHD(STH_RA_D)		!COPY SOME DATA
	  QUBD(QUB_DEC_D)=STHD(STH_DEC_D)
	  QUBD(QUB_FRQ_D)=STHD(STH_FRQ_D)
	  QUBE(QUB_BAND_E)=STHE(STH_BAND_E)
	  QUBE(QUB_HAB_E)=STHE(STH_HAB_E)
	  QUBE(QUB_HAI_E)=STHE(STH_HAI_E)
	  QUBJ(QUB_SCN_J)=STHJ(STH_SCN_J)
	  QUBJ(QUB_NIFR_J)=STHJ(STH_NIFR_J)
	  QUBJ(QUB_NPOL_J)=STHI(STH_PLN_I)
	  QUBJ(QUB_INST_J)=STHJ(STH_INST_J)
	  CALL WNGMV(QUB__L,QUB,
	1            A_B(A_J(QUA+QUA_BPT_J)+
	1            MOD(A_J(QUA+QUA_CNT_J),
	1            A_J(QUA+QUA_NLINE_J))*QUB__L)) !SAVE LINE
	  A_J(QUA+QUA_CNT_J)=A_J(QUA+QUA_CNT_J)+1 !COUNT
C
C SAVE IN FILE
C
	  IF (MOD(A_J(QUA+QUA_CNT_J),
	1       A_J(QUA+QUA_NLINE_J)).EQ.0) THEN !BUFFER FULL
	    IF (.NOT.NSCQW0(QUA,A_J(QUA+QUA_FCA_J),A_J(QUA+QUA_NLINE_J),
	1       A_B(A_J(QUA+QUA_BPT_J)))) THEN
	      CALL WNCTXT(F_TP,'Error writing/sorting Qube lines')
	      GOTO 800
	    END IF
	  END IF
C
C NEXT SET
C
	END DO

C
C SAVE LAST PART
C
	IF (MOD(A_J(QUA+QUA_CNT_J),
	1       A_J(QUA+QUA_NLINE_J)).NE.0) THEN !STILL IN BUFFER
	  IF (.NOT.NSCQW0(QUA,A_J(QUA+QUA_FCA_J),MOD(A_J(QUA+QUA_CNT_J),
	1                 A_J(QUA+QUA_NLINE_J)),
	1                 A_B(A_J(QUA+QUA_BPT_J)))) THEN
	    CALL WNCTXT(F_TP,'Error writing/sorting Qube lines')
	    GOTO 800
	  END IF
	END IF
C
C MERGE PARTS
C
	IF (.NOT.NSCQW1(QUA,A_J(QUA+QUA_FCA_J),A_J(QUA+QUA_CNT_J),
	1               A_B(A_J(QUA+QUA_BPT_J)))) GOTO 800
C
C GET TABLE AREAS
C
	IF (A_J(QUA+QUA_BPT_J).NE.0)
	1       CALL WNGFVA(A_J(QUA+QUA_NLINE_J)*QUB__L,
	1                   A_J(QUA+QUA_BPT_J)+A_OB) !FREE BUFFER SPACE
	A_J(QUA+QUA_BPT_J)=0
	A_J(QUA+QUA_NLINE_J)=0
	IF (.NOT.WNGGVA(2*LB_J*A_J(QUA+QUA_NFRQ_J)*
	1		A_J(QUA+QUA_NBLK_J),J)) GOTO 10
	A_J(QUA+QUA_IBPT_J)=(J-A_OB)/LB_J	!FIELD DESCRIPTOR AREA
	IF (.NOT.WNGGVA(LB_I*A_J(QUA+QUA_NIFR_J),J)) GOTO 10
	A_J(QUA+QUA_PIFR_J)=(J-A_OB)/LB_I
	IF (.NOT.WNGGVA(LB_E*(IFE__H-IFE__L+1)*
	1		A_J(QUA+QUA_NIFR_J),J)) GOTO 10
	A_J(QUA+QUA_PANG_J)=(J-A_OB)/LB_E
	IF (.NOT.WNGGVA(LB_D*A_J(QUA+QUA_NFRQ_J),J)) GOTO 10
	A_J(QUA+QUA_PFRQ_J)=(J-A_OB)/LB_D
	IF (.NOT.WNGGVA(LB_E*A_J(QUA+QUA_NHA_J),J)) GOTO 10
	A_J(QUA+QUA_PHA_J)=(J-A_OB)/LB_E
	A_J(QUA+QUA_SCNT_J)=MAX(10,A_J(QUA+QUA_MEMSZ_J)/
	1		(4*LB_E*STHIFR)) !SORT BUF LENGTH
	IF (.NOT.WNGGVA(4*LB_E*STHIFR*
	1			A_J(QUA+QUA_SCNT_J),
	1		A_J(QUA+QUA_SBPT_J))) GOTO 10 !SORT BUFFER
	A_J(QUA+QUA_SBPT_J)=A_J(QUA+QUA_SBPT_J)-A_OB !BYTE OFFSET
	IF (.NOT.WNGGVA(LB_J*MAX(A_J(QUA+QUA_NHA_J),
	1			A_J(QUA+QUA_NFRQ_J)),
	1		A_J(QUA+QUA_CPMAP_J))) GOTO 10 !ITF/IFT PTR TABLE
	A_J(QUA+QUA_CPMAP_J)=(A_J(QUA+QUA_CPMAP_J)-A_OB)/LB_J
	IF (.NOT.WNGGVA(LB_J*MAX(A_J(QUA+QUA_NHA_J),
	1			A_J(QUA+QUA_NFRQ_J)),
	1		A_J(QUA+QUA_CIPMAP_J))) GOTO 10
	A_J(QUA+QUA_CIPMAP_J)=(A_J(QUA+QUA_CIPMAP_J)-A_OB)/LB_J
	IF (.NOT.WNGGVA(STH__L,A_J(QUA+QUA_CSTH_J))) GOTO 10 !CURRENT SECTOR
	A_J(QUA+QUA_CSTH_J)=A_J(QUA+QUA_CSTH_J)-A_OB
C
C GET SORTED DATA FILE
C
	LFNM=WNFFNM('001','TMP')
	IF (.NOT.WNFOP(A_J(QUA+QUA_SFCA_J),LFNM,'WT'))
	1		GOTO 20			!GET SORTED DAT FILE
C
C GET OUTPUT DATA FILE
C
	LFNM=WNFFNM('002','TMP')
	IF (.NOT.WNFOP(A_J(QUA+QUA_IFCA_J),LFNM,'WT'))
	1		GOTO 20			!GET SORTED DAT FILE
C
C RETURN INFO
C
	INFO(QINFO_FLD) =A_J(QUA+QUA_NFLD_J)    !RETURN INFO
	INFO(QINFO_F)   =A_J(QUA+QUA_NFRQ_J)
	INFO(QINFO_T)   =A_J(QUA+QUA_NHA_J)
	INFO(QINFO_I)   =A_J(QUA+QUA_NIFR_J)
C
	RETURN
C
C ERRORS
C
 800	CONTINUE
	NSCQOP=.FALSE.				!ERROR
	INFO(QINFO_FLD)=0                       !GIVE CORRECT INFO
	INFO(QINFO_F)  =0
	INFO(QINFO_T)  =0
	INFO(QINFO_I)  =0
	GOTO 801
C
C NSCQCL
C
	ENTRY NSCQCL(QUA,FCA,SETS)
C
	NSCQCL=.TRUE.				!ASSUME OK
	CALL NSCQWF(QUA,FCA)			!FORCE ERRORS OUT
	GOTO 801
C
 801	CONTINUE
	IF (QUA.NE.0) THEN
	  IF (A_J(QUA+QUA_BPT_J).NE.0)
	1       CALL WNGFVA(A_J(QUA+QUA_NLINE_J)*QUB__L,
	1                   A_J(QUA+QUA_BPT_J)+A_OB) !FREE BUFFER SPACE
	  IF (A_J(QUA+QUA_SBPT_J).NE.0)
	1	CALL WNGFVA(4*LB_E*STHIFR*
	1			A_J(QUA+QUA_SCNT_J),
	1			A_J(QUA+QUA_SBPT_J)+A_OB) !FREE SORT BUF
	  IF (A_J(QUA+QUA_CPMAP_J).NE.0)
	1	CALL WNGFVA(LB_J*MAX(A_J(QUA+QUA_NFRQ_J),
	1				A_J(QUA+QUA_NHA_J)),
	1			LB_J*A_J(QUA+QUA_CPMAP_J)+A_OB) !ITF/IFT TABLE
	  IF (A_J(QUA+QUA_CIPMAP_J).NE.0)
	1	CALL WNGFVA(LB_J*MAX(A_J(QUA+QUA_NFRQ_J),
	1				A_J(QUA+QUA_NHA_J)),
	1			LB_J*A_J(QUA+QUA_CIPMAP_J)+A_OB) !ITF/IFT TABLE
	  IF (A_J(QUA+QUA_CSTH_J).NE.0)
	1	CALL WNGFVA(STH__L,A_J(QUA+QUA_CSTH_J)+A_OB) !CURRENT SECTOR
	  IF (A_J(QUA+QUA_IBPT_J).NE.0)
	1	CALL WNGFVA(2*LB_J*A_J(QUA+QUA_NFRQ_J)*
	1		A_J(QUA+QUA_NBLK_J),
	1		A_J(QUA+QUA_IBPT_J)*LB_J+A_OB) !FREE FIELD DESCRIPTOR
	  IF (A_J(QUA+QUA_PIFR_J).NE.0)
	1	CALL WNGFVA(LB_I*A_J(QUA+QUA_NIFR_J),
	1		A_J(QUA+QUA_PIFR_J)*LB_I+A_OB) !FREE TABLES
	  IF (A_J(QUA+QUA_PANG_J).NE.0)
	1	CALL WNGFVA(LB_E*(IFE__H-IFE__L+1)*A_J(QUA+QUA_NIFR_J),
	1		A_J(QUA+QUA_PANG_J)*LB_E+A_OB)
	  IF (A_J(QUA+QUA_PFRQ_J).NE.0)
	1	CALL WNGFVA(LB_D*A_J(QUA+QUA_NFRQ_J),
	1		A_J(QUA+QUA_PFRQ_J)*LB_D+A_OB)
	  IF (A_J(QUA+QUA_PHA_J).NE.0)
	1	CALL WNGFVA(LB_E*A_J(QUA+QUA_NHA_J),
	1		A_J(QUA+QUA_PHA_J)*LB_E+A_OB)
	  IF (A_J(QUA+QUA_PWGT_J).NE.0)		!FREE DATA AREAS
	1	CALL WNGFVA(4*LB_E*A_J(QUA+QUA_NDAT_J),
	1		A_J(QUA+QUA_PWGT_J)*LB_E+A_OB)
	  IF (A_J(QUA+QUA_PDAT_J).NE.0)
	1	CALL WNGFVA(4*LB_X*A_J(QUA+QUA_NDAT_J),
	1		A_J(QUA+QUA_PDAT_J)*LB_X+A_OB)
	  IF (A_J(QUA+QUA_PMOD_J).NE.0)
	1	CALL WNGFVA(4*LB_X*A_J(QUA+QUA_NDAT_J),
	1		A_J(QUA+QUA_PMOD_J)*LB_X+A_OB)
	  IF (A_J(QUA+QUA_POUT_J).NE.0)
	1	CALL WNGFVA(4*LB_X*A_J(QUA+QUA_NDAT_J),
	1		A_J(QUA+QUA_POUT_J)*LB_X+A_OB)
	  CALL WNFCL(A_J(QUA+QUA_FCA_J))	!CLOSE TMP FILE
	  CALL WNFCL(A_J(QUA+QUA_SFCA_J))	!CLOSE SORTED FILE
	  CALL WNFCL(A_J(QUA+QUA_IFCA_J))	!CLOSE OUTPUT FILE
	  CALL WNGFVA(QUA__L,QUA*LB_J+A_OB)     !FREE CONTROL AREA
	  QUA=0
	END IF
	CALL WNDSTR(FCA,SETS)			!RESET SET SEARCH
C
	RETURN
C
C
	END