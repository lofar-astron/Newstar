C+ NCOCPB.FOR
C  JPH 930202
C
C  Revisions:
C	JPH 930331	Correct indexing of CHKJ
C	JPH 930517	Optional suppression of CHKLST update
C	JPH 930610	Negative IDATP option
C       AXC 010709	Linux port - TABS
C
C
        LOGICAL FUNCTION 
        1        NCOCPB(FCAIN,FCAOUT,LEN,IDATP,BUF,ODATP,CHK,COPIED)
C
C Make unique copy of input block to output file
C
C  Result:
C
C        NCOCPB_L=
C                NCOCPB( FCAIN_J:I, FCAOUT_J:I, LEN_J:I, IDATP_J:I,
C                        BUF_B:[I]O, ODATP_J:O,
C                        CHKJ[2,0:*]:IO, COPIED_L:O )
C        Copy data block of LEN bytes at file address IDATP in file FCAIN
C to file FCAOUT, using buffer BUF in which the blocks contents are
C also returned to caller. The output file address is returned in
C ODATP.
C        To copy only once, IDATP/ODATP pairs are collected in a list CHK,
C against which a check is made before a copy is performed.
C        COPIED indicates whether a copy was actually made.
C        Caller is responsible for:
C                - allocating CHK(2,0:n) and initialising
C        its first element with the value -n, where n is >= the number of
C        different blocks to be copied;
C                - making BUF >= LEN bytes
C        Special cases:
C - CHKLST(0,0)=0 (or an argument 0 submitted in lieu of CHKLST): Do not
C   record this copy, - to be used when caller is certain that a block being 
C   copied will not be presented again.
C - IDATP <0: Input from file address -IDATP is already in BUF, do not read it
C   again
C
C
C  Include files:
C
        INCLUDE 'WNG_DEF'
C
C  Parameters:
C
C  Arguments:
C
        INTEGER                FCAIN, FCAOUT                !file pointers
        INTEGER                LEN                        !data block length
        INTEGER                IDATP,ODATP                !file addresses
        BYTE                BUF(0:*)                !data block
        INTEGER                CHK(2,0:*)                !ckeck list - max and actual
                                                ! lengths are maintained in
                                                ! row 0
        LOGICAL                COPIED                        !"copied" status
C
C  Function references:
        LOGICAL                WNFRD,WNFWR                !buffered read, write
        INTEGER                WNFEOF                        !find EOF file address
C
C  Data declarations:
C
        INTEGER                MXNCHK, NCHK                !max and actual CHK lengths
C-
C  Initialise
C
        NCOCPB=.FALSE.                                !preset failure
C
C  Initialise check list: 
C
        IF (CHK(1,0).NE.0) THEN                        !valid check list?
          IF (CHK(1,0).LT.0) THEN                !yes, first call?
            CHK(1,0)=-CHK(1,0)                        !yes, initialise max. and
            CHK(2,0)=0                                ! actual list lengths
          ENDIF
          NCHK=CHK(2,0)                                !copy to local
        ELSE                                        !no check list
          NCHK=0                                !set nothing to check
        ENDIF
        MXNCHK=CHK(1,0)
C
C test if already done
C
        DO I=1,NCHK
          IF (ABS(IDATP).EQ.CHK(1,I)) THEN        !this IDATP?
            ODATP=CHK(2,I)                        !yes, return corresp. ODATP
            COPIED=.FALSE.                        !report not copied
            GOTO 800                                ! and exit
          ENDIF
        ENDDO
C
C not done, copy block, update checklist
C
        IF (IDATP.GT.0) THEN
          IF (.NOT.WNFRD
        1        (FCAIN,LEN,BUF,IDATP)) THEN        !read
            CALL WNCTXT(F_TP,'Error reading data')
            GOTO 900
          ENDIF
        ENDIF
        ODATP=WNFEOF(FCAOUT)                        !get output file address
        IF (.NOT.WNFWR(FCAOUT,LEN,BUF,ODATP)) THEN        !write
          CALL WNCTXT(F_TP,'Error writing data')
          GOTO 900
        ENDIF
        COPIED=.TRUE.                                !report block was copied
C
        IF (MXNCHK.NE.0) THEN                        !valid check list?
          NCHK=NCHK+1                                !new entry
          IF (NCHK.GT.MXNCHK) THEN
            CALL WNCTXT(F_TP,'NCOCPB checklist overflow')
            CALL WNGEX
          ENDIF
          CHK(2,0)=NCHK                                !new list length
          CHK(1,NCHK)=ABS(IDATP)                !entry: in- and corresponding
          CHK(2,NCHK)=ODATP                        ! output file address
        ENDIF
C
 800        NCOCPB=.TRUE.                                !success
 900        RETURN
C
        END
