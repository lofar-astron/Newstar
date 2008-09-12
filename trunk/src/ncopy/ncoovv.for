C+ NCOOVV.FOR
C  JPH 930519
C
C  Revisions:
C        JPH 931006        Fieldwidth 6 --> 7 for VOLGNR printout
C        JPH 931007        NSCSTH --> NSCSTG
C        JPH 941006        NSC_DEF --> NCO_DEF
C        AXC 010709        Linux Port - TABS
C
        SUBROUTINE NCOOVV
C
C  Print overview of sectors in SCN file
C
C  Result:
C
C        CALL NCOOVV
C
C  Include files:
C
        INCLUDE 'WNG_DEF'
        INCLUDE 'NCO_DEF'
        INCLUDE 'GFH_O_DEF'                !GENERAL FILE HEADER
        INCLUDE 'SGH_O_DEF'                !SUB-GROUP HEADER
        INCLUDE 'STH_O_DEF'                !SET HEADER
        INCLUDE 'SCH_O_DEF'                !SCAN HEADER
C
C  Parameters:
        INTEGER                PWIDTH, TWIDTH        !width of ASCII output lines
        PARAMETER        (PWIDTH=131, TWIDTH=78)        
                                        !allow for WNCTXT's 1-wide left margin
C
C
C  Arguments:
C
C
C  Function references:
C
        LOGICAL        WNDNOD
        LOGICAL        WNFOP
        LOGICAL WNFRD                        !READ DATA
        LOGICAL WNFWR                        !WRITE DATA
        INTEGER WNFEOF                        !GET FILE POINTER
        CHARACTER*32 WNTTSG                !PRINT SET NAME
        LOGICAL NSCSTG                        !GET A SET WITH VERSION CHECK
        LOGICAL NSCSCH                        !READ SCAN HEADER
        LOGICAL NSCSCW                        !WRITE SCAN HEADER
C
C  Data declarations:
C
        INTEGER                SET(0:7,0:1)        !ALL SETS
        INTEGER                STHP                !SET HEADER POINTER
        INTEGER         SNAM(0:7)        !SET NAME
        INTEGER                PSNAM(0:7)        !previous sector name
        BYTE                 STH(0:STHHDL-1)        !SET HEADER
          INTEGER*2          STHI(0:STHHDL/2-1)
          INTEGER          STHJ(0:STHHDL/4-1)
          REAL                  STHE(0:STHHDL/4-1)
          DOUBLE PRECISION STHD(0:STHHDL/8-1)
          EQUIVALENCE         (STH,STHI,STHJ,STHE,STHD)
        CHARACTER*19        CSNAM                !ASCII sector name
        REAL                HAE                !end HA
C-
C
C*****************************************************************************
C GET NODE 
C
 100        CONTINUE
        IF (.NOT.WNDNOD('INPUT_SCN_NODE',' ',
	1	'SCN','R',NODIN,IFILE)) THEN !NODE
C*******
          IF (E_C.EQ.DWC_ENDOFLOOP) RETURN        !READY WITH SHOW
          CALL WNCTXT(F_TP,'Node does not exist')
          GOTO 100
        ELSE IF (E_C.EQ.DWC_NULLVALUE) THEN
          RETURN                                !END
        ELSE IF (E_C.EQ.DWC_WILDCARD) THEN
          GOTO 100                                !MUST SPECIFY
        END IF
        IF (.NOT.WNFOP(FCAIN,IFILE,'R')) THEN        !OPEN SCN FILE
          CALL WNCTXT(F_TP,'Cannot open file attached to node')
          GOTO 100
        END IF
        CALL NSCPFH(F_TP,FCAIN)                        !PRINT FILE HEADER
C
C INIT
C
        DO I=0,7
          PSNAM(I)=-1
          DO I1=0,1
            SET(I,I1)=0
          END DO
        END DO
        SET(0,0)=1                                !1 LINE
        DO I=0,7
          SET(I,1)=-1                                !*
        END DO
C
C Output header
C
        CALL WNCFSV(F_P,F_LL,PWIDTH)                !set record length
        CALL WNCFSV(F_T,F_LL,TWIDTH)
        CALL WNCTXT (F_TP, 'Size !UJ bytes !/',
        1        WNFEOF(FCAIN))                        !report file size
        CALL WNCTXT (F_TP,                        !print heading
        1        '!-19$AS !3$AS'//                !SNAM, SETN
        1        ' !-7$AS'//                        !field
        1        ' !-7$AS'//                        !volgnr
        1        ' !-7$AS !-7$AS'//                !FREQ, BAND
        1        ' !-5$AS !-5$AS'//                !HAB, HAE
        1        ' !-4$AS'//                        !SCN
        1        ' !-4$AS'//                        !NIFR,PLN
        1        '    !-8$AS'//                        !MDL
        1        ' !-30AS'//
        1        '!/',
        1        'Sector','(#)',
        1        'Field',
        1        'Volgnr',
        1        'FREQ', 'BAND',
        1        'HAB','HAE',
        1        'SCNS',
        1        'IF P',
        1        'MDL',
        1        'STHP     FDP      OHP      SHP' )
C
C Loop over all sectors
C
        DO WHILE (NSCSTG(FCAIN,SET,STH,STHP,SNAM))!all sets
          CSNAM=WNTTSG(SNAM,3)                        !get "." set name
          DO I=0,7
            IF (SNAM(I).NE.PSNAM(I)) GOTO 10        !compare against previous
          ENDDO
 10          CONTINUE
          IF (I.GT.0) CSNAM(1:4*I)=' '                !blank out components that have
C                                                ! not changed
          DO I=0,7
            PSNAM(I)=SNAM(I)
          ENDDO
          HAE= STHE(STH_HAB_E) + (STHJ(STH_SCN_J)-1)*STHE(STH_HAI_E)
          CALL WNCTXT (F_TP,
        1        '!19$AS !3$UJ3'//                !SNAM, SETN
        1        ' !-7$AD'//                        !field
        1        ' !7$UJ7'//                        !volgnr
        1        ' !7$D7.2 !7$E7.3'//                !FREQ, BAND
        1        ' !5$EAF5.1 !5$EAF5.1'//        !HAB, HAE
        1        ' !4$UJ4'//                        !SCN
        1        ' !2$UJ2 !1$UI1'//                !NIFR,PLN
        1        '    !8$XJ8'//                        !MDL
        1        ' !8$XJ8 !8$XJ8 !8$XJ8 !8$XJ8',        !STHP,FDP,OHP,SHP
        1        CSNAM,STHJ(STH_SETN_J),
        1        STH(STH_FIELD_1),STH_FIELD_N,
        1        STHJ(STH_VNR_J),
        1        STHD(STH_FRQ_D),STHE(STH_BAND_E),
        1        STHE(STH_HAB_E),HAE,
        1        STHJ(STH_SCN_J),
        1        STHJ(STH_NIFR_J),STHI(STH_PLN_I),
        1        STHJ(STH_MDL_J),
        1        STHP,STHJ(STH_FDP_J),STHJ(STH_OHP_J),STHJ(STH_SHP_J) )
        END DO
C
C READY
C
 800        CALL WNCTXT (F_TP, '!/!/!/')
        CALL WNFCL(FCAIN)                                !CLOSE FILE
C
        RETURN
C
C ERROR
C
 900        CONTINUE
        GOTO 800
C
C
        END
