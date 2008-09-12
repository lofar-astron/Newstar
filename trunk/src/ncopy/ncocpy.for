C+ NCOCPY.FOR
C  JPH 930119
C
C  Revisions:
C        JPH 930329        integrate NSCREG code for OUTPUT_SCTS
C        JPH 930426        replace STH(,IN:OUT) arrays by ISTH, OSTH etc.
C        JPH 930514        restructure with internal subroutines
C                        all-wildcard output sets means new group
C        JPH 930525        check if sector already copied
C        JPH 930719        output sector numbering
C        JPH 930720        proper addressing of polarisations in model
C        JPH 930824        Simplify for 1always 4-polarisation model
C        JPH 931007        OSCH eqv ISCH. - Zero Y noises for 1 output polsn
C        JPH 931018        Add model input for SIM option. - Use DAT i.s.o. XMOD
C        JPH 931109        Remove simulation
C        JPH 940107        Use NSCSCF/NSCSFW i.s.o. NSCSCR/NSCSDW to also copy 
C                        flags
C        JPH 940208        Do not record STH addresses in checklist, Reinitialise
C                         checklist for every new input group. Extend checklist.
C        JPH 940218        FLW I*2 --> I*4
C        CMV 940228        Correct call to NSCPFL
C        CMV 940518        Select copy of Model and IF-data
C        JPH 941006        Remove c#print, c#error lines
C        JPH 960220        Interferometer selection for ifr table, visibilities.
C        JPH 960305        Also for model, - no selection is needed for the IF data
C        JPH 960617        Correct order of actions. Bug fix: Remove comment C from
C                         OIFR=0.
C        JPH 960624        Zero SCH_IFRA/MC
C        JPH 960725        Add YX mode (ONPOL=-2)
C        JPH 961213        SHORTCOPY option: SCANS selection
C        AXC 010709        Linux port - TABS
C	 CMV 040629	   Allow to copy autocorrelations
C	 CMV 041102	   Corrected (severe and stupid) bug in copy of autos
C
C
        SUBROUTINE NCOCPY
C
C  Create new SCN file
C
C  Result:
C
C        CALL NCOCPY
C                Copies selected sets from input to output file
C                Input and output files have been opened by NSCNOD,
C                which has also initialised a new output file
C                with a GFH block
C
C  Include files:
C
        INCLUDE 'WNG_DEF'
        INCLUDE 'FDW_O_DEF'
        INCLUDE 'FDX_O_DEF'
        INCLUDE 'OHW_O_DEF'
        INCLUDE 'SCW_O_DEF'
        INCLUDE 'SHW_O_DEF'
        INCLUDE 'GFH_O_DEF'                        !group file header
        INCLUDE 'SGH_O_DEF'                        !subgroup header
        INCLUDE 'MDH_O_DEF'
        INCLUDE 'MDL_O_DEF'
        INCLUDE 'IFH_O_DEF'
        INCLUDE 'STH_O_DEF'                        !SET HEADER
        INCLUDE 'SCH_O_DEF'                        !SCAN HEADER
        INCLUDE 'NCO_DEF'
        INCLUDE 'SCN_DEF'
C
C  Parameters:
        INTEGER                C,S, XX,XY,YX,YY
        PARAMETER        (C=0,S=1, XX=0,XY=1,YX=2,YY=3)
C Estimate of checklist size: 128-field mosaic, 8 frequencies, 18 sectors per
C  frequency: 128 OH, 128*8 SH, 128*8 MDH give 2176
        INTEGER                CHKLEN
        PARAMETER        (CHKLEN=4096)
C
C  Arguments:
C
C
C  Function references:
C
        INTEGER                WNFEOF                        !FILE LENGTH
        LOGICAL                WNFWR                        !WRITE DISK
        LOGICAL                WNFRD                        !READ DISK
        LOGICAL                WNDLNF,WNDLNG,WNDLNK        !LINK sectors
        CHARACTER*32        WNTTSG                        !sector NAME
        LOGICAL                NCOCPB                        !make unique copy of block
        LOGICAL                NSCSIF                        !READ IFR DATA
        LOGICAL                NSCSCM                        !read model data
        LOGICAL                NSCSCR, NSCSCF                !READ SCANls *.for
        LOGICAL                NSCSDW, NSCSFW                !write scan
        LOGICAL                NSCSTG                        !GET SET
C
C  Data declarations:
C
        LOGICAL                COPIED                        !"data copied" flag from NCOCPB
C
        INTEGER                LEN, ILEN, OLEN                !data block lengths
        INTEGER                SCNFST, SCNLST                !scan # range within HA range
        INTEGER                ONPOLC                        !# of polsn in current output
        INTEGER                ISCN,OSCN                !scan loop index
        INTEGER                OPOL                        !output polsn index
        INTEGER                INIFR                        !# of input IFRs
        INTEGER                ONIFR                        !# of output IFRs
        INTEGER                IIFR                        !input IFR index
        INTEGER                OIFR                        !output IFR index
        INTEGER*2        IIFRT(0:STHIFR-1)        !buffer for input IFR TABLE
        INTEGER*2        OIFRT(0:STHIFR-1)        !buffer for output IFR TABLE
          EQUIVALENCE        (IIFRT,OIFRT)                !build OIFRT in place
C
        REAL                IWGT(0:STHIFR-1,XX:YY),
        1                OWGT(0:STHIFR-1,XX:YY)        !weights
        INTEGER                IFLW(0:STHIFR-1,XX:YY),
        1                OFLW(0:STHIFR-1,XX:YY)        !flag/weight words        
        REAL                IDAT(C:S,0:STHIFR-1,XX:YY),
        1                ODAT(C:S,0:STHIFR-1,XX:YY) !data (cos/sin, ifr, polsn)
         INTEGER         IIFRA (0:1, 0:STHIFR-1)
         REAL                 ANG (0:2, 0:STHIFR-1)
        REAL            BASEL (0:STHIFR-1)          ! BASELINE TABLE: -1 indicates
                                                !  ifr absent
        
        INTEGER                IPOL                        !polarisation index in WGT, DAT
C
        COMPLEX                ICMOD(XX:YY,0:STHIFR-1)        !MODEL DATA, 4 polsns
        COMPLEX                OCMOD(XX:YY,0:STHIFR-1)
C
        INTEGER                ISRC, IFR, IPL                !source, ifr, polsn loop indices
        INTEGER                INP                        !input pointer
        INTEGER                OUTP                        !output pointer
        INTEGER                MDHP                        !model hdr ptr offset in STH
        INTEGER                MDDP                        !model data ptr offset in STH
C
        INTEGER                ISNAM(0:7)                !current input set name
        INTEGER                OSNAM(0:7)                !current output set name
        INTEGER                LVL                        !SGH level: index for xxSNAM
        INTEGER                ISTHP,OSTHP                !SEctor HEADER POINTER
        INTEGER                OMODP                        !output model ptr
        INTEGER                ICURGRP                        !input current group
C


        BYTE                        ISTH(0:STHHDL-1)!input sector header
          INTEGER*2                ISTHI(0:STHHDL/2-1)
          INTEGER                ISTHJ(0:STHHDL/4-1)
          REAL                        ISTHE(0:STHHDL/4-1)
          DOUBLE PRECISION        ISTHD(0:STHHDL/8-1)
          EQUIVALENCE                (ISTH,ISTHI,ISTHJ,ISTHE,ISTHD)
C
        BYTE                        OSTH(0:STHHDL-1)!output sector header
          INTEGER*2                OSTHI(0:STHHDL/2-1)
          INTEGER                OSTHJ(0:STHHDL/4-1)
          REAL                        OSTHE(0:STHHDL/4-1)
          DOUBLE PRECISION        OSTHD(0:STHHDL/8-1)
          EQUIVALENCE                (OSTH,OSTHI,OSTHJ,OSTHE,OSTHD)
C
        BYTE                        ISCH(0:SCHHDL-1)!input SCAN HEADER
          INTEGER*2                ISCHI(0:SCHHDL/2-1)
          INTEGER                ISCHJ(0:SCHHDL/4-1)
          REAL                        ISCHE(0:SCHHDL/4-1)
          DOUBLE PRECISION        ISCHD(0:SCHHDL/8-1)
          EQUIVALENCE                (ISCH,ISCHI,ISCHJ,ISCHE,ISCHD)
C
        BYTE                        OSCH(0:SCHHDL-1)!output SCAN HEADER
          INTEGER*2                OSCHI(0:SCHHDL/2-1)
          INTEGER                OSCHJ(0:SCHHDL/4-1)
          REAL                        OSCHE(0:SCHHDL/4-1)
          DOUBLE PRECISION        OSCHD(0:SCHHDL/8-1)
          EQUIVALENCE                (OSCH,OSCHI,OSCHJ,OSCHE,OSCHD)
C
        EQUIVALENCE                (OSCH,ISCH)
C
        INTEGER                        CHKLST(2,0:CHKLEN)!checklist for NCOCPB
C
        BYTE                 IFH(0:MDHHDL-1)                !IF HEADER
          INTEGER*2         IFHI(0:IFHHDL/2-1)
          INTEGER         IFHJ(0:IFHHDL/4-1)
          REAL                 IFHE(0:IFHHDL/4-1)
          EQUIVALENCE         (IFH,IFHI,IFHJ,IFHE)
C
        INTEGER                GFHJ(0:GFHHDL/4-1)        !group file header
        BYTE                MDH(0:MDHHDL-1)         !MODEL HEADER
          INTEGER        MDHJ(0:MDHHDL/4-1)
          EQUIVALENCE        (MDH,MDHJ)
        BYTE                FDW(FDWHDL+FDXHDL)!WSRT TAPE BLOCKS
          BYTE                OHW(OHWHDL)
          BYTE                SCW(SCWHDL)
          BYTE                SHW(SHWHDL)
          INTEGER*2        BUF(0:2,XX:YY,0:STHIFR-1)!work buffer for NCOCPB; also
                                                  ! used for copying IF data
        EQUIVALENCE        (GFHJ,MDH,FDW,OHW,SCW,SHW,BUF) ! length is largest of
                                                ! these blocks: OHWHDL=25380
C
        REAL                        MAXD                !maximum data value
        REAL                        HA                !current hour angle
C-
C
C INIT
C
        ICURGRP=-1
C
C INPUT SET LOOP
C
        DO WHILE (NSCSTG(FCAIN,INSCTS(0,0),
        1        ISTH(0),ISTHP,ISNAM(0)))        !Read next STH
          CALL WNDSTI(FCAIN,ISNAM)                !get "." input name
           INIFR=ISTHJ(STH_NIFR_J)                !# of input ifrs
C
C Check which scans are within HA range
C
          SCNFST=-( (-HARAN(0)+ISTHE(STH_HAB_E))
        1        /ISTHE(STH_HAI_E) )                !start of range: round up
          SCNLST=   ( HARAN(1)-ISTHE(STH_HAB_E))
        1        /ISTHE(STH_HAI_E)                !end of range: round down
          SCNFST=MAX(SCNFST,SCANS(0))                !acknowledge SHORTCOPY SCANS
          SCNLST=MIN(SCNLST,SCANS(1))                ! specification           
          IF (SCNFST.LT.0) SCNFST=0
          IF (SCNLST.GE.ISTHJ(STH_SCN_J))
        1        SCNLST=ISTHJ(STH_SCN_J)-1
          IF (SCNFST.LE.SCNLST) THEN                !any scan within HA range?
C
C Check for enough input polarisations
C
            INPOL=ISTHI(STH_PLN_I)                !yes, get # of input polsns
            IF (ABS(ONPOL).GT.INPOL) THEN                !enough input polsns?
              CALL WNCTXT(F_TP,
        1        'Can only copy !UJ polarisations in input sector !AS',
        1        INPOL,WNTTSG(ISNAM,0))
            ENDIF
            ONPOLC=MIN(INPOL,ABS(ONPOL))
C
C******************************************************************************
C Copy sector with everything attached
C******************************************************************************
C
C Make output sector name
C
            DO LVL=SCN_OBS,SCN_CHN                !obs, field, channel 
              OSNAM(LVL)=ISNAM(LVL)                !copy input component
            END DO
            IF (ISNAM(SCN_GRP).NE.ICURGRP) THEN        !new input group?
              ICURGRP=ISNAM(SCN_GRP)
              IF (.NOT.WNFRD(FCAOUT,GFHHDL,GFHJ,0))!yes, read GFH
        1        GOTO 990
              OSNAM(SCN_GRP)=GFHJ(GFH_LLEN_J)        !first free group nr
              CALL WNCTXT(F_TP,
        1        'Copying input group !UJ to output group !UJ',
        1        ISNAM(SCN_GRP),OSNAM(SCN_GRP))
C
              CHKLST(1,0)=-CHKLEN                !re-initialise checklist
            ENDIF
C
C Make a preliminary disk copy of the STH just to check if it is new and
C  to get it entered in CHKLST
C
            OSTHP=WNFEOF(FCAOUT)
            IF (.NOT.WNFWR(FCAOUT,STHHDL,ISTH(0),OSTHP) )
        1        THEN
              GOTO 990
            ENDIF
C
C Set up output STH and adjust scan parameters in it
C
            CALL WNGMV
        1        (STHHDL,ISTH(0),OSTH(0))        !copy STH block
            OSTHE(STH_HAB_E)=
        1        ISTHE(STH_HAB_E) +SCNFST*ISTHE(STH_HAI_E)
            OSTHJ(STH_SCN_J)=SCNLST-SCNFST+1
            OSTHI(STH_PLN_I)=ONPOLC
C
C Clear noise entries for Y that become invalid if only XX present
C
            IF (ONPOLC.EQ.1) THEN
              OSTHE(STH_REDNS_E+2)=0
              OSTHE(STH_REDNS_E+3)=0
              OSTHE(STH_ALGNS_E+2)=0
              OSTHE(STH_ALGNS_E+3)=0
              OSTHE(STH_OTHNS_E+2)=0
              OSTHE(STH_OTHNS_E+3)=0
            ENDIF
C
C Read input, build output IFR table AND WRITE IT
C
            IF (.NOT.WNFRD(FCAIN,LB_I*INIFR,        !read input IFR table
        1                IIFRT,ISTHJ(STH_IFRP_J))) THEN
              CALL WNCTXT
        1        ('Error reading interferometer table for set !AS, 
        1                WNTTSG(ISNAM(0))')
              GOTO 990
            END IF
             CALL NSCMBL(ISTHE(STH_RTP_E),INIFR,IIFRT,
        1                        SIFRS,BASEL)        ! MAKE BASEL.
            OIFR=0
            DO IIFR=0,INIFR-1
                IF (BASEL(IIFR).GE.0) THEN
                OIFRT(OIFR)=IIFRT(IIFR)
                OIFR=OIFR+1
              ENDIF
            ENDDO
             ONIFR=OIFR
            OSTHJ(STH_NIFR_J)=ONIFR
            OSTHJ(STH_SCNL_J)=SCHHDL+3*LB_I*ONIFR*ONPOLC
            OUTP=WNFEOF(FCAOUT)
            IF (.NOT.WNFWR(FCAOUT,LB_I*ONIFR,
        1             OIFRT,OUTP)) THEN                !write output IFR table
              GOTO 990
            ENDIF
            OSTHJ(STH_IFRP_J)=OUTP                !set pointer in STH
C
C Copy WSRT header blocks
C
            GOTO 2000
2001            CONTINUE
C
C Copy scans
C
            OUTP=WNFEOF(FCAOUT)
            OSTHJ(STH_SCNP_J)=OUTP                !set scans pointer in STH
            GOTO 2020
 2021            CONTINUE
C
C Copy model
C
            IF (DO_MDL) THEN                        !Copy model
              GOTO 2010
 2011              CONTINUE
            ELSE                                !No model
              OSTHJ(STH_MDL_J  )=0                !Clear pointers
              OSTHJ(STH_MDL_J+1)=0
              OSTHJ(STH_MDD_J  )=0
              OSTHJ(STH_MDD_J+1)=0
            END IF
C
C Copy IF data
C
            IF (DO_IFH.AND.ISTHJ(STH_IFHP_J).NE.0) THEN !Copy IF data
              GOTO 2030
 2031              CONTINUE
            ELSE
              OSTHJ(STH_IFHP_J)=0
              OSTHJ(STH_IFHL_J)=0
            END IF
C
C** manipulate STH corrections here
C
C Rewrite output STH
C
            IF (.NOT.WNFWR(FCAOUT,STHHDL,
        1                OSTH(0),OSTHP)) THEN        !rewrite output STH
              CALL WNCTXT(F_TP,
        1        'Error writing sector header !AS', WNTTSG(OSNAM))
              GOTO 990
            ENDIF

C******************************************************************************
C Link output sector
C******************************************************************************
C
            IF (.NOT.WNDLNK(0+GFH_LINK_1,        !link sector in absolute list
        1            OSTHP,STH_SETN_1,FCAOUT)) THEN 
              GOTO 990
            ENDIF
C
            IF (.NOT.WNDLNF(0+GFH_LHD_1,
        1        OSNAM(SCN_GRP),SGH_NAME_1,
        1        FCAOUT,SGPH(SCN_GRP),
        1        SGNR(SCN_GRP))) THEN                !find/create group SGH
C
            ENDIF
            DO LVL=SCN_OBS,SCN_CHN                !find/create obsvn, field, 
C                                                ! channel SGHs
              IF (.NOT.WNDLNF
        1        (SGPH(LVL-1)+SGH_LHD_1,
        1        OSNAM(LVL),
        1        SGH_NAME_1,FCAOUT,SGPH(LVL),
        1        SGNR(LVL))) THEN
                GOTO 990
              ENDIF
            ENDDO
            IF (.NOT.WNDLNG                        !create sector SGH and
        1        (SGPH(SCN_CHN)+SGH_LHD_1,OSTHP,        ! link STH to it
        1        SGH_NAME_1,FCAOUT,SGPH(SCN_SCT),
        1        SGNR(SCN_SCT))) THEN
              GOTO 990
            ENDIF
          ENDIF        ! any scan within HA range"
 800          CONTINUE
        ENDDO !sector loop
C
C READY
C
 900        CONTINUE
        CALL NSCPFH (F_TP, FCAOUT)                 !show output header
        CALL NSCPFL (F_TP, FCAOUT,NODOUT,.FALSE.)! and layout
        CALL WNFCL(FCAIN)
        CALL WNFCL(FCAOUT)
C
        RETURN
C
C Fatal-error exits
C
 990        CONTINUE
        CALL WNCTXT(F_TP,
        1        'Error copying sector !AS',WNTTSG(ISNAM))
        CALL WNGEX                                !STOP
C
        RETURN

C******************************************************************************
C Internal subroutine to copy WSRT blocks
C******************************************************************************
C
 2000        CONTINUE
            IF (.NOT.NCOCPB(FCAIN,FCAOUT,        !FD plus FDX block
        1        ISTHJ(STH_NFD_J),
        1        ISTHJ(STH_FDP_J),BUF,
        1        OSTHJ(STH_FDP_J),CHKLST,
        1        COPIED)) THEN
CC              CALL WNCTXT(F_TP,'Error copying FD/FDX block')
C#ERROR 'copy FD/FDX'
              GOTO 990
            ENDIF
C
            IF (.NOT.NCOCPB(FCAIN,FCAOUT,        !OH block
        1        ISTHJ(STH_NOH_J),
        1        ISTHJ(STH_OHP_J),BUF,
        1        OSTHJ(STH_OHP_J),CHKLST,
        1        COPIED)) THEN
CC              CALL WNCTXT(F_TP,'Error copying OH block')
C#ERROR 'copy OH'
              GOTO 990
            ENDIF
C
            IF (.NOT.NCOCPB(FCAIN,FCAOUT,        !SC block
        1        ISTHJ(STH_NSC_J),
        1        ISTHJ(STH_SCP_J),BUF,
        1        OSTHJ(STH_SCP_J),CHKLST,
        1        COPIED)) THEN
              CALL WNCTXT(F_TP,'Error copying SC block')
              GOTO 990
            ENDIF
C
            IF (.NOT.NCOCPB(FCAIN,FCAOUT,        !SH block
        1        ISTHJ(STH_NSH_J),
        1        ISTHJ(STH_SHP_J),MDH,
        1        OSTHJ(STH_SHP_J),CHKLST,
        1        COPIED)) THEN
              CALL WNCTXT(F_TP,'Error copying SH block')
              GOTO 990
            ENDIF
        GOTO 2001

 2010        CONTINUE
C******************************************************************************
C Internal subroutine to copy model
C******************************************************************************
C
C Model: MDH blocks
C
        DO MDHP=STH_MDL_J,STH_MDL_J+1                !loop over MDH[0:1] in STH
          IF (ISTHJ(MDHP).NE.0) THEN 
            IF (.NOT.NCOCPB(FCAIN,FCAOUT,MDHHDL,                
        1        ISTHJ(MDHP),MDHJ,                !copy MDH block
        1        OSTHJ(MDHP),CHKLST,
        1        COPIED)) THEN
              CALL WNCTXT(F_TP,'Error copying MDH block')
C#ERROR 'copy MDH'
              GOTO 990
            ENDIF
C#PRINT 'MDH', WNFEOF(FCAOUT)
C
C Model components. If there is an MDH, COPIED tells us if it was already there
C  or copied now, in which case the components must also be copied. In that 
C  case the header was returned in MDHJ so we can read it.
C We process the model components one by one because we cannot be sure 
C  that BUF can hold them all.
C (It would be more efficient to process them at least in groups.)
C This section will be executed only when the MDH is copied for the first 
C  time, so there is no need for NCOCPB to remember the source components in 
C  CHKLST
C
            IF (COPIED) THEN
              INP=MDHJ(MDH_MODP_J)                !input file address of model
              OMODP=WNFEOF(FCAOUT)                !remember output model ptr
              DO ISRC=0,MDHJ(MDH_NSRC_J)-1        !MDH_NSRC sources
                IF (.NOT.NCOCPB(FCAIN,                !of length MDLHDL each
        1                FCAOUT,MDLHDL,INP,BUF,
        1                OUTP,0,COPIED)) THEN
                  CALL WNCTXT(F_TP,'Error copying model sources')
                  GOTO 990
                ENDIF
                INP=INP+MDLHDL                        !next source
              ENDDO                                !end source loop
C
              J=OSTHJ(MDHP)                        !output file addr. of model hdr
              IF (.NOT.WNFWR(FCAOUT,LB_J,OMODP,
        1                J+MDH_MODP_1)) THEN        !store remembered MODP in MDH
                CALL WNCTXT(F_TP,'Error copying MDH block')
C#ERROR 'update MDH'
                GOTO 990
              ENDIF
            ENDIF! COPIED
          ENDIF! model present
        ENDDO! models #1 and 2
C
C Model visibilities. Input is pointed at by STH_MDD[0:1], output is appended
C  and its address stored in STH_MDD. The range [SCNFST,SCNLST] of scans to
C  be copied was determined above
C
        DO MDDP=STH_MDD_J,STH_MDD_J+1                !model data 1 and 2
          IF (ISTHJ(MDDP).NE.0) THEN                !present?
            ILEN=4*INIFR*LB_X                        !the model data area has
                                                 ! 4 polsns regardless of 
            OLEN=4*ONIFR*LB_X                        ! what is actually needed

            INP=ISTHJ(MDDP)+SCNFST*LEN                !input file addr. of model data
                                                !for first scan processed
            OUTP=WNFEOF(FCAOUT)
            OSTHJ(STH_MDD_J)=OUTP                !pointer in output STH
            DO ISCN=SCNFST,SCNLST
              IF (.NOT.WNFRD(FCAIN,ILEN,ICMOD,INP))
        1                GOTO 2029
              OIFR=0
              DO IIFR=0,INIFR-1
                IF (BASEL(IIFR).GE.0) THEN
                  DO IPL=XX,YY
                    OCMOD(IPL,OIFR)=ICMOD(IPL,IIFR)
                   ENDDO
                  OIFR=OIFR+1
                 ENDIF
              ENDDO
              IF (.NOT.WNFWR(FCAOUT,OLEN,OCMOD,OUTP))
        1                GOTO 2029
              OUTP=OUTP+OLEN                        !move to next output
              INP=INP+ILEN                        ! and input scans
            ENDDO !ISCN
C#PRINT 'Model visibilities', WNFEOF(FCAOUT)
          ENDIF !MDDP#0
        ENDDO !MDDP 
        GOTO 2011
C
C Error
C
2029          CONTINUE
          CALL WNCTXT(F_TP,'Error copying model visibilities')
            GOTO 990

 2020        CONTINUE
C******************************************************************************
C Internal subroutine to copy scan
C******************************************************************************
C
C Copy observed visibilities
C
	CALL WNCTXT(F_TP,'Copying sector !AS',WNTTSG(ISNAM,0))
        DO ISCN=SCNFST,SCNLST
          OSCN=ISCN-SCNFST
          IF (.NOT.NSCSCF(FCAIN,ISTH(0),        !read input scan
        1        IIFRT,ISCN,CAP,CDAP,
        1        ISCH(0),IWGT,IDAT,IFLW)) THEN
            CALL WNCTXT(F_TP,'Error reading scan !UJ of sector !AS',
        1        ISCN,WNTTSG(ISNAM,0))
            GOTO 990
          ENDIF
C
C Invalidate interferometer corrections. - This is a quick and dirty fix!
C
          OSCHJ(SCH_IFRMC_J)=0
          OSCHJ(SCH_IFRAC_J)=0
C
C Invalidate terms that are absent if only XX present
C NOTE: There may also be correction parameters for Y but we need not bother
C  with them because they will be ignored by everyone
C
          IF (ONPOLC.EQ.1) THEN
            OSCHE(SCH_REDNS_E+2)=0
            OSCHE(SCH_REDNS_E+3)=0
            OSCHE(SCH_ALGNS_E+2)=0
            OSCHE(SCH_ALGNS_E+3)=0
            OSCHE(SCH_OTHNS_E+2)=0
            OSCHE(SCH_OTHNS_E+3)=0
          ENDIF
C
C Select data for selected interferometers. 
C
          OIFR=0
          DO IIFR=0,INIFR-1
            IF (BASEL(IIFR).GE.0) THEN
              DO IPL=XX,YY
                ODAT(C,OIFR,IPL)=IDAT(C,IIFR,IPL)
                ODAT(S,OIFR,IPL)=IDAT(S,IIFR,IPL)
                 OWGT(  OIFR,IPL)=IWGT(  IIFR,IPL)
                 OFLW(  OIFR,IPL)=IFLW(  IIFR,IPL)
               ENDDO
              OIFR=OIFR+1
             ENDIF
          ENDDO
C
C Copy XY to XX, YX to YY for YX mode
C
          IF (ONPOL.EQ.-2) THEN                        ! YX mode?
            DO OIFR=0,ONIFR-1
              DO IPL=XY,YX
                ODAT(C,OIFR,3*(IPL-1))=ODAT(C,OIFR,IPL)
                ODAT(S,OIFR,3*(IPL-1))=ODAT(S,OIFR,IPL)
                OWGT(  OIFR,3*(IPL-1))=OWGT(  OIFR,IPL)
                OFLW(  OIFR,3*(IPL-1))=OFLW(  OIFR,IPL)
              ENDDO
            ENDDO
          ENDIF
C
C** manipulate SCH corrections here
C
          IF (.NOT.NSCSFW(FCAOUT,OSTH(0),        !write output scan
        1        0,OSCN,0,0,
        1        OSCH(0),OWGT,ODAT,OFLW)) THEN
            CALL WNCTXT(F_TP,'Error writing scan !UJ of sector !AS',
        1        OSCN, WNTTSG(OSNAM,0))
C#ERRORC 'scan write', WNTTSG(OSNAM,0)
            GOTO 990
          ENDIF
        ENDDO
C
        GOTO 2021

 2030        CONTINUE
C
C*************************************************************
C Internal subroutine to copy the IF data (total powers)
C*************************************************************
C
        IF (.NOT.NCOCPB(FCAIN,FCAOUT,IFHHDL,
        1        ISTHJ(STH_IFHP_J),IFH,
        1        OSTHJ(STH_IFHP_J),CHKLST,COPIED)) THEN        !Copy header
           CALL WNCTXT(F_TP,'Error copying IFH block')
           GOTO 990
        END IF
C
C If the header was copied, we also need to do the data
C The hour-angle range has to be the same as for the visibilities
C
        IF (COPIED) THEN                        !Copy data
           J=ISTHJ(STH_IFHP_J)+IFHHDL                !Input data pointer
           J1=OSTHJ(STH_IFHP_J)+IFHHDL                !Output data pointer
           I1=4*STHTEL*LB_I                        !Data length
C
           OSCN=0                                !Count output scans
           DO ISCN=0,IFHJ(IFH_NTP_J)-1                                !Total powers
             HA=IFHE(IFH_HAB_E)+ISCN*IFHE(IFH_HAI_E)                 !Get Hour angle
             IF (HA.GE.HARAN(0).AND.HA.LE.HARAN(1)) THEN        !In range
                OSCN=OSCN+1                                        !Count scan
                IF (.NOT.WNFRD(FCAIN,I1,BUF,J)) GOTO 990        !Read data
                IF (.NOT.WNFWR(FCAOUT,I1,BUF,J1)) GOTO 990        !Write data
                J1=J1+I1                        !Next in output                
             END IF
             J=J+I1                                !Next in input
           END DO
           IFHJ(IFH_NTP_J)=OSCN                        !Update number of scans
C
           OSCN=0                                !Count output scans
           DO ISCN=0,IFHJ(IFH_NIF_J)-1                !IF-data (not yet used)
             HA=IFHE(IFH_HAB_E)+ISCN*IFHE(IFH_HAI_E)                 !Get Hour angle
             IF (HA.GE.HARAN(0).AND.HA.LE.HARAN(1)) THEN        !In range
                OSCN=OSCN+1                                        !Count scan
                IF (.NOT.WNFRD(FCAOUT,I1,BUF,J)) GOTO 990        !Read data
                
                IF (.NOT.WNFWR(FCAOUT,I1,BUF,J1)) GOTO 990        !Write data
                J1=J1+I1                        !Next in output                
             END IF
             J=J+I1                                !Next in input
           END DO
           IFHJ(IFH_NIF_J)=OSCN                        !Update number of scans
C
           OSTHJ(STH_IFHL_J)=IFHHDL+I1*(IFHJ(IFH_NTP_J)+IFHJ(IFH_NIF_J))
C
           IF (HARAN(0).GT.IFHE(IFH_HAB_E)) IFHE(IFH_HAB_E)=HARAN(0) !And HAB
           IF (.NOT.WNFWR(FCAOUT,IFHHDL,
        1                  IFH,OSTHJ(STH_IFHP_J))) THEN        !Rewrite
              CALL WNCTXT(F_TP,'Error rewriting IFH')
              GOTO 990
           END IF
        END IF
C
        GOTO 2031
C
C
        END
