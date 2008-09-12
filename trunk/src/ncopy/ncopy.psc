!+ NCOPY.PSC
!  JPH 930316
!
!  Revisions:
!       JPH 931012      Add SIMULATE option
!       JPH 931102      Add KEEP_MODEL. - Clone i.s.o. inherit POLARISATION
!       CMV 940518      Add COPY_MODEL and COPY_IFDATA
!       JPH 940913      Correct OUTPUT_SECTORS prompt
!                       Remove () from prompts
!       JPH 941005      Reorganise .pef files.
!                       Add X_ NGEN parameters and APPLY/DE_APPLY, MODELB
!       JPH 960220      SELECT_IFRS
!       JPH 960725      Add YX polarisation mode
!
!  Get overall action
!       Ref:    NCODAT
!
KEYWORD=OPTION
        DATA_TYP=C
        IO=I
        LENGTH=12
        NVALUES=1
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES
        SEARCH=L,P
        OPTIONS=COPY,SHORTCOPY, OVERVIEW,QUIT
        HELP="
Actions:
.
  COPY          Copy sectors from one SCN file to another.
  SHORTCOPY     As copy, but only scans selected by scan number; this option
                 allows you to cut out the invalid trailing scan from a mosaic
                 sector
.
  OVERWIEW      Display and log an overview of all sectors in a SCN file
  QUIT          Exit from NCOPY "
!
!
! input and output nodes
!
INCLUDE=SCNNODE_PEF
!
! sector selection
!
INCLUDE=SCNSETS_PEF:SCN_SETS,OVERVIEW   !
INCLUDE=SCNSETS_PEF:SCN_GROUPS,SCN_OBSS,SCN_FIELDS,SCN_CHANNELS,SCN_SECTORS
!
!
! Until further notice, user can not control output sector numbering. The
!  following entry is just a placeholder.
!!KEYWORD=OUTPUT_SECTORS
!!      DATA_TYP=C
!!      IO=I
!!      LENGTH=32
!!      NVALUES=1
!!      DEFAULT=*/NOASK
!!      SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
!       SEARCH=L,P
!!      PROMPT="new index pattern (grp.obs.fld.chn.seq)"
!!      HELP="
!!Do not override the default value"
!
!
INCLUDE=SELECT_PEF:HA_RANGE,SELECT_IFRS
!
!
KEYWORD=SCANS
        DATA_TYP=J
        IO=I
        NVALUES=2
        SEARCH=L,P
        PROMPT="First and last scan number from each sector"
        DEFAULTS= 0,10000
        HELP="
From all sectors selected, only the scan numbers within the selected scan
number and HA ranges wiull be copied. E.g., the specification
.
                SCANS= 0,4
.
will remove the last scan (number 5) from allo sectors of a mosaic observation
that produces 6-scan sectors.
.
Remember that the first scan in a sector is numbered 0. "
!
!
KEYWORD=POLARISATION
        DATA_TYP=C
        LENGTH=4
        IO=I
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES
        SEARCH=L,P
        OPTIONS=XYX,XY,X; [YX]
        DEFAULT=XYX
        PROMPT="Select polarisations"
        HELP="
Select polarisations to be copied:
        XYX:    XX, XY, YX, YY
        XY:     XX, YY only
        X:      XX only
.
        YX:     for special applications only:
                 overwrite XX with XY, YY with YX, then output as if XY "
!
!
!  Specify model needs to be copied
!       Ref:    NCODAT
!
KEYWORD=COPY_MODEL
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="copy model data?"
        HELP=" Specify YES if you want to copy the model together with the data"
!
!  Specify details wanted
!       Ref:    NCADAT
!
KEYWORD=COPY_IFDATA
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="copy IF-data/Total Powers?"
        HELP=" Specify YES if you want to copy IF-data (Total Powers) together
with the data"
!
!
! NGEN except FLAG parameters
!
INCLUDE=NGEN_PEF:X_LOG,LOG,X_RUN,RUN,X_INFIX,INFIX,X_DATAB,DATAB        !
INCLUDE=NGEN_PEF:X_MEMORY,MEMORY                                        !
INCLUDE=NGEN_PEF:X_APPLY,APPLY,X_DE_APPLY,DE_APPLY,X_MODELB,MODELB
