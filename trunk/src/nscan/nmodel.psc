!+ NMODEL.PSC
!  WNB 900327
!
!  Revisions:
!       WNB 910820      Add extinction, refraction, Faraday
!       WNB 910827      Add CALIB
!       WNB 910828      Add RUN
!       WNB 910909      Add DATAB and INFIX
!       WNB 910909      Add OUTPUT_NODE
!       WNB 910913      New (de-)apply and loops
!       WNB 911007      Add instr. pol.
!       WNB 911230      Add DNCLOW
!       WNB 920626      Add DCLOW, change Rotation measure description
!       WNB 920818      Add DAREA
!       WNB 921104      Text select ifrs; HA range; J2000
!       WNB 921211      Make PSC
!       WNB 921217      Add AFIND
!       JEN 930308      INCLUDE=NSETS_PEF, remove keywords MAPS, SCAN_SETS
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keyword(s) SCAN_NODE, MAP_NODE
!       JEN 930312      Remove keyword(s) SELECT_IFRS, POLARISATION, HA_RANGE
!       HjV 930426      Change name keywords REFERENCE_SCAN,
!                       REFERENCE_SET, OUTPUT_NODE
!       WNB 930514      Add manual find, FIND_TYPE
!       WNB 930517      Add GIDS_SOURCES (was removed in change of 930426)
!       WNB 930928      Continuation lines; extend REFERENCE_DATA
!       WNB 931008      Add EDIT to ACTION; text; moved CONVERT_TO from .pef
!       WNB 931011      Add SUPDATE
!       WNB 931119      Add REDIT, FEDIT to action
!       WNB 940821      Add UPDATE_TYPE
!       JPH 940913      Remove () on prompts
!       JPH 941206      Help texts; prompt formatting
!       JPH 941222      / in OPTIONS --> ,
!       CMV 950110      Remove _ in option string
!       WNB 950626      Add grouped update options
!       WNB 950630      Add Update options
!       WNB 950706      Add LCOMBINE, LCLUSTER, LCONSTRAINT
!       JPH 950929      Merge private shadow-version; cosmetic changes
!       JPH 961112      Help texts
!	WNB 990729	Add X0-X3 as model updates
!
!
!  Get overall action
!       Ref:    NMODAT
!
KEYWORD=ACTION
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Action"
        OPTIONS=-
HELP, HANDLE; SAVE, GET; FIND, UPDATE; |-
CONVERT, EDIT, REDIT, FEDIT ,BEAM,DEBEAM; FROM_OLD, TO_OLD, NVS, CVXL; QUIT|
        HELP=" Specify action to perform:
.
   General:
.
        HELP      show some explanation on model lists
        HANDLE    select general model-handling branch
.
   Model construction:
.
        FIND    find sources in map (.WMP file)
        UPDATE  update sources by comparison with visibilities (.SCN file)
.
   Transfer of model to/from .SCN file:
.
        SAVE      save model data in .SCN file
        GET       get model from .SCN file
.
   Wholescale conversions of model list:
.
        CONVERT   Convert model list from epoch to epoch or coordinate to
                   coordinate with conversion of l,m coordinates and intensities
                   if necessary
        EDIT      Change model-list overall parameters without conversion of
                   coordinates and intensity (except possible field rotation if
                   converting from apparant <-> epoch)
        REDIT     Change reference coordinates and frequency. Intensities will
                   be corrected for spectral index only.
        FEDIT     As REDIT, but intensities will also be corrected for the
                   effect of different beams at different frequencies
!!                      Primary or synthesised?
        BEAM      Correct sources for attenuation by primary beam
        DEBEAM    Apply primary-beam attenuation to sources
.
   Utilities:
.
        FROM_OLD  convert old format source list (use B1950 or Apparent by
                   preference)
        TO_OLD    convert to old format source list
        NVS       make new version of model file if necessary
        CVXL      convert formats between machines
.
        QUIT    finish"
!
!  Get find type
!       Ref:    NMODAT
!
KEYWORD=FIND_TYPE
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Finding mode"
        OPTIONS=POS,ABS, MANUAL; QUIT
        HELP="
Specify how to select candidate sources:
.
        POS     Select sources with highest positive brightness
        ABS     Select sources with highest absolute brightness
        MANUAL  Select sources manually with the cursor on a GIDS screen.
                 Sources are selected by pushing MB1; MB3 will finish;
                 MB2 will not calculate position, but put grid point as clean
                 source"
!!                      This should come as an explanation from the code
!
!  Get update type
!       Ref:    NMODAT
!
KEYWORD=UPDATE_TYPE
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Update mode"
        OPTIONS=I, LM, ILM, SILM; EXTEND; QUV, PESTIMATE;|-
X00, X01, X02, X03 ; QUIT
        HELP="
Specify the parameters to be redetermined per source in the update process.
.
   Position and flux:
        I       Intensity only
        LM      Position only
        ILM     Intensity and position
        SILM    Intensity, spectral index and position (requires a multichannel
                 observation with <bandwidth per channel>/<frequency> greater
                 than ..)
!                       ??
.
   Source size:
        EXTEND  Intensity and size parameters for elliptic-Gaussian model
                 (major/minor axes, position angle)
.
   Source polarisation:
        QUV     Stokes Q/I, U/I, V/I
        PESTIM  Estimate the total linear polarisation, and store it as Stokes
                 Q. NOTE that this improper use of the model list is not
                 recorded, so you it is your responsibility to interpret this
                 kluge correctly.
.
   Specially programmed updates -- ask AGdB about details
	X00	Type extra 0
	X01	Type extra 1
	X02	Type extra 2
	X03	Type extra 3
.
NOTES:
        1. All sources in the model are taken into account in determining the
residual visibilities on which the update fit is performed. However, no updates
are attempted for sources of type other than 0 nor for CLEAN components.

        If you want to protect one or more sources from being updated, you can
do so by temporarily changing their type (OPTION=MODIFY, MODIFY_OPTION=FEDIT,
see the Help text there).
!               \whichref{}{}
.
        2. It is recommended that you use a set of interferometers that gives a
uniform baseline coverage, e.g. by selecting fixed-movable (FM) interferometers
only. "
!
!  Get update mode
!       Ref:    NMODAT
!
KEYWORD=UPDATE_MODE
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Update type"
        OPTIONS=SEPARATE; CLUSTER, LCLUSTER; COMBINED, LCOMBINED;|-
CONSTRAINED, LCONSTRAINED; QUIT
        HELP="
Specify how the update process should proceed:
.
        SEPARATE        each source to be updated is looked at separately,
                         and they are fiddled to cater for close sources
.
        CLUSTER         sources are clustered to bypass closeness problems
.
        COMBINED        all sources solved in one go, with sub-clustering
.
        CONSTRAINED     use constraints and clustering. The standard
                         constraint used is to fix all variables for all
                         but the lowest intensity source
.
        L<type>         Loop up to 20 cycles till convergence:
                         Note that the loop option does not always
                         work, and is very slow. Preferably use for a
                         small number of well separated sources/clusters. "
!
!  Get update cluster size area
!       Ref:    NMODAT
!
KEYWORD=UPDATE_CLUSTER
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR,NULL_VALUES,WILD_CARD
        SEARCH=L,P
        PROMPT="Update cluster size in arcsec (l,m)"
        HELP="
Specify the cluster size for update in arcsec in the l and m direction. "
!
!  Confirm manual search
!       Ref:    NMOFMD
!
KEYWORD=GIDS_SOURCES
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Select sources on GIDS display (Yes/No)?""
        HELP="
Specify if you want to point at sources on the GIDS display for the current map"
!
!  Get file
!       Ref:    NMODAT
!
KEYWORD=OLD_FILE
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="old model filename"
        HELP="
Specify the filename (includinhg extension) the an old-format RMODEL file to be
read or written. "
!
!  Get calibrator list
!       Ref:    RMOCAL
!
KEYWORD=CALIBRATORS
        DATA_TYP=J
        IO=I
        NVALUES=128
        SWITCH=LOOP,WILD_CARDS,NULL_VALUES
        SEARCH=L,P
        PROMPT="List of sources or *"
        HELP="
Specify up to 128 numbers of sources in the model list that you want to use as
calibrators."
!
!  Get source conversion action
!       Ref:    NMOCVT
!
KEYWORD=CONVERT_TO
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        OPTIONS=B1950,J2000,APPARENT,LOCAL
        PROMPT="Target coordinate system"
        HELP="
Specify the coordinate system for the output model-list.
.
If you are CONVERTing, the output model list's coordinates and intensities will
reflect the new type, frequency, instrument and central coordinates.
.
If you are EDITing, the new data will be copied only, but the coordinates will
be rotated to the new Pole if you are converting between apparent and epoch.
!!              ???
"
!
!  Get coordinate reference scan node
!       Ref:    NMOCVS
!
KEYWORD=REF_SCN_NODE
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="File name"
        HELP="
Specify the node name with the model overall coordinates to use.
.
Instead of a file name you may reply with a '*'. You will then be prompted for
'manual' inpout of data, unless you are CONVRTing between apparent and epoch
coordinates.
!!                      ???
!!.
!!Note that the node name can be preceded with dev:[dir..] (or /dev/dir... if
!!Unix). The specified dev and/or directories will become the default
!!database for all node related names. Atstart of program the default is the
!!current directory. The database specified should be an existing directory.
!!.
!!By placing part of the node name in parentheses (), the string defined
!!in such a way can in subsequent node questions be referenced with a #.
!!Hence the following inputs will translate as:
!!.
!!  ngc1204.21cm.long                   ngc1204.21cm.long
!!  [dwl.ger.ngc1204]ngc1204.90cm.short [dwl.ger.ngc1204]ngc1204.90cm.short
!!  (ngc1204.21cm.)long                 [dwl.ger.ngc1204]ngc1204.21cm.long
!!  a.#long                             [dwl.ger.ngc1204]a.ngc1204.21cm.long
!!              Is this of any use? If so, why specifically here?
"
!
!  Get coordinate reference set
!       Ref:    NMOCVS
!
KEYWORD=REF_SCN_SET
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=64
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="ONE sector for reference coordinates: grp.obs.fld.chn.seq"
        HELP="
Specify ONE .SCN-file sector from which the reference coordinates (RA, DEC,
frequency) are to be taken.
.
grp.obs.fld.chn.seq = group.obs.field.channel.sequence_number "
!
!  Get reference data
!       Ref:    NMOCVT
!
KEYWORD=REFERENCE_DATA
        DATA_TYP=D
        IO=I
        NVALUES=5
        SWITCH=LOOP,VECTOR,WILD_CARDS,NULL_VALUES
        SEARCH=L,P
        PROMPT="RA, DEC, Freq, rot.,instr."
        HELP="
Specify
.
        RA, DEC (decimal degrees)
        precession rotation angle (degrees)
        spectral-index reference frequency (MHz)
        instrument code (0=WSRT; 1=ATCA)
.
for the new reference source list position. "
!!              Order in prompt and help differ
!
!  Get reference frequency
!       Ref:    NMOOFR
!
KEYWORD=REFERENCE_FREQ
        DATA_TYP=D
        IO=I
        SWITCH=LOOP,WILD_CARDS,NULL_VALUES
        SEARCH=L,P
        PROMPT="Reference frequency (MHz)"
        HELP="
Specify the spectral index reference frequency (MHz)."
!
!
!  Get area
!       Ref:    NMADAR
!
KEYWORD=AREA
        DATA_TYP=J
        IO=I
        NVALUES=4
        SWITCHES=LOOP,VECTOR,NULL_VALUES,WILD_CARD
        SEARCH=L,P
        PROMPT="Map area: l,m, dl,dm"
        HELP="
Specify the map area to be selected:
.
        l, m    position in grid spacings of centre of area
                0,0 is the map centre, increaing toward the upper right (i.e.
                 with DEcreasing RA and INcreasing DEC)
.
        dl, dm  horizontal and vertical area sizes "
!
!  Get source amplitude limit
!       Ref:    NMODAT
!
KEYWORD=MAP_LIMIT
        DATA_TYPE=R
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=0.
        MAXIMUM=1.
        SEARCH=L,P
        PROMPT="Lower relative intensity limit"
        HELP="
Specify the lowest limit with respect to the maximum in the map that will be
considered a valid source"
!
! Get sources to add
!       Ref:    NMODAT
!
KEYWORD=MAX_NUMBER
        DATA_TYPE=J
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=1
        MAXIMUM=200
        SEARCH=L,P
        PROMPT="Maximum number of sources to add"
        HELP="
Specify the maximum number of sources that will be found. "
!
! Get source ID
!       Ref:    NMODAT
!
KEYWORD=ID_START
        DATA_TYPE=J
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        CHECKS=MINIMUM
        MINIMUM=1
        SEARCH=L,P
        PROMPT="Start identification number"
        HELP="
Specify the start of the ID number to be used in the source list."
!-
INCLUDE=NGEN_PEF
!-
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF
!-
INCLUDE=WMPNODE_PEF     !
INCLUDE=WMPSETS_PEF
!-
INCLUDE=MDLNODE_PEF
!-
INCLUDE=NMODEL_PEF
!-
