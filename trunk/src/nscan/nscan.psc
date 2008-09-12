!+ NSCAN.PSC
!  WNB 900131
!
!  Revisions:
!       WNB 910820      Add extinction, refraction, Faraday
!       WNB 910828      Add RUN
!       WNB 910909      Add DATAB and INFIX
!       WNB 910910      Add MODEL data an COPY
!       WNB 910912      Add to (de)apply model and ifr
!       WNB 910913      New (de-)apply and loops
!       WNB 910918      Text magtapes
!       WNB 911007      Include instrum. pol.
!       WNB 911014      Add CLIP flag option
!       WNB 911031      Add WERR option
!       WNB 911230      NMODEL
!       WNB 920504      Add NOISE flag <0 (temporary solution)
!       WNB 920626      Add DCLOW, change Rotation measure description
!       WNB 920828      Explanation NVS
!       WNB 920828      Add SET_PATTERN
!       WNB 920831      Add Stokes conversion to NVS
!       WNB 921022      Text magtapes
!       WNB 921104      Text Select IFRS; HA range; J2000
!       WNB 921211      Make PSC
!       WNB 921221      Add AERR, parall. angle to NVS
!       JEN 930308      add INCLUDE=NSETS_PEF, remove keyword SETS
!       JEN 930311      change SET_ACTION into SECTOR_ACTION
!       JEN 930311      Drastic improvement af all HELP-texts
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keyword(s) INPUT_SCAN, OUTPUT_SCAN, INOUT_SCAN
!       JEN 930312      Remove keyword(s) SELECT_IFRS, POLARISATION, HA_RANGE
!       JEN 930312      Remove keyword(s) INPUT_UNIT, OUTPUT_UNIT
!       HjV 930426      Change name keyword SET_PATTERN
!       WNB 930608      Add USER_FLAG, some DELETE_TYPE (FLAG, UNFLAG, UFLAG)
!                       change "delete" in "flag".
!                       Add NAME and FLAGS option to SECTOR_ACTION
!       WNB 930609      Add FLAG_OPTION; restructure with FLAG_TYPE
!       WNB 930610      Add SHOW, CORR, X/Y... FLAG_OPTIONS/_TYPES and more
!       WNB 930615      Add GET, FORCE flag options/types
!       WNB 930615      Change FLAG_OPTION in _MODE
!       WNB 930615      Add CLEAR, LOAD, UNLOAD to FLAG_MODE; PUT_RANGE
!       WNB 930617      Text PUT_RANGE; add FLAG_LIMIT; add READ/WRITE options
!       WNB 930617      Change FLAG_TYPE to operations, add FLAG_OPTION
!       WNB 930617      Split OPERATIONS: too many keywords!
!       WNB 930619      Remove SHOW and FLAG options and related keywords
!       WNB 930707      Text COPY option
!       WNB 930819      Add NOPT
!       WNB 930825      Remove UVFITS_POLAR
!       HjV 930922      Add SHOW for keyword OPTION and add keywords
!                       FILE_ACTION, SECTOR_ACTION, SCAN_ACTION and EDIT
!       CMV 931116      Split off SHOW keywords to NSHOW.PEF
!       CMV 931220      Effectively remove COPY option (option still there)
!       CMV 940223      Add LIST option
!       CMV 940422      Add LOADIF option and IFSETS prompt
!       HjV 940519      Add OLD_DATTYP
!       JPH 940913      Remove () on prompts
!       JPH 941005      Reorganise .pef files
!       JPH 941019      OPTIONS formatting
!       JPH 941206      UNITS_PEF
!       JPH 950118      Add WARC (CMV 941107)
!       CMV 950123      Add suboptions for WARC
!       HjV 950130      Change LOADIF in IFLOAD, Add LEIDEN
!       HjV 950612      Add MDLNODE_PEF
!       JPH 950821      Text mods
!       JPH 960404      Merge WARC-->ARC, TAPE_TYPE (HjV 951113)
!       JPH 961112      Add help text
!	CMV 970206	Add BITPIX for UVFITS
!
!  Get overall action
!       Ref:    NSCDAT
!
KEYWORD=OPTION
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Action"
        OPTIONS=-
QUIT; LOAD,IFLOAD,LEIDEN,LIST,ARC,DUMP; UVFITS,PFITS; | -
SHOW; [CVX,NVS,NOPT; REGROUP; FROM_OLD,WERR,AERR,VFIX]
        HELP="
Specify the action to be performed by the program NSCAN:
.
   Operations on WSRT 'circle' observation files (on classical, DAT or Exabyte
   tape, magnetic or optical disk):
.
        LOAD     transfer observed visibilities into a .SCN file.
        IFLOAD   same, but also transfer the 'IF' data (total powers etc). The
                  extra data will enlarge the .SCN file by some 30%; you need
                  them only if you suspect that the on-line gain calibration has
                  been incorrect.
        LEIDEN   transfer observed visibilities from standard LEIDEN file into a
                  .SCN file
        LIST     show contents of selected labels
        ARC      as list, but update the Scissor database if at NFRA
        DUMP     copy selected label(s) byte for byte into a disk file
.
   Conversion to and listing of data in FITS format
.
        UVFITS   convert .SCN file to UVFITS tape/disk file for use in AIPS
        PFITS    print AIPS-like FITS info (but also other) from UVFITS file
                   header
.
   Display and editing of .SCN files:
.
        SHOW     show/edit data and header information in .SCN file. (This
                   option is duplicated in NFLAG.)
.
   Utilities:
.
        QUIT     leave the program NSCAN
.
        CVX      convert a .SCN file from other machine's number format
        NVS      convert a .SCN file to newest version. Needs only to be run if
                   programs report that the data has the wrong version
                  - calculates parallactic angle for ATNF
        NOPT     update contents of sector/scan headers and polarisation format
                  of visibility data:
                   - convert input data in Stokes parameter format to XX format
                   - calculate UT start
                   - calculate precession rotation angle if not filled
                   - recalculate MJD for observations aborted at Wbork
        REGROUP  create a second index for each of one or more sets of sector.!
.
   One-time fixes:
.
        FROM_OLD convert old (R-series) uv-data file into NEWSTAR .SCN file
        WERR     correct one-time mosaic WSRT tape errors (1991 data only)
        AERR     change sign of phases 
        VFIX     recalculate velocities"
!
!  Get ARC / LIST suboption
!       Ref:    NSCDAT
!
!
KEYWORD=TYPE_TAPE
        DATA_TYP=C
        IO=I
        LENGTH=6
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Which type tape"
        OPTIONS=WSRT,LEIDEN
        HELP="
Specify the type of the tape.
.
Currently we support two types: WSRT and LEIDEN."
!
!  Get WARC suboption
!       Ref.    NSCDAT
!
!
KEYWORD=ARC_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="ARC Action"
        OPTIONS=CHECK,ARCHIVE
        HELP="
Checking in a WSRT tape has two phases. In the first phase, the ARC option
CHECK is used to verify the general integrity of the tape. The tape is listed
and all blocks are read. In the second phase labels are copied to the archive
medium (with the DUMP option) and the Scissor database is updated through ARC
option ARCHIVE."
!
!  Get WERR action
!       Ref:    NSCDAT
!
KEYWORD=WERR_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=24
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="WERR action"
        OPTIONS=WE0,WE1; QUIT
        HELP="
Specify correction to be performed:
.
   WE0:   correct Hour angles for tape error in splitted mosaic tapes before
           online version 62
   WE1:   correct all Hour angles with a constant offset
   QUIT:  no more"
!
!
!  Get input file
!       Ref:    NSCDAT
!
KEYWORD=INPUT_FILE
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="input filename"
        HELP="
Specify the input disk-file name.
.
For the NSCAN LOAD-from-disk option, or for the NSCAN PFITS option, don't
specify a file extension: it will be made by the program on the basis of the
tape label number.
.
In the case of other NSCAN options, give the full file name."
!!                      WNDPOH
!
!  Get integration time
!       Ref:    NSCDAT
!
KEYWORD=INTEGRATION_TIME
        DATA_TYP=J
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        CHECK=MAXIMUM,MINIMUM
        MAXIMUM=3600
        MINIMUM=10
        SEARCH=L,P
        PROMPT="integration time (sec)"
        DEFAULT=120
        HELP="
Specify the desired integration time per output scan in seconds.

120 sec is a good default; a lower value should be selected if visibilities
vary rapidly as a consequence of the source distribution in the observed field,
atmospheric conditions or interference.
.
For standard observations, the number must be a multiple of the basic
integration time used for the observation (which is some multiple of 10 sec).
.
In WSRT mosaic observations, slewing occurs in the last 10 sec of the dwell
time on each subfield. The integration time you specify must therefore at most
be <dwell time> minus 10 sec. You may also use a submultiple of this value
without losing data.
.
    Example:
        If the dwell time is 90 sec, there is 80 sec of valid data. HA_INT = 80,
    40, 20 or 10 will use it all. However, HA_INT = 30 or 60 will result in the
    loss of the last 20 sec.
.
        The volume of your data in the .SCN depends primarily on the
integration time. For each integration interval a 'scan' is created which
consists of
.
        <nr of polarisations> * <nr of interferometers> * 12 + 1024 bytes
.
Model visibilities which may be added later will occupy another
.
        4 * <nr of interferometers> * 12 bytes .                 NOTE:
        The WSRT runs on Universal Time; the quantum of integration time is
therefore 10 UT seconds, and a full 12-sidereal-hour observation contains
somewhat less than 12*360 of these quanta. You can mostly ignore this subtlety,
but the difference may be of practical importance in some situations.  "
!
!  Get output file
!       Ref:    NSCDAT
!
KEYWORD=OUTPUT_FILE
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="output filename"
        HELP="
Specify the full name for the output disk file.
.
For the NSCAN DUMP option, don't specify a file extension: it will be made by
the program on the basis of the label number.
.
For the other NSCAN options, specify a full file-name."
!!              WNDPOH
!
!  Get input labels
!       Ref:    NSCDAT
!
KEYWORD=INPUT_LABELS
        DATA_TYP=J
        IO=I
        NVALUES=256
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="input tape labels"
        HELP="
Specify the tape labels to be read.
.
   '*' selects all labels
.
   '<begin> TO <end>> BY <step>' selects labels <begin>, <begin>+<step>, ...
        up to <end>. The 'BY <step>' part may be omitted.
   BEWARE: The notation <begin>-<end>:<step> will be interpreted as an
        expression and give incorrect results
.
Each of the selected labels will be stored in the .SCN file as a separate
'observation' (obs) in the current 'group' grp. (Remember that .SCN file
'sectors' are identified by indices grp.obs.fld.chn.seq).
.
Example:
        If the current group nr is <g>, the selected input labels will be
stored sequentially in the .SCN file as observations <g>.0, <g>.1, <g>.2 "
!
!  Get output label
!       Ref:    NSCDAT
!
KEYWORD=OUTPUT_LABEL
        DATA_TYP=J
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="output tape label"
        HELP=" Specify the first output tape label.
* or 0 indicates the end of the tape (i.e. append the new labels at the end)."
!
!  Get pointing sets to do
!       Ref:    NSCDAT
!
KEYWORD=POINTING_SETS
        DATA_TYP=J
        IO=I
        NVALUES=512
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Mosaic field sequence number(s)"
        HELP=" Select the SEQUENCE numbers of the fields (= mosaic 'pointing
centres') to be selected. (see Note below).
.
NOTE that WSRT mosaic fields are numbered starting at 1
.
   '*' selects all fields
.
   '<begin> TO <end>> BY <step>' selects fields <begin>, <begin>+<step>, ...
        up to <end>. The 'BY <step>' part may be omitted.
   BEWARE: The notation <begin>-<end>:<step> will be interpreted as an
        expression and give incorrect results
.
Each of the selected fields will be stored in the .SCN file as a separate
'field' (fld) under the current 'group' and 'observation' grp.obs. (Remember
that .SCN file 'sectors' are identified by indices grp.obs.fld.chn.seq).
.
NOTE:
        The numbers you specify refer to the SEQUENCE (starting with 0) in
which the mosaic fields aqppear on the tape. Normally, an observation starts
with field 0 and the sequence numbers equal the field numbers. However, if your
observation starts 'somehere in the middle', this is no longer the case and you
must be careful in selecting your fields. Example:
.
        Your observation starts at field 19. To select fields 19 through 24,
specify POINTING_SETS= 0 TO 5. "
!
!  Get channels to do
!       Ref:    NSCDAT
!
KEYWORD=CHANNELS
        DATA_TYP=J
        IO=I
        NVALUES=512
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="frequency channel number(s)"
        HELP="
Select the frequency channels/bands to be included. NOTE that the WSRT
continuum channel (i.e. the sum of all channels) always has number 0
.
   '*' selects all channels
.
   '<begin> TO <end>> BY <step>' selects channels <begin>, <begin>+<step>, ...
        up to <end>. The 'BY <step>' part may be omitted.
   BEWARE: The notation <begin>-<end>:<step> will be interpreted as an
        expression and give incorrect results
.
Each of the selected channels will be stored in the .SCN file as a separate
'channel' (chn) under the current 'group', 'observation' and 'field'
grp.obs.fld. (Remember that .SCN file 'sectors' are identified by indices
grp.obs.fld.chn.seq). "
!
!  Ask wether IF datasets (total powers etc) should be loaded
!       Ref:    NSCDAT
!
KEYWORD=IFSETS
        DATA_TYP=J
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        CHECK=MAXIMUM,MINIMUM
        MAXIMUM=3600
        MINIMUM=0
        SEARCH=L,P
        PROMPT="integration time for total-power data"
        HELP="
Specify the desired integration time for IF sets with Total Power data."
!
!  Get IAT-UTC
!       Ref:    NSCUVF
!
KEYWORD=IAT_UTC
        DATA_TYP=R
        IO=I
        NVALUES=20
        SWITCHES=VECTOR
        SEARCH=L,P
        DEFAULTS=48257,26,900000,27,900000,27,900000,27,900000,27, -
                900000,27,900000,27,900000,27,900000,27,900000,27 /NOASK
        PROMPT="MJD, leap seconds,..."
        HELP="
Specify the IAT-UTC values as pair(s) of numbers:
.
   MJD at which the leap seconds occur
   The total number of leap seconds as from that date.
.
The first value is for 1 Jan 1991. Values before that are known by the program."
!
!  Get BITPIX
!       Ref:    NSCUVF
!
KEYWORD=BITPIX
        DATA_TYP=J
        IO=I
        NVALUES=1
        SWITCHES=NULL_VALUES
        SEARCH=L,P
        DEFAULT=16
        PROMPT="Precision for writing UVFITS output"
        HELP="
Specify the precision for writing UVFITS data.
.
   16     2 bytes integers
   32     4 bytes integers
  -32     IEEE Floating point values (not yet supported)"
!
!
!  Get mosaic RA
!       Ref:    NSCWE0
!
KEYWORD=WERR_RA
        DATA_TYP=D
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        UNITS=DEG,RAD,CIR,HMS
        SEARCH=L,P
        PROMPT="RA of mosaic centre"
        HELP="
Specify the Right Ascension of the centre of the mosaic area"
!
!  Get mosaic HA
!       Ref:    NSCWE0
!
KEYWORD=WERR_HA
        DATA_TYP=D
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        UNITS=DEG,RAD,CIR,HMS
        SEARCH=L,P
        PROMPT="Hour-angle correction (deg)"
!!      DEFAULT=0 /NOASK                (suggestion by JPH)
        HELP="
Specify the angle to be added to all hour angles in the .SCN file
.
NOTE: This is a special feature to be used only to correct errors made in the
on-line observation. "
!
!  Get ref velocity
!       Ref:    NSCWE3
!
KEYWORD=WERR_VEL
        DATA_TYP=D
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        UNITS=DEG,RAD,CIR,HMS
        SEARCH=L,P
        PROMPT="Reference velocity (m/sec)"
        HELP="
Specify the reference velocity to recalc set velocities for
.
NOTE: This is a special feature to be used only to correct errors made in the
conversion to SCN file from an MS. "
!
!  Get data offset
!       Ref:    NSCDAT
!
KEYWORD=HAB_OFFSET
        DATA_TYP=R
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Start-offset (sec)"
!!      DEFAULT="0 /NOASK"              suggestion JPH
        HELP="
Specify the time-offset from the start of an observation, at which the
integration for the output scans should start. Example: an offset of n will
throw away the first n (rounded up to a multiple of 10) seconds of data.
.
NOTE: This feature was useful in early mosaic experiments in which the slewing
between fields occurred in the first integration interval(s) on the new field.
It is now standard practice to slew during the final integration interval on
the old source. "
!
!  Get set pattern
!       Ref:    NSCREG
!
KEYWORD=SCN_SET_PATTERN
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=1
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="new index pattern (grp.obs.fld.chn.seq)"
        HELP="
Specify a new index pattern (group.observation.field.channel.sequence_number)
into which to change the index pattern of the input Set of uv-data Sectors.
.
Each index in the pattern that contains an * is copied from the input Set.
Other fields are used as is (true for the first four fields, i.e.
grp.obs.fld.chn.)
.
Example: To change the indices of the Set
        0.*.15283.* to
        0.*.0    .*
.
give the first selection as the input Set, and the second as the pattern."
!
!
!  Get old R-series data type
!       Ref:    NSCDAT
!
KEYWORD=OLD_DATTYP
        DATA_TYP=J
        IO=I
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="Old R-series data format"
        HELP="
Specify the old R-series data type:
.
        0= local
        1= VAX, D_FORMAT
        2= VAX, G_FORMAT
        3= ALLIANT
        4= CONVEX
        5= IEEE
        6= DEC station
        7= SUN station
        8= HP  station"
!
!  Get polarisation
!       Ref:    NSCPL2 - called only by NCSDAT (JPH 941005)
!
KEYWORD=SELECT_IQXY
        DATA_TYP=C
        LENGTH=4
        IO=I
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="output polarisations (ONE 'value') |"
        OPTIONS= I,Q,U,V, IQ,UV, IQUV, X,XXY,YYX,Y, XY,YX, XYX
        HELP="
Specify the polarisation(s) to be written:
.
        I:      I only
        Q:      Q only
        U:      U only
        V:      V only
.
        IQ:     I and Q
        UV:     U and V
        IQUV:   I, Q, U and V
.
        X:      XX only
        XXY:    XY only
        YYX:    YX only
        Y:      YY only
.
        XY:     XX and YY
        YX:     XY and YX
        XYX:    all four: XX,XY,YX,YY"
!
!-
INCLUDE=NSHOW_PEF
!-
INCLUDE=NGEN_PEF
!-
INCLUDE=UNIT_PEF        !
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF      !
!-
INCLUDE=MDLNODE_PEF     !
INCLUDE=NMODEL_PEF
!-
