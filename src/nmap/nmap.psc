!+ NMAP.PSC
!  WNB 910219
!
!  Revisions:
!       WNB 910815      Circular UV_AREA
!       WNB 910820      Add extinction, refraction, Faraday
!       WNB 910822      Add FIDDLE
!       WNB 910828      Add RUN
!       WNB 910909      Add DATAB and INFIX
!       WNB 910911      Add NSUM
!       WNB 910912      Add other sums
!       WNB 910913      New (de-)apply, loops
!       WNB 910918      Text magtapes
!       WNB 911007      Add instrum. pol.
!       WNB 911104      Add mosaic combine
!       WNB 911105      Add EDIT keyword
!       WNB 911230      NMODEL
!       WNB 920626      Add DCLOW, change Rotation measure description
!       WNB 920811      Add USE_NOISE
!       WNB 920817      Add circular weight
!       WNB 920818      Add FITS_SCALE
!       WNB 921022      Text magtapes
!       WNB 921104      Text select ifrs; J2000
!       WNB 921119      Add WRLFITS, CUBIC
!       WNB 921201      Larger map size; default memory
!       WNB 921202      Reorganise for data clean
!       WNB 921211      Make PSC
!       JEN 930308      INCLUDE=NSETS_PEF, remove keyword MAPS
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keyword(s) INPUT_MAP, OUTPUT_MAP, INOUT_MAP
!       JEN 930312      Remove keyword(s) OUTPUT_UNIT
!       HjV 930426      Change name keywords NODE_1, NODE_2 ,MAPS_1, MAPS_2
!       WNB 930602      Add WGT_LIMIT
!       WNB 930826      Change Stokes text
!       WNB 930928      Continuation lines
!       WNB 930929      Text FILENAME; Add Fiddle LOAD, LOAD_OPTION
!       WNB 930930      Add UNLOAD
!       WNB 921215      Describe new EDIT; use NSHOW_PEF
!       CMV 940506      Increase max. mapsize for MOSCOM
!       HjV 940518      Add OLD_DATTYP
!       HjV 940714      Add RFITS as option for OPTION
!       JPH 940913      Correct WMP_SET_1/2 prompts
!                       Remove () from prompts
!       JPH 940923      NSETS --> WMPSETS, SCNSETS
!       JPH 940929      FIELD_CENTRE
!       JPH 941013      Remove WILD_CARDS and NULL_VALUES where they are invalid
!                       USER_COMMENT, QDATAS, UV_ARREA,CLIP_AREA, CLIP_LEVELS
!                        from NMAP.PEF
!                       Remove CONVOL_WIDTH
!                       Remove UNITS=M, put metres in prompts. (First tried
!                        UNITS="m" but this failed.)
!                       HA_RESOLUTION in UT rather than ST seconds
!                       Many changes in texts
!       JPH 941107      Correct CLIP_LEVEL help text.
!       JPH 941116      CLIP_LEVEL NON_DESCENDING
!       JPH 941117      Fine-tuning. \textrefs
!       JPH 950112      LENGTHs back to low values. - Minor text changes.
!                       Remove all UNITS
!       JPH 950126      Correct HELP text for MAP_COORD
!       JPH 950207      More HELP text
!       CMV 951113      Add option CSUM/RSUM for complex summation
!       JPH 960402      Help user in HOLOG chaos: HOLOG --> OLDHOLOG, NEWHOLOG
!                        as synonym for WMP
!       JPH 960814      Prompt texts
!       JPH 961112      Help texts
!	WNB 061023	Changed max FT size to 8192
!
!
!  Get overall action
!       Ref:    NMADAT
!
KEYWORD=OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Action|"
        OPTIONS=-
MAKE,FIDDLE; W16FITS,W32FITS,WRLFITS, RFITS; SHOW; QUIT;|-
[FROM_OLD,TO_OLD, CVX,NVS]
        HELP="
Specify action to perform:
.
  Primary operations:
        MAKE      make map(s) from visibility data in .SCN file
        FIDDLE    combine or change maps in .WMP file
.
  FITS conversions:
        W16FITS   write FITS tape/disk with 16 bits data
        W32FITS   write FITS tape/disk with 32 bits data
        WRLFITS   write FITS tape/disk with 32 bits float data
        RFITS     read FITS tape/disk data
.
  Miscellaneous:
        SHOW      show/edit map data
        QUIT      finish
.
  Format conversions:
        CVX       convert a map file from other machine's format to local
                   machine's
        NVS       convert a map file to newest version. Needs to be run only if
                   indicated by program
        FROM_OLD  convert from R-series format
        TO_OLD    convert to R-series format "
!
!  Get Fiddle action
!       Ref:    NMADAT
!
KEYWORD=FIDDLE_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Fiddle action|"
        OPTIONS=-
BEAM,DEBEAM, FACTOR; EXTRACT, COPY, LOAD;|-
ADD,AVER, POL,ANGLE; SUM,CSUM,RSUM,MOSCOM; QUIT
        HELP="
Specify action to perform.
.
   In-place modifications: Modify data of (an) input image(s):
.
        BEAM    correct map for primary beam attenuation so it will represent
                 the 'true' sky
        DEBEAM  apply primary beam attenuation to map so it will reprsent the
                 product of the 'true' sky and the primary beam
        FACTOR  multiply image with a constant factor
.
   Unary operations: Create 1 new image from each input image:
.
        EXTRACT extract an area from (a) image(s)
        COPY    copy image(s)
        LOAD    read or write (an) image(s) in a foreign format (e.g. Holog)
.
   Binary combinations: make (a) new output image(s) from (a) pair(s) of input
   images:
.
        ADD     weighted sum of two images: F1*image1 + F2*image2
        AVER    weighted average of two images:
                        (F1*image1 + F2*image2) / [abs(F1) + abs(F2)]
        POL     degree of linear polarisation from Q and U maps:
                        sqrt (Qmap**2 + Umap**2)]
        ANGLE   polarisation orientation (radians) from Q and U maps:
                        0.5*atan (Umap / Qmap)
.
   Combinations of more than two images:
.
        SUM     weighted summation of (a) set(s) of images in a single .WMP file
                 (you will be prompted to select the weighing method)
        CSUM    weighted summation of pairs of images. You will be prompted
                 for (a) set(s) of "real" images and (a) set(s) of
                 "imaginary" images and complex weighting factors.
        RSUM    idem, but the complex weighting factors will be calculated
                 based on a specified rotation measure.
        MOSCOM  'mosaic combine': merge a set of maps (generally for different
                 field centres) into one output map
.
   Miscellaneous:
.
        QUIT    Return to OPTIONS level "
!
!  Specify cube output FITS
!       Ref:    NMADAT
!
KEYWORD=CUBIC
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        PROMPT="Make line cube (Yes/No) ?"
        HELP="
Specify if you want to output the FITS maps in one cube or in separate maps"
!
!  Specify noise use in MOSCOM
!       Ref:    NMADAT
!
KEYWORD=USE_NOISE
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        PROMPT="Weigh with noise (Yes/No) ?"
        HELP="
Specify if you want the noise of the individual maps to be used as a weight in
the MOSCOM combination"
!
!  Specify lowest relative weight for MOSCOM
!       Ref:    NMADAT
!
KEYWORD=WGT_LIMIT
        DATA_TYP=R
        IO=I
        SEARCH=L,P
        CHECKS=MAXIMUM,MINIMUM
        MAXIMUM=1.
        MINIMUM=0.
        PROMPT="Relative weight limit"
        HELP="
Specifies the relative weight as compared to the expected maximum weight of
data points combined on one line, below which no output will be generated"
!
!  Get Sum action
!       Ref:    NMADAT
!
KEYWORD=SUM_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        OPTIONS=SUM,NSUM,NSSUM, BSUM,BNSUM, FSUM; QUIT
        PROMPT="Weighing method"
        HELP="
Specify the type of weight to use in the averaging. In all cases the summation
produces a weighted average map over all SETS_1, the weights depending on the
method you select:
.
        SUM     weight(i)= 1
        NSUM    weight(i)= normalisation factor of map(i)
        NSSUM   weight(i)= 1 / (<noise in map(i)>**2)
.
        BSUM    weight(i)= bandwidth of map(i)
        BNSUM   weight(i)= bandwidth * normalisation factor of map(i)
.
        FSUM    weight(i)= factors to be specified by you.
.
        QUIT    quit AVERaging"
!
!  Get Load action
!       Ref:    NMADAT
!
KEYWORD=LOAD_OPTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        OPTIONS=WMP, UNLOAD; NEWHOLOG, [OLDHOLOG]; QUIT
        PROMPT="Load action/image type |"
        HELP="
Specify the type of foreign map and what to do with it:
.
        WMP      read WMP format maps from contiguous binary file
        UNLOAD   inverse of WMP: write WMP maps to contiguous binary file
                  this option can also be used to load in an SAOIMAGE file
                  (specify a data offset of 512)
.
        NEWHOLOG read HOLOG file from the WSRT (this option, synonymous with
                  WMP, was added to help the user)
        OLDHOLOG read a Holog map in old IBM-coded format (this option used to
                  be called HOLOG)
.
        QUIT    quit LOADing"
!
! Map multiplication factors
!       Ref:    NMADAT
!
KEYWORD=MAP_FACTORS
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCH=VECTOR
        SEARCH=L,P
        PROMPT="Input-map multipliers"
        HELP="
Specify the factors by which the input maps have to be multiplied.
.
You may specify up to 8 factors which will be used as multipliers in the
weighted summation the input maps you selected. If there are more maps to be
summed than factors specified, the factors will be cyclically re-used. "
!
! Sum multiplication factors
!       Ref:    NMADAT
!
KEYWORD=SUM_FACTORS
        DATA_TYP=R
        IO=I
        NVALUES=8
        SEARCH=L,P
        PROMPT="Summing multipliers"
        HELP="
Specify up to 8 weight factors by which the input maps have to be multiplied.
If the number of weights you give is less than the number of maps to be
combined, the weights will re-used in a cyclic fashion.
.
Example:
        1,-1 will average the first, third, ... maps in your WMP_SETS
specification with the negated second, fourth ... maps.
.
Note: When used together with the RSUM option, the weights will be used both
for the Real and Imaginary map (they are real weights)."
!
!
! Complex sum multiplication factors
!       Ref:    NMADAT
!
KEYWORD=CSUM_FACTORS
        DATA_TYP=R
        IO=I
        NVALUES=16
        SEARCH=L,P
        PROMPT="Summing multipliers"
        HELP="
Specify up to 8 complex weight factors by which the input maps have to be
multiplied. For each factor the real part should be given first, the imaginary
next. So 1,0,0,-1  means add the real part of the first map-pair to the
imagingary part of the second map-pair:
.
    (1+0*i)*(map1r + i*map1i) + (0-1*i)*(map2r + i*map2i)
.
If the number of weights you give is less than the number of maps  to be
combined, the weights will re-used in a cyclic fashion. Example:
        1,1,-1,-1 will average the first, third, ... pairs in your WMP_SETS
specification with the negated second, fourth ... pairs."
!
! Rotation measure
!       Ref:    NMADAT
!
KEYWORD=ROTATION_MEASURE
        DATA_TYP=R
        IO=I
        NVALUES=512
        SEARCH=L,P
        PROMPT="Rotation measure"
        HELP="
Specify one (or more) rotation measure(s) (RM). Pairs of input maps will be
phase-rotated to the the frequency of the first input map and averaged:

    THETAn = 2 * RM * ( (c/FRQn)**2 -  (c/FRQ1)**2 )

    Qout = SUM( Wn * ( cos(THETAn)*Qn - sin(THETAn)*Un ) ) / SUM( Wn )
    Uout = SUM( Wn * ( sin(THETAn)*Qn + cos(THETAn)*Un ) ) / SUM( Wn )

A pair of Qout/Uout will be produced for each RM given. E.g. to get average Q/U
maps for rotation measures from 0 to 5 with intervals of 0.5, specify
ROTATION_MEASURE=0 TO 5 BY 0.5."
!
! Polarisation level
!       Ref:    NMADAT
!
KEYWORD=MAP_LEVEL
        DATA_TYP=R
        IO=I
        CHECK=MINIMUM
        MINIMUM=0.
        SEARCH=L,P
        PROMPT="Polarisation threshold (W.U)"
        HELP="
Specify the minimum level in Wetsrebork Units that is still to be considered
valid linear polarisation. Polarisation levels below this threshold will be set
to zero in the output map(s)."
!
!  Get FITS comment
!       Ref:    NMAWFT
!
KEYWORD=COMMENT
        DATA_TYP=C
        IO=I
        LENGTH=70
        SWITCHES=NULL_VALUES
        SEARCH=L,P
        PROMPT="FITS comment (<=70 chars)"
        HELP="
The given text will be included as COMMENT in FITS output."
!
!  Get FITS scale
!       Ref:    NMADAT
!
KEYWORD=FITS_SCALE
        DATA_TYP=C
        IO=I
        LENGTH=4
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        OPTIONS=JY,WU
        PROMPT="Units of source flux"
        HELP="
Specify the output units of the FITS data:
.
        JY      jansky per beam
        WU      Westerbork units (1 W.U. = 5 mJy)"
!! WU per beam???
!
!  Get input file
!       Ref:    NMADAT
!
KEYWORD=INPUT_FILE
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES
        SEARCH=L,P
        PROMPT="Input file name"
        HELP="
Specify the file name (including extension) of the file to be converted."
!
!  Get file name
!       Ref:    NMADAT, NMAFLD
!
KEYWORD=FILENAME
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,NULL_VALUES
        SEARCH=L,P
        PROMPT="Name for output disk file"
        HELP="
Specify the file name (without an extension) to be used in creating a
pseudo-tape output file name (e.g. FITS write).
.
Specify a full filename otherwise (e.g. LOAD/UNLOAD in FIDDLE)."
!! To be covered by WNDPOH
!
!  Get input labels
!       Ref:    NMADAT
!
KEYWORD=INPUT_LABELS
        DATA_TYP=J
        IO=I
        NVALUES=256
        SWITCHES=LOOP,WILD_CARDS
        SEARCH=L,P
        PROMPT="Input tape labels"
        HELP="
Specify the tape labels to be read. * specifies all labels on the tape.
.
Remember that WMP-file images are identified by indices grp.obs.fld.chn.seq).
Each of the selected tape labels will be stored in the WMP-file as a separate
field (FLD) in the group (GRP) being created. The CHN, POL and TYP indices will
reflect the nature of the input data, SEQ will be 0.
.
Example:
        INPUT_LABELS=3,6,8 will cause a new GRP to be created in which these
labels will be stored under the image indices
.
        <newgrp>. 0. <chn for label 3>. <pol for label 3>. <typ for label 3>. 0
        <newgrp>. 1. <chn for label 6>. <pol for label 6>. <typ for label 6>. 0
        <newgrp>. 2. <chn for label 8>. <pol for label 8>. <typ for label 8>. 0
"
!! JPH 941005: The old text referred to index 1 as observation i.s.o field. This
!!  is clearly wrong, but it must be checked if the new interpretation is
!!  correct.
!
!
!  Get output label
!       Ref:    NMADAT
!
KEYWORD=OUTPUT_LABEL
        DATA_TYP=J
        IO=I
        SWITCHES=LOOP,WILD_CARDS
        SEARCH=L,P
        PROMPT="Output label"
        HELP="
Specify the first output tape label. If this label already exists, it and all
the subsequent labels will be overwritten.

Specify * or 0 to write the new label behind all existing ones."
!
!
!  Get input/output Fiddle node
!       Ref:    NMADAT
!
KEYWORD=WMP_NODE_1
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,WILD_CARDS,NULL_VALUE
        SEARCH=L,P
        PROMPT="First WMP node name"
        HELP="
Specify the node name for the first Fiddle input set of images."
!! WNDPOH
!
! Get second Fiddle node
!       Ref:    NMADAT
!
KEYWORD=WMP_NODE_2
        DATA_TYP=C
        IO=I
        LENGTH=80
        SWITCHES=LOOP,WILD_CARDS,NULL_VALUE
        SEARCH=L,P
        PROMPT="Second WMP node name"
        HELP="
Specify the node name for the second Fiddle input set of images.
.
Specify * if this is the same as the first node (NODE_1)."
!! WNDPOH
!
!  Get input maps
!       Ref:    NMADAT
!
KEYWORD=WMP_SET_1
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=64
        SWITCHES=LOOP,WILD_CARDS,NULL_VALUE
        SEARCH=L,P
        PROMPT="First image set(s) to be used: grp.fld.chn.pol.typ.seq"
        HELP="
Specify the first image set(s) to be used."
!
!  Get input maps
!       Ref:    NMADAT
!
KEYWORD=WMP_SET_2
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=64
        SWITCHES=LOOP,WILD_CARDS,NULL_VALUE
        SEARCH=L,P
        PROMPT="Second image set(s) to be used: grp.fld.chn.pol.typ.seq"
        HELP="
Specify the second image Set(s) to be used"
!
!  Get area
!       Ref:    NMADAR
!
KEYWORD=AREA
        DATA_TYP=J
        IO=I
        NVALUES=4
        SWITCHES=LOOP,VECTOR,WILD_CARDS,NULL_VALUE
        SEARCH=L,P
        PROMPT="Area: l,m, dl,dm"
        HELP="
Specify the area to be selected:
.
        l, m    position in grid spacings of centre of area
                0,0 is the map centre, increaing toward the upper right (i.e.
                 with DEcreasing RA and INcreasing DEC)
.
        dl, dm  horizontal and vertical area sizes"
!
!  Get data action
!       Ref:    NMAPRT
!
KEYWORD=DATA_ACTION
        DATA_TYP=C
        IO=I
        LENGTH=8
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP,NULL_VALUES
        SEARCH=L,P
        PROMPT="Action to perform on the data"
        OPTIONS=SHOW; NOISE,OFFSET; QUIT
        HELP="
Specify action to perform:
.
        SHOW    show detailed map data
.
        NOISE   calculate noise
        OFFSET  calculate noise and offset
.
        QUIT    quit data part"
!
!  Get polarisation
!       Ref:    NMADAT
!
KEYWORD=MAP_POLAR
        DATA_TYP=C
        LENGTH=4
        IO=I
        NVALUES=4
        CHECKS=OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Select 1 to 4 output-map polarisation(s)|"
        OPTIONS=-
XX,XY,YX,YY; I,L, Q,U,V; XXI,XYI,YXI,YYI; II,LI, QI,UI,VI
        DEFAULTS=XX
        HELP="
Specify up to four polarisations for the maps to make:
.
        XX      XX only
        XY      XY only
        YX      YX only
        YY      YY only
.
        I       Stokes I
        L       'line' Stokes I: Incomplete input data (e.g. no valid XX or YY)
                 will be filled in aassuming that the field is unpolarised
                 (Q=U=V=0)
        Q       Stokes Q
        U       Stokes U
        V       Stokes V
.
Each of the above may be suffixed with 'I' to indicate that visibilities must
be pre-multiplied with sqrt(-1)"
!
!  Get coordinate type
!       Ref:    NMADAT
!
KEYWORD=MAP_COORD
        DATA_TYP=C
        LENGTH=12
        IO=I
        CHECKS=ABBREV_OPTIONS
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Map coordinate system| "
        OPTIONS=B1950_J2000,APPARENT; REFERENCE,AREFERENCE
        HELP="
Specify the coordinate system for the map.
.
There are two choices to be made: The first is whether the map is to be made in
apparent coordinates for the epoch of the observation or in fixed-epoch
coordinates. The latter are fixed for each instrument: B1950 for the WSRT,
J2000 for the ATNF. The second choice is whether the 'reference position' for
the map must coincide with the fringe-stopping centre or is to be specified by
you.
.
The reference position is the position at which the map plane is tangent to the
celestial sphere; it defines the geometry of the map's (l,m) grid in terms of
RA and DEC.
.
The reference position is important for mosaic mapping: The FIDDLE/MOSCOM
operation that combines mosaic subfields into a single large map will only work
if all input maps have the same reference.
.
For a single mosaic, the program by default uses the mosaic centre as the
reference for all subfield maps. However, if you intend to combine multiple
mosaics into a 'super-mosaic', only you can define the common reference centre
that will be needed.
.
You have the following options for your reply:
.
   Reference position defined by the observation (i.e. coinciding with the
    fringe stopping centre):
.
        B1950_J2000   in epoch coordinates for the epoch defined by the
                       instrument with which the observation was made:
                        B1950 for the WSRT
                        J2000 for the ATNF
.
        APPARENT      in apparent coordinates at the time of observation
.
   Reference position to be defined by the user through an additional parameter
    REF_COORD:
.
        REFER         in B1950_J2000 coordinates
.
        AREFER        in APPARENT coordinates. Note that this will not work for
"
!
!  Get reference coordinates
!       Ref:    NMADAT
!
KEYWORD=REF_COORD
        DATA_TYP=D
        IO=I
        NVALUES=2
        SWITCHES=LOOP,VECTOR
        SEARCH=L,P
        PROMPT="Map reference coordinates: RA,DEC (decimal deg)"
        HELP="
Specify (in decimal degrees) the RA and DEC of the reference coordinates to use
in producing the map. The coordinate system is B1950/J2000 or apparent as
defined by your value for parameter MAP_COORD.  "
!
!  Get user comment
!       Ref:    NMADAT
!
KEYWORD=USER_COMMENT
        DATA_TYP=C
        IO=I
        LENGTH=24
        SWITCHES=NULL_VALUES
        SEARCH=L,P
        PROMPT="Comment to be included in map header(s) (<=24 chars)|"
        HELP="
Give, optionally, a descriptive comment for the maps."
!
!  UV special coordinates
!       Ref:    NMADAT
!
KEYWORD=UV_COORDINATES
        DATA_TYP=C
        LENGTH=8
        IO=I
        SWITCH=LOOP
        CHECKS=ABBREV_OPTIONS
        SEARCH=L,P
        PROMPT="UV coordinate system"
        OPTIONS=UV,BASHA,IFRHA
        HELP="
Specify the type of UV coordinates wanted for UV-plane type output
! {\em \textref{OUTPUT}{.output}: COVER, REAL, IMAG, AMPL, PHASE options }
.
        UV      standard UV coordinates: interferometer tracks are ellipses
.
        BASHA   hour-angle (horizontal) and interferometer baseline (vertical)
                 coordinates: interferometer tracks are horizontal lines;
                 redundant baselines overlap
.
        IFRHA   as BASHA, but vertical axis is the interferometer ordinal number
                 in the sequence 01,02,...,0D,12,13,...,CD)"
!! Check redundant overlap
!
! HA resolution
!       Ref:    NMADAT
!
KEYWORD=HA_RESOLUTION
        DATA_TYPE=R
        IO=I
        SWITCH=LOOP
        CHECKS=MAXIMUM,MINIMUM
        MAXIMUM=15.
        MINIMUM=.04
        SEARCH=L,P
!!      UNITS=DEG,RAD,CIR,HMS
        PROMPT="Hour-angle averaging interval (UT seconds)"
        DEFAULTS=.50137
        HELP="
Specify the width in UT degrees of hour angle over which visibilities will be
averaged (to reduce the noise per plotted point).
.
Note:   Observations are taken at multiples of 10 UT seconds and it is
        therefore convenient to specify this parameter in UT seconds as well.
        The number you specify will be converted to a sidereal hour-angle
        interval."
!
!  Baseline resolution
!       Ref:    NMADAT
!
KEYWORD=BAS_RESOLUTION
        DATA_TYP=R
        IO=I
        SWITCH=LOOP
        CHECKS=MAXIMUM,MINIMUM
        MINIMUM=9.
        MAXIMUM=300.
        SEARCH=L,P
        PROMPT="Baseline averaging interval (m)"
        HELP="
Specify the width in metres of baseline over which visibilities will be
averaged.
.
The minimum value is 9, representing the smallest baseline increment ever
present in practice in a (set of) WSRT observation(s). The maximum is
(arbitrarily) fixed at 300."
!
!  Interferometer resolution
!       Ref:    NMADAT
!
KEYWORD=IFR_RESOLUTION
        DATA_TYP=R
        IO=I
        SWITCH=LOOP
        CHECKS=MAXIMUM,MINIMUM
        MINIMUM=.1
        MAXIMUM=16.
        SEARCH=L,P
        PROMPT="Interferometer separation"
        HELP="
Specify the vertical separation in grid points between interferometers"
!! Check what this means
!
!  FFT size
!       Ref:    NMADAT
!
KEYWORD=FT_SIZE
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=2
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=4,4
        MAXIMUM=8192,8192
        SEARCH=L,P
        PROMPT="FFT size"
        HELP="
Specify the size of the Fourier transform in the horizontal and vertical
directions. Note that for 8k*8k maps the number of points/beam should
not be too close to 2 to stay within map size.
.
If the size in both direction is <= 17, a Direct Fourier Transform (DFT) will
be made instead of the standard operation of interpolating onto a rectangular
grid followed by a Fast Fourier Transform (FFT).
.
The standard operation suffers from 'aliasing' artefacts associated with the
periodic nature of the FFT. These artefacts are suppressed, to a level that is
generally acceptable, through a very careful choice of the convolution function
used in the interpolstion to a rectangular grid, but they cannot be avoided
completely.
.
By avoiding the interpolation altogether, the DFT method is free from these
aliasing effects."
!
!  Output size
!       Ref:    NMADAT
!
KEYWORD=OUT_SIZE
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=2
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=16,16
        MAXIMUM=16384,16384
        SEARCH=L,P
        PROMPT="Output map size"
        HELP="
Specify the size in grid points of the output map(s) in the horizontal and
vertical directions."
!
!  Output centre
!       Ref:    NMADAT
!
KEYWORD=OUT_CENTRE
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=2
        SEARCH=L,P
        PROMPT="Output map centre"
        HELP="
Specify the centre of the output map in the l and m direction in pixels with
respect to the mosaic reference position.
.
If you specify an *, you will be prompted for l,m and RA,DEC position"
! ?? OUT_CENTRE <--> LM_CENTRE ?
!
!  Output centre
!       Ref:    NMADAT
!
KEYWORD=LM_CENTRE
        DATA_TYP=D
        IO=I
        SWITCH=LOOP,VECTOR,NULL_VALUES,WILD_CARDS
        NVALUES=2
        SEARCH=L,P
        PROMPT="Output map centre"
        HELP="
Specify the centre of the output map in the l and m direction; in arcsec with
respect to the mosaic reference position.
.
If you specify an *, you will prompted for RA,DEC position"
!
!  Output centre
!       Ref:    NMADAT
!
KEYWORD=RADEC_CENTRE
        DATA_TYP=D
        IO=I
        SWITCH=LOOP,VECTOR
        NVALUES=2
        SEARCH=L,P
        PROMPT="Output map centre: RA,DEC (decimal deg)"
        HELP="
Specify the centre of the output map: RA and DEC in decimal degrees."
!
!  Field size
!       Ref:    NMADAT
!
KEYWORD=FIELD_SIZE
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUES
        SEARCH=L,P
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=.001,.001
        MAXIMUM=180.,180.
        PROMPT="Fieldsize l,m (deg)"
        HELP="
Specify the l and m field size of the map to be transformed. The default will
produce a map with a resolution of about 3.5 grid intervals per
synthesized-beam half-width.
.
If you give a NULL answer (two double quotes), you will be prompted for the
grid steps. "
!
!  Grid size
!       Ref:    NMADAT
!
KEYWORD=GRID_SIZE
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUES
        SEARCH=L,P
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=.001,.001
        MAXIMUM=180.,180.
        PROMPT="Grid interval in l,m (arcsec)"
        HELP="
Specify the l and m grid steps in arcseconds for the map to be made.

In most applications you may define GRID_SIZE as you please. NOTE however, that
any number of maps that you want to combine into a single mosaic (FIDDLE MOSCOM
option) must all share the same GRID_SIZE (as well as the same reference
coordinates, parameters MAP_COORD and REF_COORD)
!!      {\em See \textref{MAP_COORD}{.map.coord},
!!               \textref{REF_COORD}{.ref.coord} }
"
!
!  Taper type
!       Ref:    NMADAT
!  Note: The associated UNIFORM and TAPER_VALUE parameters are in NMAP.PEF
!
KEYWORD=TAPER
        DATA_TYP=C
        IO=I
        LENGTH=8
        SWITCH=LOOP
        CHECKS=ABBREV_OPTIONS
        OPTIONS=GAUSS, LINEAR, OVERR, RGAUSS, NATURAL
!!      DEFAULTS=
        SEARCH=L,P
        PROMPT="Taper type"
        HELP="
The taper is a function of baseline length used to de-emphasize the long
baselines and consequently reduce the near-in sidelobes of the synthesized beam.
.
You may specify the following functions, some of them to be supplemented later
with a baseline-scale parameter TAPER_VALUE:

! {\em see public parameter
!  \textref{TAPER_VALUE}{nmap_public_intfc.taper.value} }
.
        GAUSS    exp -(<baseline>/TAPER_VALUE)**2
                 standard WSRT beam: good compromise between near-in sidelobes,
                  beam width and noise
.
        LINEAR   max (0, 1-baseline/TAPER_VALUE)
.
        OVERR    1 / baseline   (no scale)
.
        RGAUSS   exp -(<baseline>/TAPER_VALUE)**2 / <baseline>
                 broader beam, very low near-in sidelobes, poorer noise
.
        NATURAL  no taper       (no scale)
                 optimum signal/noise ratio, narrower beam with very strong
                  near-in sidelobes
.
NOTES: - Unless you have specified UNIFORM=NATURAL for the UV coverage mode, the
        1/<baseline> density variation of measured visibility points is already
        being accounted for, so OVERR and RGAUSS should not be chosen. LINEAR
        does not combine very well with NATURAL either.

! {\em cf. parameter \textref{UNIFORM}{nmap_public_intfc.uniform} }
"
!
!  Convolution type
!       Ref:    NMADAT
!
KEYWORD=CONVOLVE
        DATA_TYP=C
        LEN=8
        SWITCH=LOOP
        CHECK=ABBREV_OPTION
        OPTIONS=GAUSS,BOX,P4ROL,P6ROL,EXPSINC
        SEARCH=L,P
        PROMPT="Convolution type"
        HELP="
This is the interpolation function to be used in horizontally and vertically
interpolating the observed visibilities onto the rectangular grid to be used in
the Fast Fourier Transform.
.
The choice of function determines the detailed aliasing properties of the
map(s). NMAP chooses appropriate horizontal and vertical width parameters for
each. You may specify one of the following functions:
.
   Gaussian-based:
.
        EXPSINC Sinc*exp on 6*6 grid points: An 'approximation' to the ideal
                 sinc (=sin(x)/x) function. This is the function selected as the
                 default for map-making after extensive experience with all of
                 the options available here.

        GAUSS   Gaussian type over 4*4 grid points: The function used in the
                 first years of WSRT operations; it was later replaced by the
                 prolate spheroids. The expense in computing time is the same as
                 for P4ROL.
.
   Prolate spheroids: These function minimise the 'power' (= the integral of the
    intensity squared) 'aliased in' from sources outside the map
.
        P4ROL   Prolate spheroidal function with 4*4 grid points.
        P6ROL   Prolate spheroidal function with 6*6 grid points: By using more
                 points in the interpolation, this function pushes the aliasing
                 down considerably, - at the expense of a factor two or more in
                 computing time for the interpolation
.
All the above functions may also be used in constructing a UV-plane for display
(UV_COORDINATES=UV).
.
For plotting visibilities versus baseline or interferometer
(UV_COORDINATES=BASHA or IFRHA), they are of little use, and the default one
would normally select is
.
        BOX     Shift to nearest grid point.
! {\em see parameter \textref{UV\_COORDINATES}{.uv.coordinates} }
"
!!!
!!!  Specify the convolution width
!!!       Ref:    NMADAT
!!!
!!KEYWORD=CONVOL_WIDTH
!!      DATA_TYP=R
!!      IO=I
!!      SWITCH=LOOP,VECTOR
!!      NVALUES=2
!!      MIN_NVALUES=2
!!      CHECKS=MINIMUM,MAXIMUM
!!      MINIMUM=.5,.5
!!      MAXIMUM=8.,8.
!!      SEARCH=L,P
!!      PROMPT="Convolution width"
!!      HELP="
!!Specify the number of grid intervals at which to truncate the convolution
!!function in the U and V coordinate directions."
!
!  Specify correction for convolution
!       Ref:    NMADAT
!
KEYWORD=DECONVOLVE
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUES
        SEARCH=L,P
        PROMPT="Correct map for convolution taper (Yes/No) ?"
        DEFAULTS=YES
        HELP="
The interpolation (convolution) in the visibility domain results in a
multiplication ('tapering') of the output map(s) and antenna pattern(s) by the
Fourier transform of the convolving function; i.e., toward the edge of the map
the sources, sidelobes and grating responses appear weaker than they actually
are.
.
By default this effect will be corrected for by dividing the map through the
taper. A side effect of this correction is that the noise, which is uniform
over the whole uncorrected map, is amplified toward the map edges.
.
Here you are given the option to bypass this correction. e.g. because uniform
noise is more important for your application than source fluxes. "
!
!  Specify data details
!       Ref:    NMADAT
!
KEYWORD=QDATAS
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUES
        SEARCH=L,P
        PROMPT="Special data selection (Yes/No) ?"
        DEFAULTS=NO
        HELP="
Maps are normally made directly from the .SCN-file visibilities. Answering YES
here gives you access to some specials including
.
        - making a map from model visibilities in the .SCN file;
        - selecting visibilities from an annulus in the UV plane;
        - clipping extreme amplitudes in an annulus in the UV plane;
        - shifting the pointing centre to which the visibilities refer (and
           consequently the centre of the map to be made from them)."
!
!  Define data to use
!       Ref:    NMADAT
!
KEYWORD=USER_DATA
        DATA_TYP=C
        LENGTH=8
        IO=I
        SWITCH=LOOP
        CHECKS=ABBREV_OPTIONS
        OPTIONS=STANDARD,MODEL
        SEARCH=L,P
        PROMPT="Visibilities to use"
        HELP="
Specify the type of visibilities to use:
.
        STANDARD  observed visibilities
.
        MODEL     model visibilities (to be specified later with type=0
sources) "
!
!  Clipping
!       Ref:    NMADAT
!
KEYWORD=CLIPPING
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        PROMPT="Clipping?"
        HELP="
'Clipping' means discarding data in a certain annulus that fall within a
certain range of values (yet to be specified).
! {\em parameters \textref{CLIP_AREA}{.clip.area},
!                 \textref{CLIP_LEVELS}{.clip.levels} }
.
It is a simple (and primitive) method of eliminating data affected by strong
interference. (Note that NFLAG provides a much wider scala of operations to
find and suppress interference.)"
!
!  Clip area
!       Ref:    NMADAT
!
KEYWORD=CLIP_AREA
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR
        CHECK=MINIMUM
        MINIMUM=0.,0.
!!      UNITS="m"
        PROMPT="UV-radius range for clipping (m)"
        DEFAULTS=0.,100000.
        SEARCH=L,P
        HELP="
Specify the (circular) UV-plane radii (in metres) between which you want to
clip the data. The default is to clip everywhere."
!
!  Clip levels
!       Ref:    NMADAT
!
KEYWORD=CLIP_LEVELS
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR
        CHECKS=NON_DESCENDING
        SEARCH=L,P
        PROMPT="Amplitude range to be discarded"
        DEFAULTS=100000.,100000.
        HELP="
Specify amplitude range (in Westerbork Units) of visibility magnitudes that you
want to discard.
.
In the annulus defined by CLIP_AREA, values between the limits you specify will
be discarded.
! {\em parameter \textref{CLIP_AREA}{.clip.area} }
.
NOTE: It would be more natural to define a range within which visibilities are
considered valid. As it is, only the lower limit is actually useful, allowing
you to define a rejection threshold for interference. To do so, specify your
threshold for the lower and 'infinity' for the upper limit, e.g.
.
                <threshold>,100000 "
!
!  Source subtraction
!       Ref:    NMADAT
!
KEYWORD=SUBTRACT
        DATA_TYP=L
        IO=I
        SEARCH=L,P
        PROMPT="Model subtraction (Yes/No) ?"
        HELP="
Reply YES if you want to subtract a source model. You will then be prompted to
provide details on the model you want to subtract.
! {\em see the \textref{NMODEL HANDLE}{nmodel_public_intfc} interface }
"
!
!  Field centre shifts
!       Ref:    NMADAT
!
KEYWORD=FIELD_SHIFT
        DATA_TYP=R
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUES
        DEFAULTS=0.,0.
        SEARCH=L,P
        PROMPT="l,m field shift (arcsec)"
        HELP="
Specify the field-centre shift in l,m coordinates.
.
(l,m) are 'horizontal' and 'vertical' Cartesian coordinates in a plane tangent
to the celestial sphere at the reference centre. The coordinate system is
B1950/J2000 or apparent as defined by your value for parameter MAP_COORD.
!!      {\it See \textref{MAP_COORD}{.map.coord},
!!               \textref{REF_COORD}{.ref.coord} }
.
If you enter a null value (\ or ""), you will be prompted for a FIELD_CENTRE
instead. This option is intended for instrumental test programs only and has
not been tested for general applications; use it at your own risk if you wish. "
!
!  Field centre shifts
!       Ref:    NMADAT
!
KEYWORD=FIELD_CENTRE
        DATA_TYP=D
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUES
        SEARCH=L,P
        PROMPT="Field centre: RA,DEC (decimal deg)"
        HELP="
Specify the map centre wanted in the apparent-coordinate frame.

Default is the fringe-stopping centre. "
!
!  Data type used
!       Ref:    NMADAT
!
KEYWORD=DATA_TYPE
        DATA_TYP=C
        LEN=8
        SWITCH=LOOP
        CHECKS=ABBREV_OPTION
        OPTION=NORMAL, COS,SIN, AMPL,PHASE
        DEFAULTS=NORMAL
        SEARCH=L,P
        PROMPT="Data transformation for display"
        HELP="
Specify how to transform the complex input visibilities. NORMAL is the default;
the others are for special experiments and diagnostics only.
.
        NORMAL  Complex value
.
        COS     Real part
        SIN     Imaginary part
.
        AMPL    Amplitude
        PHASE   Phase "
!
!  Output maps
!       Ref:    NMADAT
!
KEYWORD=OUTPUT
        DATA_TYP=C
        LENGTH=8
        NVALUES=8
        SWITCH=LOOP
        CHECKS=ABBREV_OPTION
        SEARCH=L,P
!!      PROMPT="Specify up to 8 output image types"
!!      PROMPT set by WNDPOH
        OPTIONS=MAP,AP; COVER; REAL,IMAG, AMPL,PHASE
!!      DEFAULT=MAP,AP
        HELP="
Specify one or more output types:
.
   Standard image types for map-making:
        MAP     Output (a) map(s)
        AP      Output (an) antenna pattern(s)
.
   Visibility-domain outputs, for diagnostics only:
        COVER   Output the 'antenna-pattern' convolved visibilities
.
        REAL    Output the real part of the convolved visibilities
        IMAG    Output the imaginary part of the convolved visibilities
.
        AMPL    Output the amplitude of the convolved visibilities
        PHASE   Output the phase of the convolved visibilities"
!
!
!  Get old R-series data type
!       Ref:    NMADAT
!
KEYWORD=OLD_DATTYP
        DATA_TYP=J
        IO=I
        SWITCHES=LOOP
        SEARCH=L,P
        PROMPT="Old R-series data format"
        HELP="
Specify the old R-series data type:
.
        0       local
        1       VAX, D_FORMAT
        2       VAX, G_FORMAT
        3       ALLIANT
        4       CONVEX
        5       IEEE
        6       DEC station
        7       SUN station
        8       HP  station"
!-
INCLUDE=NGEN_PEF        !
INCLUDE=NSHOW_PEF       !
!-
INCLUDE=NMODEL_PEF      !
INCLUDE=MDLNODE_PEF     !
!-
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF      !
!
INCLUDE=NMAP_PEF        !
INCLUDE=WMPNODE_PEF     !
INCLUDE=WMPSETS_PEF     !
!
INCLUDE=UNIT_PEF
!-

