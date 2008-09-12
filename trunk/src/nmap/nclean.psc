!+ NCLEAN.PSC
!  WNB 910809
!
!  Revisions:
!       WNB 910828      Add RUN
!       WNB 910909      Add DATAB and INFIX
!       WNB 910913      New (de-)apply and loops
!       WNB 911007      Include instrum. pol.
!       WNB 911230      NMODEL
!       WNB 920106      Add UVCOVER
!       WNB 920114      Add restore
!       WNB 920626      Add DCLOW, change Rotation measure description
!       WNB 921104      Add J2000
!       WNB 921202      Add DATA clean; change MEMORY_USE to CMEMORY_USE
!       WNB 921211      Make PSC
!       WNB 921216      Add GRATING_FACTOR
!       JEN 930308      INCLUDE=NSETS_PEF, remove keyword MAPS,MAP
!       JEN 930308      change keyword AP into AP_SET
!       JEN 930312      INCLUDE=NCOMM_PEF
!       JEN 930312      Remove keyword(s) INPUT_MAP
!       HjV 930426      Change name keyword AP_SET
!       CMV 931116      Add DMEMORY_USE for second try if too small
!       JPH 940221      Comment out BEAM_TYPE. - Correct language
!       CMV 940810      Removed max. check for memory size
!       JPH 940913      Correct antenna-pattern prompt
!                       Remove () from prompts
!       JPH 941018      Reorganisation of PEF files
!       JPH 941025      Remove SELECT_PEF.
!       JPH 941028      Shift in-line comments to new lines.
!                       Newlines between HELP=" and text
!       HjV 941115      Use entire MDLNODE_PEF
!       JPH 941201      Help texts, prompt formatting
!       CMV 950306      Included HA_RANGE once more (SELECT_PEF)
!       HjV 950512      Add DATA_FACTOR
!       JPH 960411      Merge changes from 941025 to present
!	JPH 991018	Extend memory space [C]MEMORY_USE from 4 to 32 MB
!	CMV 050115	Extend memory space to 128MB, components to 100k
!
!
! Work memory size
!       Ref:    NCLDAT
!
!
KEYWORD=CMEMORY_USE
        DATA_TYP=J
        IO=I
        SWITCH=NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=32000
        MAXIMUM=128000000
        SEARCH=L,P
        DEFAULT=150000 /NOASK
        PROMPT="UV-clean memory size"
        HELP="
Specify the work memory size in bytes for the UV Clean option, to be allowed in
defining the beam patch and to be used in executing the Fourier transforms.
.
The default shown is normally adequate; a larger value may speed up the
execution of major cycles in UV Clean.
!.
!NCLEAN will not accept a value in excess of 32000000 (32 MB).
"
!
! Work memory size
!       Ref:    NCLDAT
!
KEYWORD=DMEMORY_USE
        DATA_TYP=J
        IO=I
        SWITCH=NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=32000
        MAXIMUM=32000000
        SEARCH=L,P
        DEFAULT=300000
        PROMPT="Work memory size"
        HELP="
The memory workspace in bytes needed is 12 times the size of the rectangle
enclosing all selected areas in the map plane. NCLEAN normally allocates up to
300 KB for this purpose. To satisfy the present need, you must either accept
the value suggested here or specify a smaller set of areas, or both.
!.
!NCLEAN will not accept a value in excess of 32000000 (32 MB).
"
!
! Specify clean type
!       Ref:    NCLDAT
!
KEYWORD=OPTION
        DATA_TYPE=C
        LENGTH=24
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        CHECKS=ABBREV_OPT
        OPTIONS=BEAM,UVCOVER,DATA, UREST; COMPON,HISTO; QUIT
        PROMPT="Operation wanted"
        HELP="
CLEANing:
.
   BEAM         Hogbom clean: Use the map and the beam to clean. This
                 method is inaccurate away from the map centre due to the
                 aliasing associated with visibility gridding.
.
   UVCOVER      Standard Clark Clean: Find and provisionally subtract sources
                 in some map areas first (minor cycles), then properly subtract
                 the sources found (major cycle). The number of minor cycles
                 between major cycles is determined by the program; the user can
                 steer this through a few control parameters.
                The method suffers from the same limitations as BEAM, but is
                 faster for cleaning extended sources.
.
   DATA         Cotton-Schwab Clean: Find sources as in UVCOVER, but perform
                 the major cycle on the original SCN-file visibilities, making a
                 new map from the residuals as input for the next major cycle.
                 This method rigorously avoids the aliasing limitations of BEAM
                 and UVCOVER, at the price of being very much slower.
.
   UREST        Use a clean component list and a map to restore the clean
                 components in the map.
.
For exploring the data before committing more serious work:
.
   HISTO        Produce only a histogram of selected areas in the map and/or
                 antenna pattern
.
   COMPON       Execute minor cycles as for UVCOVER, but omit the following
                 major cycle. The result is a source model and a map in which
                 these sources are provisionally subtracted in selected areas.
                It is the fastest of the four CLEAN variants but produces an
                 inaccurate residual map.
                This option may be used to quickly get a feel for the
                 minor-cycle control of UVCOVER and DATA Clean, or to make an
                 initial data model for NCALIB SELFCAL or NMODEL UPDATE.
.
Other options:
.
   QUIT          Terminate NCLEAN."
!
!
!  Get input map
!       Ref:    NCLDAT
!
KEYWORD=WMP_SETS0
!! should be INPUT_MAP
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=1                               !!was 32
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="ONE input map: grp.fld.chn.pol.0.seq"
        HELP="
Specify ONE input map (group.field.channel.polar.type(=0).sequence_number)"
!
!
!  Get input antenna pattern
!       Ref:    NCLDAT
!
KEYWORD=AP_WMP_SET
!! should be ANTENNA_PATTERN
        DATA_TYP=C
        IO=I
        LENGTH=32
        NVALUES=1                       !!was 32
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        PROMPT="ONE antenna pattern (grp.fld.chn.pol.1.seq)"
        HELP="
Specify ONE antenna pattern
                (group.field.channel.polarisation.type(=1).sequence_number)"
!
! Specify clean sub-type
!       Ref:    NCLDAT
!
!!KEYWORD=BEAM_TYPE
!!      DATA_TYPE=C
!!      LENGTH=24
!!      IO=I
!!      SWITCH=LOOP,NULL_VALUES,WILD_CARDS
!!      SEARCH=L,P
!!      CHECKS=ABBREV_OPT
!!      OPTIONS=FULL,PATCH
!!      PROMPT="Beam action option"
!!      HELP="
!!Specify the type of clean to do:
!!
!!PATCH Use in the clean a contiguous patch area of the beam. This produces
!!      a faster minor cycle, but maybe more major cycles if there are
!!      grating rings present
!!FULL  Use in the clean all points in the beam above a certain level.
!!
!!******************* This option is not yet implemented *******************"
!
! Deconvolution
!       Ref:    NCLDAT
!
KEYWORD=DECONVOLUTION
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUES,WILD_CARDS
        SEARCH=L,P
        DEFAULT=NO /NOASK
        PROMPT="Correct antenna pattern for mapping taper: YES/NO"
        HELP="
The gridding convolution in map-making is usually compensated for by
multiplying the output map and antenna pattern with a taper function that rises
toward the map edges.
.
If this is the case, answering YES here will instruct the program to account
for this effect; this will reduce the aliasing errors in the residual map,
allowing you to clean a somewhat larger part of the map. YES will in general
produce a result with less aliasing, NO the reverse."
!
! Log components
!       Ref:    NCLDAT
!
KEYWORD=COMPON_LOG
        DATA_TYP=J
        IO=I
        NVALUES=2
        SWITCH=LOOP,VECTOR,NULL_VALUES,WILD_CARDS
        CHECKS=MINIMUM
        MINIMUM=0,0
        SEARCH=L,P
        PROMPT="Reporting interval for components found: | terminal, printer"
        HELP="
You may give two numbers <n> here, indicating that every <n>-th component must
be reported. The first number applies to your terminal window, the second to
the log file. A value of 0 means 'no reporting at all.
.
Example:
        2,0 specifies that every other component will be typed, and none
logged. "
!! default?
!
! Major cycle step depth
!       Ref:    NCLDAT
!
KEYWORD=CYCLE_DEPTH
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        CHECK=MAXIMUM,MINIMUM
        MAXIMUM=1.
        MINIMUM=.001
        DEFAULT=.05 /ASK
        PROMPT="Major cycle depth"
        HELP="
Specify the level relative to the initial map maximum in the CLEAN window to
which you want to clean in one major cycle."
!! maximum in first or in current major cycle??
!
! Data multiplication factor
!       Ref:    NCLDAT
!
KEYWORD=DATA_FACTOR
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        DEFAULT=1. /ASK
        PROMPT="Data multiplication factor"
        HELP="
Specify the factor by which to multiply the input map-data.
.
This option is only relevant for DATA clean. The first input-map and all maps
created by DATA clean will be multiplied by this factor."
!
! Map multiplication factor
!       Ref:    NCLDAT
!
KEYWORD=MAP_FACTOR
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        DEFAULT=1. /ASK
        PROMPT="Multiplication factor"
        HELP="
Specify the factor by which to multiply the residual map before restoring.
.
Normally one should use the default value of 1; a value of 0 serves to make a
map of the CLEAN components only."
!
! Clean limit
!       Ref:    NCLDAT
!
KEYWORD=CLEAN_LIMIT
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARD
        SEARCH=L,P
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=.00001
        MAXIMUM=1.
        DEFAULT=.1 /ASK
        PROMPT="Limit in fraction of map maximum"
        HELP="
Specify the level to which to clean in fraction of the initial map maximum."
!
! Number of components
!       Ref:    NCLDAT
!
KEYWORD=COMPON_LIMIT
        DATA_TYP=J
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=1
        MAXIMUM=100000
        DEFAULT=100 /ASK
        PROMPT="Maximum number of components to find"
        HELP="
Specify the maximum number of components to be cleaned."
!
! Clean factor
!       Ref:    NCLDAT
!
KEYWORD=LOOP_GAIN
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUES,WILD_CARD
        SEARCH=L,P
        CHECKS=MINIMUM,MAXIMUM
        MINIMUM=.01
        MAXIMUM=1.
        DEFAULT=.4 /ASK
        PROMPT="Clean loop-gain factor"
        HELP="
For each component found, the antenna pattern shifted to its position is
subtracted from the CLEAN window. To prevent overshoots, it is customary to
subtract not the complete component but only a fraction of it. The magnitude of
this fraction is defined here. "
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
        PROMPT="Window area l,m,dl,dm"
        HELP="
The CLEAN window is defined as the union of up to 32 rectangular areas (which
may arbitrarily overlap). For BEAM clean this is also the window within which
source responses will be subtracted.
.
You are being prompted for these areas one by one until you give a null reply
(<CR> only). An area is specified by four numbers: l,m, dl,dm, where
.
   l,m     l and m in grid points for the area centre; (l,m)=(0,0) at the map
            centre, increasing toward the upper right (i.e. with decreasing RA
            and increasing DEC)
   dl,dm   width and height of the area in grid points "
!
! Major cycle grating correction
!       Ref:    NCLDAT
!
KEYWORD=GRATING_FACTOR
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        CHECK=MINIMUM
        MINIMUM=0.
        DEFAULT=1. /ASK
        PROMPT="Grating factor"
        HELP="
In the minor cycles of UVCOVER, DATA and COMPON clean, the maximum error made
in only subtracting part of the antenna pattern is estimated by <number of
sources> times <ypical sidelobe level in antenna pattern> times GRATING_FACTOR.
.
You may lower this latter factor if you are not worried about the effects of
far-out sidelobes on the minor-cycle cleaning process. A lower value will allow
more source components to be collected in minor cycles before a major cycle
must be started."
!!
!!When determining the end of a minor cycle, the program also looks at the
!!total influence of parts outside the beam patch on the remaining data. By
!!lowering this factor, this influence will be taken only partly into
!!account. Make it small if you are not worried about the total influence of
!!far-out sidelobes on the result."
!
! Get prussian hat
!       Ref:    NCLDAT
!
KEYWORD=PRUSSIAN_HAT
        DATA_TYP=R
        IO=I
        SWITCH=LOOP,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        CHECKS=MAXIMUM,MINIMUM
        MAXIMUM=.5
        MINIMUM=0.
        PROMPT="Prussian hat value"
        HELP="
For extended sources a prussian hat (i.e. a nominal additional peak value on
the central value of the antenna pattern) may give better clean results. Values
of .1 to .4 could be tried."
!                       \whichref{Cornwell}
!
! Get residual wanted
!       Ref:    NCLDAT
!
KEYWORD=RESIDUAL
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUE,WILD_CARD
        SEARCH=L,P
        PROMPT="residual map?"
        HELP="
Specify if the residual map must be written.
.
For BEAM and UVCOVER cleaning, this map is an automatic by-product of the
process and the choice is whether or not to write it to the .WMP file. The map
will be given the same indices as the input map except for an incremented
sequence number.
.
For UVDAT cleaning, the residual map must be constructed by making a new map
from the original visibilities, in which the CLEAN components just found are
subtracted. The new map will OVERWRITE the input map."
!               {\em See elsewhere for the\
!               \textref{rationale}{nclean_descr.residual.map} }
!
! Get restored wanted
!       Ref:    NCLDAT
!
KEYWORD=RESTORE
        DATA_TYP=L
        IO=I
        SWITCH=NULL_VALUE,WILD_CARD
        SEARCH=L,P
        PROMPT="restored map (YES/NO)?"
        HELP="
Specify if a restored map must be written.
.
A restored map consists of the CLEAN components convolved with a hypothetical
beam that has no sidelobes, superimposed on the residuals. It is an
approximation to  what you would have observed with complete contiguous UV
coverage (hoth in hour angle and baseline) up to the longest baseline available.
.
You will be given the option to suppress the residuals (parameter MAP_FACTOR),
in which case you get a map of the CLEAN components only."
!
! Get restore beam
!       Ref:    NCLDAT
!
KEYWORD=RESTORE_BEAM
        DATA_TYP=R
        IO=I
        NVALUES=3
        SWITCH=LOOP,VECTOR,NULL_VALUE,WILD_CARD
        SEARCH=L,P
        PROMPT="dl, dm arcsec, pa deg"
        HELP="
Specify the restore beam width:
.
   dl      width of beam in arcsec (full-halfwidth)
.
   dm      width of beam in arcsec (full-halfwidth)
.
   pa      position angle of skewed beam in degrees (anti-clockwise;
            0 deg is horizontally to the right (+l direction)
.
The default beam is a two-dimensional Gaussian truncated at a level of .xx
relative to the maxiumum. If an antenna pattern for your map is available, the
l,m beam widths are derived from it; else a rule-of-thumb formula is used:
.
        half-width = 12 arcsec * 1400/frequency(MHz) "
!! How about RA, DEC widths?
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
!-
INCLUDE=NMAP_PEF        ! for data clean
!-
INCLUDE=NGEN_PEF
!-
INCLUDE=WMPNODE_PEF     !
INCLUDE=WMPSETS_PEF     !
!-
INCLUDE=MDLNODE_PEF     !
INCLUDE=NMODEL_PEF      !
!-
INCLUDE=SCNNODE_PEF:SCN_NODE    !
INCLUDE=SCNSETS_PEF             !
INCLUDE=SELECT_PEF
!-
