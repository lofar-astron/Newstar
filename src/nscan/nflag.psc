!+ NFLAG.PSC
!  WNB 930618
!
!  Revisions:
!       WNB 930619      Add OPERATION_2, TOTEL, RED, NORED, RRESID
!       WNB 930623      Add SHOW DATA T option
!       WNB 930713      Text only
!       JEN 930721      Improvement of flagging help-text
!       JEN 930803      Add Operation ARESID
!       JEN 930811      Add Operations SHOW,STAT,STH
!       JEN 930811      Put CLIP/NOIS operations into new OPERATION_2
!                       Rename old OPERATION_2 to OPERATION_3
!       JEN 930818      Add operation DT1
!       JEN 930820      Add operation RESET
!       JEN 930820      Add keyword DRY_SCANS
!       JEN 930821      Disabled FORCE options in FLAG_MODE (tentatively)
!       CMV 930826      Enable FORCE options with improved help text
!       JEN 930912      Drastic reorganisation of flagging user interface:
!                       FLAG_OPTION hase become the central pivot for:
!                       - FLAG_MODE (....)
!                       - OPS_FLIST (interactions with flag list)
!                       - OPS_MANUAL (manual flagging operations)
!                       - OPS_CLIP (data-driven flagging operations)
!                       - OPS_NOISE (data-driven flagging operations)
!                       - OPS_DETERM (deterministic flagging operations)
!                       - OPS_COPY (copy flags within hypercube)
!       JEN 930924      Add OPS_INSPECT
!       CMV 931116      Moved SHOW-keywords to NSHOW_PEF
!       JEN 931117      Added keyword SUB_CUBE
!       JEN 931122      Added keyword PBAS_LIMITS, SHADOW_DIAM
!       JEN 931122      Added keyword DT1_LIMITS, CLIP_LIMIT
!       JEN 931123      Added keyword ABCS_LIMITS
!       JEN 931126      Introduced OPS_SCANS for all Scan-header ops.
!       JEN 940216      Added DRYRUN/NODRY options to FLAG_MODE
!       JEN 940216      Added QXY options to OPS_CLIP
!       CMV 940707      Removed SCANS option from MANUAL, added HARANGE
!       JEN 940714      Added extra STATISTICS options
!       CMV 940817      Added ELEVATION option
!       JPH 940913      Remove () from prompts
!       JPH 940923      NSETS --> SCNSETS
!       JPH 940927      Disable CBI in OPS_DETERM (because not available)
!       JPH 940929      Disable CHAN in OPS_INSPECT (because not available)
!                       Text changes. NOCO --> NOCORR, NODRY --> NODRYRUN
!       JPH 941020      SCNNODE_PEF. SELECT_PEF
!       JPH 941202      Extensive reworking of HELP texts. Prompt formatting.
!       JPH 941222      / in OPTIONS --> ,
!       JPH 950109      Add USER_FLAG (which got lost somewhere?). MDLNODE_PEF
!       JPH 951011      Help texts, formatting. Remove UNITS
!       JEN 960225      Added UXY,VXY options to OPS_CLIP
!       JPH 960402      Overdue checkin; format and text changes relative to
!                        version of 960130 in master system
!
!  Get overall action
!       Ref:    NFLDAT
!
KEYWORD=OPTION 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="NFLAG main branch" 
        OPTIONS=FLAG; SHOW, QUIT 
        HELP=" Specify the nature of the operation you want to perform:
.
        FLAG:    Set, clear and/or browse data flags in a .SCN file 
                  and/or browse its data statistics 
.
        SHOW:    Show/edit data and header information in .SCN file. This
option 
                  is a clone of the SHOW option in NSCAN and is available here 
                  for convenience. 
.
        QUIT:    Terminate NFLAG " 
!
!
!  Specify subset of hypercube
!       Ref:    NFLCUB
!
KEYWORD=SUB_CUBE 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Type of secondary data cube" 
        OPTIONS=YES, IFR,POL,HA; NO 
        HELP=" You may define a 'secondary cube' that includes only part of the
primary data cube. The current flagging operation will affect only the cross
section of the primary and secondary cubes.
.
Unlike the primary cube (which can only be redefined through the FLAG_MODE
parameter), the secondary cube definition applies only to the current operation
and will evaporate when it completes. 
.
Specify here how you want to define the secondary cube. You may give ONE option
at a time; the prompt will reappear until you reply with NO. 
.
        YES  Polarisations, interferometers and hour-angle range. 
        IFR  Interferometers 
        POL  Polarisations 
        HA   Hour-angle range 
        NO   Accept the current settings 
!!              Check volatility, interpretation of NO, one at a time and
!!              defaulting
!       \whichref{}{}
" 
!
!  Get input file
!       Ref:    NFLFL5
!
KEYWORD=INPUT_FILE 
        DATA_TYP=C 
        IO=I 
        LENGTH=80 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Input .FLF file name (including extension)" 
        HELP=" Specify the full name of the input binary-flags file. The
recommended filename extension is .FLF."
!!              Why not automatic .FLF as for other types?
!
!  Get output file
!       Ref:    NFLFL5
!
KEYWORD=OUTPUT_FILE 
        DATA_TYP=C 
        IO=I 
        LENGTH=80 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Output .FLF file name (including extension)" 
        HELP=" Specify the full name for the output binary-flags file. The
recommended filename extension is .FLF."
!
!  Get flag option
!       Ref:    NFLFLG
!
KEYWORD=FLAG_OPTION 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Type of flagging operation|" 
        OPTIONS=-
CLIPDATA, DETERM; CLEAR, MANUAL, HASCANS, FCOPY;|-
FLIST; MODE; INSPECT, STATIST, QUIT 
        DEFAULT=INSPECT 
        HELP=" Specify the type of flagging operation to be performed:
.
   Selective flagging on the basis of data/coordinate/statistics values: 
.
        CLIPDATA  Flag data points according to a data-derived criterion 
                   (amplitudes, selfcal residuals, etc.) 
        DETERM    Flag data points on the basis of their coordinates (position, 
                   elevation, etc.) 
.
   Deterministic clearing, setting and copying of flags: 
.
        CLEAR     Clear flags from scan headers and data (see also MANUAL) 
        MANUAL    Manual flagging operations (includes CLEAR flags). 
!!                      What is this about MANUAL vs CLEAR?
        HASCANS   Operations on the scan-header flags. These affect ENTIRE 
                   scans, and in doing so IGNORE the interferometers (parameter 
                   IFRS) and polarisations (parameter SELECT_XYX) selections 
                   that you defined for your primary/secondary data cubes. 
        FCOPY     Copy flags from one place in the hypercube to another. 
!!                      From somewhere into the hypercube?
!!                      From the hypercube to somewhere else?
.
   Operation on the flag list in core: 
.
        FLIST     Operations on the internal flag list 
                   (including reading-from/writing-to a .FLF file). 
.
   Define operating environment: 
.
        MODE      Go to FLAG_MODE parameter to change 'environment' parameter 
                   values. This includes re-definition of the primary data
cube. 
.
   Navigation: 
.
        INSPECT   Go to the flag-INSPECTion group of operations. 
        STATIST   Go to the data-STATISTICS group of operations. 
.
        QUIT      Return to primary OPTION. " 
!
!  Get flag mode
!       Ref:    NFLFLG
!
KEYWORD=FLAG_MODE 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        OPTIONS= -
HYPER, SECTORS, NODE; CORR, NOCORR, UFLAG;|-
DRYRUN,NODRYRUN, SHOW,NOSHOW, NOTRACE,TRACE; QUIT 
        DEFAULT=QUIT 
        PROMPT=-
"Set ONE environment parameter at a time; QUIT or <CR> when done|" 
        HELP=" You may select one of the options at a time.
 The logical ones, such as CORR/NOCORR will take effect immediately. For the
others, you will be prompted for (a) new value(s).  The values shown below as
defaults are the ones with which NFLAG starts up. 
.
   Redefinition of the primary data cube: 
.
        HYPER:    Change the primary data cube by selecting new polarisation, 
                   interferometer and hour-angle ranges. 
        SECTORS:  As HYPER but including a change of the SCN_SECTORS selection. 
        NODE:     Completely new primary data cube (i.e. including new .SCN 
                   file) 
.
   Data-correction modes. Shown in parentheses is NFLAG's initial setting: 
.
        CORR/NOCORR (=NOCORR): 
                Reading mode for visibility data: without/with corrections 
                 applied and ignoring/acknowledging flags. 
                 (NOCORR mode will be ignored in those cases for which the use 
                 of uncorrected data would be pointless, e.g. ARESID, RRESID). 
.
        UFLAG (=ALL): 
                Default flag types to be acknowledged when reading data in 
                 CORR mode. 
.
   Run modes: 
.
        SHOW/NOSHOW (=SHOW): 
                After each flagging operation, show/don't show a summary of the 
                 new flag settings for the data cube and flag types selected
for 
                 that operation. 
                Note that in either mode the affected flags will be counted 
                 anyway, so you can use INSPECT afterwards to show summatries 
                 from various perspectives 
.
        DRYRUN/NODRYRUN (=DRYRUN): 
                For any operation that would change flags in the .SCN file, 
                 do/don't do a 'trial run' first, - e.g. to get a feeling for 
                 sensible default values for clip limits etc. 
                 (For NODRYRUN safe default values will be chosen for such 
                 limits so that nothing will happen until the user sets his own 
                 values.) 
.
        NOTRACE/TRACE (=NOTRACE): 
                Dont't/do trace the flagging operations through messages on the 
                 terminal. TRACE is likely to produce a lot of output and 
                 intended for debugging purposes only. 
.
   Navigation: 
.
        QUIT:    Go back to FLAG_OPTION." 
!
!  Operation on the internal flag list:
!       Ref:    NFLFLG
!
KEYWORD=OPS_FLIST 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Flag-list operation|" 
        OPTIONS=-
DELETE, GET,PUT; LOAD,UNLOAD, READ,WRITE; HEADER, LIST, LCOUNT;|-
INSPECT, STATIST, MODE, CLEAR, QUIT 
        HELP=" Specify operations on the internal flag list: The entries in
this list represent flagging instructions. Each entry contains a flag type, and
ranges for the secondary data cube (i.e. hour angle, polarisation,
interferometer and channel ranges) to which it applies.
.
   Flagging operations involving the internal flag list: 
.
        DELETE  Clear the entire list. 
.
        GET     Collect flags of specified type(s) from the specified primary 
                 data cube, translate them into flaqg-list entries and merge 
                 them into the list. 
                The default flag type is ALL, but it can be changed (parameter 
                 FLAG_MODE). 
!                       {\em see \whichref{paremeter FLAG_MODE}{} }
!!                      correct?
.
        PUT     Use he entries in the internal flag list to set flags for 
                 selected visibilities or scans. 
                If an entry in the list results in setting the flags on all
data 
                 points in a scan, the corresponding scan-header flag is set 
                 instead for efficiency reasons. 
.
   Transfer of the flag list to/from an external file. Two types of file may be 
   used: 
.
      The .FLF file is in a compact binary format that is efficient in both 
      operating speed and disk-space. It is accessed through: 
        LOAD    Merge contents of a .FLF file into the internal flag list 
        UNLOAD  Write the internal flag list to a new .FLF file 
.
     The list can also be stored in an ASCII file which you can inspect and
edit 
     as you please. ASCII files are bulky and take much more time to process. 
     They are accessed through: 
        READ    Merge contents of an ASCII flag-list file into the internal
flag 
                 list 
        WRITE   Write the internal flag list to a new ASCII flag-list file 
.
   Inspection: 
.
        HEADER  Show the contents of the flag-list header. 
        LIST    Show the contents of the flag-list (can be long!).
!!                       Implement control-C
        LCOUNT  Count the flags in the flag list (NOT in the data!).
                 These can be INSPECTed in various one-dimensional projections. 
.
   Navigation: 
.
        INSPECT Make detour into the (flag) INSPECTion section of the program 
        STATIST Make detour into the (data) STATISTics section of the program 
        MODE    Make detour into the environment MODE-control section of the 
                 program 
        CLEAR   Go to the CLEAR-flags operation. 
        QUIT    Go back to what you were doing before. " 
!
!  Manual flagging operations
!       Ref:    NFLFLG
!
KEYWORD=OPS_MANUAL 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Manual-flagging operation|" 
        OPTIONS=-
CLEAR, CLDAT, CLHEAD; UVDATA, HARANGE; INSPECT, STATIST, MODE, QUIT 
        HELP=" Specify one of the manual flagging operations. Remember that
only the selected primary/secondary data cube is affected. Make sure that you
understand which of the 8 flag types are affected.
!!                      Is it the secondary?
.
   Clear flags. You will be prompted for the flag-type(s) to be cleared: 
   BEWARE! Much work can be undone with careless clearing!
.
        CLEAR   Clear all flags of the specified type(s) in both the 
                 scan headers and the uv data of the selected cube. 
!!                      What if a scan "protrudes" outside the cube?
        CLDAT   Clear flags on the visibilities only (i.e. leave scan headers 
                 alone). 
        CLHEAD  Clear flags in the scan headers only. 
.
   Set flags: 
.
        UVDATA  Set flags of the specified type(s) in the individual 
                 visibilities, for selected interferometer(s) within the
primary 
                 data cube. 
                Default flag type is MANUAL, but can be overridden (parameter 
                 FLAG_MODE). 
        HARANGE Set flags of the specified type(s) on the individual 
                 visibilities, for selected interferometer(s) within the
primary 
                 data cube. 
                Default flag type is MANUAL, but can be overridden (parameter 
                 FLAG_MODE). 
                You will be repeatedly prompted for an hour-angle range. 
.
   Navigation: 
.
        INSPECT Make detour into the (flag) INSPECTion section of the program 
        STATIST Make detour into the (data) STATISTics section of the program 
        MODE    Make detour into the enviornment MODE-control section of the 
                 program 
        CLEAR   Go to the CLEAR-flags operation. 
        QUIT    Return to FLAG_OPTION prompt. " 
!
!  Get flagging operation
!       Ref:    NFLFLG
!
KEYWORD=OPS_FCOPY 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Flag-copy operation" 
        OPTIONS=TOTEL, TOPOL, TODATA, TOHEAD;|-
INSPECT, STATIST, MODE, CLEAR, QUIT 
        HELP=" FCOPY propagates flags of the selected type from individual
interferometers to groups of interferometers that have something in common with
the flagged ones.
.
Flags are looked for in the entire primary data cube, but changes affect only
the primary/secondary cube. Make sure that you understand which of the 8 flag
types are affected!
!!              Primary/secondary: Correct?
.
   Operations that modify flags on individual data points: 
.
        TOTEL  Copy flags from interferometers to telescopes: For each scan 
                selected, flag all data that share a dipole (X or Y channel of 
                a telescope) with any interferometer for which the flag 
                selected is set. 
.
        TOPOL  Copy flags to all polarisations: For each scan selected, flag
all 
                polarisations of all telescope pairs for which the flag
selected 
                is already set for at least one of them. (This is a sort of 
                mini-PUT). 
!!                      Each scan <--> primary data cube?
.
   Operations that affect ENTIRE scans. NOTE that these IGNORE ANY SELECTIONS 
   you have defined for interferometers (parameter IFRS) and/or polarisations 
   (parameter SELECT_XYX). 
!               {\em see the dictinction between\
!               \whichref{data and header flags}{} }
.
        TOHEAD  For each flag selected, set the scan-header flag if it is set 
                 for more than a given number (paremeter TOH_LIMIT) of data 
                 points. 
                This invalidates the entire scan. The operation is reversible 
                 (parameter OPS_MANUAL = CLHEAD); the individual data flags 
                 are not affected. 
!                       {\em See parameters \textref{TOH_LIMIT}{.toh.limit},
!                       \textref{OPS_MANUAL}{.ops.manual} }
.
        TODATA  Transfer flags of the type selected from the header of each
scan 
                 to all the scan's data, deleting them in the headers. 
.
   Navigation: 
.
        INSPECT Make detour into the (flag) INSPECTion section of the program 
        STATIST Make detour into the (data) STATISTics section of the program 
        MODE    Make detour into the enviornment MODE-control section of the 
                 program 
        CLEAR   Go to the CLEAR-flags operation. 
        QUIT    Return to FLAG_OPTION. " 
!
!  Get flagging operation
!       Ref:    NFLFLG
!
KEYWORD=OPS_SCANS 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT=Select scan-statistics flagging criterion | 
        OPTIONS=-
SCANS; MAXABCS; ANOISE, XAN, YAN, RNOISE, XRN, YRN;|-
INSPECT, STATIST, MODE, CLEAR, QUIT 
        HELP=" The operations available here flag scan headers and thereby
affect ENTIRE scans; in doing so they IGNORE ANY SELECTIONS you have defined
for interferometers (parameter IFRS) and/or polarisations (parameter
SELECT_XYX). Apart from this, only those scans that overlap with the primary
data cube are affected.
.
For each of the options an appropriate flag is selected, as noted below. If you
have defined your own selection through the USER_FLAGS parameter, that
selection will override NFLAG's defaults. 
!                       {\em see \textref{FLAG_MODE}{.flag.mode} }
.
!               {\em see the dictinction between
!               \whichref{data and header flags}{} }
.
   Flagging the scans that you select. 
.
        SCANS   Flag scans manually; you will be given the option to select a 
                 secondary data cube for this operation. 
.
   Flagging on data statistics. The default flag type is CLIP. 
.
        MAXABCS Flag all scans in which the modulus of either the real (cosine) 
                 or imaginary (sine) part of any visibility falls outside a 
                 range to be specified. 
.
   Flagging on noise statistics. The default flag type is NOISE. 
.
        ANOISE  Flag all scans in which any of the four noise values (X/Y 
                 gain/phase) recorded in the latest NCALIB ALIGN or SELFCAL 
                 operation exceeds a threshold (yet to be prompted for). 
.
        XAN,YAN As ANOISE, but checking only the X resp. Y noise values. 
.
        RNOISE  Flag all scans in which any of the four noise values (X/Y 
                 gain/phase) recorded in the latest NCALIB REDUN operation 
                 exceeds a threshold (yet to be prompted for). 
.
        XRN,YRN As RNOISE, but checking only the X resp. Y noise values. 
.
   Navigation: 
.
        INSPECT Make detour into the (flag) INSPECTion section of the program 
        STATIST Make detour into the (data) STATISTics section of the program 
        MODE    Make detour into the enviornment MODE-control section of the 
                 program 
        CLEAR   Go to the CLEAR-flags operation. 
        QUIT    Return to FLAG_OPTION. " 
!
!  Get flagging operation
!       Ref:    NFLFLG
!
KEYWORD=OPS_CLIPDATA 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        SWITCHES=LOOP,NULL_VALUES 
        CHECKS=ABBREV_OPTIONS 
        SEARCH=L,P 
        PROMPT=Select data-thresholding criterion | 
        OPTIONS=-
AMPL, COS, SIN; ARESID, RRESID; QXY,UXY,VXY; DT1;|-
INSPECT, STATIST, MODE, CLEAR, QUIT | 
        HELP=" Remember that only the primary data cube is affected.
.
The following operations flag individual data points, according to a flagging
criterion that is derived from the visibilities themselves. You will be
prompted for an upper limit. For each visibility point exceeding the limit the
flag will be set if it exceeds the limit, for the others it will be cleared:
Current flags are NOT preserved. 
.
The flag type is CLIP, unless you have defined your own type through the
USER_FLAGS parameter. 
.
   Flagging on straight data values exceeding the threshold. These criteria are 
   intended for use with CORRECTED data (Parameter FLAG_MODE=CORR). 
.
        AMPL    Threshold applies to absolute value 
        COS     Threshold applies to real (cosine) part. 
        SIN     Threshold applies to imaginary (sine) part. 
.
   Flagging on Selfcal/Redundancy residuals. You will be prompted to specify
the 
   Selfcal source model used in the latest NCALIB SEFCAL/REDUN run. 
   (NOTE that the residuals here are VECTOR differences between observation 
    and source model; this definition is different from that used by NPLOT.) 
.
        ARESID  Threshold applies to the magnitude of the residual 
        RRESID  Threshold is the absolute difference of the residual's
magnitude 
                 from the average magnitude of all residuals in the scan for
the 
                 same baseline. (In short: This flags outliers in sets of 
                 redundant baselines.) 
                NOTE: This operation can be performed even on uncalibrated
data, 
                 provided a source model is available. 
.
   Flagging on polarisation-related criteria: 
.
        QXY     Threshold applies to the magnitude of the difference between XX 
                 and YY visibilities per interferometer (i.e. magnitude of 
                 Stokes Q for parallel dipoles). 
        UXY     Threshold applies to ABS(XY-YX) per interferometer
                 (i.e. ABS(U) if the dipoles are parallel).
        VXY     Threshold applies to ABS(j(XY+YX)) per interferometer
                 (i.e. ABS(V) if the dipoles are parallel).
.
                For unpolarised sources, this criterion should be as effective 
                 as ARESID above. 
.
   Flagging on discontinuities in time. The flags raised here serve only as an 
   ALERT that there is a 'jump' in the data; you will have to decide yourself 
   what to do about the problem. 
.
        DT1     Threshold is the difference in amplitude of a data point with 
                 its counterpart in the preceding scan. 
.
  Navigation: 
.
        INSPECT Make detour into the (flag) INSPECTion section of the program 
        STATIST Make detour into the (data) STATISTics section of the program 
        MODE    Make detour into the environment MODE-control section of the 
                 program 
        CLEAR   Go to the CLEAR-flags operation. 
        QUIT    Return to FLAG_OPTION. " 
!
!  Get flagging operation
!       Ref:    NFLFLG
!
KEYWORD=OPS_DETERM 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Select coordinate criterion |" 
        OPTIONS=-
SHAD, PBAS ,ELEVATION; REDUN, NONRED; INSPECT, STATIST, MODE, CLEAR, QUIT 
!CBI disabled
        HELP=" Remember that only the primary data cube is affected.
.
   Flagging of data points on the basis of their coordinates. The flag type is 
   SHADOW, unless you have selected your own type (parameter FLAG_MODE). 
.
        SHAD      In each scan, flag all data that are affected by 'shadowing', 
                   i.e. all interferometers in which the aperture of either 
                   telecope is partly blocked by another telescope in the line 
                   of sight. 
.
        PBAS      Flag all data for which the length of the projected baseline 
                   is within a range (for which you will be prompted). This is
a 
                   way to eliminate short baselines susceptible to
interference, 
                   without losing more data than necessary. 
.
        ELEVATION Flag all uv-data that have been taken at elevations below a 
                   limit (for which you will be prompted). 
.
        REDUN     Flag all redundant baselines. 
        NONRED    Flag all non-redundant baselines. 
.
   Navigation: 
.
        INSPECT Make detour into the (flag) INSPECTion section of the program 
        STATIST Make detour into the (data) STATISTics section of the program 
        MODE    Make detour into the enviornment MODE-control section of the 
                 program 
        CLEAR   Go to the CLEAR-flags operation. 
        QUIT    Return to FLAG_OPTION. " 
!!- CBI       Flag all uv-data that are affected by interference from the
!!            Control Building (CB) in Westerbork.
!
!  Get flagging operation
!       Ref:    NFLFLG
!
KEYWORD=OPS_STATIST 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT=-
"Statistics operation - (ACC, SCANS, UVDAT, <name>;|-
GROUPS, EXPLAIN; INSPECT, MODE, QUIT)" 
        HELP=" Accumulate statistics from data and/or scan headers over the
primary or secondary data cube and display it.
.
   Accumulate: 
        ACC     Accumulate statistics from data and scan headers, excluding
data 
                 points/headers that are flagged. 
                You may select a secondary data cube for this purpose
(parameter 
                 XXX). 
.
   Inspect the results of accumulation: 
.
        SCANS   Show the collected scan-header statistics. 
        UVDAT   Show the collected visibility statistics. 
        <name>  Show the statistics of the named 'accumulation group' (use 
                 GROUPS and EXPLAIN above to see what choices you have). 
.
   These options will produce a Table with the following columns: 
.
        <name>  The name of the quantity for which statistics have been 
                 calculated. 
        mean    Its average value 
        rms     Its rms magnitude 
        rmsms   Its rms deviation w.r.t. the mean. 
        rmsvar  The rms difference, between pairs of successively processed 
                 values. 
        wtot    Total weight. Usually it indicates the total nr of samples. 
        minval  Its minimum value. 
        maxval  Its maximum value. 
        <unit>  The units in which the above values (except wtot) are
expressed. 
.
   Get some extra help: 
.
        GROUPS  Show the names of the currently defined 'accumulation groups' 
        EXPLAIN Show explanation of statistical quantities. 
.
   Navigation: 
.
        INSPECT Make detour into the (flag) INSPECTion section of the program 
        MODE    Make detour into the enviornment MODE-control section of the 
                 program 
        QUIT    Go back to what you were doing before. " 
!
!  Get flagging operation
!       Ref:    NFLFLG
!
KEYWORD=OPS_INSPECT 
        DATA_TYP=C 
        IO=I 
        LENGTH=24 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT= "-
Projection (COUNT, FTYP, TEL, IFR, HA) with optional '_' plus|-
range modifier (X, Y, XY, YX, MAN, CLIP, NOISE, ADD, SHAD, U3, U2, U1, OLD)|-
OR navigation control (STATIST, MODE)" 
!!CHAN
        HELP="
Count and inspect flags in the primary data cube. A complete reply consists of
one of the primary options plus a modifier suffix. 
.
   Count flags (no modifier!):
.
        COUNT   Count the flags of all types that are set, for subsequent 
                 inspection. 
                You may define a secondary data cube for this operation 
                By default, flags of all types are counted; you may change this 
                 (parameter FLAG_MODE). 
.
   Show the flag counts in various data-cube dimensions (<mod> stands for one
of 
   the optional modifiers listed below): 
.
        FTYP_<mod>  Show the counts per flag-type (data and headers). 
        TEL_<mod>   Show the counts per telescope. 
        IFR_<mod>   Show the counts per interferometer. 
        HA_<mod>    Show the counts per hour-angle (total over all 
                     interferometers and polarisations). 
!!      CHAN_<mod>  Show the counts per frequency channel.
.
     Modifiers: You may limit the display of counts to a sub-set through one of 
     the following modifiers (prefixed with an underscore, e.g. TEL_SHAD, or 
     HA_XX). The display will be equivalent to that for a secondary cube 
     covering the same selection. 
.
        X, Y            XX alone, YY alone 
        XY, YX          only XX and YY, only XY and YX 
        MAN, CLIP, NOIS, ADD, SHAD, U3, U2, U1, OLD 
                        for one flag type 
        <interferometer designation> 
                        for a (group of) interferometer(s), e.g. FTYP_9A, HA_8* 
        <telescope> 
                        for a (group of) telescope(s), e.g. FTYP_8 
!!                      Implemented??
.
   Navigation: 
.
        STATIST Make detour into the (data) STATISTics section of the program 
        MODE    Make detour into the enviornment MODE-control section of the 
                 program 
        QUIT    Back out from INSPECT: Return to what you were doing before. " 
!
!  Get user-specified flag to use
!       Ref:    NFLOPS
!
KEYWORD=USER_FLAGS 
        DATA_TYP=C 
        IO=I 
        LENGTH=16 
        NVALUES=16 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=NULL_VALUES,WILD_CARDS,LOOP 
        SEARCH=L,P 
        PROMPT="Select flag type(s) for ALL flagging operations|" 
        OPTIONS=NONE, [ALL]; MAN, CLIP, NOISE, ADD, SHAD, U1,U2,U3; [OLD] 
        HELP=" Each flagging operation changes a specific flag type by default.
You may define here an alternative (set of) flag type(s) to be used in ALL
subsequent flagging operations. Reply NONE to revert to the default settings.
.
The purpose is to allow you to do experimental flagging with one of the USER
flags without messing up the flagging that you have already done. 
.
        OLD     use the flag type for the 'OLD' class (i.e. flagged before 
                 930609, and converted with NVS option) 
                 NB: OLD uses the same flag-bit as MAN 
!!              I suppose this can now be removed.
" 
!
!  Get user specified flag to use
!       Ref:    NFLOPS
!
KEYWORD=USER_FLAG 
        DATA_TYP=C 
        IO=I 
        LENGTH=16 
        NVALUES=16 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=NULL_VALUES,WILD_CARDS,LOOP 
        SEARCH=L,P 
        PROMPT="default flag type(s)" 
        OPTIONS=NONE, ALL, MAN, CLIP, NOISE, ADD, SHAD,|-
U1,U2,U3; OLD 
        HELP=" Specify the default flag type(s) for flagging operations.
.
    NONE:      no flag type specified (i.e. use default types) 
    ALL or *   all flag types (not very useful) 
    MAN        flag type for the MANUAL class of operations 
    CLIP       flag type for the CLIP class of operations 
    NOISE      flag type for the NOISE class of operations 
    ADD        flag type for the ADDITIVE class of operations 
    SHAD       flag type for the SHADOW class of operations 
    U1, U2, U3 'user' flag types, i.e. types you may use as you please 
.
Note:   Flags U1, U2, U3 can be used to experiment with some flagging operation 
        without affecting the flag type that is 'officially' associated 
        with the same operation. 
.
    OLD        use the flag type for the 'OLD' class (i.e. flagged before 
                930609, and converted with NVS option) 
                NB: OLD uses the same flag-bit as MAN " 
!
!  Get user specified flag to use
!       Ref:    NFLOPS
!
KEYWORD=SELECT_FLAG 
        DATA_TYP=C 
        IO=I 
        LENGTH=16 
        NVALUES=16 
        CHECKS=ABBREV_OPTIONS 
        SWITCHES=NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        PROMPT="Flag type(s) to use in data selection|" 
!!              ??
        OPTIONS=NONE, ALL, MAN, CLIP, NOISE, ADD, SHAD ,U1,U2,U3; [OLD] 
        HELP=" Some operations test flags in selecting data. By default, NFLAG
selects the test flags appropriate to each operation; only those data are
accepted for which these flags are CLEAR.
.
You may define here an alternative (set of) flag type(s) to be used in ALL
subsequent testing operations; you may select more than one, separated by
commas. Reply NONE to revert to the default settings. 
.
   ALL or *     test all flag types 
   MAN          test the flag type for the MANUAL class of operations 
   CLIP         test the flag type for the CLIP class of operations 
   NOISE        test the flag type for the NOISE class of operations 
   ADD          test the flag type for the ADDITIVE class of operations 
   SHAD         test the flag type for the SHADOW class of operations 
   U1, U2, U3   'user' flag types, i.e. types you may use as you please 
   NONE         revert to NFLAG's default types per operation 
.
   OLD          the flag type for the 'OLD' class (i.e. flagged before 
                 930609, and converted with NVS option) 
                 NB: OLD uses the same flag-bit as MAN 
!!              I suppose this can now be removed.
" 
!
!  Get put range
!       Ref:    NFLFLG
!
KEYWORD=PUT_RANGE 
        DATA_TYP=C 
        IO=I 
        LENGTH=8 
        NVALUES=4 
        SWITCH=VECTOR,NULL_VALUES,WILD_CARD 
        SEARCH=L,P 
        PROMPT=-
Ranges for expansion volume around PUT points:|-
chan.HA.ifr.pol 
        HELP=" PUT performs flagging commands from the internal flag list on
your primary data cube. Each command consists of a flag/unflag mask plus ranges
in the frequency-
channel, interferometer, polarisation and hour-angle 'dimensions' of the TARGET
points to which it must be applied. 
.
PUT has the capability to EXPAND, within the limits of the primary data cube,
the operation over a 4-dimensional volume of data around each target point.
The extension volume is specified through RANGES along the channel, hour-angle,
interferometer and polarisation axes in this order. Except for interferometers,
a range is formulated in terms of units of sequential position rather than
physical coordinates. It can be one of the foloowing: 
.
   .            do not extend along this axis 
   *            extend to all positions along this axis 
   <n> or <n>C  extend over <n>/2-1 positions on either side (incrementing <n> 
                 if it is even) 
   <n>L         extend over <n>-1 positions to the 'left' (i.e. toward lower 
                 coordinates) 
   <n>R         extend over <n>-1 fields to the 'right' (i.e. toward higher 
                 coordinates) 
.
   The following special notations can also be used in each dimension: 
.
        <n>=0   suppress flagging altogether (length of the nextension cube is 
                 zero) 
        <n>=1   identical to . (length of extension cube is 1: no extension)  
   The following shorthand can be used for the entire reply: 
.
        *       short for 1,1,1,1 or .,.,., (You may do better to avoid this 
                 form because this use of a wildcard character is anomalous.) 
.
?  The interferometer range works on telescope basis, i.e. the given range is ?
  valid for both receptors (e.g. 0Y and AX). 
!!              Clarify, it must be different from the other coordinates...
.
Example: 
        If a particular ineterferometer is flagged in one of frequency
channels, 
                PUT_RANGE= *,.,.,. 
 will propagate the flag to all channels for that interferometer " 
!!              How does one access the PUT_EXPAND_xx keywords?
!
!  Get put range
!       Ref:    NFLFLG
!
KEYWORD=PUT_EXPAND_CH 
        DATA_TYP=J 
        IO=I 
        SWITCH=LOOP,NULL_VALUES,WILD_CARD 
        CHECKS=MINIMUM 
        MINIMUM=0 
        SEARCH=L,P 
        PROMPT="PUT expansion half-width in channel numbers" 
        HELP=" PUT performs flagging commands from the internal flag list on
your primary data cube. Each command consists of a flag/unflag mask plus ranges
in the frequency-
channel, interferometer, polarisation and hour-angle 'dimensions' of the TARGET
points to which it must be applied. 
.
You have activated PUT's option to EXPAND, within the limits of the primary
data cube, the operation over a 4-dimensional volume of data around each target
point. You may now select the number of channels on either side of a target
channel over which you want to expand the flagging.  Example: 
        If you reply 2, a total of 5 channels centered on each target channel
will be flagged: two above and two below the target. " 
!
!  Get HA
!       Ref:    NFLFLG
!
KEYWORD=HA 
        DATA_TYP=R 
        IO=I 
        NVALUES=2 
        SWITCHES=LOOP,NULL_VALUES,WILD_CARDS 
!!      UNITS=DEG,RAD,CIR,HMS
        SEARCH=L,P 
        PROMPT="hour angle range to process (deg)" 
        HELP=" Specify the start and end of the HA-range to be processed. If
only one value is given, the end value will be the same."
!
!  Get flagging limit
!       Ref:    NFLOPS
!
KEYWORD=LIMIT 
        DATA_TYP=R 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Flagging threshold" 
        HELP=" Specify the limit threshold value for the (un)flag criterion
selected. The units (e.g. W.U.) are those appropriate for the criterion."
!
!  Get clipping threshold:
!       Ref:    NFLOPS
!
KEYWORD=CLIP_LIMIT 
        DATA_TYP=R 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Clip threshold (W.U.)" 
        HELP=" Specify the threshold value (W.U.) for the clipping criterion
selected.
.
The default value shown is three times the rms value calculated in a 'trial
run' through the first <N> valid (see Note below) scans in the
primary/secondary data cube. This is intended to be an educated guess at a
sensible 3-sigma threshold for your operation, but you must make your own
assessment of the probable suitability of this value. 
.
Notes: 
     The new setting of the target flag in each scan or data point will 
      OVERRIDE the current one rather than being ORed into it. 
.
     The number <N> of dry-run scans is defined by the parameter DRY_SCANS; the 
      default is 25. 
.
     The flags used in selecting valid scans are defined by parameter ?
SELECT_FLAGS; the default is ALL. 
!               {\em see parameters \textref{DRY_SCANS}{.dry.scans},
!                \textref{SELECT_FLAG}{.select.flag} }
" 
!
!  Get flag limits
!       Ref:    NFLOPS
!
KEYWORD=CLIP_LIMITS 
        DATA_TYP=R 
        IO=I 
        NVALUES=2 
        SWITCHES=LOOP,VECTOR,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Clip limits" 
        HELP=" Specify the range within which the test value will be accepted.
For values outside the range the flag will be set, for those within the range
it will be cleared.
.
The default value shown is three times the rms value calculated in a 'trial
run' through the first <N> valid (see Note below) scans in the
primary/secondary data cube. This is intended to be an educated guess at a
sensible 3-sigma threshold for your operation, but you must make your own
assessment of the probable suitability of this value. 
.
Notes: 
     The new setting of the target flag in each scan or data point will 
      OVERRIDE the current one rather than being ORed into it. 
.
     The number <N> of dry-run scans is defined by the parameter DRY_SCANS; the 
      default is 25. 
.
     The flags used in selecting valid scans are defined by parameter ?
SELECT_FLAGS; the default is ALL. 
!               {\em see parameters \textref{DRY_SCANS}{.dry.scans},
!                \textref{SELECT_FLAG}{.select.flag} }
!
!!              rms, 3-sigma is not the right thing here - clarify
" 
!
!  Get threshold for option DT1:
!       Ref:    NFLOPS
!
KEYWORD=DT1_LIMIT 
        DATA_TYP=R 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Discontinuity threshold (W.U. per 10 sec)" 
        HELP=" The DT1 option looks for 'un-physical jumps' in time in the
cosine and sine values of the uv-data. The actual difference (in W.U.) between
successive time-samples is divided by the time that separates them, in units of
the integration time (e.g. 60 sec). The flag is set if this value exceeds the
threshold.
" 
!
!  Get flag limits
!       Ref:    NFLOPS
!
KEYWORD=LIMITS 
        DATA_TYP=R 
        IO=I 
        NVALUES=2 
        SWITCHES=LOOP,VECTOR,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="flagging limits" 
        HELP=" Specify the LOWER and UPPER limiting values for the (un-)flag
criterion. The unit (e.g. W.U.) depends on the criterion under consideration.
.
!
!  Get flag limits
!       Ref:    NFLOPS
!
KEYWORD=ABCS_LIMITS 
        DATA_TYP=R 
        IO=I 
        NVALUES=2 
        SWITCHES=LOOP,VECTOR,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Acceptance range for ABS(cos), ABS(sin)" 
        HELP=" An ENTIRE scan will be flagged if for any valid data point in it
the modulus of either the real (cos) or imaginary (sin) part falls outside the
acceptance range.
.
This option allows you to reject scans containing interference peaks, or 'bad'
data (i.e. abnormally low amplitudes). 
.
NOTES: 
.
     The new setting of the target flag in each scan or data point will 
      OVERRIDE the current one rather than being ORed into it. 
.
     The flags used in selecting valid scans are defined by parameter ?
SELECT_FLAGS; the default is ALL. 
!               {\em see parameters \textref{DRY_SCANS}{.dry.scans},
!                \textref{SELECT_FLAG}{.select.flag} }
!
!!              rms, 3-sigma is not the right thing here - clarify
NB: When new thresholds are given, all Scans in the specified data cube are
(un-)flagged accordingly, including the Scans that were flagged already. " 
!
!  Get flag limits
!       Ref:    NFLOPS
!
KEYWORD=PBAS_LIMITS 
        DATA_TYP=R 
        IO=I 
        NVALUES=2 
        SWITCHES=LOOP,VECTOR,NULL_VALUES,WILD_CARDS 
        SEARCH=L,P 
        PROMPT="Projected-baseline range to be flagged (m)" 
        HELP=" Specify the range of lengths of PROJECTED baseline to be
flagged. The default flag-type is SHADOW.
.
Short baselines are more sensitive to interference. In flagging them wholesale,
however, one also loses visibility data representing extended source structure;
a more selective procedure is to be preferred. 
.
Another application of this parameter is to force a uv coverage that is more
nearly circular in the PROJECTED uv plane, and hence produces a more circularly
symmetric antenna pattern. To this end, set the lower limit less than or equal
to 
.
        <longest baseline> * sin DEC 
.
Instead of permanently modifying the .SCN file in this way, one may use the
NMAP parameters CWEIGHT_TYPE and CWEIGHT_VALUE to achieve a similar effect. 
!               {\em see NMAP public paremeter
!                \textref{CWEIGHT_TYPE}{nmap_public_keys.cweight.type} }
.
Examples: 
        PBAS_LIMITS = 0, 288 
.
   causes all projected baselines up to 288 m (inclusive) to be flagged. 
.
   For a source at a declination of 30 deg, 
.
        PBAS_LIMITS = 1500, 10000 
.
   will flag projected baselines longer than 1500 m.  " 
!
!  Get flag limits
!       Ref:    NFLOPS
!
KEYWORD=SHADOW_DIAM 
        DATA_TYP=R 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES 
        SEARCH=L,P 
        PROMPT="Telescope shadowing diameter (metres)" 
        HELP=" Specify the diameter (m) of the telescopes.
.
A telescope is 'shadowed' as soon as part of its aperture gets blocked by the
presence of another telecope in its line of sight. This condition occurs when
the projected baseline between the two telecopes becomes less than the
telescope diameter D (25 m for the WSRT). If you want to retain as much as
possible of your data, you may specify a somewhat smaller number (e.g. 0.8 D)
to take into account the fact that the edge of the aperture is only weakly
illuminated. 
.
For the hour-angles at which a telescope is shadowed, all intreferometers are
flagged of which it is a part. The default flag-type is SHADOW. 
.
  ? Another flag may be selected through the FLAG_MODE parameter. " 
!
!  Get flagging count limit
!       Ref:    NFLOPS
!
KEYWORD=TOH_LIMIT 
        DATA_TYP=J 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES 
        CHECKS=MINIMUM 
        MINIMUM=1 
        SEARCH=L,P 
        PROMPT="Maximum number of data flags to tolerate per scan" 
        HELP=" NFLAG will check in each scan the number of points for which the
flag type selected is set. If that number exceeds this value you define here,
the entire scan will be flagged. This will subsequently result in faster
processing
(because individualdata points need not be tested) at the expense of the loss
of some healthy data. 
.
The operation of setting header flags is reversible: The individual data flags
are retained so you can restore the present condition by clearing the header
flags. " 
!
!  Get nr of scans for trial run:
!       Ref:    NFLOPS
!
KEYWORD=DRY_SCANS 
        DATA_TYP=J 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES 
        CHECKS=MINIMUM 
        MINIMUM=1 
        SEARCH=L,P 
        PROMPT="Number of scans to use in trial runs" 
        HELP="
'Clipping' operations need (a) threshold value(s) that is (are) related to the
magnitude of the data at hand. 
.
The program determines a 'rasonable' default for such cases by performing a
'trial run' on the first of the scans in the data cube selected. You may
specify their number here. The trial run evaluates the clipping criterion but
does NOT modify the .SCN file.  For data with reasonably stationary statistics,
the default number should be adequate. A larger number obviously will take more
time to process. If you specify a very large number, the trial run will include
the entire data cube selected. " 
!
!  Get flag limits
!       Ref:    NFLOPS
!
KEYWORD=ELEVATION_LIMIT 
        DATA_TYP=R 
        IO=I 
        SWITCHES=LOOP,NULL_VALUES 
!!      UNITS=DEG,RAD,CIR,HMS
        SEARCH=L,P 
        PROMPT="Elevation lower limit (deg)" 
        HELP=" Specify the lower limit for acceptable elevations. All points
corresponding to lower elevations are rejected. The default flag-type is SHADOW.
" 
!
INCLUDE=NGEN_PEF 
!
INCLUDE=NSHOW_PEF 
!
INCLUDE=FLFNODE_PEF     !
!
INCLUDE=SCNNODE_PEF     !
INCLUDE=SCNSETS_PEF     !
INCLUDE=SELECT_PEF 
!
INCLUDE=NMODEL_PEF      !
INCLUDE=MDLNODE_PEF 
!-
