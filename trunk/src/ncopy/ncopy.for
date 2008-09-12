C+ NCOPY.FOR
C  JPH 930316
C
C  Revisions:
C        JPH 931018        SIM, REV options
C        JPH 9411..        Remove SIM, REV
C        CMV 960122        Warning if /NORUN ignored
C        JPH 961213        SHORTCOPY option
C        AXC 010709        Linux port - TABS
C
        SUBROUTINE NCOPY
C
C  Main routine to copy Scan files
C
C  Result:
C
C
C  Include files:
C
        INCLUDE 'WNG_DEF'
        INCLUDE 'NCO_DEF'
C
C  Parameters:
C
C
C  Arguments:
C
C
C  Function references:
C
        LOGICAL WNDRUN                !TEST RUN
C
C  Data declarations:
C
C-
C
C PRELIMINARIES
C
        CALL NCOINI                                !INIT PROGRAM
        IF (.NOT.WNDRUN()) 
        1        CALL WNCTXT(F_TP,'Ignored option /NORUN')
C
C DISTRIBUTE
C
 10        CONTINUE
        CALL NCODAT                                !GET USER DATA
        IF (OPT.EQ.'QUI') THEN
          CALL WNGEX                                !READY
        ELSEIF (OPT.EQ.'COP' .OR. OPT.EQ.'SHO') THEN
          CALL NCOCPY
        ELSEIF (OPT.EQ.'OVE') THEN
          CALL NCOOVV
        END IF
        GOTO 10
C
C
C
        END
