C+ NMOWRI.FOR
C  WNB 910809
C
C  Revisions:
C
	LOGICAL FUNCTION NMOWRI(FCA,IDX)
C
C  Write a source model
C
C  Result:
C
C	NMOWRI_L = NMOWRI( FCA_J:I, IDX_J:I)
C				Write the source list in area IDX
C				to a node. The list will be sorted and merged.
C
C  Include files:
C
	INCLUDE 'WNG_DEF'
	INCLUDE 'NMO_DEF'
C
C  Entries:
C
C
C  Parameters:
C
C
C  Arguments:
C
	INTEGER FCA			!FILE TO WRITE TO
	INTEGER IDX			!MODEL INDEX
C
C  Function references:
C
	LOGICAL NMOWRS			!WRITE SOURCE FILE
C
C  Data declarations:
C
C-
	NMOWRI=NMOWRS(FCA,GMDH(0,IDX))			!WRITE THE SOURCE FILE
C
	RETURN
C
C
	END
