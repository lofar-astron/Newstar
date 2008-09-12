/* newerfile.c 

Compare modification times of two files.

Invocation
	newerfile.exe <file1> <file2>

Return status:
	-1	Error on either file
	 0	<file1> older than or as old as <file2>
	 1	<file1> newer than <file2>

History:
	JPH 950822	Original

*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <time.h>


main (argc, argv)
  int argc; char *argv[];
{		
  struct stat buf;
  time_t tm[2];
/*  char filename[64]; */
  int i;

  for (i=0; i<2; i++){
    *++argv;
    if (stat (*argv, &buf) == (-1)) {
      return -1;
    } else {
      tm[i] = buf.st_mtime;
    }
  }
  return tm[0]>tm[1];
}
