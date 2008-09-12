/* include.c */

/* Recursive execution of \include and \input directives in LaTeX files
   The input and output files are given as invocation arguments

History:
	JPH 940...
	JPH 940818	Correct comments
	JPH 940915	Append \n to included files
*/

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>

int copy (in, out)
  FILE *in, *out;
{
  FILE*nin;
  char l[4096];
  char *lp, *fp, *sp;

  l[0]='%'; l[1]=' ';
  while (fgets (l+2, 4096, in)) {
    for (sp=l+2; *sp ==' ' || *sp =='	'; sp++);
    lp= sp;
    if (! strncmp(sp, "\\input", 6)) lp+= 6;
    if (! strncmp(sp, "\\include", 8)) lp+= 8;
    if (lp ==sp) {
      fputs (l+2, out);
    } else {
      fputs (l, out);
      for (;*lp ==' ' || *lp =='{'; ++lp); fp= lp++;
      for (lp++ ;*lp !='}' && *lp !='\n' && *lp !=' '; lp++); *lp= 0;
      nin = fopen (fp, "r");
      if (errno) {
	printf("ERROR	Can't open %s\n", fp); return errno;
      }    
      copy (nin, out);
      if (errno) {return errno;}
    }
  }
  fputs ("\n", out);	/* be sure to terminate last line  */
  return errno;
}

int main (argc, argv)
  char** argv;
  int argc;
{
  FILE *in, *out;

  in= fopen(argv[1], "r");
  if (errno) {
    printf("ERROR	Can't open %s\n", argv[1]); return errno;
  }; 
  out= fopen(argv[2], "w");
  if (errno) {
    printf("ERROR	Can't open %s\n", argv[2]); return errno;
  };
  return copy (in, out);
}
