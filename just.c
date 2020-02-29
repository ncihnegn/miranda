/* program for text justification */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *                                                                        *
 * Revised to C11 standard and made 64bit compatible, January 2020        *
 *------------------------------------------------------------------------*/

/* usage:  just [-<width>] [-t<tolerance>] [file...]
   if no files given uses standard input.  Default width is 72.
        1) blank lines remain blank.
        2) if a line begins with blanks, these are preserved and it is not
           merged with the previous one.
        3) lines which begin with more than THRESHOLD (currently 7) spaces
           or have a '>' in column 1, have their layout frozen.
   otherwise each line is merged with the following lines and reformatted so
   as to maximise the number of words in the line and justify it to the
   specified width.  Justification is not performed on a line however if
   it would require the insertion of more than tolerance (default 3)
   extra spaces in any one place.
*/

/* now handles tabs ok - if you change THRESHOLD, see warning in getln */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#define MAXBUF 3600
#define MAXWIDTH 2400
#define THRESHOLD 7
int tolerance=3;
    /* the largest insert of extra spaces we are willing to tolerate
       in one place */

main(argc,argv)
int argc;
char *argv[];
{ int width=72;
  FILE *j_in=fopen(".justwidth","r");
  if(j_in){ if(fscanf(j_in,"%d",&width)!=1)width=72;
	    fclose(j_in); }
  while(argc>1&&argv[1][0]=='-')
    if(argv[1][1]=='t'&&isdigit(argv[1][2]))
    { sscanf(argv[1]+2,"%d",&tolerance);
      argc--; argv++;
    }else
    if(isdigit(argv[1][1])||argv[1][1]=='-'&&isdigit(argv[1][2]))
    { sscanf(argv[1]+1,"%d",&width);
      argc--; argv++;
    }
    else fprintf(stderr,"just: unknown flag %s\n",argv[1]),exit(1);
  if(width<0)width = -width, tolerance=0;
  if(width==0)width=MAXWIDTH, tolerance=0;
  if(width<6||width>MAXWIDTH)
    { fprintf(stderr,"just: silly width %d\n",width);
      fprintf(stderr,"(legal widths are in the range 6 to %d)\n",MAXWIDTH);
      exit(1);
    }
  if(argc==1)justify(width,stdin,"input"); else
  while(--argc>0)
    { FILE *fp=fopen(*++argv,"r");
      if(fp==NULL)
        { fprintf(stderr,"just: cannot open %s\n",*argv);
          break;
        }
      else justify(width,fp,*argv);
    }
  exit(0);
}

static char buf[MAXBUF+2],*bp=buf;

#include <string.h>
#define index(s,c) strchr(s,c)

int linerr=0;

justify(width,fp,fn)
int width;
FILE *fp;
char *fn;
{ int c=' ';  /* c initialised to anything != EOF */
  int worderr=0,w;
  /*if(fp==stdin)setbuf(fp,NULL); /* to fix weird bug when "just" used in
                                   pipeline - DT 15/1/85 */
  /* note - above has disastrous effect on system time used, for large
     inputs, therefore switched off 19/2/87 - fortunately bug seems to be
     fixed in 4.2 BSD */
  linerr=0;
  while(c!=EOF&&(c=getc(fp))!=EOF)
 /* 1st part of test needed because ungetc(EOF,fp) ineffective */
  if(c=='\f')putchar('\f');else  /* formfeed counts as blank line */
  { ungetc(c,fp);
    getln(fp,0);
    if(bp==buf||buf[0]=='>'||indent(buf)>THRESHOLD) /* blank or frozen line */
      { puts(bp=buf); continue; }
  /*otherwise perform justification up to next indented,blank or frozen line*/
    squeeze(buf);
    while(bp-buf>(w=width+bs_cor(buf))||!isspace(c=peek(fp))&&c!=EOF&&c!='>')
      if(bp-buf<=w/*idth+bs_cor(buf)*/)
        { pad(); getln(fp,1); }
      else{ /* cut off as much as you can use */
            char *rp = &buf[width];
	    { char *sp=index(buf,'\b'); /* correction for backspaces */
	      while(sp&&sp<=rp)rp += 2, sp=index(sp+1,'\b'); }
            while(*rp!=' ' && rp>buf)rp--;  /* searching for word break */
            if(rp==buf)
              { worderr=1;
                while(rp-buf<width-1)putchar(*rp++);  /* print width-1 chars */
                putchar('-');  /* to signify forced word break */
                putchar('\n');
              }
            else { while(rp[-1]==' '||rp[-1]=='\b'&&rp[-2]=='_')
	             rp -= rp[-1]==' '?1:2; /* find start of break */
		   if(*rp=='_'&&rp[1]=='\b'&&rp[2]==' ')rp += 2;
				     /* leave trace of underlined gap */
                   while((*rp==' '
			  ||*rp=='_'&&rp[1]=='\b'&&rp[2]==' '
			  ||*rp=='\b'&&rp[1]==' ')
			  &&rp<bp)*rp++ = '\0';  /* find end of break */
                   rjust(buf,width);
                 }
            /* shuffle down what's left */
            strcpy(buf,rp);
            bp -= rp-buf;
          }
    puts(bp=buf);
  }
  if(worderr)
    fprintf(stderr,
           "just: warning -- %s contained words too big for line\n",fn);
  if(linerr)
    fprintf(stderr,
           "just: warning -- %s contained disastrously long lines\n",fn);
}

getln(fp,crush)
FILE *fp;
int crush;
{ char *lose=fgets(bp,MAXBUF-MAXWIDTH+(buf-bp),fp);
  if(index(bp,'\t')&&indent(bp)<=THRESHOLD&&bp[0]!='>')
    { /* line contains tabs and is not frozen */
      char *p;
      while(p=index(bp,'\t'))*p=' '; /* replace each tab by one space */
      /* WARNING - if THRESHOLD:=8 or greater, will need to change this to
	 handle leading tabs more carefully, expanding each to right number
	 of spaces, to preserve indentation */
      /* at the moment, however, any line containing tabs in its indentation
	 will be frozen anyway */
    }
  bp += strlen(bp);
  if(bp[-1]=='\n')*--bp='\0';
  else
     { /* amendment to cope with arbitrarily long input lines:
	  if no newline found, break at next beginning-of-word */
       int c;
       while((c=getc(fp))!=EOF&&!isspace(c)&&bp-buf<MAXBUF)*bp++ = c;
       if(c==EOF||c=='\n')*bp='\0'; else
       if(bp-buf==MAXBUF)  /* give up! */
         { linerr = 1;
           *bp++ = '\\';  /* to signify forced break */
           *bp = '\0'; } else
       { *bp++ = ' ';
	 while((c=getc(fp))==' '||c=='\t');
	 if(c!=EOF&&c!='\n')ungetc(c,fp);
	 *bp = '\0'; }
     }
  /* remove trailing blanks */
  while(bp[-1]==' '&&!(bp[-2]=='\b'&&bp[-3]=='_'))*--bp='\0';
  /* eliminate all but one underlined trailing space */
  if(crush)
   while(bp[-1]==' '&&bp[-2]=='\b'&&bp[-3]=='_'&&
	 bp[-4]==' '&&bp[-5]=='\b'&&bp[-6]=='_')bp -= 3,*bp='\0';
  if(crush)
    squeeze(buf);
}

indent(s)   /* size of white space at front of s */
char *s;
{ int i=0;
  while(*s==' '||*s=='\t')
  if(*s++ == ' ')i++;
  else i = 8*(1+i/8);
  return(i);
}

#define istermch(c)  ((c)=='.'||(c)=='?'||(c)=='!')

squeeze(s)  /* remove superfluous blanks between words */
char *s;
{ char *t;
  int eosen;
  /* if(1){ bp=s+strlen(s); return; }  /* temporary measure to isolate bugs */
  t = s = s + indent(s);
  for(;;)
  { while(*t&&*t!=' '&&!(*t=='_'&&t[1]=='\b'&&t[2]==' '))*s++ = *t++;
    eosen= istermch(t[-1]);
    *s++ = *t;
    if(*t=='\0')break;
    if(*t==' ')
    { if(eosen&&t[1]==' ')*s++ = ' ';  /* upto one extra space after 
                                   sentence terminator is preserved */
      while(*++t==' ');  /* eat unnecessary spaces */ }
    else
    { /* deal with underlined spaces */
      *s++ = '\b'; *s++ = ' ';
      if(eosen&&t[3]=='_'&&t[4]=='\b'&&t[5]==' ')
	 *s++ = '_', *s++ = '\b', *s++ = ' '; /* xta space after termch */
      t += 3;
      while(t[0]=='_'&&t[1]=='\b'&&t[2]==' ')t += 3; }
  }
  bp=s-1;
}

peek(fp)
FILE *fp;
{ int c=getc(fp);
  ungetc(c,fp);
  return(c);
}

rjust(s,width)  /* print s justified to width */
char *s;
int width;
{ int gap=width-strlen(s)+bs_cor(s),wc=words(s)-1;
  int i,r;
  static leftist=0;  /* bias for odd spaces when r>0 */
  char *printword();
  if(wc)i=gap/wc,r=gap%wc;
  if(wc==0||i+(r>0)>tolerance){char *t=s+strlen(s);
                               fputs(s,stdout);
                               if(t[-1]=='\b'&&t[-2]=='_')putchar(' ');
                               putchar('\n');
                               return;}else
  if(leftist)
       for(;;)
       { s=printword(s);
         if(!*s)break;
         spaces(i+(r-- >0),s[0]=='_'&&s[1]=='\b'&&s[2]==' ');
       }
   else
     { r = wc-r;
       for(;;)
       { s=printword(s);
         if(!*s)break;
         spaces(i+(r-- <=0),s[0]=='_'&&s[1]=='\b'&&s[2]==' ');
       }
     }
  leftist = !leftist;
  putchar('\n');
}

pad()  /* insert space(s) if necessary when joining two lines */
{ if(bp[-1]!=' ')*bp++ = ' ';
  if(istermch(bp[-2]))*bp++ = ' '; else
  if(bp[-1]==' '&&bp[-2]=='\b'&&bp[-3]=='_'
     &&istermch(bp[-4]))*bp++ = '_', *bp++ = '\b', *bp++ = ' ';
}

spaces(n,ul)
int n,ul;
{ while(n--)if(ul)printf("_\b ");
	    else putchar(' ');
}

words(s)  /* counts words (naively defined) in s */
char *s;
{ int c=0;
  while(*s)
  if(*s++!=' '&&s[-1]!='\b'&&(*s==' '|| *s=='_'&&s[1]=='\b'&&s[2]==' '))c++;
  return(c+1);
}

char *printword(s)  /* prints a word preceded by any leading spaces and
                       returns remainder of s */
char *s;
{ while(s[0]=='_'&&s[1]=='\b'&&s[2]==' ') /* underlined spaces */
     putchar(*s++),putchar(*s++),putchar(*s++);
  while(*s==' ')putchar(*s++);
  while(*s&&*s!=' '&&!(*s=='_'&&s[1]=='\b'&&(s[2]==' '||s[2]=='\0')))
       putchar(*s++);
  if(s[0]=='_'&&s[1]=='\b'&&s[2]=='\0')s++,s++,printf("_\b ");
		     /* restore trailing underlined space */
  return(s);
}

bs_cor(s) /* correction to length due to backspaces in string s */
char *s;
{ int n=0;
  while(*s++)if(s[-1]=='\b')n += 2;
  if(s[-1]=='\0'&&s[-2]=='\b'&&s[-3]=='_')n--; /* implied space before \0 */
  return(n);
}
