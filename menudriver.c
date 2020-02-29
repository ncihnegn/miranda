/* general purpose menu driver */
/* alternatively there is an equivalent shell script, menudriver.sh */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *                                                                        *
 * Revised to C11 standard and made 64bit compatible, January 2020        *
 *------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/wait.h>
#include <signal.h>
typedef void (*sighandler)();
#define pnlim 1024
struct stat buf;

char *menuviewer;
char *viewer="less";
/*
#ifdef UWIN
  "more -ne";
#else
  "more -d";
#endif  */

#define VIEWERPAUSESATEND .
/* this modifies default behaviour of menudriver to return straight to menu
   after displaying section, to avoid two layers of prompt;
   choice can be overriden by environment variable RETURNTOMENU=YES/NO */

#ifdef VIEWERPAUSESATEND
int fastback=1;
#else
int fastback=0;
#endif

void callshell(char[]);
void clrscr(void);
void menudrive(char*);
void pushlast(void);
void poplast(void);
void settings(void);
void singleton(char*);
int subdir(void);

char next[40]="",cmd[80],last[40]=".";
int val, ok=0;

#include <string.h>
#define index(s,c) strchr(s,c)

int main(argc,argv)
int argc;
char *argv[];
{ char *v=getenv("VIEWER"),*fb=getenv("RETURNTOMENU");
  menuviewer=getenv("MENUVIEWER");
  if(argc>2)fprintf(stderr,"menudriver: wrong number of args\n"),exit(1);
/*
#ifdef VIEWERPAUSESATEND */
  if(!menuviewer)menuviewer="cat";
/*
#else
  if(!menuviewer)menuviewer=viewer;
#endif */
  if(v)viewer=v;
  if(fb)fastback=!(*fb=='N'||*fb=='n');
#ifdef CURSES
  setupterm(0,1,&ok);
  if(ok!=1)fprintf(stderr,"warning: cannot find terminfo entry\n");
#endif
  menudrive(argc==1?".":argv[1]); }

int lastval() /* checks if last is a number (and if so leaves value in val) */
{ if(strcmp(last,".")==0&&subdir())
    /* special case, have just entered subdir */
    { poplast();
      if(sscanf(last,"%d",&val)==1)
        { chdir(".."); return(1); }
      pushlast(); return(0); }
  return(sscanf(last,"%d",&val)==1);
}

void menudrive(dir)
char *dir;
{ char *np;int c,bad=0;
  if(chdir(dir)==-1)singleton(dir); /* apparently not a directory */
  while(stat("contents",&buf)==0)
   { if(next[0]=='\0'||bad)
       { clrscr();
         /* invalid selection notified here, after clearing screen */
         if(bad)
           { if(strcmp(next,".")==0)
               printf("no previous selection to substitute for \".\"\n");
             else printf("selection \"%s\" not valid\n",next);
             bad=0; }
         strcpy(cmd,menuviewer);
         strcat(cmd," ");
         strcat(cmd,"contents");
         system(cmd);
         printf("::please type selection number (or return to exit):");
         /* read remainder of line into next, less leading white space */
         np=next; c=getchar();
         while(c==' '||c=='\t')c=getchar();
         while(c!='\n'&&c!=EOF)*np++=c,c=getchar();
         if(c==EOF)exit(0);
         /* remove trailing white space */
         if(next[0]!='!')while(np[-1]==' '||np[-1]=='\t')np--;
         *np='\0'; }
     if(next[0]=='\0'){ chdir(".."); poplast(); continue; }
     if(strcmp(next,".")==0)strcpy(next,last); /* repeat last option */
     if(strcmp(next,"+")==0&&lastval())(void)sprintf(next,"%d",val+1);
     if(strcmp(next,"-")==0&&lastval())(void)sprintf(next,"%d",val-1);
     if(stat(next,&buf)==0)
         { if(strcmp(next,".")==0||strcmp(next,"..")==0||index(next,'/'))
             { bad=1; continue; } /* no pathnames - see below */
           if(S_ISDIR(buf.st_mode)) /* directory */
           { char hold[pnlim];
             if(!getcwd(hold,pnlim))
                fprintf(stderr,"panic: cwd too long\n"),exit(1);
             if(chdir(next)==-1||stat("contents",&buf))
               bad=1,chdir(hold);
             else strcpy(last,next),pushlast(),next[0]='\0'; } else
           if(S_ISREG(buf.st_mode)) /* regular file */
             { clrscr();
#ifndef UWIN
               if(buf.st_mode&S_IXUSR) /* executable (by owner) */
#else
               if(strcmp(next,"99")==0)
#endif
                 { strcpy(cmd,"./");
                   strcat(cmd,next);
                   system(cmd);
                   if(fastback)
                     { printf("[Hit return to continue]");
                       while(getchar()!='\n');
                     }
                 } else
                   { strcpy(cmd,viewer);
                     strcat(cmd," ");
                     strcat(cmd,next);
                     system(cmd); }
               if(fastback)
                 {  strcpy(last,next);
                    next[0]='\0';
                 }
               else
             { printf(
            "::next selection (or return to go back to menu, or q to quit):"
                     );
              /* read remainder of line into next, less leading white space */
               strcpy(last,next);
               np=next; c=getchar();
               while(c==' '||c=='\t')c=getchar();
               while(c!='\n'&&c!=EOF)*np++=c,c=getchar();
               if(c==EOF)exit(0);
               /* remove trailing white space */
               if(next[0]!='!')while(np[-1]==' '||np[-1]=='\t')np--;
               *np='\0';
             }
             } } else
     if(strcmp(next,"???")==0) /* ask to see menudriver settings */
       { settings();
         printf("[Hit return to continue]");
         while(getchar()!='\n');
         next[0]='\0';
       } else
     if(strcmp(next,"q")==0||strcmp(next,"/q")==0)exit(0); else
     if(next[0]=='!') /* shell escape - handy for editing manual! */
       { static char syscm[80];
         if(next[1]=='\0'||next[1]=='!')
           if(syscm[0])
             { if(next[1]=='!')strcat(syscm,next+2);
               printf("!%s\n",syscm); }
           else
             printf("no previous shell command to substitute for \"!\"\n");
         else strcpy(syscm,next+1);
         if(syscm[0])callshell(syscm); /* `system' always gets /bin/sh */
         printf("[Hit return to continue]");
         while(getchar()!='\n');
         next[0]='\0'; }
     else bad=1;
   }
}
/* possibly a bug - can retreat above original dir, if parent contains a
   "contents" file - difficult to detect this in a general way, however */
/* pathnames banned because 
   (i) upward pathname will not return correctly if a directory (see above)
   (ii) direct selection of a grandchild directory leads to
        (a) returns via child, instead of directly
        (b) meaning of "." is screwed up while in (a)                  */
/* could fix all this by - rewrite handling of subdirectory to hold=getwd()
   and exit by chdir(hold) instead of chdir("..") - will need to make this
   recursive, or else have stack of holdwd's */

void singleton(fil)
char *fil;
{ if(stat(fil,&buf)==0 && S_ISREG(buf.st_mode)) /* regular file */
    { clrscr();
#ifndef UWIN
      if(buf.st_mode&S_IXUSR) /* executable (by owner) */
        { strcpy(cmd,"./");
          strcat(cmd,fil);
          system(cmd);
          fastback=0; } else
#endif
          { strcpy(cmd,viewer);
            strcat(cmd," ");
            strcat(cmd,fil);
            system(cmd); }
      if(!fastback)
          { printf("[Hit return to continue]");
            while(getchar()!='\n'); }
      exit(0);
    }
  else fprintf(stderr,"menudriver: cannot access \"%s\"\n",fil),
       exit(1);
}

void callshell(v)
char v[];
{ static char *shell=NULL;
  sighandler oldsig; int pid;
  if(!shell)
    { shell=getenv("SHELL");
      if(!shell)shell="/bin/sh"; }
  oldsig= signal(SIGINT,SIG_IGN);
  if(pid=fork())
    { /* parent */
     if(pid==-1)
       perror("UNIX error - cannot create process");
     wait(0);
     (void)signal(SIGINT,oldsig); }
  else execl(shell,shell,"-c",v,(char *)0);
}

void settings()
{ printf("current values of menudriver internal variables are\n\n");
  printf("        VIEWER=%s\n",viewer);
  printf("        MENUVIEWER=%s\n",menuviewer);
  printf("        RETURNTOMENU=%s\n",fastback?"YES":"NO");
  printf("\n\
These can be modified by setting environment variables of the same names\n\n\
VIEWER is the program used to display individual sections\n\n\
MENUVIEWER is the program used to display contents pages\n\n\
RETURNTOMENU=NO/YES  causes  a second prompt to be given/not given after\n\
displaying section (ie before returning to contents page).  It should be\n\
`YES' if VIEWER is a program that pauses for input at end  of  file,  or\n\
`NO' if VIEWER is a program that quits silently at end of file.\n\n");
}

/*
Symptoms  of  a  wrong  setting  are  (i)  bottom part of manual section\n\
disappears from screen before you have had a chance to read it - to cure\n\
this  set  RETURNTOMENU=NO; or (ii) having to quit through two layers of\n\
prompt at bottom of a manual section before getting back to the contents\n\
page - to cure this set RETURNTOMENU=YES;\n\n");
*/

char lastvec[100],*lastp=lastvec+1;
int giveup=0;

int subdir()
{ return(lastp>lastvec+1); }

void pushlast()
{ int n=strlen(last);
  if(last[0]=='.')
    /* pathological cases */
    if(last[1]=='\0')return; else
    if(last[1]=='.'&&last[2]=='\0')
      { poplast(); return; }
  if(lastp+n>lastvec+100) /* overflow */
    { giveup=1; return; }
  /*if(strcmp(lastp,last)==0)
    lastp+=n+1,strcpy(last,lastp); else /* here we were */
    /* suppressed 'cos interferes with special case in lastval() */
  strcpy(lastp,last),lastp+=n+1,strcpy(last,".");
}

void poplast()
{ strcpy(lastp,last); /* just in case we come back immediately */
  lastp--;
  if(giveup||lastp<=lastvec)return; /* underflow */
  while(*--lastp);
  strcpy(last,++lastp);
}

#ifndef CURSES

/* to clear screen */
void clrscr()
{ printf("\x1b[2J\x1b[H"); fflush(stdout);
}

#else
/* alternative method needs curses lib, compile with -DCURSES, or -DUWIN
   and -lncurses */

#ifdef UWIN
#include <ncurses/curses.h>
#include <ncurses/term.h>
#else
#include <curses.h>
#include <term.h>
#endif

void clrscr()
{ if(ok!=1)return;
  putp(clear_screen);
  fflush(stdout);
}
/* end of clrscr method using curses */
#endif

