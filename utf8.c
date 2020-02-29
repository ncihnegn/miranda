#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "utf8.h"

/* defines outUTF8(), fromUTF8()
   to translate Unicode chars to, from UTF-8 byte sequences - DAT 12.4.2009 */

#define out(u,fil) putc((int)(u),(fil))

/*
FILE *f;
int nextch()
{ return getc(f); }
*/

static char errs[24];
char *p;

void utf8report()
{ fprintf(stderr,"protocol error - %s sequence:%s\n",
          strstr(errs,"EOF")?"incomplete":"invalid",errs);
  exit(1);
}

#define foo(a) if(a==EOF)strcat(p," EOF"),p+=4;else sprintf(p," %#x",a),p+=strlen(p)
#define err(a) {*errs='\0';p=errs;foo(a); utf8report(); }
#define err2(a,b)  {*errs='\0';p=errs;foo(a);foo(b); utf8report(); }
#define err3(a,b,c)  {*errs='\0';p=errs;foo(a);foo(b);foo(c); utf8report(); }
#define err4(a,b,c,d)  {*errs='\0';p=errs;foo(a);foo(b);foo(c);foo(d); utf8report(); }

#define nextch(x) ((x)=getc(fil))

/*
#define nextch(x) ((x)=*(*p)++)
unicode scanUTF8(char **p)
*/

unicode fromUTF8(FILE *fil)
/* returns a unicode value or EOF for end of input */
{ unsigned c0,c1,c2,c3;
  if((nextch(c0))==EOF)return(EOF);
  if(c0<=0x7f) /* ascii */
    return(c0);
  if((c0&0xe0)==0xc0)
    { /* 2 bytes */
      if((nextch(c1))==EOF)err2(c0,c1); 
      if((c1&0xc0)!=0x80)err2(c0,c1);
      return((c0&0x1f)<<6|c1&0x3f);
    }
  if((c0&0xf0)==0xe0)
    { /* 3 bytes */
      if((nextch(c1))==EOF)err2(c0,c1);
      if((c1&0xc0)!=0x80)err2(c0,c1);
      if((nextch(c2))==EOF)err3(c0,c1,c2);
      if((c2&0xc0)!=0x80)err3(c0,c1,c2);
      return((c0&0xf)<<12|(c1&0x3f)<<6|c2&0x3f);
    }
  if((c0&0xf8)==0xf0)
    { /* 4 bytes */
      if((nextch(c1))==EOF)err2(c0,c1);
      if((c1&0xc0)!=0x80)err2(c0,c1);
      if((nextch(c2))==EOF)err3(c0,c1,c2);
      if((c2&0xc0)!=0x80)err3(c0,c1,c2);
      if((nextch(c3))==EOF)err4(c0,c1,c2,c3);
      if((c3&0xc0)!=0x80)err4(c0,c1,c2,c3);
      return((c0&7)<<18|(c1&0x3f)<<12|(c2&0x3f)<<6|c3&0x3f);
    }
  err(c0);
}

void outUTF8(unicode u, FILE *fil)
{ if(u<=0x7f)
  /* ascii */
    out(u,fil); else
  if(u<=0x7ff)
  /* latin1 and other chars requiring 2 octets */
    out(0xc0|(u&0x7c0)>>6,fil),out(0x80|u&0x3f,fil); else
  if(u<=0xffff)
  /* to here is basic multilingual plane */
    out(0xe0|(u&0xf000)>>12,fil),out(0x80|(u&0xfc0)>>6,fil),out(0x80|u&0x3f,fil); else
  if(u<=0x10ffff)
  /* other planes - rarely used - 4 octets */
    out(0xf0|(u&0x1c0000)>>18,fil),out(0x80|(u&0x3f000)>>12,fil),out(0x80|(u&0xfc0)>>6,fil),
        out(0x80|u&0x3f,fil); else
  /* codes above 0x10ffff not valid */
  fprintf(stderr,"char 0x%lx out of unicode range\n",u),exit(1);
}
