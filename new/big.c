/* MIRANDA INTEGER PACKAGE */
/* package for unbounded precision integers */

/**************************************************************************
 * Copyright (C) Research Software Limited 1985-90.  All rights reserved. *
 * The Miranda system is distributed as free software under the terms in  *
 * the file "COPYING" which is included in the distribution.              *
 *------------------------------------------------------------------------*/

#include "data.h"
#include "big.h"
#include <errno.h>

static double logIBASE,log10IBASE;
word big_one;

void bigsetup()
{ logIBASE=log((double)IBASE);
  log10IBASE=log10((double)IBASE);
  big_one=make(INT,1,0);
}

word isnat(x)
word x;
{ return(tag[x]==INT&&poz(x));
}

word sto_word(i)  /* store C long long as mira bigint */
long long i;
{ word s,x;
  if(i<0)s=SIGNBIT,i= -i; else s=0;
  x=make(INT,s|i&MAXDIGIT,0);
  if(i>>=DIGITWIDTH)
    { word *p = &rest(x);
      *p=make(INT,i&MAXDIGIT,0),p= &rest(*p);
      while(i>>=DIGITWIDTH)
           *p=make(INT,i&MAXDIGIT,0),p= &rest(*p); }
  return(x);
} /* change to long long, DT Oct 2019 */

#define maxval (1ll<<60)

long long get_word(x) /* mira bigint to C long long */
word x;
{ long long n=digit0(x);
  word sign=neg(x);
  if(!(x=rest(x)))return(sign?-n:n);
{ word w=DIGITWIDTH;
  while(x&&w<60)n+=(long long)digit(x)<<w,w+=DIGITWIDTH,x=rest(x);
  if(x)n=maxval; /* overflow, return large value */
  return(sign?-n:n);
}} /* change to long long, DT Oct 2019 */

word bignegate(x)
word x;
{ if(bigzero(x))return(x);
  return(make(INT,hd[x]&SIGNBIT?hd[x]&MAXDIGIT:SIGNBIT|hd[x],tl[x]));
}

word bigplus(x,y)
word x,y;
{ if(poz(x))
    if(poz(y))return(big_plus(x,y,0));
    else return(big_sub(x,y));
  else
    if(poz(y))return(big_sub(y,x));
    else return(big_plus(x,y,SIGNBIT)); /* both negative */
}

word big_plus(x,y,signbit) /* ignore input signs, treat x,y as positive */
word x,y; word signbit;
{ word d=digit0(x)+digit0(y);
  word carry = ((d&IBASE)!=0);
  word r = make(INT,signbit|d&MAXDIGIT,0); /* result */
  word *z = &rest(r); /* pointer to rest of result */
  x = rest(x); y = rest(y);
  while(x&&y) /* this loop has been unwrapped once, see above */
       { d = carry+digit(x)+digit(y);
	 carry = ((d&IBASE)!=0);
         *z = make(INT,d&MAXDIGIT,0);
	 x = rest(x); y = rest(y); z = &rest(*z); }
  if(y)x=y; /* by convention x is the longer one */
  while(x)
       { d = carry+digit(x);
	 carry = ((d&IBASE)!=0);
         *z = make(INT,d&MAXDIGIT,0);
	 x = rest(x); z = &rest(*z); }
  if(carry)*z=make(INT,1,0);
  return(r);
}

word bigsub(x,y)
word x,y;
{ if(poz(x))
    if(poz(y))return(big_sub(x,y));
    else return(big_plus(x,y,0)); /* poz x, negative y */
  else
    if(poz(y))return(big_plus(x,y,SIGNBIT)); /* negative x, poz y */
    else return(big_sub(y,x)); /* both negative */
}

word big_sub(x,y) /* ignore input signs, treat x,y as positive */
word x,y;
{ word d = digit0(x)-digit0(y);
  word borrow = (d&IBASE)!=0;
  word r=make(INT,d&MAXDIGIT,0);  /* result */
  word *z = &rest(r);
  word *p=NULL; /* pointer to trailing zeros, if any */
  x = rest(x); y = rest(y);
  while(x&&y) /* this loop has been unwrapped once, see above */
       { d = digit(x)-digit(y)-borrow;
	 borrow = (d&IBASE)!=0;
	 d = d&MAXDIGIT;
         *z = make(INT,d,0);
	 if(d)p=NULL; else if(!p)p=z;
	 x = rest(x); y = rest(y); z = &rest(*z); }
  while(y) /* at most one of these two loops will be invoked */
       { d = -digit(y)-borrow;
	 borrow = ((d&IBASE)!=0);
	 d = d&MAXDIGIT;
         *z = make(INT,d,0);
	 if(d)p=NULL; else if(!p)p=z;
	 y = rest(y); z = &rest(*z); }
  while(x) /* alternative loop */
       { d = digit(x)-borrow;
	 borrow = ((d&IBASE)!=0);
	 d = d&MAXDIGIT;
         *z = make(INT,d,0);
	 if(d)p=NULL; else if(!p)p=z;
	 x = rest(x); z = &rest(*z); }
  if(borrow) /* result is negative  - take complement and add 1 */
    { p=NULL;
      d = (digit(r)^MAXDIGIT) + 1;
      borrow = ((d&IBASE)!=0);  /* borrow now means `carry' (sorry) */
      digit(r) = SIGNBIT|d;  /* set sign bit of result */
      z = &rest(r);
      while(*z)
	   { d = (digit(*z)^MAXDIGIT)+borrow;
             borrow = ((d&IBASE)!=0);
             digit(*z) = d = d&MAXDIGIT;
	     if(d)p=NULL; else if(!p)p=z;
	     z = &rest(*z); }
    }
  if(p)*p=0; /* remove redundant (ie trailing) zeros */
  return(r);
}

word bigcmp(x,y)  /* returns +ve,0,-ve as x greater than, equal, less than y */
word x,y;
{ word d,r,s=neg(x);
  if(neg(y)!=s)return(s?-1:1);
  r=digit0(x)-digit0(y);
  for(;;)
     { x=rest(x); y=rest(y);
       if(!x)if(y)return(s?1:-1);
             else return(s?-r:r);
       if(!y)return(s?-1:1);
       d=digit(x)-digit(y);
       if(d)r=d; }
}

word bigtimes(x,y) /* naive multiply - quadratic */
word x,y;
{ if(len(x)<len(y))
    { word hold=x; x=y; y=hold; }  /* important optimisation */
  word r=make(INT,0,0);
  word d = digit0(y);
  word s=neg(y);
  word n=0;
  if(bigzero(x))return(r);  /* short cut */
  for(;;)
     { if(d)r = bigplus(r,shift(n,stimes(x,d)));
       n++;
       y = rest(y);
       if(!y)
         return(s!=neg(x)?bignegate(r):r);
       d=digit(y); }
}


word shift(n,x) /* multiply big x by n'th power of IBASE */
word n,x;
{ while(n--)x=make(INT,0,x);
  return(x);
} /* NB - we assume x non-zero, else unnormalised result */

word stimes(x,n)  /* multiply big x (>=0) by digit n (>0) */
word x,n;
{ unsigned d= n*digit0(x);  /* ignore sign of x */
  word carry=d>>DIGITWIDTH;
  word r = make(INT,d&MAXDIGIT,0);
  word *y = &rest(r);
  while(x=rest(x))
       d=n*digit(x)+carry,
       *y=make(INT,d&MAXDIGIT,0),
       y = &rest(*y),
       carry=d>>DIGITWIDTH;
  if(carry)*y=make(INT,carry,0);
  return(r);
}

word b_rem;  /* contains remainder from last call to longdiv or shortdiv */

word bigdiv(x,y)  /* may assume y~=0 */
word x,y;
{ word s1,s2,q; 
  /* make x,y positive and remember signs */
  if(s1=neg(y))y=make(INT,digit0(y),rest(y));
  if(neg(x))
    x=make(INT,digit0(x),rest(x)),s2=!s1; 
  else s2=s1;
  /* effect: s1 set iff y negative, s2 set iff signs mixed */
  if(rest(y))q=longdiv(x,y);
  else q=shortdiv(x,digit(y));
  if(s2){ if(!bigzero(b_rem))
	    { x=q;
	      while((digit(x)+=1)==IBASE) /* add 1 to q in situ */
                   { digit(x)=0;
		     if(!rest(x)){ rest(x)=make(INT,1,0); break; }
		     else x=rest(x);
		   }
	    }
          if(!bigzero(q))digit(q)=SIGNBIT|digit(q);
	}
  return(q);
}

word bigmod(x,y)  /* may assume y~=0 */
word x,y;
{ word s1,s2;
  /* make x,y positive and remember signs */
  if(s1=neg(y))y=make(INT,digit0(y),rest(y));
  if(neg(x))
    x=make(INT,digit0(x),rest(x)),s2=!s1; 
  else s2=s1;
  /* effect: s1 set iff y negative, s2 set iff signs mixed */
  if(rest(y))longdiv(x,y);
  else shortdiv(x,digit(y));
  if(s2){ if(!bigzero(b_rem))
	    b_rem = bigsub(y,b_rem);
	}
  return(s1?bignegate(b_rem):b_rem);
}

/* NB - above have entier based handling of signed cases  (as  Miranda)  in
   which  remainder  has  sign  of  divisor.   To  get  this:-  if signs of
   divi(sor/dend) mixed negate quotient  and  if  remainder  non-zero  take
   complement and add one to magnitude of quotient */

/* for alternative, truncate based handling of signed cases (usual in  C):-
   magnitudes  invariant  under  change  of  sign,  remainder  has  sign of
   dividend, quotient negative if signs of divi(sor/dend) mixed */

word shortdiv(x,n) /* divide big x by single digit n returning big quotient
		 and setting external b_rem as side effect */
              /* may assume - x>=0,n>0 */
word x,n;
{ word d=digit(x),s_rem,q=0;
  while(x=rest(x))  /* reverse rest(x) into q */
       q=make(INT,d,q),d=digit(x);  /* leaving most sig. digit in d */
  { word tmp;
    x=q; s_rem=d%n; d=d/n;
    if(d||!q)q=make(INT,d,0); /* put back first digit (if not leading 0) */
    else q=0;
    while(x) /* in situ division of q by n AND destructive reversal */
         d=s_rem*IBASE+digit(x),digit(x)=d/n,s_rem=d%n,
	 tmp=x,x=rest(x),rest(tmp)=q,q=tmp;
  }
  b_rem=make(INT,s_rem,0);
  return(q);
}

word longdiv(x,y)  /* divide big x by big y returning quotient, leaving
		 remainder in extern variable b_rem */
              /* may assume - x>=0,y>0 */
word x,y;
{ word n,q,ly,y1,scale;
  if(bigcmp(x,y)<0){ b_rem=x; return(make(INT,0,0)); }
  y1=msd(y);
  if((scale=IBASE/(y1+1))>1) /* rescale if necessary */
    x=stimes(x,scale),y=stimes(y,scale),y1=msd(y);
  n=q=0;ly=len(y);
  while(bigcmp(x,y=make(INT,0,y))>=0)n++;
  y=rest(y);  /* want largest y not exceeding x */
  ly += n;
  for(;;)
     { word d,lx=len(x);
       if(lx<ly)d=0; else
       if(lx==ly)
         if(bigcmp(x,y)>=0)x=bigsub(x,y),d=1;
	 else d=0;
       else{ d=ms2d(x)/y1;
	     if(d>MAXDIGIT)d=MAXDIGIT;
	     if((d -= 2)>0)x=bigsub(x,stimes(y,d));
	     else d=0;
             if(bigcmp(x,y)>=0)
	       { x=bigsub(x,y),d++;
                 if(bigcmp(x,y)>=0)
		   x=bigsub(x,y),d++; }
	   }
       q = make(INT,d,q);
       if(n-- ==0)
	 { b_rem = scale==1?x:shortdiv(x,scale); return(q); }
       ly-- ; y = rest(y); }
} /* see Bird & Wadler p82 for explanation */

word len(x) /* no of digits in big x */
word x;
{ word n=1;
  while(x=rest(x))n++;
  return(n);
}

word msd(x) /* most significant digit of big x */
word x;
{ while(rest(x))x=rest(x);
  return(digit(x)); /* sign? */
}

word ms2d(x) /* most significant 2 digits of big x (len>=2) */
word x;
{ word d=digit(x);
  x=rest(x);
  while(rest(x))d=digit(x),x=rest(x);
  return(digit(x)*IBASE+d);
}

word bigpow(x,y)  /* assumes y poz */
word x,y;
{ word d,r=make(INT,1,0);
  while(rest(y))  /* this loop has been unwrapped once, see below */
       { word i=DIGITWIDTH;
	 d=digit(y);
	 while(i--)
	      { if(d&1)r=bigtimes(r,x); 
		x = bigtimes(x,x);
		d >>= 1; }
	 y=rest(y);
       }
  d=digit(y);
  if(d&1)r=bigtimes(r,x); 
  while(d>>=1)
       { x = bigtimes(x,x);
         if(d&1)r=bigtimes(r,x); }
  return(r);
}

double bigtodbl(x)
word x;
{ word s=neg(x);
  double b=1.0, r=(double)digit0(x);
  x = rest(x);
  while(x)b=b*IBASE,r=r+b*digit(x),x=rest(x);
  if(s)return(-r);
  return(r);
} /* small end first */
/* note: can return oo, -oo
   but is used without surrounding sto_/set)dbl() only in compare() */

/* not currently used
long double bigtoldbl(x)
word x;
{ int s=neg(x);
  long double b=1.0L, r=digit0(x);
  x = rest(x);
  while(x)b=b*IBASE,r=r+b*digit(x),x=rest(x);
/*printf("bigtoldbl returns %Le\n",s?-r:r); /* DEBUG
  if(s)return(-r);
  return(r);
} /* not compatible with std=c90, lib fns eg sqrtl broken */

word dbltobig(x)  /* entier */
double x;
{ word s= (x<0);
  word r=make(INT,0,0);
  word *p = &r;
  double y= floor(x);
/*if(fabs(y-x+1.0)<1e-9)y += 1.0; /* trick due to Peter Bartke, see note */
  for(y=fabs(y);;)
     { double n = fmod(y,(double)IBASE);
       digit(*p) = (word)n;
       y = (y-n)/(double)IBASE; 
       if(y>0.0)rest(*p)=make(INT,0,0),p=&rest(*p);
       else break;
     }
  if(s)digit(r)=SIGNBIT|digit(r);
  return(r);
}
/* produces junk in low order digits if x exceeds range in which integer
   can be held without error as a double -- NO, see next comment */
/* hugs, ghci, mira produce same integer for floor/entier hugenum, has 2^971
   as factor so the low order bits are NOT JUNK -- 9.1.12 */

/* note on suppressed fix:
   choice of 1e9 arbitrary, chosen to prevent eg entier(100*0.29) = 28
   but has undesirable effects, causing eg entier 1.9999999999 = 2
   underlying problem is that computable floor on true Reals is _|_ at
   the exact integers.  There are inherent deficiences in 64 bit fp,
   no point in trying to mask this */

double biglog(x)  /* logarithm of big x */
word x;
{ word n=0;
  double r=digit(x);
  if(neg(x)||bigzero(x))errno=EDOM,math_error("log");
  while(x=rest(x))n++,r=digit(x)+r/IBASE;
  return(log(r)+n*logIBASE);
}

double biglog10(x)  /* logarithm of big x */
word x;
{ word n=0;
  double r=digit(x);
  if(neg(x)||bigzero(x))errno=EDOM,math_error("log10");
  while(x=rest(x))n++,r=digit(x)+r/IBASE;
  return(log10(r)+n*log10IBASE);
}

word bigscan(p)  /* read a big number (in decimal) */
            /* NB does NOT check for malformed number, assumes already done */
char *p;    /* p is a pointer to a null terminated string of digits */
{ word s=0,r=make(INT,0,0);
  if(*p=='-')s=1,p++; /* optional leading `-' (for NUMVAL) */
  while(*p)
       { word d= *p-'0',f=10;
	 p++;
	 while(*p&&f<PTEN)d=10*d+*p-'0',f=10*f,p++;
	 /* rest of loop does r=f*r+d; (in situ) */
	 d= f*digit(r)+d;
       { word carry=d>>DIGITWIDTH;
	 word *x = &rest(r);
	 digit(r)=d&MAXDIGIT;
	 while(*x)
	      d=f*digit(*x)+carry,
	      digit(*x)=d&MAXDIGIT,
	      carry=d>>DIGITWIDTH,
	      x = &rest(*x);
	 if(carry)*x=make(INT,carry,0);
       }}
/*if(*p=='e')
    { int s=bigscan(p+1);
      r = bigtimes(r,bigpow(make(INT,10,0),s); } */
  if(s&&!bigzero(r))digit(r)=digit(r)|SIGNBIT;
  return(r);
}
/* code to handle (unsigned) exponent commented out */

word bigxscan(p,q)  /* read unsigned hex number in '\0'-terminated string p to q */
               /* assumes redundant leading zeros removed */
char *p, *q;
{ word r; /* will hold result */
  word *x = &r;
  if(*p=='0'&&!p[1])return make(INT,0,0);
  while(q>p)
       { unsigned long long hold;
         q = q-p<15 ? p : q-15; /* read upto 15 hex digits from small end */
         sscanf(q,"%llx",&hold);
         *q = '\0';
         word count=4; /* 15 hex digits => 4 bignum digits */
         while(count-- && !(hold==0 && q==p))
              *x = make(INT,hold&MAXDIGIT,0),
              hold >>= DIGITWIDTH,
              x = &rest(*x);
       }
  return r;
}

word bigoscan(p,q)  /* read unsigned octal number in '\0'-terminated string p to q */
               /* assumes redundant leading zeros removed */
char *p, *q;
{ word r; /* will hold result */
  word *x = &r;
  while(q>p)
       { unsigned word hold;
         q = q-p<5 ? p : q-5; /* read (upto) 5 octal digits from small end */
         sscanf(q,"%o",&hold);
         *q = '\0';
         *x = make(INT,hold,0),
         x = &rest(*x);
       }
  return r;
}

word digitval(c)
char c;
{ return isdigit(c)?c-'0':
         isupper(c)?10+c-'A':
         10+c-'a'; }

word strtobig(z,base) /* numeral (as Miranda string) to big number */
                 /* does NOT check for malformed numeral, assumes
	            done and that z fully evaluated */
word z; word base;
{ word s=0,r=make(INT,0,0),PBASE=PTEN;
  if(base==16)PBASE=PSIXTEEN; else
  if(base==8)PBASE=PEIGHT;
  if(z!=NIL&&hd[z]=='-')s=1,z=tl[z]; /* optional leading `-' (for NUMVAL) */
  if(base!=10)z=tl[tl[z]]; /* remove "0x" or "0o" */
  while(z!=NIL)
       { word d=digitval(hd[z]),f=base;
         z=tl[z];
         while(z!=NIL&&f<PBASE)d=base*d+digitval(hd[z]),f=base*f,z=tl[z];
         /* rest of loop does r=f*r+d; (in situ) */
         d= f*digit(r)+d;
       { word carry=d>>DIGITWIDTH;
         word *x = &rest(r);
         digit(r)=d&MAXDIGIT;
         while(*x)
              d=f*digit(*x)+carry,
              digit(*x)=d&MAXDIGIT,
              carry=d>>DIGITWIDTH,
              x = &rest(*x);
         if(carry)*x=make(INT,carry,0);
       }}
  if(s&&!bigzero(r))digit(r)=digit(r)|SIGNBIT;
  return(r);
}

extern char *dicp;

word bigtostr(x) /* number to decimal string (as Miranda list) */
word x;
{ word x1,sign,s=NIL;
#ifdef DEBUG
  extern word debug;
  if(debug&04)  /* print octally */
    { word d=digit0(x);
      sign=neg(x);
      for(;;)
	   { word i=OCTW;
	     while(i--||d)s=cons('0'+(d&07),s),d >>= 3;
             x=rest(x);
             if(x)s=cons(' ',s),d=digit(x);
	     else return(sign?cons('-',s):s); }
    }
#endif
  if(rest(x)==0)
    { extern char *dicp;
      sprintf(dicp,"%d",getsmallint(x));
      return(str_conv(dicp)); }
  sign=neg(x);
  x1=make(INT,digit0(x),0); /* reverse x into x1 */
  while(x=rest(x))x1=make(INT,digit(x),x1);
  x=x1;
  for(;;)
     { /* in situ division of (reversed order) x by PTEN */
       word d=digit(x),rem=d%PTEN;
       d=d/PTEN; x1=rest(x);
       if(d)digit(x)=d;
       else x=x1; /* remove leading zero from result */
       while(x1)
            d=rem*IBASE+digit(x1),
            digit(x1)=d/PTEN,
            rem=d%PTEN,
            x1=rest(x1);
       /* end of in situ division (also uses x1 as temporary) */
       if(x)
         { word i=TENW;
	   while(i--)s=cons('0'+rem%10,s),rem=rem/10; }
       else
	 { while(rem)s=cons('0'+rem%10,s),rem=rem/10;
           return(sign?cons('-',s):s); }
     }
}

word bigtostrx(x) /* integer to hexadecimal string (as Miranda list) */
word x;
{ word r=NIL, s=neg(x);
  while(x)
       { word count=4; /* 60 bits => 20 octal digits => 4 bignum digits */
         unsigned long long factor=1;
         unsigned long long hold=0;
         while(count-- && x) /* calculate value of (upto) 4 bignum digits */
              hold=hold+factor*digit0(x),
              /* printf("::%llx\n",hold), /* DEBUG */
              factor<<=15,
              x=rest(x);
         sprintf(dicp,"%.15llx",hold); /* 15 hex digits = 60 bits */
         /* printf(":::%s\n",dicp); /* DEBUG */
         char *q=dicp+15;
         while(--q>=dicp)r = cons(*q,r);
       }
  while(digit(r)=='0'&&rest(r)!=NIL)r=rest(r); /* remove redundant leading 0's */
  r = cons('0',cons('x',r));
  if(s)r = cons('-',r);
  return(r);
}

word bigtostr8(x) /* integer to octal string (as Miranda list) */
word x;
{ word r=NIL, s=neg(x);
  while(x)
       { char *q = dicp+5;
         sprintf(dicp,"%.5o",digit0(x));
         while(--q>=dicp)r = cons(*q,r);
         x = rest(x); }
  while(digit(r)=='0'&&rest(r)!=NIL)r=rest(r); /* remove redundant leading 0's */
  r = cons('0',cons('o',r));
  if(s)r = cons('-',r);
  return(r);
}

#ifdef DEBUG
wff(x) /* check for well-formation of integer */
word x;
{ word y=x;
  if(tag[x]!=INT)printf("BAD TAG %d\n",tag[x]);
  if(neg(x)&&!digit0(x)&&!rest(x))printf("NEGATIVE ZERO!\n");
  if(digit0(x)&(~MAXDIGIT))printf("OVERSIZED DIGIT!\n");
  while(x=rest(x))
       if(tag[x]!=INT)printf("BAD INTERNAL TAG %d\n",tag[x]); else
       if(digit(x)&(~MAXDIGIT))
	 printf("OVERSIZED DIGIT!\n"); else
       if(!digit(x)&&!rest(x))
	 printf("TRAILING ZERO!\n");
  return(y);
}

normalise(x)  /* remove trailing zeros */
word x;
{ if(rest(x))rest(x)=norm1(rest(x));
  return(wff(x));
}

norm1(x)
word x;
{ if(rest(x))rest(x)=norm1(rest(x));
  return(!digit(x)&&!rest(x)?0:x);
}

#endif

/* stall(s)
char *s;
{ fprintf(stderr,"big integer %s not yet implemented\n",s);
  exit(0);
}

#define destrev(x,y,z)  while(x)z=x,x=rest(x),rest(z)=y,y=z;
/* destructively reverse x into y using z as temp */

/* END OF MIRANDA INTEGER PACKAGE */

