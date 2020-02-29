all: mira miralib/menudriver exfiles
#install paths relative to /
#for linux, MacOS X, Cygwin:
BIN=usr/bin
LIB=usr/lib#beware no spaces after LIB
MAN=usr/share/man/man1
#for Solaris:
#BIN=usr/local/bin
#LIB=usr/local/lib#beware no spaces after LIB
#MAN=usr/local/man/man1
CC = gcc -w
CFLAGS = #-O #-DCYGWIN #-DUWIN #-DIBMRISC #-Dsparc7 #-Dsparc8
#be wary of using anything higher than -O as the garbage collector may fall over
#if using gcc rather than clang try without -O first
EX = #.exe        #needed for CYGWIN, UWIN
YACC = byacc #Berkeley yacc, gnu yacc not compatible
# -Dsparc7 needed for Solaris 2.7
# -Dsparc8 needed for Solaris 2.8 or later
mira: big.o cmbnms.o data.o lex.o reduce.o steer.o trans.o types.o utf8.o y.tab.o \
			    version.c miralib/.version fdate .host Makefile
	$(CC) $(CFLAGS) -DVERS=`cat miralib/.version` -DVDATE="\"`./revdate`\"" \
	    -DHOST="`./quotehostinfo`" version.c cmbnms.o y.tab.o data.o lex.o \
	    big.o reduce.o steer.o trans.o types.o utf8.o -lm -o mira
	strip mira$(EX)
y.tab.c y.tab.h: rules.y
	$(YACC) -d rules.y
big.o cmbns.o data.o lex.o reduce.o steer.o trans.o types.o y.tab.o: \
                     data.h combs.h utf8.h y.tab.h Makefile
data.o: .xversion
big.o data.o lex.o reduce.o steer.o trans.o types.o: big.h
big.o data.o lex.o reduce.o steer.o rules.y types.o: lex.h
utf8.o: utf8.h Makefile
cmbnms.o: cmbnms.c Makefile
cmbnms.c combs.h: gencdecs
	./gencdecs
miralib/menudriver: menudriver.c Makefile
	$(CC) $(CFLAGS) menudriver.c -o miralib/menudriver
	chmod 755 miralib/menudriver$(EX)
	strip miralib/menudriver$(EX)
#alternative: use shell script
#	ln -s miralib/menudriver.sh miralib/menudriver
tellcc:
	@echo $(CC) $(CFLAGS)
cleanup:
#to be done on moving to a new host
	-rm -rf *.o fdate miralib/menudriver mira$(EX)
	./unprotect
	-rm -f miralib/preludx miralib/stdenv.x miralib/ex/*.x #miralib/ex/*/*.x
	./hostinfo > .host
install:
	make -s all
	cp mira$(EX) /$(BIN)
	cp mira.1 /$(MAN)
	rm -rf /$(LIB)/miralib
	./protect
	cp -pPR miralib /$(LIB)/miralib
	./unprotect
	find /$(LIB)/miralib -exec chown `./ugroot` {} \;
release:
	make -s all
	-rm -rf usr
	mkdir -p $(BIN) $(LIB) $(MAN)
	cp mira$(EX) $(BIN)
	cp mira.1 $(MAN)
	./protect
	cp -pPR miralib $(LIB)/miralib
	./unprotect
	find usr -exec chown `./ugroot` {} \;
	tar czf `rname`.tgz ./usr
	-rm -rf usr
SOURCES = .xversion big.c big.h gencdecs data.h data.c lex.h lex.c reduce.c rules.y \
          steer.c trans.c types.c utf8.h utf8.c version.c fdate.c
sources: $(SOURCES); @echo $(SOURCES)
exfiles:
	@-./mira -make -lib miralib ex/*.m
mira.1.html: mira.1 Makefile
	man2html mira.1 | sed '/Return to Main/d' > mira.1.html
