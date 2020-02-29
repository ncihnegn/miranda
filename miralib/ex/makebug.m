> makebug size = [Tofile "/tmp/big.m" (big_def size)]
> big_def n = "big_list\n=[\n" ++ lay (rep n "  \"hello\",") ++ "  \"hello\"]\n"

This tests garbage collection during compilation.  First turn on gc
reports by saying
	/gc

To generate /tmp/big.m of chosen size say, e.g.
	makebug 10000
to get mira to compile the result say
	/f /tmp/big
Saying this repeatedly (or /f %) will force recompilations
Or from the command line after quitting mira
	rm /tmp/big.x   #to force recompilation
	mira -make -gc /tmp/big

If the heap becomes corrupted  you  will  see  strange  type  errors  or
"impossible event" messages.  From Rick Morgan at Durham, May 1990.
