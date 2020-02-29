||The infinite list of all prime numbers, by the sieve of Eratosthenes.
||To see the list, just say `primes', or if you prefer
||	lay(map show primes)
||will print them one per line.  Hit control-C (interrupt) to stop.

primes = sieve[2..]
sieve (p:x) = p:sieve[n|n<-x;n mod p~=0]
