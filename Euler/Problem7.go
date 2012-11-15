/* http://projecteuler.net/profile/cian.png
 * Problem 7: What is the 10001st prime number?
 * 2012-07-14 */

package main

import (
	"flag"
	"fmt"
	"library"
	"os"
	"strconv"
)

func main() {
	flag.Parse()
	if len(flag.Args()) != 1 {
		fmt.Println("usage: Problem7 <integer>")
		os.Exit(1)
	}
	n, err := strconv.ParseInt(flag.Arg(0), 10, 64)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	primes := library.Sieve(n)
	if len(primes) > 10001 {
		// primes[0] is the first prime, so we're off by one.
		fmt.Printf("10001st prime: %d.\n", primes[10000])
	} else {
		fmt.Printf("Only %d primes up to %d.\n", len(primes), n)
	}
}
