/* http://projecteuler.net/profile/cian.png
   Problem 10: Calculate the sum of all the primes below two million.
   2012-07-16 */

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
		fmt.Println("usage: Problem10 <integer>")
		os.Exit(1)
	}
	n, err := strconv.ParseInt(flag.Arg(0), 10, 64)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	primes := library.Sieve(n)
	var sum int64 = 0
	for _, p := range primes {
		sum += p
	}
	fmt.Println(sum)
}
