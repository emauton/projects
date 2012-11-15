/* http://projecteuler.net/profile/cian.png
   Problem 268. Counting numbers with at least four distinct prime factors less than 100.
   2012-07-18 */

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
		fmt.Println("usage: Problem268 <integer>")
		os.Exit(1)
	}
	n, err := strconv.ParseInt(flag.Arg(0), 10, 64)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	primes := library.Sieve(100)
	tuples := make([]map[int64]bool, 0, 4)
	tup := make(map[int64]bool, 0)
	for _, p := range primes {
		tup[p] = true
	}
	tuples = append(tuples, tup)
        for i := 1; i <= 3; i++ {
		tup := make(map[int64]bool, 0)
		for _, p := range primes {
			for t, _ := range tuples[i-1] {
				if t%p != 0 {
					tup[t*p] = true
				}
			}
		}
		tuples = append(tuples, tup)
	}
	for t, _ := range tuples[3] {
		factors := library.CanonicalForm(t)
		fmt.Println(t, factors)
	}
	fmt.Println(n)
}
