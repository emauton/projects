/* http://projecteuler.net/profile/cian.png
   Problem 9. Find the only Pythagorean triplet, {a, b, c}, for which a + b + c = 1000.
   Based on ideas in http://en.wikipedia.org/wiki/Pythagorean_triple.
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
		fmt.Println("usage: Problem9 <integer>")
		os.Exit(1)
	}
	t, err := strconv.ParseInt(flag.Arg(0), 10, 64)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	var m, n int64
L:
	// Generate all pairs (m, n) with m > n.
	// Choose t as an arbitrary upper bound for m; it is never reached (see below).
	for m = 2; m < t; m++ {
		for n = 1; n < m; n++ {
			// We treat this as an upper bound: if c > t, then
			// certainly a + b + c > t.
			if m*m+n*n > t {
				break L
			}
			// We only want to generate primitive triplets.
			if library.GCD(m, n) != 1 {
				continue
			}
			// Generate a triple from m, n per Euclid's formula.
			a := m*m - n*n
			b := 2 * m * n
			c := m*m + n*n
			// t is our target; if it's some multiple of a+b+c, we're done.
			if t%(a+b+c) == 0 {
				k := t / (a + b + c)
				fmt.Printf("(%d, %d, %d)\n", k*a, k*b, k*c)
			}
		}
	}
}
