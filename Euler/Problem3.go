/* http://projecteuler.net/profile/cian.png
 * Problem 3. Find the largest prime factor of a composite number.
 * 2012-07-13 */

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
		fmt.Println("usage: Problem3 <integer>")
		os.Exit(1)
	}
	target, error := strconv.ParseInt(flag.Arg(0), 10, 64)
	if error != nil {
		fmt.Println(error)
		os.Exit(1)
	}
	factorization := library.CanonicalForm(target)
	var max int64
	for prime, _ := range factorization {
		if prime > max {
			max = prime
		}
	}
	fmt.Println(max)
}
