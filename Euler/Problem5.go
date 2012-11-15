/* http://projecteuler.net/profile/cian.png
 * Problem 5. What is the smallest number divisible by each of the numbers 1 to 20?
 * 2012-07-14 */

package main

import (
	"flag"
	"fmt"
	"library"
	"math"
	"os"
	"strconv"
)

func main() {
	flag.Parse()
	if len(flag.Args()) != 1 {
		fmt.Println("usage: Problem5 <integer>")
		os.Exit(1)
	}
	n, err := strconv.ParseInt(flag.Arg(0), 10, 64)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	powers := make(map[int64]int, 0)
	var i int64
	for i = 2; i < n; i++ {
		factors := library.CanonicalForm(i)
		for prime, power := range factors {
			p, present := powers[prime]
			if present {
				if power > p {
					powers[prime] = power
				}
			} else {
				powers[prime] = power
			}
		}
	}
	var answer int64 = 1
	for prime, power := range powers {
		answer *= int64(math.Pow(float64(prime), float64(power)))
	}
	fmt.Println(answer)
}
