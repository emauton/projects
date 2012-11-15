/* http://projecteuler.net/profile/cian.png
   Problem 12: What is the value of the first triangle number to have over five
               hundred divisors?
   2012-07-17 */

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
		fmt.Println("usage: Problem12 <integer>")
		os.Exit(1)
	}
	n, err := strconv.Atoi(flag.Arg(0))
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	var triangle, i int64 = 0, 0
	for i = 1; ; i++ {
		triangle += i
		if len(library.Factors(triangle)) > n {
			fmt.Println(triangle)
			break
		}
	}
}
