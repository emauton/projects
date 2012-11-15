/* http://projecteuler.net/profile/cian.png
 * Problem 1. Find the sum of all the multiples of 3 or 5 below 1000.
 * 2012-07-11 */

package main

import "fmt"

func SumMultiples(step int, max int) (sum int) {
	for x := step; x < max; x += step {
		sum += x
	}
	return
}

func main() {
	fmt.Println(SumMultiples(3, 1000) +
		SumMultiples(5, 1000) -
		SumMultiples(15, 1000))
}
