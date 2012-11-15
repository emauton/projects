/* http://projecteuler.net/profile/cian.png
 * Problem 2. Find the sum of even terms less than 4 million in the Fibonacci sequence.
 * 2012-07-12 */

package main

import "fmt"

func main() {
	a, b, sum := 1, 2, 0
	for b < 4e6 {
		if b%2 == 0 {
			sum += b
		}
		a, b = b, a+b
	}
	fmt.Println(sum)
}
