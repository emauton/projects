/* http://projecteuler.net/profile/cian.png
 * Problem 4. Find the largest palindromic product of two 3-digit numbers.
 * 2012-07-13 */

package main

import (
	"fmt"
	"library"
)

func main() {
	a, b, max := 0, 0, 0
	for i := 999; i > 99; i-- {
		for j := i; j > 99; j-- {
			if i*j <= max {
				break
			}
			if library.Palindrome(i * j) {
				max = i * j
				a = i
				b = j
			}
		}
	}
	fmt.Printf("%d * %d = %d\n", a, b, max)
}
