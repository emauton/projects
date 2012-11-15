/* http://projecteuler.net/profile/cian.png
 * Problem 6. What is the difference between the sum of the squares and the square of the sums?
 * 2012-07-14 */

package main

import "fmt"

func main() {
	sumSquares, sumTotal := 0, 0
	for i := 1; i <= 100; i++ {
		sumSquares += i * i
		sumTotal += i
	}
	fmt.Println(sumTotal*sumTotal - sumSquares)
}
