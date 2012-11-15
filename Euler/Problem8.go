/* http://projecteuler.net/profile/cian.png
 * Problem 8. Find the greatest product of five consecutive digits in a 1000-digit number.
 * 2012-07-14 */

package main

import (
	"fmt"
	"io/ioutil"
)

func Multiply(digits []int) (n int) {
	n = 1
	for _, d := range digits {
		n *= d
	}
	return
}

func main() {
	buf, err := ioutil.ReadFile("data/Problem8")
	if err != nil {
		panic(err)
	}

	digits := make([]int, 0)
	for _, c := range buf {
		if int(c) != 10 {
			digits = append(digits, int(c)-48)
		}
	}

	a, b := 0, 5
	max := 0
	for b <= len(digits) {
		n := Multiply(digits[a:b])
		if n > max {
			max = n
		}
		a, b = a+1, b+1
	}

	fmt.Println(max)
}
