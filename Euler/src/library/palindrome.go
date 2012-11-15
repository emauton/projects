// http://projecteuler.net/profile/cian.png 2012-07-13

package library

// Digits decomposes a decimal number into its digits.
func Digits(n int) (digits []int) {
	digits = make([]int, 0)
	for n > 0 {
		digits = append(digits, n%10)
		n /= 10
	}
	return
}

// PalindromeList checks whether a list of integers is palindromic.
func PalindromeList(digits []int) bool {
	if len(digits) < 2 {
		return true
	}
	a, b := 0, len(digits)-1
	if digits[a] != digits[b] {
		return false
	}
	return PalindromeList(digits[a+1 : b])
}

// Palindrome checks whether a number is palindromic.
func Palindrome(n int) bool {
	digits := Digits(n)
	return PalindromeList(digits)
}
