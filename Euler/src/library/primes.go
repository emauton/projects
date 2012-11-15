// http://projecteuler.net/profile/cian.png 2012-07-13

// Package library contains support code for my Project Euler fun & games.
package library

import (
	"math"
	"sort"
)

// Sieve finds all primes less than or equal to n.
// A simple implementation of the sieve of Eratosthenes.
func Sieve(n int64) (primes []int64) {
	if n < 2 {
		primes = []int64{}
		return
	}
	// We could do this in half the memory by discarding even entries
	// entirely and doing some index futzing; as it is, we ignore them.
	composite := make([]bool, n+1)
	composite[0] = true // 0, 1 are really just "nonprime", but the
	composite[1] = true // code reads better with the name "composite".
	composite[2] = false

	sqrt := int64(math.Floor(math.Sqrt(float64(n))))
	var i int64
	for i = 3; i <= sqrt; i += 2 {
		if !composite[i] {
			for j := i * i; j <= n; j += i {
				composite[j] = true
			}
		}
	}

	primes = make([]int64, 0)
	primes = append(primes, 2)
	for i = 3; i <= n; i += 2 {
		if !composite[i] {
			primes = append(primes, i)
		}
	}
	return
}

// CanonicalForm finds the prime factorization of n as a map of prime : power.
func CanonicalForm(n int64) (factorization map[int64]int) {
	sqrt := int64(math.Floor(math.Sqrt(float64(n))))
	primes := Sieve(sqrt)
	factorization = make(map[int64]int, len(primes)+1)
	for _, p := range primes {
		for n%p == 0 {
			n /= p
			factorization[p]++
		}
	}
	if n > 1 {
		factorization[n] = 1
	}
	return
}

// SortInt64 fudges the missing sort.Int64s() by going via sort.Float64s().
func SortInt64(x []int64) {
	via := make([]float64, 0, len(x))
	for _, i := range x {
		via = append(via, float64(i))
	}
	sort.Float64s(via)
	for i, v := range via {
		x[i] = int64(v)
	}
	return
}

// Factors returns all factors of n.
func Factors(n int64) (factors []int64) {
	factorization := CanonicalForm(n)
	found := make(map[int64]bool, 0)
	found[1] = true
	for prime, power := range factorization {
		for p := 0; p < power; p++ {
			new_factors := make([]int64, 0)
			for f, _ := range found {
				new_factors = append(new_factors, f*prime)
			}
			for _, f := range new_factors {
				found[f] = true
			}
		}
	}
	factors = make([]int64, 0, len(found))
	for f, _ := range found {
		factors = append(factors, f)
	}
	SortInt64(factors)
	return
}

// GCD uses Euclid's method to compute the greatest common divisor of m and n.
func GCD(m, n int64) int64 {
	for n != 0 {
		m, n = n, m%n
	}
	return m
}
