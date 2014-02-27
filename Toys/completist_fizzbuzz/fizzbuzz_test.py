#!/usr/bin/env python
'''Tests for FizzBuzz.'''

import fizzbuzz
import unittest

class TestFizzBuzz(unittest.TestCase):
  def test_plain(self):
    self.assertEqual('2', fizzbuzz.fizzbuzz(2))
    self.assertEqual('14', fizzbuzz.fizzbuzz(14))

  def test_div3(self):
    self.assertEqual('Fizz', fizzbuzz.fizzbuzz(3))
    self.assertEqual('Fizz', fizzbuzz.fizzbuzz(27))
    self.assertNotEqual('Fizz', fizzbuzz.fizzbuzz(15))

  def test_div5(self):
    self.assertEqual('Buzz', fizzbuzz.fizzbuzz(5))
    self.assertEqual('Buzz', fizzbuzz.fizzbuzz(25))
    self.assertNotEqual('Buzz', fizzbuzz.fizzbuzz(15))

  def test_divboth(self):
    self.assertEqual('FizzBuzz', fizzbuzz.fizzbuzz(15))
    self.assertEqual('FizzBuzz', fizzbuzz.fizzbuzz(75))

if __name__ == '__main__':
  unittest.main()
