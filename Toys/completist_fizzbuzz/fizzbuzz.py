#!/usr/bin/env python
'''A testable fizzbuzz program.'''

def fizzbuzz(n):
  '''Get the 'FizzBuzz' value for 'n'.
  For 'n' a multiple of 3 return 'Fizz'; a multiple of five return 'Buzz';
  a multiple of both 3 and 5 return 'FizzBuzz'. For all others, return n.
  Args:
    - n: integer
  Returns:
    - string
  '''
  fb = ''
  if n % 3 == 0:
    fb += 'Fizz'
  if n % 5 == 0:
    fb += 'Buzz'
  return fb if len(fb) > 0 else str(n)

if __name__ == '__main__':
  for n in range(1, 101):
    print(fizzbuzz(n))
