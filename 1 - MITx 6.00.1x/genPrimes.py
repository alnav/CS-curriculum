#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu May  2 17:16:20 2019

@author: alnav
"""

#def genPrimes():
#    listPrimes = [3,5]
#    num = 5
#    while True:
#        num += 1
#        for n in listPrimes:
#            if (num % n) == 0:
#                break
#            else:
#                listPrimes.append(num)
#                yield num
#        
#               

def genPrimes():
    primes = []   # primes generated so far
    last = 1      # last number tried
    while True:
        last += 1
        for p in primes:
            if last % p == 0:
                break
        else:
            primes.append(last)
            yield last