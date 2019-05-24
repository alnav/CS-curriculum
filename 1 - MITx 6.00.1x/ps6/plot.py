#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon May  6 08:43:24 2019

@author: alnav
"""
import pylab as plt
mySamples = []
myLinear = []
myQuadratic = []
myCubic = []
myExponential = []

for i in range(0, 30):
    mySamples.append(i)
    myLinear.append(i)
    myQuadratic.append(i**2.5)
    myCubic.append(i**3)
    myExponential.append(1.5**i)
    
plt.figure("lin")
plt.xlabel("linear")
plt.ylabel("quadratic")
plt.title("linear vs quadratic")
plt.ylim(0,10000)

plt.plot(myLinear, myQuadratic, label = 'linear')
plt.legend(loc = 'upper left')
plt.figure("quadratic")
plt.ylim(0,10000)

plt.plot(myLinear, myExponential)
#plt.plot(myLinear, myExponential)
#plt.plot(myQuadratic, myExponential)