# -*- coding: utf-8 -*-
animals = { 'a': ['aardvark'], 'b': ['baboon'], 'c': ['coati']}

animals['d'] = ['donkey']
animals['d'].append('dog')
animals['d'].append('dingo')


def how_many(aDict):
    '''
    aDict: A dictionary, where all the values are lists.

    returns: int, how many values are in the dictionary.
    '''
    howMany = 0
    for i in aDict.values():
        howMany += len(i)
    return howMany

def biggest(aDict):
    '''
    aDict: A dictionary, where all the values are lists.

    returns: The key with the largest number of values associated with it
    '''
    # Your Code Here
    newDict = {}
    # Create new dictionary with values as length of lists
    for key in animals.keys():
        newDict[key] = len(animals[key])
    # Finds in newDict the key which value is the max    
    for key in newDict:
        if newDict[key] == max(newDict.values()):
            return key
    