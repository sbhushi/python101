# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 16:25:32 2019

@author: Sharath
"""

# implementing a simple blockchain

# has two main classes here block and a blockchain

# import os
# clear = lambda: os.system('cls')
# clear()


import datetime as dt
import hashlib as hl
import math

#int(math.sqrt(4))

class Block:                                #Every block is an instance of this
    blockNo = 0  # block number
    data = None  # data we want to store
    next = None  # pointer to the next
    hash = None  # every block has a hash. Number of hashes that were required to find the real block hash....(remember the 2**256 cobinations!!)
    nonce = 0       # 'n(umber used)once' a secret random generated number used for the blockchain
    previous_hash = 0x0  # Hash of previous block!!
    timestamp = dt.datetime.now()  # timestamp for the whole network for synchronization

    # storing its own data

    def __init__(self, data):
        self.data = data                                #takes an argument data in this case

    # the part where we caculate the hash

    def hash(self):
        h = hl.sha256()                   #run the sha 256 hashing algorithm. The function is available by importing it directly...
        #Secure hash algorithm has different types for more info see wikipedia
        h.update(
            str(self.nonce).encode('utf-8') +
            str(self.data).encode('utf-8') +
            str(self.previous_hash).encode('utf-8') +           #the one that ties all the blocks together
            str(self.timestamp).encode('utf-8') +
            str(self.blockNo).encode('utf-8')
        )
        return h.hexdigest()

    # the above gives the complicated character with random numbers and alphabets

    # printing the block now
    def __str__(self):
        # return "Block Hash: " + str(self.hash()) + "\n Block No: " + str(self.blockNo)+\
        #        "\n -------"     #+"\n Block data: "+self.data()
        return "Block Hash: " + str(self.hash()) + "\nBlockNo: " + str(self.blockNo) + "\nBlock Data: " + str(
            self.data) + "\nHashes: " + str(self.nonce) + "\n--------------"

    # now for blockchain class ----- basically a linked list


class Blockchain:
    diff = 0  # difficulty level....MODIFY THIS!!!!!!! Default value is ZERO
    maxNonce = 2 ** 32    #maximum number that can be stored in 32 bit memory
    target = 2 ** (256 - diff)  #reducing the target range
    #The logic behind which is: the acceptable range has become smaller, so it's more tough to
    #mine a block..blocks has to be less than or equal to target range to be accepted!!

    block = Block("Genesis") # first block, name assigned to the beginning of block chain
    dummy = head = block  # head points only to first one and doesn't vary when new blocks are added

    def add(self, block):

        block.previous_has = self.block.hash()
        block.blockNo = self.block.blockNo + 1

        self.block.next = block
        self.block = self.block.next

    def mine(self, block):
        for n in range(self.maxNonce):          #to guess from 0 till maximum nounce
            if (int(block.hash(), 16) <= (self.target)): #to check the current blocks hash is <= target
                self.add(block)
                print(block)
                break
            else:
                block.nonce = block.nonce + 1



blockchain = Blockchain()

#For loop to generate 10 random blocks
for n in range(10):
    blockchain.mine(Block("Block " + str(n + 1)))

#printing out each block in a chain...The chain will be longer than just the 10 above
while(blockchain.head != None):
    print(blockchain.head)
    blockchain.head = blockchain.head.next





















