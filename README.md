# BitScala

## Reference

The BitScala library is the implementation in Scala of the main features of Bitcoin protocol.
The basis of this library is the the 
[Book of Jimmy Song 'Programming Bitcoin'](https://books.google.ch/books?id=O2aHDwAAQBAJ&printsec=frontcover&dq=programming+bitcoin&hl=en&sa=X&ved=0ahUKEwiwq57PhLniAhWv1aYKHZS5DRYQ6AEIMDAB#v=onepage&q=programming%20bitcoin&f=false).

## Purpose

The pupose of this project is to develop an understanding of the basics 
of the Bitcoin protocol. It is an effort to bring the Bitcoin knowledge
to a broader public. The main purpose of this project lies on education.
The goal is to have a broader set of developers supporting the ecosystem
Bitcoin programming.


 
## Features

- Serialization/Deserialization of transactions
- Signing and verifying transactions
- executing Bitcoin-Script

## Dependencies
There is one depending library [Bouncy Castle Provider » 1.61]( https://mvnrepository.com/artifact/org.bouncycastle/bcprov-jdk15on/1.61),
which is used for 
- signing the transaction (PrivateKey.sign: avoid self implementing deterministic K)
- verify the transaction with ECDSASigner
- Use of hashing functions: RIPEMD160Digest, SHA1Digest, SHA256Digest

