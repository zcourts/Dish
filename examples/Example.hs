module Main where
import Data.Dish.Murmur3

sample :: IO ()
sample = do let val = "abc 123"
          a <- murmur3Int val 42
          print a
          b <- murmur3IntegerX86 val 42
          print b
          c <- murmur3IntegerX64 val 42
          print c
          --
          d <- murmur3Raw  val 42 X86_32
          print d
          e <- murmur3Raw  val 42 X86_128
          print e
          f <- murmur3Raw  val 42 X64_128
          print f
          --
          g <- murmur3 val 42 X86_32
          print g
          h <- murmur3 val 42 X86_128
          print h
          i <- murmur3 val 42 X64_128
          print i
          --
          let j = unsafeMurmur3Int val 42
          print j
          let k = unsafeMurmur3IntegerX86 val 42
          print k
          let l = unsafeMurmur3IntegerX64 val 42
          print l
          