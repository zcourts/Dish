module Main where
import Data.Dish.Murmur3

sample :: IO ()
sample = do let val = "abc 123"
            print $ murmur3Int val 42
            print $ murmur3IntegerX86 val 42
            print $ murmur3IntegerX64 val 42
            --
            d <- murmur3Raw  val 42 X86_32
            print d
            e <- murmur3Raw  val 42 X86_128
            print e
            f <- murmur3Raw  val 42 X64_128
            print f
            --
            print $ murmur3 val 42 X86_32
            print $ murmur3 val 42 X86_128
            print $ murmur3 val 42 X64_128
            --
            j <- murmur3Int' val 42
            print j
            k <- murmur3IntegerX86' val 42
            print k
            l <- murmur3IntegerX64' val 42
            print l
