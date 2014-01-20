module Main where
import Data.Dish.Murmur3
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do   let val = Str "abc 123"
            let valByteString = B.pack $ strCon val
            print $ murmur3Int val 42
            print $ murmur3IntegerX86 val 42
            print $ murmur3IntegerX64 valByteString 42
            --
            d <- murmur3Raw  valByteString 42 X86_32
            print d
            e <- murmur3Raw  val 42 X86_128
            print e
            f <- murmur3Raw  val 42 X64_128
            print f
            --
            print $ murmur3 val 42 X86_32
            print $ murmur3 valByteString 42 X86_128
            print $ murmur3 val 42 X64_128
            --
            j <- murmur3Int' val 42
            print j
            k <- murmur3IntegerX86' val 42
            print k
            l <- murmur3IntegerX64' valByteString 42
            print l
