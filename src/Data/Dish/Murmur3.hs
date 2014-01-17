{-# LANGUAGE ForeignFunctionInterface #-}
{- |  
This is a Haskell wrapper around the C port of MurmurHash3.
<https://github.com/zcourts/murmur3>
MurmurHash3 is available at 
<https://code.google.com/p/smhasher/wiki/MurmurHash3>
The hash functions are designed to work efficiently on x86 processors; 
in particular, they make some assumptions about the endianness of the processor, 
and about the speed of unaligned reads. 
-} 
module Data.Dish.Murmur3(
                         -- * Default/Direct Map
                         -- $default
                         murmur3,
                         -- * X86 32 bits
                         -- $32
                         murmur3Int,
                         -- * X86 128 bits
                         -- $x128
                         murmur3IntegerX86,
                         -- * X64 128 bits
                         -- $64_128
                         murmur3IntegerX64,
                         -- * FFI
                         murmur3Raw,
                         -- * Unsafe
                         unsafeMurmur3Int,
                         unsafeMurmur3IntegerX86,
                         unsafeMurmur3IntegerX64,
                         -- * Murmur3 Hash Version
                         MHV(..)
                         ) where
import Foreign.C
import Foreign.Ptr
import Foreign
import qualified Data.List as L
import qualified Data.Bits as B
import qualified System.IO.Unsafe as US

-- Murmur Hash version, one of 
data MHV = X86_32 | X86_128 | X64_128


{-$default 
Simple, verbose interface for generating hashes
-}
-- | Base function, which allows you to choose which 'MHV' to use        
murmur3 :: String -- ^ The string to be hashed
           -> Int -- ^ A seed value for the hash function
           -> MHV -- ^ Which Murmur Hash version to use 'X86_32', 'X86_128' or 'X64_128'
           -> IO [Int] -- ^ returns 4, 32 bit ints, if 'X86_32' is used only the first has a value and the other 3 are 0
murmur3 v s ver = do m <- murmur3Raw v s ver; toArr m
  where 
    toArr :: [CUInt] -> IO [Int]
    toArr [] = return []
    toArr l = return $ b l []     
              where b :: [CUInt] -> [Int] -> [Int]
                    b xs l2 = foldl (\ list x -> list ++ [w x] ) l2 xs
                    w :: CUInt -> Int
                    w = fromIntegral

{-$32
Generate 32 bit hash values
-}                    
{- | has the lowest throughput, but also the lowest latency. If you're making a 
hash table that usually has small keys, this is probably the one you want to use 
on 32-bit machines. It has a 32-bit output. -}    
murmur3Int :: String -- ^ The string to be hashed
                     -> Int -- ^ A seed value for the hash function
                     -> IO Int -- ^ 32 bit number generated from the string
murmur3Int val seed = do v <- murmur3Raw val seed X86_32; 
                         -- safe to use L.head, list is never empty even if all vals are 0
                         return $ fromIntegral (L.head v)

{-$x128
Generate 128 bit hash values, optimized for 32 bit systems
-}
{- | Generate a 128 bit hash from the given value, this function's implementation 
     is optimized for 32 bit architectures but works on any.
     Has about 30% higher throughput than 'murmur3Int'. Be warned, though, 
     that its latency for a single 16-byte key is about 86% longer!
-}
murmur3IntegerX86 :: String -- ^ The string to be hashed
                     -> Int -- ^ A seed value for the hash function
                     -> IO Integer -- ^ 128 bit number generated from the string
murmur3IntegerX86 val seed = x128 val seed X86_128
   
{-$64_128
Generate 128 bit hash values, optimized for 64 bit systems
-}
{- | Generate a 128 bit hash from the given value, this function's implementation 
     is optimized for x64 architectures but works on any.
     Its throughput is 250% higher than 'murmur3IntegerX86', but it has roughly 
     the same latency. 
-}
murmur3IntegerX64 :: String -- ^ The string to be hashed
                     -> Int -- ^ A seed value for the hash function
                     -> IO Integer -- ^ 128 bit number generated from the string
murmur3IntegerX64 val seed = x128 val seed X64_128

foreign import ccall "MurmurHash3_x86_32" c_x86_32
  ::  CString -> CInt -> CUInt ->  Ptr CUInt -> IO ()

foreign import ccall "MurmurHash3_x86_128" c_x86_128
  ::  CString -> CInt -> CUInt ->  Ptr CUInt -> IO ()

foreign import ccall "MurmurHash3_x64_128" c_x64_128
  ::  CString -> CInt -> CUInt ->  Ptr CUInt -> IO ()

-- | all murmur functions use this and manipulate its response to return a different format  
murmur3Raw :: String -> Int -> MHV -> IO [CUInt]
murmur3Raw val seed ver = do
  val' <- withCAStringLen val $ \x -> return x
  let cstr = strFromCStr val'
  let strLength = strLFromCStr val'
  outPtr <- mallocArray arrSize
  doHash ver cstr strLength (fromIntegral seed) outPtr
  peekArray arrSize outPtr
  where arrSize = 4
        strFromCStr :: CStringLen -> CString
        strFromCStr = fst
        strLFromCStr :: CStringLen -> CInt
        strLFromCStr i = fromIntegral $ snd i
        --version value size seed out 
        doHash :: MHV -> CString -> CInt -> CUInt -> Ptr CUInt -> IO()
        doHash X86_32  v s se o = c_x86_32 v s se o
        doHash X86_128 v s se o = c_x86_128 v s se o
        doHash X64_128 v s se o = c_x64_128 v s se o

unsafeMurmur3Int :: String -> Int -> Int
unsafeMurmur3Int val seed        = US.unsafePerformIO $ murmur3Int val seed

unsafeMurmur3IntegerX86 :: String -> Int -> Integer
unsafeMurmur3IntegerX86 val seed = US.unsafePerformIO $ murmur3IntegerX86 val seed

unsafeMurmur3IntegerX64 :: String -> Int -> Integer
unsafeMurmur3IntegerX64 val seed = US.unsafePerformIO $ murmur3IntegerX64 val seed

x128 :: String -> Int -> MHV -> IO Integer
x128 val seed ver= do 
  v <- hash ver 
  return $ twiddle 0 v 
  where hash :: MHV -> IO [CUInt]
        hash X86_128 = murmur3Raw val seed X86_128
        hash X64_128 = murmur3Raw val seed X64_128
        hash _       = return []
        twiddle :: Integer -> [CUInt] -> Integer
        twiddle i [] = i
        twiddle i (0:xs) = twiddle i xs -- don't shift when val is 0
        twiddle i (x:xs) = twiddle (B.shift i (B.bitSize x) `B.xor` fromIntegral x) xs