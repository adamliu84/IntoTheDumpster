{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

import Foreign.C.Types
import Foreign.C.String (CString, withCString)
-- << Struct implementation
import Foreign.Storable (Storable(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Alloc (alloca)

data Point = Point
    { x :: CInt
    , y :: CInt
    } deriving (Show)

instance Storable Point where 
    sizeOf _ = sizeOf (undefined :: CInt) + sizeOf (undefined :: CInt)
    alignment _ = alignment (undefined :: CInt)

    peek ptr = do
        pX <- peekByteOff ptr 0
        pY <- peekByteOff ptr (sizeOf (undefined :: CInt))
        return Point { x = pX, y = pY }

    poke ptr (Point {..}) = do
        pokeByteOff ptr 0 x
        pokeByteOff ptr (sizeOf (undefined :: CInt)) y
-- >>

-- << Foreign Function Interface (FFI) binding
foreign import ccall "cpp_library.h add_cpp_numbers"
    addCppNumbers :: CInt -> CInt -> CInt

foreign import ccall "cpp_library.h multiply_cpp_numbers"
    multiplyCppNumbers :: CInt -> CInt -> CInt  

foreign import ccall "cpp_library.h log_cpp"
    logCppString :: CString -> IO ()

foreign import ccall "cpp_library.h update_cpp_point"
    updatePointCpp :: Ptr Point -> IO (Ptr Point)
-- >>

printCppString :: String -> IO ()
printCppString str = withCString str logCppString

updateCppStruct :: CInt -> CInt -> IO Point
updateCppStruct x y = do
    alloca $ \pPtr -> do
        let initialPoint = Point {x, y}
        putStrLn $ "Initial Point: " ++ show initialPoint    
        poke pPtr initialPoint
        updatePointCpp pPtr
        updatedPoint <- peek pPtr
        return updatedPoint

main :: IO ()
main = do
    let addResult = addCppNumbers 5 7
        multiplyResult = zipWith multiplyCppNumbers [1..3] [4..6]      
    print addResult
    print multiplyResult
    printCppString "It is from Haskell"
    updateCppStruct 8 12 >>= print  

