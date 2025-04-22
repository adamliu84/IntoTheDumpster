{-# LANGUAGE NumericUnderscores #-} -- NOTE  enabled by default in GHC 8.6.1

module NumericUnderscoresExample where

-- Integer literals with underscores for readability
largeNumber :: Integer
largeNumber = 1_000_000_000 -- One billion

anotherLargeNumber :: Integer
anotherLargeNumber = 10_000_000 -- Ten million

smallNumber :: Integer
smallNumber = 123_456

-- Floating-point literals with underscores
piValue :: Double
piValue = 3.141_592_653_589_793

smallFraction :: Float
smallFraction = 0.000_001

scientificNotation :: Double
scientificNotation = 1.234_e6 -- Represents 1.234 * 10^6

anotherScientific :: Double
anotherScientific = 5.67_e-3 -- Represents 5.67 * 10^-3

-- Underscores can be used anywhere within the numeric part
unusualFormatting :: Integer
unusualFormatting = 1_2_3_4_5

-- They are ignored by the compiler, only for human readability
sameAsAbove :: Integer
sameAsAbove = 12345

-- Binary literals
binaryNumber :: Int
binaryNumber = 0b1010_1111

anotherBinary :: Int
anotherBinary = 0b1111_0000

-- Octal literals
octalNumber :: Int
octalNumber = 0o755_644

anotherOctal :: Int
anotherOctal = 0o123_456

-- Hexadecimal literals
hexNumber :: Int
hexNumber = 0xBAD_CAFE

anotherHex :: Int
anotherHex = 0xFF_00_FF

main :: IO ()
main = do
  putStrLn $ "Large number: " ++ show largeNumber
  putStrLn $ "Another large number: " ++ show anotherLargeNumber
  putStrLn $ "Small number: " ++ show smallNumber
  putStrLn $ "Pi value: " ++ show piValue
  putStrLn $ "Small fraction: " ++ show smallFraction
  putStrLn $ "Scientific notation: " ++ show scientificNotation
  putStrLn $ "Another scientific: " ++ show anotherScientific
  putStrLn $ "Unusual formatting: " ++ show unusualFormatting
  putStrLn $ "Same as above: " ++ show sameAsAbove
  putStrLn $ "Binary number: " ++ show binaryNumber
  putStrLn $ "Another binary: " ++ show anotherBinary
  putStrLn $ "Octal number: " ++ show octalNumber
  putStrLn $ "Another octal: " ++ show anotherOctal
  putStrLn $ "Hexadecimal number: " ++ show hexNumber
  putStrLn $ "Another hex: " ++ show anotherHex

-- << OUTPUT
-- Large number: 1000000000
-- Another large number: 10000000
-- Small number: 123456
-- Pi value: 3.141592653589793
-- Small fraction: 1.0e-6
-- Scientific notation: 1234000.0
-- Another scientific: 5.67e-3
-- Unusual formatting: 12345
-- Same as above: 12345
-- Binary number: 175
-- Another binary: 240
-- Octal number: 252836
-- Another octal: 42798
-- Hexadecimal number: 195939070
-- Another hex: 16711935
-- >>