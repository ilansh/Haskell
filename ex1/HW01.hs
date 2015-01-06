{-

-}

module HW01 where

isThisWorking :: String
isThisWorking = "Yes"

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = n `div` 10 

toDigits :: Integer -> [Integer]
toDigits n
	| n < 1 	= []
	| otherwise	= lastDigit n:toDigits (dropLastDigit n)

reverseList :: [Integer] -> [Integer]
reverseList [] 			= []
reverseList (x:[]) 		= [x]
reverseList (x:y:xs)	= y:x:reverseList (xs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] 		= []
doubleEveryOther (x:[]) 	= [x]
doubleEveryOther (x:y:zs) 	= x:(y * 2):doubleEveryOther zs 
