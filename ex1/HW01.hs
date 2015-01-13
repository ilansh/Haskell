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
	| otherwise	= toDigits (dropLastDigit n) ++ [lastDigit n]

reverseList :: [Integer] -> [Integer]
reverseList [] 			= []
reverseList (x:xs)	= reverseList (xs) ++ [x]

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper []           = []
doubleEveryOtherHelper (x:[])       = [x]
doubleEveryOtherHelper (x:y:zs)     = x:(y * 2):doubleEveryOtherHelper(zs)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] 		= []
doubleEveryOther (x:[]) 	= [x]
doubleEveryOther (x:xs) 	= reverseList (doubleEveryOtherHelper (reverseList (x:xs) ))

sumDigits :: [Integer] -> Integer
sumDigits []        = 0
sumDigits (x:[])    = (x `div` 10) + (x `mod` 10)
sumDigits (x:xs)    = (sumDigits [x]) + (sumDigits xs)

validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x)) `mod` 10) == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a,b)] ++ hanoi (n - 1) c b a 