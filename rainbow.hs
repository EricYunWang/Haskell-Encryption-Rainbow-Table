import RainbowAssign
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Maybe as Maybe


pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8          -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

--Part 1: pwReduce
--This function takes a hash value and reduce it to passward.
--Uses a helper function
pwReduce :: Hash -> Passwd
pwReduce n = reverse (pwReduceHelp n)

--Helper function for pwReduce
pwReduceHelp :: Hash -> Passwd
pwReduceHelp n = take pwLength [i |i <- [toLetter (fromEnum n `mod` nLetters)] ++ pwReduceHelp (n `div` toEnum nLetters)]


--Part 2: rainbowTable
--This function takes an integer (width), and list of passward to create a rainbow table
--Uses multiple helper functions
--Exception not handled: _ [] case. Reason: empty rainbow table should not exist
rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable wid (x:xs) = Map.fromList (recHelper wid (x:xs))

--Helper function for rainbowTable
--Recursion through the list of passwords
--Repeatedly apply hashing and reduce in another helper function
recHelper :: Int -> [Passwd] -> [(Hash,Passwd)]
recHelper n [] = []
recHelper n (x:xs) = [(repeatHash n x , x )] ++ (recHelper n xs) 

--Helper function for recHelper
--Repeatedly apply hashing and reduce to the input password
repeatHash :: Int -> Passwd -> Hash
repeatHash 0 m = pwHash m
repeatHash n m = repeatHash (n-1) (pwReduce (pwHash m))


--Part 3: findPassword
--This function takes a table, width, hash value, and output a maybe password
--Uses multiple helper functions
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword tabl wid hash = repeatedSearch (passwordList tabl (findCandidates tabl wid hash)) wid hash

--hashReverse and passWdRepeat are two useful helper functions
--hashReverse applies pwReduce then pwHash to the input hash exactly once
hashReverse :: Hash -> Hash
hashReverse m = pwHash(pwReduce m)
--passWdRepeat applies pwHash then pwReduce to the input password exactly once
passWdRepeat :: Passwd -> Passwd
passWdRepeat n = pwReduce(pwHash n)

--Helper function for findPassword
--Takes in a table, width and hash value, recursively input matches into a list of hash using Map.member
--The list of hash is then applied to another helper function
--https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html
findCandidates :: Map.Map Hash Passwd -> Int -> Hash -> [Hash]
findCandidates tabl wid hash 
  | wid == 0 && (Map.member hash tabl) = [hash]
  | wid == 0 = []
  | Map.member hash tabl = [hash] ++ findCandidates tabl (wid-1) (hashReverse hash)
  | otherwise = findCandidates tabl (wid-1) (hashReverse hash)

--Helper function for findPassword
--Takes in a table, list of hash from findCandidates, recursively input matches into a list of passwords using Map.lookup
--catMaybes is used to convert the list of maybe passwords to just list of passwords
--The list of passwords is then applied to another helper function
--https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-Maybe.html#v:catMaybes
passwordList :: Map.Map Hash Passwd -> [Hash] -> [Passwd]
passwordList tabl [] = []
passwordList tabl (x:xs) = catMaybes ([Map.lookup x tabl]) ++ passwordList tabl xs

--Helper function for findPassword
--Takes in a list of passwords, width, hash, and returns the first password match as maybe
--Utilize haskell lazy evaluation with recursion
--This outputs the final answer
repeatedSearch :: [Passwd] -> Int -> Hash -> Maybe Passwd
repeatedSearch [] wid hash = Nothing
repeatedSearch (x:xs) wid hash = go (x:xs) wid hash
  where
    go [] wid1 hash = Nothing
    go (x:xs) wid1 hash 
      | hash == pwHash x = Just x
      | wid1 == 0 = go xs wid hash
      | otherwise = go (passWdRepeat x :xs) (wid1 -1) hash

