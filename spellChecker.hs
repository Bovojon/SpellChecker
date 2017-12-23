import System.Environment
import Data.List
import Data.Function
import Data.Foldable
import Data.Char

closestWords :: [String] -> String -> [(String, Int)]
closestWords dict word  
    |(length dict) == 1 = addIfTop10 (word, distance) []
    | otherwise = addIfTop10 ((head dict), distance) (closestWords (tail dict) word)
                      where distance = levenshtein (head dict) word

addIfTop10 :: Ord b => (a, b) -> [(a, b)] -> [(a, b)]
addIfTop10 (word, distance) ls
    | (length ls) < 10 = addInSortedPosition (word, distance) ls
    | otherwise = addIfMin (word, distance) ls

-- Function to add the word in its position in sorted order [("Car",11),("Race",10),("Toy",10)]
addInSortedPosition :: Ord b => (a, b) -> [(a, b)] -> [(a, b)]
addInSortedPosition (word, distance) ls
    | (length ls) == 0 = [(word, distance)]
    | distance >= (snd (head ls)) = [(word, distance)] ++ ls
    | otherwise = [head ls] ++ (addInSortedPosition (word, distance) (tail ls))

-- Function to replace the word in its position in sorted order only if it has minimum distance than anyone element in the list
addIfMin :: Ord b => (a, b) -> [(a, b)] -> [(a, b)]
addIfMin (word, distance) ls
    | distance >= (snd (head ls)) = ls
    | otherwise = addInSortedPosition (word, distance) (tail ls)

-- Function to check if a word is in the dictionary provided
checkSpell :: [String] -> [Char] -> [Char]
checkSpell dict word 
    | (word `elem` dict) = ""
    | otherwise = word

-- Function to remove empty words
refineList :: [String] -> [String]
refineList ls = filter (\ x-> x /= "" ) ls

-- Function to calculate levenshtein distance for the given word vs each word in a dicitonary
levDistanceList :: [String] -> String -> [(String, Int)]
levDistanceList dict word
    | (length dict) == 0 = []
    | otherwise = [(head dict, levenshtein word (head dict))] ++ (levDistanceList (tail dict) word)

-- for this method the main idea has been taken from the source listed at the end of this program
-- calculate levenshtein distance between two strings
levenshtein :: String -> String -> Int
-- base case
levenshtein "" "" = 0

-- simple case where on string is longer than the other
levenshtein "" dictWord = length dictWord

-- The reverse case from the once above
levenshtein word "" = length word

-- The main method for equal length string
levenshtein word dictWord
   | last word == last dictWord = levenshtein (init word) (init dictWord)
   | otherwise = minimum [1 + levenshtein (init word) dictWord, 
                          1 + levenshtein word (init dictWord),
                          1 + levenshtein (init word) (init dictWord)]


logSuggestion:: FilePath -> [String] -> [[(String, Int)]] -> IO ()
logSuggestion file misspelled ls = do
                                    let output = map (\(a, b) -> printFormat a b) (zip misspelled ls)
                                    forM_ output $ \s -> appendFile file s 
    
printFormat :: [Char] -> [([Char], Int)] -> [Char]
printFormat word ls = "\n"++word ++": " ++output
                        where list = (map (\tup -> fst tup) (reverse ls))
                              output = concat (intersperse " " list)

isOnlyNumber :: String -> Bool
isOnlyNumber ""  = False
isOnlyNumber "." = False
isOnlyNumber xs  =
  case dropWhile isDigit xs of
    ""       -> True
    ('.':ys) -> all isDigit ys
    _        -> False

removePunctuation :: String -> [String]
removePunctuation xs = words ([if (not (isAlpha x)) then ' ' else toLower x | x<-xs])

tokenizeWords :: [String] -> [String]
tokenizeWords ls
    | length ls == 0 = []
    | isOnlyNumber (head ls) = [] ++ (tokenizeWords (tail ls))
    | otherwise = (removePunctuation (head ls)) ++ (tokenizeWords (tail ls))

---------------------------------------------------------------

--main :: IO [String]
main = do
        [dictFile, wordFile, output] <- getArgs
        file <- readFile wordFile
        dictionary <- readFile dictFile
        --dictionary <- readFile "assign-9-files/dictionaries/words.txt"
        --dictionary <- readFile "assign-9-files/dictionaries/google-10000-english-master/google-10000-english-no-swears.txt"
        --dictionary <- readFile "M.txt"
        --file <- readFile "A.txt"
        --let output = "K.txt"
        
        writeFile output "Suggested Words for the errors\n"
        
        -- Make dictionary
        let dict = (map (map toLower) (lines dictionary))
        let wordsList = tokenizeWords (words file)
        
        -- Check every word and create a list of misspelled words
        let misspelled = (map (checkSpell dict) wordsList)
        let misWords = refineList misspelled
        
        print "Incorrect Words"
        print misWords
        print "Finding suggestions for the words:"
        
        let distanceList = (map (closestWords dict) misWords)
        --print distanceList
        
        logSuggestion output misWords (distanceList)
        
        print "Output successfully logged."



------------- References
-- https://swizec.com/blog/levenshtein-distance-in-haskell/swizec/4801
-- https://stackoverflow.com/questions/30029029/haskell-check-if-string-is-valid-number
-- https://stackoverflow.com/questions/15318696/do-some-replacement-in-haskell-list-comprehensions