import Data.Char (isDigit)

main :: IO()
main = do
    content <- readFile "input.txt"
    let fileLines = lines content
    putStrLn $ show $ sum[getCalibrationValue $ extractDigits $ x | x <- fileLines]


extractDigits :: String -> String
extractDigits xs = [x | x <- xs, isDigit x]

getCalibrationValue :: String -> Int
getCalibrationValue input
    | length input == 1 = read (input ++ input)
    | otherwise         = read (take 1 input ++ last input : [])