import System.IO
match :: String -> String -> String 
match xs ys = [if elem x ys then x else '-' | x <- xs]

play word = do { putStr "?";
                guess <- getLine;
                if guess == word then
                    putStrLn "You got it!!!" 
                else 
                    do {putStrLn (match word guess);
                        play word; }}

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do { x <- getCh;
                if x == '\n' then
                    do {putChar x;
                        return []; }
                else
                    do {putChar '-';
                        xs <- sgetLine;
                        return (x:xs) } }

hangman :: IO()
hangman = do { 
            putStrLn "Thinf of a word: ";
            word <- sgetLine;
            putStrLn "Try to guess it: ";
            play word
            }

-- -- Length of a string
strlen ::IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine 
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters."


hello = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")

revInput = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            revInput  

reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 