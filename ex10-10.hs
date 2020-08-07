-- 4
adder = do {  
            putStr "How many numbers? ";
            lines <- readInt;
            numbers <- reader lines [];
            putStrLn ("The total is " ++ show (sum numbers))
           }

reader lines ns = do {
                    n <- readInt; 
                    if (lines-1) > 0 then
                        reader (lines - 1) (n:ns)
                    else 
                        return (n:ns)}

readInt :: IO Int
readInt = do { n <- getLine; 
           return (read n)}
