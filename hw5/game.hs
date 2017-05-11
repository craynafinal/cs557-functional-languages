import Data.Char
import System.Random

main = do putStrLn "Welcome to my awesome game!"
          randomRIO (1,100) >>= game

game secret = do putStr "What is your guess? "
                 l <- getLine
                 case atoi l of
                   Just n -> guess secret n
                   Nothing -> do putStrLn "Please enter a number!"
                                 game secret

guess secret n
    | n==457 || n==557 = do putStrLn ("The secret is " ++ show secret)
                            game secret
    | n==secret  = putStrLn "you win!"
    | n <secret  = do putStrLn "too low!"
                      game secret
    | otherwise  = do putStrLn "too high!"
                      game secret

-- putStrLn ("You guessed " ++ show n ++", square n = " ++ show (n*n))

atoi  :: String -> Maybe Int
atoi s = if not (null s) && all isDigit s
           then Just (read s)
           else Nothing

       

---do putStrLn ("The secret is " ++ show secret)
