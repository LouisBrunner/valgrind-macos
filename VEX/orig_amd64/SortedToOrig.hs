
module Main where

main 
  = do x1 <- readFile "test2.sorted"
       let x2 = lines x1
           x3 = zip [1 ..] x2
           x4 = concat (map qq x3)
       --putStr x4
       writeFile "test2.orig" x4


qq :: (Int, String) -> String
qq (n, s0) 
   = let ws     = words s0
         bytes  = head ws
         rest   = unwords (tail ws)
         bytes2 = foo bytes
     in
     unlines [
        "",
        rest,
        ". " ++ show n ++ " 0x12345678 " ++ show (1 + (length bytes `div` 2)),
        ". " ++ bytes2 ++ "C3"
     ]


foo [] = []
foo (x:y:rest) = x:y:' ':foo rest
