
module Main where

import IO
import Directory
import System

dirAA = "in-AAcommon-6077-1660"
dirBB = "in-BBtrunk"
dirCC = "in-CCaixbranch"
dirRR = "RESULT"

maybe_do :: String -> IO ()
maybe_do f
   = let r = dirRR ++ "/" ++ f
         a = dirAA ++ "/" ++ f
         b = dirBB ++ "/" ++ f
         c = dirCC ++ "/" ++ f
     in
     do x <- doesFileExist r
        if x
         then hPutStrLn stderr ("done: " ++ f)
         else 
          do hPutStrLn stderr ("  do: " ++ f)
             xx <- system ("mkdir -p " ++ basename r)
             rs <- merge3 r a b c
             hPutStrLn stderr (rs ++ f)


merge3 :: String -> String -> String -> String -> IO String
merge3 r a b c
   = do ca <- readFile a
        cb <- readFile b
        cc <- readFile c
        let same = identical3 ca cb cc
        if same
         then 
          do ec <- system ("/bin/cp " ++ a ++ " " ++ r)
             if ec == ExitSuccess
              then return "COPY: "
              else barf "/bin/cp failed"
         else 
          do ec <- system ("kdiff3 -m -o " ++ r ++ " -b " 
                           ++ a ++ " " ++ b ++ " " ++ c ++ " &> /dev/null" )
             if ec == ExitSuccess
              then return "  ok: "
              else barf "kdiff3 failed"

barf :: String -> IO a
barf who
   = do hPutStrLn stderr ("FAIL: " ++ who)
        exitWith ExitSuccess

identical3 :: String -> String -> String -> Bool
identical3 [] [] [] = True
identical3 (x:xs) (y:ys) (z:zs)
   = x == y && y == z && identical3 xs ys zs
identical3 _ _ _ = False

main :: IO ()
main
   = do t <- readFile "FILEScba"
        let fs = lines t
        mapM_ maybe_do fs

basename = reverse . drop 1 . dropWhile (/= '/') . reverse
