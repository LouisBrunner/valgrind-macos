
module Main where

import Char ( isSpace )

{- Compares a .sorted file with a raw printout of instructions
   and shows differences.

   First file (REF) is has lines of format

      hex-digits  SPACEs  insn(possibly with spaces)

   Second file (TEST) has lines of format

      insn(possibly with spaces)

   Purpose is to extract the insn (text), remove spaces, and compare.

   How to use:
(cd .. && make) && (../vex test1.orig | grep LALALA | cut -b 22- > out.txt)
/home/sewardj/Tools/HugsInst/bin/runhugs Compare.hs | grep FAIL 
-}

main = mayn "test2.sorted" "out.txt"

mayn :: String -> String -> IO ()

mayn sorted_fn dump_fn
   = do sorted <- readFile sorted_fn
        dump   <- readFile dump_fn
        let ress = zipWith check (lines (deTab sorted))
                                 (lines (deTab dump))
        putStrLn (unlines ress)


check :: String -> String -> String
check ref test 
   = let ref_clean = dropWhile isHex ref
         ok        = compere ref_clean test
         summary   = grok ("REF: " ++ trim ref_clean) 
                     ++ "   " ++ grok ("TEST: " ++ trim test)
     in
     if  ok
     then "pass:    " ++ summary
     else "FAIL:    " ++ summary

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

compere s1 s2 = filter (not . isSpace) s1 == filter (not . isSpace) s2

isHex c = c `elem` "ABCDEF0123456789abcdef"

grok str 
   = let n = length str
         limit = 40
     in
     if   n >= limit
     then str
     else take limit (str ++ repeat ' ')

deTab [] = []
deTab (c:cs) = if c == '\t' then "  " ++ deTab cs
               else c: deTab cs
