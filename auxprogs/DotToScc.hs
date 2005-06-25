
-- A program for extracting strongly connected components from a .dot
-- file created by auxprogs/gen-mdg.

-- How to use: one of the following:

-- compile to an exe:   ghc -o dottoscc DotToScc.hs
--    and then    ./dottoscc name_of_file.dot

-- or interpret with runhugs:
--    runhugs DotToScc.hs name_of_file.dot

-- or run within hugs:
--    hugs DotToScc.hs
--    Main> imain "name_of_file.dot"


module Main where

import System
import List ( sort, nub )

usage :: IO ()
usage = putStrLn "usage: dottoscc <name_of_file.dot>"

main :: IO ()
main = do args <- getArgs
          if length args /= 1
           then usage
           else imain (head args)

imain :: String -> IO ()
imain dot_file_name
   = do edges <- read_dot_file dot_file_name
        let sccs = gen_sccs edges
        let pretty = showPrettily sccs
        putStrLn pretty
     where
        showPrettily :: [[String]] -> String
        showPrettily = unlines . concatMap showScc

        showScc elems
           = let n = length elems 
             in
                [""]
                ++ (if n > 1 then ["   -- " 
                                   ++ show n ++ " modules in cycle"] 
                             else [])
                ++ map ("   " ++) elems


-- Read a .dot file and return a list of edges
read_dot_file :: String{-filename-} -> IO [(String,String)]
read_dot_file dot_file_name
   = do bytes <- readFile dot_file_name
        let linez = lines bytes
        let edges = [(s,d) | Just (s,d) <- map maybe_mk_edge linez]
        return edges
     where
        -- identify lines of the form "text1 -> text2" and return
        -- text1 and text2
        maybe_mk_edge :: String -> Maybe (String, String)
        maybe_mk_edge str
           = case words str of
                [text1, "->", text2] -> Just (text1, text2)
                other                -> Nothing


-- Take the list of edges and return a topologically sorted list of
-- sccs
gen_sccs :: [(String,String)] -> [[String]]
gen_sccs raw_edges
   = let clean_edges = sort (nub raw_edges)
         nodes       = nub (concatMap (\(s,d) -> [s,d]) clean_edges)
         ins  v      = [u | (u,w) <- clean_edges, v==w]
         outs v      = [w | (u,w) <- clean_edges, v==u]
         components  = map (sort.utSetToList) (deScc ins outs nodes)
     in
         components


--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------

-- Graph-theoretic stuff that does the interesting stuff.

-- ==========================================================--
--
deScc :: (Ord a) =>
         (a -> [a]) -> -- The "ins"  map
         (a -> [a]) -> -- The "outs" map
         [a]        -> -- The root vertices
         [Set a]       -- The topologically sorted components

deScc ins outs
   = spanning . depthFirst
     where depthFirst = snd . deDepthFirstSearch outs (utSetEmpty, [])
           spanning   = snd . deSpanningSearch   ins  (utSetEmpty, [])


-- =========================================================--
--
deDepthFirstSearch :: (Ord a) =>
                      (a -> [a])   -> -- The map,
                      (Set a, [a]) -> -- state: visited set,
                                      --      current sequence of vertices
                      [a]          -> -- input vertices sequence
                      (Set a, [a])    -- final state

deDepthFirstSearch
   = foldl . search
     where
     search relation (visited, sequence) vertex
      | utSetElementOf vertex visited   = (visited,          sequence )
      | otherwise                       = (visited', vertex: sequence')
        where
        (visited', sequence')
         = deDepthFirstSearch relation
                           (utSetUnion visited (utSetSingleton vertex), sequence)
                           (relation vertex)


-- ==========================================================--
--
deSpanningSearch   :: (Ord a) =>
                      (a -> [a])       -> -- The map
                      (Set a, [Set a]) -> -- Current state: visited set,
                                          --  current sequence of vertice sets
                      [a]              -> -- Input sequence of vertices
                      (Set a, [Set a])    -- Final state

deSpanningSearch
   = foldl . search
     where
     search relation (visited, utSetSequence) vertex
      | utSetElementOf vertex visited   = (visited,          utSetSequence )
      | otherwise = (visited', utSetFromList (vertex: sequence): utSetSequence)
        where
         (visited', sequence)
            = deDepthFirstSearch relation
                          (utSetUnion visited (utSetSingleton vertex), [])
                          (relation vertex)





--------------------------------------------------------------------
--------------------------------------------------------------------
--------------------------------------------------------------------
-- Most of this set stuff isn't needed.


-- ====================================--
-- === set                          ===--
-- ====================================--

data Set e = MkSet [e]

-- ==========================================================--
--
unMkSet :: (Ord a) => Set a -> [a]

unMkSet (MkSet s) = s


-- ==========================================================--
--
utSetEmpty :: (Ord a) => Set a

utSetEmpty = MkSet []


-- ==========================================================--
--
utSetIsEmpty :: (Ord a) => Set a -> Bool

utSetIsEmpty (MkSet s) = s == []


-- ==========================================================--
--
utSetSingleton :: (Ord a) => a -> Set a

utSetSingleton x = MkSet [x]


-- ==========================================================--
--
utSetFromList :: (Ord a) => [a] -> Set a

utSetFromList x = (MkSet . rmdup . sort) x
                  where rmdup []       = []
                        rmdup [x]      = [x]
                        rmdup (x:y:xs) | x==y       = rmdup (y:xs)
                                       | otherwise  = x: rmdup (y:xs)


-- ==========================================================--
--
utSetToList :: (Ord a) => Set a -> [a]

utSetToList (MkSet xs) = xs



-- ==========================================================--
--
utSetUnion :: (Ord a) => Set a -> Set a -> Set a

utSetUnion (MkSet [])     (MkSet [])            = (MkSet [])
utSetUnion (MkSet [])     (MkSet (b:bs))        = (MkSet (b:bs))
utSetUnion (MkSet (a:as)) (MkSet [])            = (MkSet (a:as))
utSetUnion (MkSet (a:as)) (MkSet (b:bs))
    | a < b   = MkSet (a: (unMkSet (utSetUnion (MkSet as) (MkSet (b:bs)))))
    | a == b  = MkSet (a: (unMkSet (utSetUnion (MkSet as) (MkSet bs))))
    | a > b   = MkSet (b: (unMkSet (utSetUnion (MkSet (a:as)) (MkSet bs))))


-- ==========================================================--
--
utSetIntersection :: (Ord a) => Set a -> Set a -> Set a

utSetIntersection (MkSet [])     (MkSet [])     = (MkSet [])
utSetIntersection (MkSet [])     (MkSet (b:bs)) = (MkSet [])
utSetIntersection (MkSet (a:as)) (MkSet [])     = (MkSet [])
utSetIntersection (MkSet (a:as)) (MkSet (b:bs))
    | a < b   = utSetIntersection (MkSet as) (MkSet (b:bs))
    | a == b  = MkSet (a: (unMkSet (utSetIntersection (MkSet as) (MkSet bs))))
    | a > b   = utSetIntersection (MkSet (a:as)) (MkSet bs)


-- ==========================================================--
--
utSetSubtraction :: (Ord a) => Set a -> Set a -> Set a

utSetSubtraction (MkSet [])     (MkSet [])      = (MkSet [])
utSetSubtraction (MkSet [])     (MkSet (b:bs))  = (MkSet [])
utSetSubtraction (MkSet (a:as)) (MkSet [])      = (MkSet (a:as))
utSetSubtraction (MkSet (a:as)) (MkSet (b:bs))
    | a < b   = MkSet (a: (unMkSet (utSetSubtraction (MkSet as) (MkSet (b:bs)))))
    | a == b  = utSetSubtraction (MkSet as) (MkSet bs)
    | a > b   = utSetSubtraction (MkSet (a:as)) (MkSet bs)


-- ==========================================================--
--
utSetElementOf :: (Ord a) => a -> Set a -> Bool

utSetElementOf x (MkSet [])       = False
utSetElementOf x (MkSet (y:ys))   = x==y || (x>y && utSetElementOf x (MkSet ys))



-- ==========================================================--
--
utSetSubsetOf :: (Ord a) => Set a -> Set a -> Bool

utSetSubsetOf (MkSet [])        (MkSet bs) = True
utSetSubsetOf (MkSet (a:as))    (MkSet bs)
    = utSetElementOf a (MkSet bs) && utSetSubsetOf (MkSet as) (MkSet bs)


-- ==========================================================--
--
utSetUnionList :: (Ord a) => [Set a] -> Set a

utSetUnionList setList = foldl utSetUnion utSetEmpty setList


