
module Linker 	( linker )

where

foreign import ccall "linker_top_level" linker_top_level :: Int -> IO Int

linker = linker_top_level
