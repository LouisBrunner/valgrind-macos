
module Linker 	( linkObjects )

where

import Foreign.C.String
import Foreign.Ptr

foreign import ccall "linker_top_level_INIT" 
                     linker_top_level_INIT :: IO ()

foreign import ccall "linker_top_level_ADD"
                     linker_top_level_ADD :: CString -> IO ()

foreign import ccall "linker_top_level_LINK"
                     linker_top_level_LINK :: IO (Ptr ())

linkObjects :: [String] -> IO (Ptr ())
linkObjects objs
   = do linker_top_level_INIT
        mapM add_obj objs
        main_addr <- linker_top_level_LINK
        return main_addr
     where
        add_obj :: String -> IO ()
        add_obj obj_name = withCString obj_name linker_top_level_ADD
