module Pi where

cgiMain :: [(String,String)] -> [(String,String)] -> IO ([(String,String)], String)
cgiMain _ _ =
   return ([("Content-Type", "text/plain")], show (pi::Double))
