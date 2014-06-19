
module Plow.Extras.List (groupUp) where

-- | groupUp is a function that takes a list and turns it into a list
-- of n sized lists.  except the last list which is whatever size there is left
-- very useful for concurrentIO on list



groupUp :: Int -> [t] -> [[t]]
groupUp n lst = loop [] lst
 where 
   loop l [] = l 
   loop l ol = let (t,r) = splitAt n ol
               in loop (t:l) $! r
