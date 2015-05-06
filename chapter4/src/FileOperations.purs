module FileOperations where

import Data.Path
import Data.Array


import Control.MonadPlus
import Control.Monad.Eff 
import Control.Monad.Eff.Unsafe 
import Data.Tuple
import Data.Maybe
import Data.Maybe.Unsafe
import Data.Foldable
    
allFiles :: Path -> [Path]
allFiles root = root : concatMap allFiles (ls root)

foreign import undefined :: forall a. a
            
allFiles' :: Path -> [_]
allFiles' file =  file : do
                   child <- ls file
                   guard $ isDirectory child
                   allFiles' child

allFiles2 :: Path  -> [_]
allFiles2 file   =  do
                      child <- ls file
                      let  res = if isDirectory child then
                                   allFiles2 child
                                 else
                                     [ child ]
                      res

--reecrire avec des records
--reecrire avec une liste non vide permet d'enlever les option
--type Extrema =  {biggest :: Maybe Path ,    smallest :: Maybe Path }
                          
smallestAndBiggest :: Path -> Tuple (Maybe Path) (Maybe Path)
smallestAndBiggest file =
    foldl reduce  (Tuple Nothing Nothing) (allFiles2 file)
        where
          reduce state@(Tuple  oSmallest oBiggest) file =
              if isDirectory file then state
              else
                  let usize  = size >>> fromJust 
                      biggest  = maybe file (\currBigest   -> maxby usize file currBigest ) oBiggest
                      smallest = maybe file (\currSmallest -> minby usize file currSmallest) oSmallest
                  in Tuple (Just smallest) (Just biggest)
          maxby :: forall a. (a -> Number) -> a -> a -> a  -- sans cela, il n'infere pas
          maxby f x y = if f x > f y then x else y
          minby :: forall a. (a -> Number) -> a -> a -> a
          minby f x y = if f x < f y then x else y                                     

smallestAndBiggest2 :: Path -> Maybe (Tuple Path Path)
smallestAndBiggest2 file =
    let files = allFiles2 file
        ofirst = head files
    in maybe Nothing
       (\first -> Just (foldl reduce (Tuple first first) files)) ofirst
        where
          reduce state@(Tuple smallest biggest) file =
              if isDirectory file then state
              else
                  let usize     = size >>> fromJust 
                  in Tuple ( minby usize file smallest)  ( maxby usize file biggest)
          maxby :: forall a. (a -> Number) -> a -> a -> a  -- sans cela, il n'infere pas
          maxby f x y = if f x > f y then x else y
          minby :: forall a. (a -> Number) -> a -> a -> a
          minby f x y = if f x < f y then x else y
          

factors :: Number -> [[Number]]
factors n = do
  i <- range 1 n
  j <- range i n
  guard $ i * j == n
  return [i, j]


