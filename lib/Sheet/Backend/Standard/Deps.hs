module Sheet.Backend.Standard.Deps where

import Sheet.Backend.Standard.Types

resolveDeps :: [(Pos,[Pos])] -> [Pos]
resolveDeps cdeps =
  let res = map fst
          $ filter (\c -> not $ any (== fst c)
                              $ (concatMap snd cdeps))
          $ cdeps
  in if null res
    then []
    else res ++ resolveDeps (filter (\c -> not $ any (== fst c) res) cdeps)
