{-# LANGUAGE GADTs #-}
module Route where

import Control.Applicative
import Text.Read

data Route a where
 Slash  :: Route a -> Route b -> Route (a,b)
 Static :: String -> Route ()
 Var    :: (String -> Maybe a) -> (a -> String) -> Route a
 Biject :: (a -> b) -> (b -> a) -> Route a -> Route b

int :: Route Int
int = Var readMaybe show

(</>) = Slash

example1 = Static "user" </> int
example2 = Static "foo" </> Static "user" </> int
example3 = Biject (\(((),()),a) -> a) (\a -> (((),()),a))
         $ Static "foo" </> Static "user" </> int

handle :: Route a -> String -> Maybe a
handle r str
 = snd <$> go r (split '/' str)
 where
  go :: Route r -> [String] -> Maybe ([String], r)
  go (Slash a b) rs
   = do (rs',  a') <- go a rs
        (rs'', b') <- go b rs'
        return (rs'', (a', b'))

  go (Static s) rs
   = case rs of
      (s':rest)
       | s == s'
       -> return (rest, ())
      _
       -> Nothing

  go (Var v _) rs
   = case rs of
      (r:rest)
       | Just v' <- v r
       -> return (rest, v')
      _
       -> Nothing

  go (Biject fab _fba r) rs
   = do (rs', a') <- go r rs
        return (rs', fab a')


render :: Route a -> a -> String
render (Slash a b) (a',b')
 = render a a' ++ "/" ++ render b b'
render (Static str) ()
 = str
render (Var _ sho) a
 = sho a
render (Biject _fab fba r) a
 = render r (fba a)


split :: Eq a => a -> [a] -> [[a]]
split c []
 = []
split c cs
 = go cs []
 where
  go [] r = [r]
  go (c':cs) r
   | c == c'
   = r : go cs []
   | otherwise
   = go cs (r ++ [c'])

