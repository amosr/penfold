{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Magic where

import Route


class MagicBijectionCrush a where
 type Crushed a
 crush :: a -> Crushed a
 expand :: Crushed a -> a

instance MagicBijectionCrush () where
 type Crushed () = ()
 crush = id
 expand = id

instance MagicBijectionCrush Int where
 type Crushed Int = Int
 crush = id
 expand = id


instance MagicBijectionCrush a => MagicBijectionCrush ((), a) where
 type Crushed ((), a) = a
 crush = snd
 expand a = ((), a)

instance (MagicBijectionCrush b) => MagicBijectionCrush (Int, b) where
 type Crushed (Int, b) = (Crushed Int, Crushed b)
 crush (a,b) = (crush a, crush b)
 expand (a,b) = (expand a, expand b)

instance MagicBijectionCrush a => MagicBijectionCrush (Route a) where
 type Crushed (Route a) = Route (Crushed a)
 crush = Biject crush expand
 expand = Biject expand crush




