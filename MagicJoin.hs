{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MagicJoin where

import Route


type family Join a b where
 Join () () = ()
 Join () a = a
 Join a () = a
 Join a b = (a, b)

class Join' a b where
 joiny :: a -> b -> Join a b

instance Join' () () where
 joiny _ _ = ()

instance Join' a b where
 joiny a b = (a, b)

{-
class MagicJoin a where
 type Join a b
 slash :: Route a -> Route b -> Route (Join a b)

instance MagicJoin () where
 type Join () b = b
 slash ar br
  = Biject snd ((,) ())
  $ Slash ar br

instance MagicJoin Int where
 type Join Int b = (Int,b)
 -}
