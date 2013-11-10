{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Spriggan (Spriggan, Sprite, Costume(..)) where

import Codec.Picture
import Control.Applicative
import Control.Monad.Trans.State.Strict
import Data.Sequence (Seq)
import Data.Vault.Strict

newtype Spriggan a = Spriggan {getSpriggan :: StateT Vault IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

data Costume = Costume {
  costumeImage :: DynamicImage,
  costumeCentre :: (Int, Int)
  }

data SpriteInternal {
  costumes :: Seq Costume,
  position :: (Int, Int),
  rotation :: Double,
  visible :: Bool
  }

newtype Sprite = Sprite {getSprite :: Key SpriteInternal}


