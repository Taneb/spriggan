{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Spriggan (Spriggan, Sprite, Costume(..)) where

import Codec.Picture
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Vault.Strict

newtype Spriggan a = Spriggan {getSpriggan :: StateT Vault IO a}
  deriving (Functor, Applicative, Monad, MonadIO)

data Costume = Costume {
  costumeImage :: DynamicImage,
  costumeCentre :: (Int, Int)
  }

data SpriteInternal = SpriteInternal {
  costumes :: Seq Costume,
  position :: (Int, Int),
  rotation :: Double,
  visible :: Bool
  }

defSpriteInternal :: SpriteInternal
defSpriteInternal = SpriteInternal {
  costumes = Seq.empty,
  position = (0,0),
  rotation = 0,
  visible = False
  }

newtype Sprite = Sprite {getSprite :: Key SpriteInternal}

addSprite :: Spriggan Sprite
addSprite = Spriggan $ do
  k <- liftIO newKey
  modify $ insert k defSpriteInternal
  return $ Sprite k

addCostume :: Sprite -> Costume -> Spriggan ()
addCostume (Sprite k) c = 
  Spriggan $ modify $ adjust (\s -> s {costumes = costumes s |> c}) k
