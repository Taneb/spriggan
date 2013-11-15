{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Spriggan (Spriggan, Sprite, Costume(..)) where

import Codec.Picture
import Control.Applicative
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.State.Lazy
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Vault.Lazy as V
import Linear

newtype Spriggan backend a = Spriggan {runSpriggan :: StateT V.Vault backend a}
  deriving (Functor, Applicative, Monad, MonadState V.Vault)

data Costume = Costume {
  costumeImage :: DynamicImage,
  costumeCentre :: V2 Int
  }

data Sprite = Sprite {
  costume :: Costume,
  position :: V2 Int,
  transform :: M22 Double,
  visible :: Bool
  }

defSprite :: Costume -> Sprite
defSprite c = Sprite {
  costume = c,
  position = 0,
  transform = eye2,
  visible = False
  }

newtype SpriteRef = SpriteRef {getSprite :: V.Key Sprite}

newtype SpriteCluster = SpriteCluser {getSpriteCluster :: NonEmpty SpriteRef}

data Key = KeyArrowUp | KeyArrowLeft | KeyArrowDown | KeyArrowRight
  deriving (Eq)

data Input =
  KeyDown Key |
  MouseIsInBox (V2 Int, V2 Int)
  deriving (Eq)

data ActionF a =
  Adjust SpriteRef (Sprite -> Sprite) a
  deriving (Functor)

type Action = F ActionF

class (Applicative backend, Monad backend) => Backend backend where
  run :: backend a -> IO a
  isKeyDown :: Key -> backend Bool
  mousePos :: backend (V2 Int)
  render :: [Sprite] -> backend ()

adjust :: SpriteRef -> (Sprite -> Sprite) -> Action ()
adjust s f = F $ \kp kf -> kf (Adjust s f (kp ()))


