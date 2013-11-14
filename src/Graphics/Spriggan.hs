{-# LANGUAGE DeriveFunctor #-}

module Graphics.Spriggan (Spriggan, Sprite, Costume(..)) where

import Codec.Picture
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Time.Clock
import qualified Data.Vault.Strict as V
import Linear

newtype Spriggan = Spriggan {getSpriggan :: V.Vault}
  deriving (Monoid)

data Costume = Costume {
  costumeImage :: DynamicImage,
  costumeCentre :: V2 Int
  }

data Sprite = Sprite {
  costume :: Costume,
  position :: V2 Int,
  transform :: M22 Double,
  visible :: Bool,
  actions :: [(Event, Action ())]
  }

defSprite :: Costume -> Sprite
defSprite c = Sprite {
  costumes = c,
  position = V2 0 0,
  transform = V2 (V2 1 0) (V2 0 1),
  visible = False,
  actions = []
  }

newtype SpriteRef = SpriteRef {getSprite :: V.Key (SpriteInternal)}

data Key = KeyArrowUp | KeyArrowLeft | KeyArrowDown | KeyArrow Right
  deriving (Eq)

data Event =
  KeyDown Key |
  MouseIsInBox (V2 Int, V2 Int) |
  Signal Text |
  Both Event Event |
  Not Event
  deriving (Eq)

data ActionF a =
  Adjust SpriteRef (Sprite -> Sprite) a | 
  TimeDelta (NominalDiffTime -> a) |
  BroadcastSignal Text a
  deriving (Functor)

type Action = F ActionF

class Backend backend where
  initialize :: backend -> IO a -> IO a
  isKeyDown :: Key -> IO Bool
  mousePos :: IO (V2 Int)
  render :: [SpriteInternal] -> IO ()

adjust :: SpriteRef -> (Sprite -> Sprite) -> Action ()
adjust s f = F $ \kp kf -> kf (Adjust s f (kp ()))

timeDelta :: Action NominalDiffTime
timeDelta = F $ \kp kf -> kf (TimeDelta kp)

broadcastSignal :: Text -> Action ()
broadcastSignal s = F $ \kp kf -> kf (BroadcastSignal s (kp ()))

