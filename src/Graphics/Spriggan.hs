{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Spriggan (Spriggan, Sprite, Costume(..)) where

import Codec.Picture
import Control.Applicative
import Control.Monad.State.Lazy
import Control.Wire
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import qualified Data.Sequence as Seq
import Data.List.NonEmpty
import Data.Text (Text)
import Data.Time.Clock
import Linear

newtype Spriggan backend a =
  Spriggan {getSpriggan :: StateT (IM.IntMap Sprite) backend a}
  deriving (Functor, Applicative, Monad,
            MonadState (IM.IntMap Sprite), MonadTrans)

data Costume = Costume {
  costumeImage :: DynamicImage,
  costumeCentre :: V2 Int
  }

data Sprite = Sprite {
  costume :: Costume,
  position :: V2 Int,
  transform :: M22 Double,
  visible :: Bool,
  layer :: Int
  }

defSprite :: Costume -> Sprite
defSprite c = Sprite {
  costume = c,
  position = 0,
  transform = eye2,
  visible = False,
  layer = 0
  }

newtype SpriteRef = SpriteRef {getSpriteRef :: Int}

newtype SpriteCluster = SpriteCluser {getSpriteCluster :: NonEmpty SpriteRef}

data Key = KeyArrowUp | KeyArrowLeft | KeyArrowDown | KeyArrowRight
  deriving (Eq)

data Input =
  KeyDown Key |
  MouseIsInBox (V2 Int, V2 Int)
  deriving (Eq)

data Action =
  Adjust SpriteCluster (Sprite -> Sprite)

class (Applicative backend, Monad backend) => Backend backend where
  runInBackend :: backend a -> IO a
  isKeyDown :: Key -> backend Bool
  mousePos :: backend (V2 Int)
  render :: [Sprite] -> backend ()

runSpriggan :: Backend backend =>
               Spriggan backend () ->
               Wire (Timed NominalDiffTime ()) () ((->) (Input -> Bool))
               a (Event [Action]) ->
               IO ()
runSpriggan s0 a0 =
  runInBackend $ evalStateT (getSpriggan $ s0 >> go a0) IM.empty
  where
    go a = do
      undefined
      
      
