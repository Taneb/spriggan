{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graphics.Spriggan (Spriggan, Sprite, Costume(..)) where

import Codec.Picture
import Control.Applicative
import Control.Monad.State.Lazy
import Control.Wire
import Control.Wire.Unsafe.Event
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IM
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text)
import Data.Time.Clock
import Linear

newtype Spriggan backend a =
  Spriggan {getSpriggan :: StateT (IM.IntMap Sprite) backend a}
  deriving (Functor, Applicative, Monad,
            MonadState (IM.IntMap Sprite), MonadTrans, MonadIO)

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

newtype SpriteCluster = SpriteCluster {getSpriteCluster :: NonEmpty SpriteRef}

data Key = KeyArrowUp | KeyArrowLeft | KeyArrowDown | KeyArrowRight
  deriving (Eq)

data Input = Input {
  keysPressed :: S.Set Key,
  mousePos :: V2 Int
  }
  deriving (Eq)

data Action =
  Adjust SpriteCluster (Sprite -> Sprite)

class (Applicative backend, MonadIO backend) => Backend backend where
  runInBackend :: backend a -> IO a
  getInput :: backend Input
  render :: [Sprite] -> backend ()

type SprigganWire = Wire (Timed NominalDiffTime ()) () ((->) Input)
                    () (Event [Action])

runSpriggan :: Backend backend =>
               Spriggan backend () ->
               SprigganWire -> 
               IO ()
runSpriggan s0 a0 =
  runInBackend $ evalStateT (getSpriggan $ s0 >> go a0) IM.empty
  where
    go a = go2 a clockSession_
    go2 a ses0 = do
      input <- lift getInput
      (t, ses1) <- stepSession ses0
      let (r, b) = stepWire a t (Right ()) input
      case r of
        Left () -> return ()
        Right NoEvent -> return ()
        Right (Event acts) -> do
          forM_ acts $ \(Adjust (SpriteCluster sprites@(parent :| _)) f) ->
              F.forM_ sprites $ \sprite -> do
                undefined
            
          go2 b ses1
      
