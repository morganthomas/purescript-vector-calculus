module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Monoid (class Monoid)

class (Monoid g) <= Group g where
  inverse :: g -> g

class (Field f, Group v) <= VectorSpace v f | v -> f where
  scalarMult :: f -> v -> v

type ScalarField v f = (VectorSpace v f) => v -> f
type VectorField v f = (VectorSpace v f) => v -> v

class (VectorSpace v f) <= InnerProductSpace v f where
  innerProduct :: v -> v -> f

class (VectorSpace v Number) <= HilbertSpace v where
  hilbertSpaceDummyMethod :: v

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
