module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Monoid (class Monoid)
import Data.Monoid.Additive (Additive(..))

class (Monoid v, Field f) <= VectorSpace v f | v -> f where
  vecNegate :: v -> v
  scalarMult :: f -> v -> v

instance vectorSpaceNumber :: VectorSpace (Additive Number) Number where
  vecNegate (Additive x) = Additive (-x)
  scalarMult y (Additive x) = Additive (x * y)

type ScalarField v f = (VectorSpace v f) => v -> f
type VectorField v f = (VectorSpace v f) => v -> v

type DifferentiableScalarField v f = { 
  valueAt :: ScalarField v f,
  gradientAt :: VectorField v f
}

data LinearTransformation v f = LinearTransformation (VectorField v f)

type DifferentiableVectorField v f = {
  valueAt :: VectorField v f,
  jacobianAt :: v -> LinearTransformation v f
}

class (VectorSpace v f) <= InnerProductSpace v f where
  innerProduct :: v -> v -> f

class (VectorSpace v Number) <= HilbertSpace v where
  hilbertSpaceDummyMethod :: v

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"
