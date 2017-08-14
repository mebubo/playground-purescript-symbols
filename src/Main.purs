module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Monoid (class Monoid)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol, reifySymbol)
import Prelude (class Semigroup, Unit, discard, ($), (<>))

infixl 6 type TypeConcat as <>

main :: forall eff. Eff (console :: CONSOLE | eff) Unit
main = do
    log $ reflectSymbol (SProxy :: SProxy "tinsel")
    log $ reflectSymbol (SProxy :: SProxy ("tinsel" <> " moo"))
    log $ renderSep phrases
    log $ reifySymbol "runtime" comeFrom
    log $ comeFrom (SProxy :: SProxy "compile time")

phrases :: Sep ": "
phrases = sep "foo" <> sep "bar" <> sep "baz"

newtype Sep (s :: Symbol) = Sep (Array String)

derive newtype instance semigroupSep :: Semigroup (Sep s)
derive newtype instance monoidSep :: Monoid (Sep s)

sep :: forall a. String -> Sep a
sep s = Sep [s]

renderSep :: forall a. IsSymbol a => Sep a -> String
renderSep (Sep items) = joinWith s items
    where s = reflectSymbol (SProxy :: SProxy a)

comeFrom :: forall s. IsSymbol s => SProxy s -> String
comeFrom _ = renderSep (sep "I come from " <> sep "!!!" :: Sep s)
