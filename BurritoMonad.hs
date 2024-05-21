module Main where

import Control.Monad (liftM, ap)

-- Define the meat and ingredient data types
data Meat = Chicken | Beef | Pork | Fish | Veggie deriving (Show)
data Ingredient = Cheese | Rice | Beans | Salsa | Guacamole | SourCream | Lettuce
                | Tomato | Onion | Cilantro | PicoDeGallo deriving (Eq, Show)

-- A Burrito now carries the state of the burrito being built
newtype Burrito a = Burrito { runBurrito :: (Maybe Meat, [Ingredient]) -> (a, (Maybe Meat, [Ingredient])) }

-- Implement Functor, Applicative, and Monad for Burrito
instance Functor Burrito where
    fmap = liftM

instance Applicative Burrito where
    pure x = Burrito $ \s -> (x, s)
    (<*>) = ap

instance Monad Burrito where
    return = pure
    Burrito m >>= f = Burrito $ \s -> let (a, s') = m s
                                      in runBurrito (f a) s'

-- Operations on Burritos
returnBurrito :: Meat -> [Ingredient] -> Burrito ()
returnBurrito meat ingredients = Burrito $ \_ -> ((), (Just meat, ingredients))

addMeat :: Meat -> Burrito ()
addMeat newMeat = Burrito $ \(_, ingredients) -> ((), (Just newMeat, ingredients))

addIngredient :: Ingredient -> Burrito ()
addIngredient ingredient = Burrito $ \(meat, ingredients) -> ((), (meat, ingredient : ingredients))

addMissionBurritoIngredients :: Burrito ()
addMissionBurritoIngredients = Burrito $ \(meat, ingredients) -> ((), (meat, Cheese : Rice : Beans : ingredients))

holdThe :: Ingredient -> Burrito ()
holdThe ingredient = Burrito $ \(meat, ingredients) -> ((), (meat, filter (/= ingredient) ingredients))

-- Example of building a burrito using monadic operations
buildBurrito :: Burrito [Ingredient]
buildBurrito = do
    addMeat Chicken
    addMissionBurritoIngredients
    holdThe Cheese
    addIngredient PicoDeGallo
    addIngredient Salsa
    addIngredient Guacamole
    addIngredient SourCream
    Burrito $ \(meat, ingredients) -> (ingredients, (meat, ingredients))

main :: IO ()
main = print (snd $ runBurrito buildBurrito (Nothing, []))
