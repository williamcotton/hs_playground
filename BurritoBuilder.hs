module Main where

data Meat = Chicken | Beef | Pork | Fish | Veggie deriving (Show)

data Ingredient = Cheese | Rice | Beans | Salsa | Guacamole | SourCream | Lettuce
                | Tomato | Onion | Cilantro | PicoDeGallo deriving (Eq, Show)

type Burrito = Maybe Meat -> [Ingredient] -> (Maybe Meat, [Ingredient])

returnBurrito :: Meat -> [Ingredient] -> Burrito
returnBurrito meat ingredients _ _ = (Just meat, ingredients)

addMeat :: Meat -> Burrito -> Burrito
addMeat newMeat burrito _ ingredients = (Just newMeat, ingredients)

addIngredient :: Ingredient -> Burrito -> Burrito
addIngredient ingredient burrito meat ingredients = (meat, ingredient : ingredients)

addMissionBurritoIngredients :: Burrito -> Burrito
addMissionBurritoIngredients burrito meat ingredients = (meat, Cheese : Rice : Beans : ingredients)

holdThe :: Ingredient -> Burrito -> Burrito
holdThe ingredient burrito meat ingredients = (meat, filter (/= ingredient) ingredients)

(>>>) :: Burrito -> (Burrito -> Burrito) -> Burrito
burrito >>> f = \meat ingredients -> let (newMeat, newIngredients) = burrito meat ingredients
                                     in f (returnBurrito (maybe Veggie id newMeat) newIngredients) newMeat newIngredients

tortilla :: Burrito
tortilla = returnBurrito Veggie []

buildBurrito :: (Maybe Meat, [Ingredient])
buildBurrito = let burrito = tortilla >>> addMeat Chicken >>> addMissionBurritoIngredients
                             >>> holdThe Cheese >>> addIngredient PicoDeGallo
                             >>> addIngredient Salsa >>> addIngredient Guacamole
                             >>> addIngredient SourCream
               in burrito Nothing []

main :: IO ()
main = print buildBurrito
