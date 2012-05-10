-- | This module defines synonym class 'Finite' which 
-- is 'Enum' and 'Bounded' and provides two
-- wrappers: 'Mod' for modular arithmetic and 'Sat' for
-- arithmetic with saturation.
--
module Data.Finite(    
    Finite(..),
    Mod(..), Sat(..), sat
)
    
    where 

-- | Class 'Finite' is a synonym for intersection of `Enum` and `Bounded`. 
-- Minimal complete definition is empty. If your 
-- type is instance of `Enum` and `Bounded` all you
-- need to do is write this declaration:
--
-- > instance Finite T
class (Enum a, Bounded a) => Finite a where
    -- | `domLength` is a constant function that returns
    -- total number of elements in the type domain. 
    domLength :: a -> Int
    domLength a = maxB - minB + 1
        where minB  = fromEnum $ minBound `asTypeOf` a
              maxB  = fromEnum $ maxBound `asTypeOf` a
    
-- Modulus

-- | Wrapper 'Mod' helps to define modular arithmetic.
-- If your type @T@ is instance of class 'Finite' then
-- type @Mod T@ is instance of class `Num`. 
--
newtype Mod a = Mod { getMod :: a }
    deriving (Show, Eq)


instance Finite a => Bounded (Mod a) where
    minBound = Mod minBound
    maxBound = Mod maxBound
       
instance Finite a => Enum (Mod a) where
    fromEnum = fromEnum . getMod
    toEnum n = Mod res
        where res   = toEnum $ minB + mod n (domLength res)               
              minB  = fromEnum $ minBound `asTypeOf` res
              maxB  = fromEnum $ maxBound `asTypeOf` res

instance Finite a => Finite (Mod a)


instance (Show a, Eq a, Finite a) => Num (Mod a) where
    (+) = inEnum2 (+)
    (-) = inEnum2 (-)
    (*) = inEnum2 (*)
    
    fromInteger = toEnum . fromInteger

    abs     = error "abs undefined for Mod"
    signum  = error "signum undefined for Mod"

-- Saturation


-- | Wrapper 'Sat' helps to define arithmetic with
-- saturation. If your type @T@ is instance of class 'Finite' then
-- type @Mod T@ is instance of class `Num`. If result of '+'
-- or '*' exceeds defined bounds value is clipped.
--
newtype Sat a = Sat { getSat :: a }
    deriving (Show, Eq)

instance Finite a => Bounded (Sat a) where
    minBound = Sat minBound
    maxBound = Sat maxBound


instance Finite a => Enum (Sat a) where
    fromEnum = fromEnum . getSat
    toEnum n = Sat res
        where res   = toEnum $ max minB $ min maxB n
              minB  = fromEnum $ minBound `asTypeOf` res
              maxB  = fromEnum $ maxBound `asTypeOf` res
    
instance Finite a => Finite (Sat a)


instance (Show a, Eq a, Finite a) => Num (Sat a) where
    (+) = inEnum2 (+)
    (-) = inEnum2 (-)
    (*) = inEnum2 (*)
    
    fromInteger = toEnum . fromInteger

    abs     = error "abs undefined for Sat"
    signum  = error "signum undefined for Sat"


inEnum2 :: Enum a => (Int -> Int -> Int) -> (a -> a -> a)
inEnum2 op a b = toEnum $ fromEnum a `op` fromEnum b

-- | Finding saturation of value.
-- In expression @'sat' minB maxB x@ value @x@ is clipped 
-- to be within interval @[minB, maxB]@
sat :: Ord a => a -> a -> a -> a
sat a b = max a . min b
    

