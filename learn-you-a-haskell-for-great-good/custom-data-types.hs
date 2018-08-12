module Main where
import           BinaryTree
import qualified Data.Map     as Map
import           Person
import           Shapes
import           TrafficLight
import           Vector

-- enum example (sorry, Pluto...rip)
data Planet = Mercury | Venus | Earth | Mars | Jupiter | Saturn | Uranus | Neptune
                deriving (Eq, Ord, Show, Read, Enum, Bounded)

-- locker example setup
data LockerState = Taken | Free deriving(Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

-- Lookup if a locker is Free
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNum map = case Map.lookup lockerNum map of
                                Nothing -> Left $ "Locker " ++ show lockerNum ++ " does not exist!"
                                Just (state, code) -> if state /= Taken
                                                        then Right code
                                                      else Left $ "Locker " ++ show lockerNum ++ " is taken"

-- This type is meant to implement something like javascripts truthy-falsey
-- behavior for variables. e.g. in js if("") is false and if("hello") is true
class YesNo a where
  yesno :: a -> Bool

-- YesNo if statement takes in yesno type, then any (what to do if true)
-- and any (what to do if false)
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
    then yesResult
    else noResult

-- Make some things implement YesNo
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

-- id is a std library function which takes parameter and returns the same thing
instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing  = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _         = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True

main :: IO ()
main = do
    print $ area $ (Circle (Point 10 20) 10)
    print $ area $ (Rectangle (Point 0 2) (Point 4 3))
    print $ firstName person
    print $ flavor person
    print $ vect1 `vplus` vect2
    print $ vect1 `scalarProd` 2
    print $ vect1 `dotProd` vect2
    print $ Mercury > Venus
    print $ succ Venus
    print $ [Mercury .. Mars]
    print $ lockerLookup 100 lockerMap
    print $ lockerLookup 101 lockerMap
    print $ lockerLookup 200 lockerMap
    print $ treeInsert 12 binaryTree
    print $ treeElem 4 binaryTree
    print $ 2 `treeElem` binaryTree
    print $ 120 `treeElem` binaryTree
    print $ yesno []
    print $ yesno [1..3]
    print $ yesno "" -- This is really just empty list char so same as yesno []
    print $ yesno Red
    print $ yesno Nothing
    print $ yesno $ Just 4
    print $ yesnoIf [] "Yes" "No"
    print $ yesnoIf (Just 3) "Yes" "No"
    -- Examples of using tree as functor
    print $ fmap (*2) binaryTree
    where
      person = Person { firstName="Scott", lastName="Martin", age=25, height=72, phoneNumber="12345", flavor="vanilla bean" }
      vect1 = Vector 3 4 8
      vect2 = Vector 12 4 9
      lockerMap = Map.fromList([(100, (Taken, "123"))
                                , (101, (Free, "123"))
                                , (102, (Free, "123"))
                                , (103, (Taken, "123"))])
      binaryTree = foldr treeInsert EmptyTree [1,3,12,18,0,5,8,10,2,7,4]
