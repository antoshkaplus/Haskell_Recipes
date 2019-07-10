module GreatGood.Monad where


--  class Monad m where
--      return :: a -> m a
--      (>>=) :: m a -> (a -> m b) -> m b
--      (>>) :: m a -> m b -> m b
--      x >> y = x >>= \_ -> y
--
--      fail :: String -> m a
--      fail msg = error msg

--  instance Monad Maybe where
--      return x = Just x
--      Nothing >>= f = Nothing
--      Just x >>= f = f x
--      fail _ = Nothing


type Birds = Int
type Pole = (Birds,Birds)


x -: f = f x

--  ghci> (0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
--  (0,2)


landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
    | abs ((left + n) - right) < 4 = Just (left + n, right)
    | otherwise = Nothing


landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
    | abs (left - (right + n)) < 4 = Just (left, right + n)
    | otherwise = Nothing

--ghci> return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
--Just (2,4)

--ghci> return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
--Nothing


banana :: Pole -> Maybe Pole
banana _ = Nothing

--  ghci> return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
--  Nothing

--  or
--  ghci> return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
--  Nothing



--  do { action1           -- by monad laws equivalent to:  do { action1
--     ; action2           --                                  ; do { action2
--     ; action3 }         --                                       ; action3 } }
--
--  action1 >>
--  do { action2
--     ; action3 }
--
--
--
--  do { x1 <- action1
--     ; x2 <- action2
--     ; mk_action3 x1 x2 }
--
--  action1 >>= (\ x1 -> action2 >>= (\ x2 -> mk_action3 x1 x2 ))
--
--  action1 >>= (\ x1 ->
--    action2 >>= (\ x2 ->
--      mk_action3 x1 x2 ))


routine :: Maybe Pole
routine = do
    start <- return (0,0)
    first <- landLeft 2 start
    Nothing                         -- banana
    second <- landRight 2 first
    landLeft 1 second



--    instance Monad [] where
--      return x = [x]
--      xs >>= f = concat (map f xs)
--      fail _ = []

--    ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
--    [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

--    listOfTuples :: [(Int,Char)]
--    listOfTuples = do
--    n <- [1,2]
--    ch <- ['a','b']
--    return (n,ch)



--    sevensOnly :: [Int]
--    sevensOnly = do
--    x <- [1..50]
--    guard ('7' `elem` show x)
--    return x

--    ghci> [ x | x <- [1..50], '7' `elem` show x ]
--    [7,17,27,37,47]



--    instance Monad ((->) r) where  -- reader monad
--        return x = \_ -> x
--        h >>= f = \w -> f (h w) w