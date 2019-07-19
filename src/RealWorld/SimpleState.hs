module RealWorld.SimpleState where

-- state transformer monad
type SimpleState s a = s -> (a, s)

--    Earlier in this chapter, we said that a monad has a type constructor with a single type
--    variable, and yet here we have a type with two parameters. The key is to understand
--    that we can partially apply a type just as we can partially apply a normal function. This
--    is easiest to follow with an example:
--
--    type StringState a = SimpleState String a

returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)

bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'


newtype State s a = State {
    runState :: s -> (a, s)
}

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
