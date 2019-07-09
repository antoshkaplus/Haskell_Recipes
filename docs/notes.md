
camelCase for function names
CapitalCase for type, type class and constructor names

files use CapitalCase

##### Cabal
Show installed packages:  
* cabal list --installed
* ghc-pkg list

##### Repl
```cabal new-repl``` - start repl  
```stack ghci``` - start repl  
```stack <filename>``` - run a script  

Press Ctrl+D to exit the GHCi REPL  
* ```:m``` - change current module  
* ```:show paths``` - from the name
* ```:t``` - check declaration type  
* ```:m + Data.List``` - to reach functions from module,
    can put multiple modules, don't need if script
    imports it
* ```:info YourTypeClass``` in GHCI to know to which 
    instances belongs
* ```:k``` get kind, ```*``` concrete type
* ```:l :r```

#### Vocabulary
* homogeneous
* type variable
* class constant
* typeclass
* guards
* high order functions
* curried functions
* infix functions
* record syntax
* type constructor
* value constructor
* fixity
* type instance implementation
* minimal completion definition for a typeclass
* class constraint
* abstract datatype 
* parameterized data type
* concrete data type
* applicative functor (Control.Applicative)
* add context of failure using maybe
* difference list

* monad transformer


#### Typeclasses
* Eq
* Ord
* Functor
* Applicative
* Monoid (operation associativity, identity)

#### Monad Laws
* left identity: return x >>= f is the same thing as f x
* right identity: m >>= return is no different than just m
* associativity: Doing (m >>= f) >>= g is just like doing m >>= (\x -> f x >>= g)

#### Monad binding/composition operators:
* (>>=) :: Monad m => m a -> (a -> m b) -> m b
* (=<<) :: Monad m => (a -> m b) -> m a -> m b
* (>>) :: Monad m => m a -> m b -> m b
* (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
* (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)

#### Spec
* low case - type variable
* If a function is comprised only of special char-
    acters, it's considered an infix function by de-
    fault.
* ```(==) :: (Eq a) => a -> a -> Bool```
    the equality function takes
    any two values that are of the same type and returns a Bool. 
    The type of those two values must be
    a member of the Eq class (this was the class constraint).
    Eq is like a type constraint on type variable.
* ```xs@(x:y:ys)``` - keep reference for the whole thing    
* there is no let + guards, you use guards + where    
* So use lambdas in this way when you want to make it explicit 
    that your function is mainly meant
    to be partially applied and passed on to a function as a parameter.
* ```$``` - lowest precedence operator, function application  
    ```map ($ 3) [(4+), (10*), (^2), sqrt]```
*```.``` - function composition
* The prefered style is to use let bindings to give labels to intermediary results or split the problem
    into sub-problems and then put it together so that the function makes sense to someone reading
    it instead of just making a huge composition chain.
* specify function for export when defining a module
* So don't put type constraints into data declarations even if it seems to make sense, 
    because you'll have to put them into the function type declarations either way.
* the Int type is an instance of the Eq typeclass because the Eq typeclass defines
    behavior for stuff that can be equated.
* ```type String = [Char]``` - aliases
* ```return``` - puts value in a box, ```<-``` - takes it out
* ```relaod``` - reload current module
* only IO code can throw, as it has order of execution
* an abstract datatype f a, which has the ability for it's value(s) to be mapped over 
    can become an instance of the Functor typeclass. Functors preserve composition of morphisms.
* ```fmap :: (a -> b) -> (f a -> f b)``` - lifting a function. 
    You can think of fmap as either a function that takes a function and a functor and then maps that
    function over the functor, or you can think of it as a function that takes a function and lifts that
    function so that it operates on functors.
* a pathological example of a type constructor being an instance of the Functor
      typeclass but not really being a functor, because it doesn't satisfy the laws. Let's say that we have
      a type:
      ```data CMaybe a = CNothing | CJust Int a deriving (Show)```
* mapping "multi-parameter" functions over functors, we get functors that contain
  functions inside them
* ```<*>``` - ```f (a -> b) -> f a -> f b```
* ```(<$>) :: (Functor f) => (a -> b) -> f a -> f b```
  ```f <$> x = fmap f x```
* ```(,,) function is the same as \x y z -> (x,y,z)```
* ```(,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"``` - can be replaced with ```zipWith3``` resulting in
    ```[('d','c','r'),('o','a','a'),('g','t','t')]```
* The newtype keyword, when we want to just take one type and wrap it in something to present it as another type:
    ```newtype ZipList a = ZipList { getZipList :: [a] }```
* ```(>>=) :: (Monad m) => m a -> (a -> m b) -> m b```, ```>>=``` - bind
* Functor => Applicative Functor => Monad
* every monad is an applicative functor
* ```do``` expressions are just different syntax for chaining monadic values. we can chain any actions as long as all of them are in the same monad.
* list comprehensions are just syntactic sugar for using lists as monads
* MonadPlus type class is for monads that can also act as monoids.
* non-deterministic value - we don't know what's inside the monad box/container ?
* reader monad allows us to treat functions as values with a context
* An expression is called referentially transparent if it can be replaced with its corresponding value without changing the program's behavior.
    This requires that the expression is pure, that is to say the expression value must be the same for the same inputs and its evaluation 
    must have no side effects. An expression that is not referentially transparent is called referentially opaque.
  

#### Naming Conventions
* 