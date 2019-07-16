module RealWorld.Show where


-- file: ch06/AutomaticDerivation.hs
data CannotShow = CannotShow
    deriving (Show)

-- will not compile if remove CannotShow  deriving from Show
data CannotDeriveShow = CannotDeriveShow CannotShow
    deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
    deriving (Show)