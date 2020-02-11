{-
  Musat Mihai-Robert
  Grupa 323CB
-}

module ClassState
where

data InstrType = Var | Func  deriving (Show, Eq)

-- Stores the vars and functions for a certain class
data ClassState = EmptyState
    | WithVar ClassState String String -- with: type, name
    | WithFunc ClassState String String [String] -- with: return, name, params

-- Return an empty class
initEmptyClass :: ClassState
initEmptyClass = EmptyState

-- Insert a variable or a function into the given class
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass c Var (n : t :[]) = WithVar c t n
insertIntoClass c Func (r : n : p) = WithFunc c r n p

-- Return all the variables or all the functions that are in the given class
getValues :: ClassState -> InstrType -> [[String]]
getValues (WithVar c t n) Var = [n, t] : (getValues c Var)
getValues (WithVar c t n) Func = getValues c Func
getValues (WithFunc c r n p) Var = getValues c Var
getValues (WithFunc c r n p) Func = (r : n : p) : (getValues c Func)
getValues EmptyState _ = []
