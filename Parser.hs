{-
  Musat Mihai-Robert
  Grupa 323CB
-}

module Parser
where

import Util
import Data.Maybe
import Data.Char
import InferenceDataType

-- Defining what is a Program
data Program = EmptyProgram
    | WithVariable Program String String -- type, name
    | WithFunction Program String String String [String] -- class, return, etc.
    | WithClass Program String String -- name, parent

-- Return the container for the empty Program
initEmptyProgram :: Program
initEmptyProgram = WithClass EmptyProgram "Global" "Global"

-- Return all the variable names in the Program
getVars :: Program -> [[String]]
getVars (WithVariable p t n) = [n, t] : (getVars p)
getVars (WithFunction p _ _ _ _) = getVars p
getVars (WithClass p _ _) = getVars p
getVars EmptyProgram = []

-- Return all the classes in the Program
getClasses :: Program -> [String]
getClasses (WithVariable p _ _) = getClasses p
getClasses (WithFunction p _ _ _ _) = getClasses p
getClasses (WithClass p n m) = n : (getClasses p)
getClasses EmptyProgram = []

-- Return the parent class for a given class
getParentClass :: String -> Program -> String
getParentClass c (WithVariable p _ _) = getParentClass c p
getParentClass c (WithFunction p _ _ _ _) = getParentClass c p
getParentClass c (WithClass p n m)
    | n == c = m
    | otherwise = getParentClass c p
getParentClass c EmptyProgram = ""

-- Auxiliary function for getFuncsForClass
getFuncs :: String -> Program -> [[String]] -> [[String]]
getFuncs s (WithVariable p _ _) acc = getFuncs s p acc
getFuncs s (WithFunction p c t n params) acc
    | s == c = getFuncs s p ((n : t : params) : acc)
    | otherwise = getFuncs s p acc
getFuncs s (WithClass p _ _) acc = getFuncs s p acc
getFuncs _ EmptyProgram acc = reverse acc

-- Return a list of functions for a given class
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass s p = getFuncs s p []

-- An instruction can be a variable, a function or a class declaration
data Instruction = VariableDeclaration String String
    | FunctionDeclaration String String String [String]
    | ClassDeclaration String String

-- Ignore all the non-letters that are at the beginning of the string
ignoreToLetter :: String -> String
ignoreToLetter (c : s)
    | isLetter c = c : s
    | otherwise = ignoreToLetter s
ignoreToLetter [] = []

-- Extract the first word (a series of continuous letters)
extractWord :: String -> (String, String)
extractWord (c : s) = if isAlphaNum c
    then (c : (fst tmp), snd tmp)
    else ("", c : s)
    where tmp = extractWord s
extractWord "" = ("", "")

-- Parse the given string, returning a pair (token, remainingString)
getToken :: String -> (String, String)
getToken s = (fst tmp, ignoreToLetter $ snd tmp)
    where tmp = extractWord $ ignoreToLetter s

-- Tokenize the given string after obtaining the pairs above
tokenize :: String -> [String] -> [String]
tokenize "" acc = reverse acc
tokenize s acc = tokenize (snd tmp) ((fst tmp) : acc)
    where tmp = getToken s

-- Parse a line into a class declaration
parseClass :: String -> Instruction
parseClass s = if length tmp > 2
    then ClassDeclaration (tmp !! 0) (tmp !! 2)
    else ClassDeclaration (tmp !! 0) "Global"
    where tmp = tokenize s []

-- Parse a line into a variable declaration
parseVariable :: String -> Instruction
parseVariable s = VariableDeclaration (tmp !! 1) (tmp !! 0)
    where tmp = tokenize s []

-- Parse a line into a function declaration
parseFunction :: String -> Instruction
parseFunction s =
    FunctionDeclaration (tmp !! 1) (tmp !! 0) (tmp !! 2) (drop 3 tmp)
    where tmp = tokenize s []

-- Parse a line and return an instruction (if one exists)
parseLine :: String -> Maybe Instruction
parseLine s
    | fst tmp == "class" = Just $ parseClass $ snd tmp
    | fst tmp == "newvar" = Just $ parseVariable $ snd tmp
    | fst tmp == "" = Nothing
    | otherwise = Just $ parseFunction $ ((fst tmp) ++ " " ++ (snd tmp))
    where tmp = getToken s

-- Parse a whole file for instructions
parse :: String -> [Instruction]
parse s = catMaybes $ map parseLine $ lines s

-- Add an instruction to a Program
interpret :: Instruction -> Program -> Program
interpret (VariableDeclaration t n) p = if elem t tmp
    then WithVariable p t n
    else p
    where tmp = getClasses p
interpret (FunctionDeclaration c t n l) p =
    if length (filter (== True) (map (\x -> elem x tmp) (c : t : l))) == 2 + length l
    then WithFunction p c t n l
    else p
    where tmp = getClasses p
interpret (ClassDeclaration n m) p = if elem n tmp
    then p
    else if elem m tmp
        then WithClass p n m
        else WithClass p n "Global"
    where tmp = getClasses p

-- Find the return type of all functions with given params in a class
findReturnType :: Program -> String -> [String] -> Maybe String
findReturnType p c params
    | length tmp > 0 = Just ((tmp !! 0) !! 1)
    | (getParentClass c p == "Global") && (c == "Global") = Nothing
    | otherwise = findReturnType p (getParentClass c p) params
    where tmp = filter (\x -> params == drop 2 x) $ getFuncsForClass c p

-- Make type inference for a given expression
infer :: Expr -> Program -> Maybe String
infer (FCall c n params) p = if varType == Nothing || (elem Nothing paramsType)
    then Nothing
    else findReturnType p (fromMaybe "" varType) (catMaybes paramsType)
    where
        varType = infer (Va c) p
        paramsType = map (\x -> infer x p) params
infer (Va s) p = if length tmp > 0
    then Just $ (tmp !! 0) !! 1
    else Nothing
    where tmp = filter (\x -> x !! 0 == s) $ getVars p





