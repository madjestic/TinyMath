{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
module Arithmetic where

import Data.Char
import Data.List

type Length = Int

combinations :: Length -> [a] -> [[a]]
combinations size set =
  [ x |  x <- cs size set, (length x) == size]
  where
    cs size xs = [1..size] >>= \n -> mapM (const xs) [1..n]

------------------------------------------------------------------------------------
-- ASSOCIATION VARIANTS --
------------------------------------------------------------------------------------
data Assoc a = Leaf a | Assoc a :+ Assoc a
                      | Assoc a :- Assoc a
                      | Assoc a :* Assoc a
                      | Assoc a :/ Assoc a
  deriving Eq

instance (a ~ Int) => Show (Assoc a) where
  show (Leaf c)  = show c
  show (a :+ b) = "(" ++ show a ++ "+" ++ show b  ++ ")"
  show (a :- b) = "(" ++ show a ++ "-" ++ show b  ++ ")"
  show (a :* b) = "(" ++ show a ++ "*" ++ show b  ++ ")"
  show (a :/ b) = "(" ++ show a ++ "/" ++ show b  ++ ")"

consWith :: a -> Char -> Assoc a -> [Assoc a]
consWith x op y
  = case op of
      '+' -> (Leaf x :+ y) : rest
      '-' -> (Leaf x :- y) : rest
      '*' -> (Leaf x :* y) : rest
      '/' -> (Leaf x :/ y) : rest
      _   -> error "unknown operator"
    where
      rest = case y of
        Leaf _ -> []
        a :+ b -> (:+ b) <$> consWith x op a
        a :- b -> (:- b) <$> consWith x op a
        a :* b -> (:* b) <$> consWith x op a
        a :/ b -> (:/ b) <$> consWith x op a
                 
                 
assocsWith :: [a] -> String -> [Assoc a]
assocsWith []  _             = []
assocsWith [x] _             = [Leaf x]
assocsWith (x:y:xs) (op:ops) = consWith x op =<< assocsWith (y:xs) (ops)
assocsWith _  []             = []

------------------------------------------------------------------------------------
-- LEXER --
------------------------------------------------------------------------------------
data Token
  = PlusT
  | MinusT
  | MultT
  | DivT
  | OpenP
  | CloseP
  | IntT Int
  deriving (Show)

lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusT    : lexer restStr
lexer ('-' : restStr) = MinusT   : lexer restStr
lexer ('*' : restStr) = MultT    : lexer restStr
lexer ('/' : restStr) = DivT     : lexer restStr 
lexer ('(' : restStr) = OpenP    : lexer restStr 
lexer (')' : restStr) = CloseP   : lexer restStr
lexer (chr : restStr) 
  | isSpace chr       = lexer restStr
lexer str@(chr : _) 
  | isDigit chr
  = IntT (stringToInt digitStr) : lexer restStr
  where
     (digitStr, restStr) = break (not . isDigit) str
     -- local function to convert a string to an integer value
     stringToInt :: String -> Int
     stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0
-- runtime error for all other characters:
lexer (chr : restString) 
  = error ("lexer: unexpected character: '" ++ [chr] ++ "'")

notIsDigit :: Char -> Bool
notIsDigit = not . isDigit

stringToInt :: String -> Int
stringToInt = foldl (\acc chr -> 10 * acc + digitToInt chr) 0

------------------------------------------------------------------------------------
-- PARSER --
------------------------------------------------------------------------------------

data Expr
  = IntLit Int          -- integer constants, leaves of the expression tree
  | Add    Expr Expr    -- addition node
  | Sub    Expr Expr
  | Mult   Expr Expr    -- multiplication node
  | Div    Expr Expr
  deriving Show

parseIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseIntOrParenExpr tokens
  = case tokens of
      (IntT n : restTokens) -> Just (IntLit n,   restTokens)
      (OpenP : restTokens) -> 
        case parseSumOrProdOrIntOrParenExpr restTokens of
          Just (expr, (CloseP : restTokens)) -> Just (expr, restTokens)
          Just _  -> Nothing -- no closing paren
          Nothing -> Nothing
      _ -> Nothing
      
parseProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrIntOrParenExpr tokens
  = case parseIntOrParenExpr tokens of
      Just (expr1, (MultT : restTokens)) -> 
          case parseProdOrIntOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Mult expr1 expr2, restTokens)
            Nothing                  -> Nothing
      Just (expr1, (DivT : restTokens)) -> 
          case parseProdOrIntOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Div expr1 expr2, restTokens)
            Nothing                  -> Nothing
      result -> result   

parseSumOrProdOrIntOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrIntOrParenExpr tokens
  = case parseProdOrIntOrParenExpr tokens of
      Just (expr1, (PlusT : restTokens)) -> 
          case parseSumOrProdOrIntOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Add expr1 expr2, restTokens)
            Nothing                  -> Nothing
      Just (expr1, (MinusT : restTokens)) -> 
          case parseSumOrProdOrIntOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Sub expr1 expr2, restTokens)
            Nothing                  -> Nothing
      result -> result

parse :: [Token] -> Expr
parse tokens =
  case parseSumOrProdOrIntOrParenExpr tokens of
    Just (expr, []) -> expr    
    _               -> error "Could not parse input" 

------------------------------------------------------------------------------------
-- EVALUATE --
------------------------------------------------------------------------------------
-- | λ> map (eval . parse . lexer) ["(1*2)/4", "3/0", "2+2"]
-- [0,0,4]
eval :: Expr -> Int
eval expr
  = case expr of
      (IntLit n)         -> n
      (Add  expr1 expr2) -> eval expr1 + eval expr2
      (Sub  expr1 expr2) -> eval expr1 - eval expr2
      (Mult expr1 expr2) -> eval expr1 * eval expr2
      (Div  expr1 expr2) ->
        if (eval expr2) /= 0
          then eval expr1 `div` eval expr2
          else 0

------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
formula :: [[Int]] -> [String]
formula xs =
  fmap show $ (concat . concat) [fmap (assocsWith x) (combinations ((length x) - 1) "+-*/") | x <- xs]

solve :: [Int] -> [Int]
solve xs = map (eval . parse . lexer) $ (formula . permutations) xs

solve' :: [Int] -> IO ()
solve' xs = do
  let set         = xs
  let ss          = solve set
  let diff        = ([1..(maximum ss)+1] \\ ss)
  -- putStrLn $ "ss (solution set)     : " ++ show ss
  -- putStrLn $ "first NN not in the ss: " ++ show diff
  putStrLn $ "solution for " ++ show set ++ " : \n" ++ (show $ head diff) ++ "\n"  

main :: IO ()
main = do
  solve' [1..2]
  solve' [1..3]
  solve' [1..4]
  solve' [1..5]
  solve' [1..6]
