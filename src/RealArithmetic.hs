{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module RealArithmetic where

import Data.Char
import Data.List
import GHC.Float
import Data.Bifunctor

import Debug.Trace as DT

type Length = Int

combinations :: Length -> [a] -> [[a]]
combinations size set =
  [ x |  x <- cs size set, length x == size]
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

instance (a ~ Double) => Show (Assoc a) where
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
assocsWith (x:y:xs) (op:ops) = consWith x op =<< assocsWith (y:xs) ops
assocsWith _  []             = []

------------------------------------------------------------------------------------
-- LEXER --
------------------------------------------------------------------------------------
data Token
  = PlusT
  | MinusT
  | MultT
  | DivT
  | DotP
  | OpenP
  | CloseP
  | RealT Double
  deriving (Show)

lexer :: String -> [Token]
lexer []              = []
lexer ('+' : restStr) = PlusT    : lexer restStr
lexer ('-' : restStr) = MinusT   : lexer restStr
lexer ('*' : restStr) = MultT    : lexer restStr
lexer ('/' : restStr) = DivT     : lexer restStr
lexer ('(' : restStr) = OpenP    : lexer restStr
lexer (')' : restStr) = CloseP   : lexer restStr
lexer ('.' : restStr) = DotP     : lexer restStr
lexer (chr : restStr)
  | isSpace chr       = lexer restStr
lexer str@(chr : '.' : chr' : _)
  | isDigit chr && isDigit chr'
  = RealT real : lexer restStr
  where
    (real, restStr) = head $ reads @Double str
lexer str@(chr : _)
  | isDigit chr
  = RealT real : lexer restStr
  where
    (real, restStr) = head $ reads @Double str
-- runtime error for all other characters:
lexer (chr : restString)
  = error ("lexer: unexpected character: '" ++ [chr] ++ "'")

notIsDigit :: Char -> Bool
notIsDigit = not . isDigit

-- stringToReal :: String -> Double
-- stringToReal = foldl (\ chr -> 10 *  + digitToInt chr) 0

------------------------------------------------------------------------------------
-- PARSER --
------------------------------------------------------------------------------------

data Expr
  = RealLit Double          -- integer constants, leaves of the expression tree
  | Add    Expr Expr    -- addition node
  | Sub    Expr Expr
  | Mult   Expr Expr    -- multiplication node
  | Div    Expr Expr
  deriving Show

parseRealOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseRealOrParenExpr tokens
  = case tokens of
      (RealT n : restTokens) -> Just (RealLit n,   restTokens)
      (OpenP : restTokens) ->
        case parseSumOrProdOrRealOrParenExpr restTokens of
          Just (expr, CloseP : restTokens) -> Just (expr, restTokens)
          Just _  -> Nothing -- no closing paren
          Nothing -> Nothing
      _ -> Nothing

parseProdOrRealOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseProdOrRealOrParenExpr tokens
  = case parseRealOrParenExpr tokens of
      Just (expr1, MultT : restTokens) ->
          case parseProdOrRealOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Mult expr1 expr2, restTokens)
            Nothing                  -> Nothing
      Just (expr1, DivT : restTokens) ->
          case parseProdOrRealOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Div expr1 expr2, restTokens)
            Nothing                  -> Nothing
      result -> result

parseSumOrProdOrRealOrParenExpr :: [Token] -> Maybe (Expr, [Token])
parseSumOrProdOrRealOrParenExpr tokens
  = case parseProdOrRealOrParenExpr tokens of
      Just (expr1, PlusT : restTokens) ->
          case parseSumOrProdOrRealOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Add expr1 expr2, restTokens)
            Nothing                  -> Nothing
      Just (expr1, MinusT : restTokens) ->
          case parseSumOrProdOrRealOrParenExpr restTokens of
            Just (expr2, restTokens) -> Just (Sub expr1 expr2, restTokens)
            Nothing                  -> Nothing
      result -> result

parse :: [Token] -> Expr
parse tokens =
  --case parseSumOrProdOrRealOrParenExpr tokens of
  case parseSumOrProdOrRealOrParenExpr tokens of
    Just (expr, []) -> expr
    _               -> error ("Could not parse input: " ++ show tokens ++ "'")

------------------------------------------------------------------------------------
-- EVALUATE --
------------------------------------------------------------------------------------
-- | λ> map (eval . parse . lexer) ["(1*2)/4", "3/0", "2+2"]
-- | > [0,0,4]
-- | \> ((eval . parse . lexer) "(2.5+3.5)*2.5") == ((eval . parse . lexer) "10 + 5")
-- | > True
eval :: Expr -> Double
eval expr
  = case expr of
      (RealLit n)         -> n
      (Add  expr1 expr2) -> eval expr1 + eval expr2
      (Sub  expr1 expr2) -> eval expr1 - eval expr2
      (Mult expr1 expr2) -> eval expr1 * eval expr2
      (Div  expr1 expr2) ->
        if eval expr2 /= 0
          then eval expr1 / eval expr2
          else 0

eval' :: String -> Double
eval' = eval . parse . lexer
-- | \> (eval' "10 + 5") == (eval' "5.5 + 9.5")
-- | > True
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
------------------------------------------------------------------------------------
formula :: [[Double]] -> [String]
formula xs =
  show <$> (concat . concat) [fmap (assocsWith x) (combinations (length x - 1) "+-*/") | x <- xs]

solve :: [Double] -> [Double]
solve xs = map (eval . parse . lexer) $ (formula . permutations) xs

solve' :: [Double] -> IO ()
solve' xs = do
  let set         = xs
  let ss          = solve set
  let diff        = [1..maximum ss+1] \\ ss
  putStrLn $ "ss (solution set)     : " ++ show ss
  -- putStrLn $ "first NN not in the ss: " ++ show diff
  putStrLn $ "solution for " ++ show set ++ " : \n" ++ show (head diff) ++ "\n"

main :: IO ()
main = do
  solve' [1..2]
  -- solve' [1..3]
  -- solve' [1..4]
  -- solve' [1..5]
  solve' [0,11,12]
  -- solve' [1..6]
