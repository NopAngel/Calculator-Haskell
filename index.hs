-- Calculadora en Haskell

module Main where

import Text.Read (readMaybe)

-- Definición de tipos de datos para la sintaxis del árbol de expresión
data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Lit Double
  deriving Show

-- Función para evaluar una expresión
eval :: Expr -> Double
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Lit x) = x

-- Función para analizar una cadena de entrada y construir el árbol de expresión
parse :: String -> Maybe Expr
parse input = case readMaybe input of
  Just x -> Just (Lit x)
  Nothing -> case words input of
    [e1, "+", e2] -> Add <$> parse e1 <*> parse e2
    [e1, "-", e2] -> Sub <$> parse e1 <*> parse e2
    [e1, "*", e2] -> Mul <$> parse e1 <*> parse e2
    [e1, "/", e2] -> Div <$> parse e1 <*> parse e2
    _ -> Nothing

main :: IO ()
main = do
  putStrLn "Calculadora Haskell"
  putStrLn "Ingresa una expresión aritmética (por ejemplo, 1 + 2 * 3):"
  input <- getLine
  case parse input of
    Just expr -> putStrLn $ "Resultado: " ++ show (eval expr)
    Nothing -> putStrLn "Expresión no válida. Inténtalo de nuevo."
