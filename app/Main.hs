module Main (main) where

import           DFAs (automato_L2, automato_L3)
import           Repl (repl)

main :: IO ()
main = do
    putStrLn "Escolha um automato:"
    putStrLn "1. L2 = { w | |w| > 3 } "
    putStrLn "2. L3 = { w | w tem prefixo 'aa' } "
    opcao <- getLine
    case opcao of
        "1" -> repl "L2 (tamanho > 3)" automato_L2
        "2" -> repl "L3 (prefixo 'aa')" automato_L3
        _   -> putStrLn "?"