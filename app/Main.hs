module Main (main) where

import DFAs (automato_L1, automato_L2, automato_L3, automato_L4)
import Repl (repl)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Escolha um automato"
    putStrLn "1. L1 = { w | aa ∨ bb sao subpalavras de w }"
    putStrLn "2. L2 = { w | |w| > 3 }"
    putStrLn "3. L3 = { w | w tem prefixo 'aa' }"
    putStrLn "4. L4 = { w | |w| eh multiplo de 3 }"
    putStr "> "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> repl automato_L1
        "2" -> repl automato_L2
        "3" -> repl automato_L3
        "4" -> repl automato_L4
        _ -> putStrLn "?"
