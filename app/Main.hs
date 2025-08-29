module Main (main) where

import DFAs (automato_L1, automato_L2, automato_L3, automato_L4, automato_L5, automato_LA, automato_LB, automato_LC, automato_LD, automato_LE)
import Repl (repl)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
    putStrLn "Escolha um automato"
    putStrLn "1. L1 = { w | aa ∨ bb sao subpalavras de w }"
    putStrLn "2. L2 = { w | |w| > 3 }"
    putStrLn "3. L3 = { w | w tem prefixo 'aa' }"
    putStrLn "4. L4 = { w | |w| eh multiplo de 3 }"
    putStrLn "5. L5 = { w | cada 0 eh seguido por no minimo dois 1's }"
    putStrLn "6. LA = { w | w nao tem as sub-palavras 000 ∨ 111 }"
    putStrLn "7. LB = { w ⋹ {0, 1}* | os ultimos 3 simbolos de w NAO SAO 000 }"
    putStrLn "8. LC = { w ⋹ {a, b}* | w NAO contem ab }"
    putStrLn "9. LD = { w ⋹ {a, b}* | w tem tamanho multiplo 3 e NAO contem ab }"
    putStrLn "10. LE = LE = { w ⋹ {0, 1}* | 1*0*1"
    putStr "> "
    hFlush stdout
    opcao <- getLine
    case opcao of
        "1" -> repl automato_L1
        "2" -> repl automato_L2
        "3" -> repl automato_L3
        "4" -> repl automato_L4
        "5" -> repl automato_L5
        "6" -> repl automato_LA
        "7" -> repl automato_LB
        "8" -> repl automato_LC
        "9" -> repl automato_LD
        "10" -> repl automato_LE
        _ -> putStrLn "Opcao invalida"
