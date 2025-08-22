{-# OPTIONS_GHC -Wno-x-partial -Wno-unrecognised-warning-flags #-}

module Repl (repl) where

import System.IO (hFlush, stdout)
import Types (Automato, processarString)

-- Mostrar transicoes step-by-step
mostrarExecucao :: (Show est, Ord est) => Automato est -> String -> IO ()
mostrarExecucao aut input = do
    let (aceito, caminho) = processarString aut input
    mapM_
        ( \(c, estadoAtual, proximoEstado) ->
            putStrLn $ "  δ(" ++ show estadoAtual ++ ", '" ++ [c] ++ "') = " ++ show proximoEstado
        )
        (zip3 input caminho (tail caminho))
    putStrLn $ ":" ++ show aceito
    putStrLn ""

repl :: (Show est, Ord est) => Automato est -> IO ()
repl aut = do
    putStrLn $ "\n" ++ show aut
    loop
  where
    loop = do
        putStr "> "
        hFlush stdout
        input <- getLine
        do
            mostrarExecucao aut input
            loop
