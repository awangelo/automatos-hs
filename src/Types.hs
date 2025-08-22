{-# LANGUAGE InstanceSigs #-}

module Types (
    processarString,
    Automato,
    mkAlfabeto,
    mkEstados,
    mkTransicao,
    mkEstadoInicial,
    mkEstadosFinais,
    mkAutomato,
) where

import qualified Data.Set as Set

newtype Automato est
    = Automato
        ( Alfabeto
        , Estados est
        , Transicao est
        , EstadoInicial est
        , EstadosFinais est
        )

instance (Show est) => Show (Automato est) where
    show :: Automato est -> String
    show (Automato (alfa, estados, _delta, q0, finais)) =
        "Automato {\n  Σ  ⇒ "
            ++ show alfa
            ++ "\n  Q  ⇒ "
            ++ show estados
            ++ "\n  δ"
            ++ "\n  q0 ⇒ "
            ++ show q0
            ++ "\n  F  ⇒ "
            ++ show finais
            ++ "\n}"

newtype Alfabeto = Alfabeto (Set.Set Char)
    deriving (Show, Eq)

newtype Estados est = Estados (Set.Set est)
    deriving (Show, Eq)

newtype Transicao est = Transicao ((est, Char) -> est)

newtype EstadoInicial est = EstadoInicial est
    deriving (Show, Eq)

newtype EstadosFinais est = EstadosFinais (Set.Set est)
    deriving (Show, Eq)

processarString :: (Ord est) => Automato est -> String -> (Bool, [est])
processarString (Automato (_, _, Transicao delta, EstadoInicial q0, EstadosFinais finais)) input =
    let caminho = scanl (curry delta) q0 input
        estadoFinal = last caminho
        aceito = Set.member estadoFinal finais
     in (aceito, caminho)

mkAutomato :: Alfabeto -> Estados est -> Transicao est -> EstadoInicial est -> EstadosFinais est -> Automato est
mkAutomato alfa estados transicao inicial finais = Automato (alfa, estados, transicao, inicial, finais)

mkAlfabeto :: [Char] -> Alfabeto
mkAlfabeto = Alfabeto . Set.fromList

mkEstados :: (Ord est) => [est] -> Estados est
mkEstados = Estados . Set.fromList

mkTransicao :: ((est, Char) -> est) -> Transicao est
mkTransicao = Transicao

mkEstadoInicial :: est -> EstadoInicial est
mkEstadoInicial = EstadoInicial

mkEstadosFinais :: (Ord est) => [est] -> EstadosFinais est
mkEstadosFinais = EstadosFinais . Set.fromList
