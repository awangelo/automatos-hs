module DFAs (automato_L2, automato_L3) where

import Types (
    Automato,
    mkAlfabeto,
    mkAutomato,
    mkEstadoInicial,
    mkEstados,
    mkEstadosFinais,
    mkTransicao,
 )

-- L2 = { w ∈ {a,b}* | |w| > 3 }
automato_L2 :: Automato String
automato_L2 =
    mkAutomato
        (mkAlfabeto ['a', 'b'])
        (mkEstados ["q_0", "q_1", "q_2", "q_3", "q_4"])
        (mkTransicao transicao_L2)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_4"])
  where
    transicao_L2 ("q_0", _) = "q_1"
    transicao_L2 ("q_1", _) = "q_2"
    transicao_L2 ("q_2", _) = "q_3"
    transicao_L2 ("q_3", _) = "q_4"
    transicao_L2 ("q_4", _) = "q_4"
    transicao_L2 _ = error "Transicao invalida."

-- L3 = { w ⋹ {a, b}* | tem prefixo "aa" }
automato_L3 :: Automato String
automato_L3 =
    mkAutomato
        (mkAlfabeto ['a', 'b'])
        (mkEstados ["q_0", "q_1", "q_2", "q_reject"])
        (mkTransicao transicao_L3)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_2"])
  where
    transicao_L3 ("q_0", 'a') = "q_1" -- primeiro 'a'
    transicao_L3 ("q_0", 'b') = "q_reject" -- começa com 'b' → rejeita
    transicao_L3 ("q_1", 'a') = "q_2" -- segundo 'a' → prefixo "aa" confirmado!
    transicao_L3 ("q_1", 'b') = "q_reject" -- segundo char não é 'a' → rejeita
    transicao_L3 ("q_2", _) = "q_2" -- uma vez confirmado prefixo, sempre aceita
    transicao_L3 ("q_reject", _) = "q_reject" -- estado de rejeição (trap state)
    transicao_L3 _ = error "Transicao invalida."
