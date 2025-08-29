module DFAs (automato_L1, automato_L2, automato_L3, automato_L4, automato_L5, automato_LA, automato_LB, automato_LC, automato_LD, automato_LE) where

import Types (
    Automato,
    mkAlfabeto,
    mkAutomato,
    mkEstadoInicial,
    mkEstados,
    mkEstadosFinais,
    mkTransicao,
 )

-- L1 = { w ⋹ {a, b}* | "aa" ∨ "bb" ⊆ w }
automato_L1 :: Automato String
automato_L1 =
    mkAutomato
        (mkAlfabeto ['a', 'b'])
        (mkEstados ["q_0", "q_1", "q_2", "q_3"])
        (mkTransicao transicao_L1)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_3"])
  where
    transicao_L1 ("q_0", 'a') = "q_1"
    transicao_L1 ("q_0", 'b') = "q_2"
    transicao_L1 ("q_1", 'a') = "q_3"
    transicao_L1 ("q_1", 'b') = "q_2"
    transicao_L1 ("q_2", 'a') = "q_1"
    transicao_L1 ("q_2", 'b') = "q_3"
    transicao_L1 ("q_3", 'a') = "q_3"
    transicao_L1 ("q_3", 'b') = "q_3"
    transicao_L1 _ = error "Transicao invalida."

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

-- L4 = { w ∈ {a,b}* | |w| eh multiplo de 3 }
automato_L4 :: Automato String
automato_L4 =
    mkAutomato
        (mkAlfabeto ['a', 'b'])
        (mkEstados ["q_0", "q_1", "q_2", "q_3", "q_4"])
        (mkTransicao transicao_L4)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_0"])
  where
    transicao_L4 ("q_0", _) = "q_1"
    transicao_L4 ("q_1", _) = "q_2"
    transicao_L4 ("q_2", _) = "q_0"
    transicao_L4 _ = error "Transicao invalida."

-- L5 = { w ⋹ {1, 0}* | cada 0 de w eh seguido por no minimo dois 1's }
automato_L5 :: Automato String
automato_L5 =
    mkAutomato
        (mkAlfabeto ['0', '1'])
        (mkEstados ["q_0", "q_1", "q_2", "q_3"])
        (mkTransicao transicao_L5)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_0"])
  where
    transicao_L5 ("q_0", '0') = "q_1"
    transicao_L5 ("q_0", '1') = "q_0"
    transicao_L5 ("q_1", '1') = "q_2"
    transicao_L5 ("q_2", '1') = "q_0"
    transicao_L5 _ = error "Transicao invalida."

-- LA = { w ⋹ {0, 1}* | w nao tem as sub-palavras "000" ∨ "111" }
automato_LA :: Automato String
automato_LA =
    mkAutomato
        (mkAlfabeto ['0', '1'])
        (mkEstados ["q_0", "q_1", "q_2", "q_3", "q_4", "q_trap"])
        (mkTransicao transicao_LA)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_0", "q_1", "q_2", "q_3", "q_4"])
  where
    transicao_LA ("q_0", '0') = "q_1"
    transicao_LA ("q_0", '1') = "q_2"
    transicao_LA ("q_1", '0') = "q_4"
    transicao_LA ("q_1", '1') = "q_2"
    transicao_LA ("q_2", '0') = "q_1"
    transicao_LA ("q_2", '1') = "q_3"
    transicao_LA ("q_3", '0') = "q_1"
    transicao_LA ("q_3", '1') = "q_trap"
    transicao_LA ("q_4", '0') = "q_trap"
    transicao_LA ("q_4", '1') = "q_2"
    transicao_LA ("q_trap", _) = "q_trap"
    transicao_LA _ = error "Transicao invalida."

-- LB = { w ⋹ {0, 1}* | os ultimos 3 simbolos de w NAO SAO 000 }
automato_LB :: Automato String
automato_LB =
    mkAutomato
        (mkAlfabeto ['0', '1'])
        (mkEstados ["q_0", "q_1", "q_2", "q_3"])
        (mkTransicao transicao_LB)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_0", "q_1", "q_2"])
  where
    transicao_LB ("q_0", '0') = "q_1"
    transicao_LB ("q_0", '1') = "q_0"
    transicao_LB ("q_1", '0') = "q_2"
    transicao_LB ("q_1", '1') = "q_0"
    transicao_LB ("q_2", '0') = "q_3"
    transicao_LB ("q_2", '1') = "q_0"
    transicao_LB ("q_3", '0') = "q_3"
    transicao_LB ("q_3", '1') = "q_0"
    transicao_LB _ = error "Transicao invalida."

-- LC = { w ⋹ {a, b}* | w NAO contem ab }
automato_LC :: Automato String
automato_LC =
    mkAutomato
        (mkAlfabeto ['a', 'b'])
        (mkEstados ["q_0", "q_1", "q_trap"])
        (mkTransicao transicao_LC)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_0", "q_1", "q_2"])
  where
    transicao_LC ("q_0", 'a') = "q_1"
    transicao_LC ("q_0", 'b') = "q_0"
    transicao_LC ("q_1", 'a') = "q_1"
    transicao_LC ("q_1", 'b') = "q_trap"
    transicao_LC ("q_trap", _) = "q_trap"
    transicao_LC _ = error "Transicao invalida."

-- LD = { w ∈ {a, b}∗ | w tem tamanho multiplo 3 e NAO contem ab }
automato_LD :: Automato String
automato_LD =
    mkAutomato
        (mkAlfabeto ['a', 'b'])
        (mkEstados ["q_0", "q_1", "q_2", "q_3", "q_4", "q_5", "q_trap"])
        (mkTransicao transicao_LD)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_0", "q_4"])
  where
    transicao_LD ("q_0", 'a') = "q_1"
    transicao_LD ("q_0", 'b') = "q_2"
    transicao_LD ("q_1", 'a') = "q_5"
    transicao_LD ("q_1", 'b') = "q_trap"
    transicao_LD ("q_2", 'a') = "q_5"
    transicao_LD ("q_2", 'b') = "q_3"
    transicao_LD ("q_3", 'a') = "q_4"
    transicao_LD ("q_3", 'b') = "q_0"
    transicao_LD ("q_4", 'a') = "q_1"
    transicao_LD ("q_4", 'b') = "q_trap"
    transicao_LD ("q_5", 'a') = "q_4"
    transicao_LD ("q_5", 'b') = "q_trap"
    transicao_LD ("q_trap", _) = "q_trap"
    transicao_LD _ = error "Transicao invalida."

-- LE = { w ⋹ {0, 1}* | 1*0*1
automato_LE :: Automato String
automato_LE =
    mkAutomato
        (mkAlfabeto ['0', '1'])
        (mkEstados ["q_0", "q_1", "q_trap"])
        (mkTransicao transicao_LE)
        (mkEstadoInicial "q_0")
        (mkEstadosFinais ["q_2"])
  where
    transicao_LE ("q_0", '0') = "q_trap"
    transicao_LE ("q_0", '1') = "q_1"
    transicao_LE ("q_1", '0') = "q_3"
    transicao_LE ("q_1", '1') = "q_2"
    transicao_LE ("q_2", '0') = "q_trap"
    transicao_LE ("q_2", '1') = "q_trap"
    transicao_LE ("q_3", '0') = "q_1"
    transicao_LE ("q_3", '1') = "q_2"
    transicao_LE ("q_trap", _) = "q_trap"
    transicao_LE _ = error "Transicao invalida."
