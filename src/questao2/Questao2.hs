module Questao2 where


-- | Verificador de tautologia
-- data Var  = A | B | C | D | E | True | False
data Prep = Prep :->: Prep | Prep :&: Prep | Not Prep | A | B | C | D | E | T | F
type Subs = [(Prep, Bool)]

-- eval :: Subs -> Prep -> Prep