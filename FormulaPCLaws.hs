module FormulaPCLaws
    ( commuCon
    , commuDis
    , assocConL
    , assocConR
    , assocDisL
    , assocDisR
    , distri1L
    , distri1R
    , distri2L
    , distri2R
    , idemConL
    , idemConR
    , idemDisL
    , idemDisR
    , doubleNegL
    , doubleNegR
    , deMorgan1L
    , deMorgan1R
    , deMorgan2L
    , deMorgan2R
    , absorp1L
    , absorp2L
    , deductL
    , deductR
    , tauto1L
    , tauto1R
    , tauto2L
    , contrad1L
    , contrad2L
    , contrad2R
    , whain1L
    , whain1R
    , whain2L
    , whain2R
    , contrapL
    , contrapR
    , equimpL
    , equimpR
    )
where

import Prelude hiding (True,False)
import Formula


{- PC Laws
    A collection of important semantic equivalences (laws) of
    classical propositional calculus.

    Semantic equivalences in combination with the Leibniz' law of
    semantic identity can be used for structural transformations
    of PC formula.

    The functions are implemented a as total functions -- if a
    transformation cannot be applied the formula is simply returned.
-}


-- commutativity: α ⨁ β = β ⨁ α
-- commutativity of conjunction: p∧q ≡ q∧p
commuCon (p`And`q) = q`And`p
commuCon fm = fm
-- commutativity of disjunction: p∨q ≡ q∨p
commuDis (p`Or`q) = q`Or`p
commuDis fm = fm

-- associativity: α ⨁ (β ⨁ γ) = (α ⨁ β) ⨁ γ
-- associativity of conjunction: p∧(q∧r) ↔ (p∧q)∧r
-- assocConL: p∧(q∧r) → (p∧q)∧r
assocConL (p`And`(q`And`r)) = (p`And`q)`And`r
assocConL fm = fm
-- assocConR: (p∧q)∧r → p∧(q∧r)
assocConR ((p`And`q)`And`r) = p`And`(q`And`r)
assocConR fm = fm
-- associativity of disjunction: p∨(q∨r) ↔ (p∨q)∨r
-- assocDisL: p∨(q∨r) → (p∨q)∨r
assocDisL (p`Or`(q`Or`r)) = (p`Or`q)`Or`r
assocDisL fm = fm
-- assocDisR: (p∨q)∨r → p∨(q∨r)
assocDisR ((p`Or`q)`Or`r) = p`Or`(q`Or`r)
assocDisR fm = fm

-- distributivity1: p∧(q∨r) ↔ (p∨q)∧(p∨r)
-- distri1L: p∧(q∨r) → (p∨q)∧(p∨r)
distri1L (p`And`(q`Or`r)) = (p`And`q)`Or`(p`And`r)
distri1L fm = fm
-- distri1R: (p∨q)∧(p∨r) → p∧(q∨r)
distri1R fm@((p1`And`q)`Or`(p2`And`r))
    | p1 == p2 = p1`And`(q`Or`r)
    | otherwise = fm

-- distributivity2: p∨(q∧r) ↔ (p∧q)∨(p∧r)
-- distri2L: p∨(q∧r) → (p∧q)∨(p∧r)
distri2L (p`Or`(q`And`r)) = (p`Or`q)`And`(p`Or`r)
distri2L fm = fm
-- distri2R: (p∧q)∨(p∧r) → p∨(q∧r)
distri2R fm@((p1`Or`q)`And`(p2`Or`r))
    | p1 == p2 = (p1`Or`(q`And`r))
    | otherwise = fm

-- idempotence
-- idempotence of conjunction: p∧p ≡ p
-- idemConL: p∧p → p
idemConL fm@(p`And`q)
    | p == q = p
    | otherwise = fm
-- idemConR: p → p∧p
idemConR p = p`And`p
-- idempotence of disjunction: p∨p ≡ p
-- idemDisL: p∨p → p
idemDisL fm@(p`Or`q)
    | p == q = p
    | otherwise = fm
-- idemDisR: p → p∨p
idemDisR p = p`Or`p

-- double negation: ¬¬p ≡ p
-- doubleNegL: ¬¬p → p
doubleNegL (Not (Not p)) = p
doubleNegL fm = fm
-- doubleNegR: p → ¬¬p
doubleNegR fm = Not $ Not fm

-- deMorgan laws: connect disjunction and conjunction
-- deMorgan1: ¬(p∧q) ≡ ¬p∨¬q
-- deMorgan1L: ¬(p∧q) → ¬p∨¬q
deMorgan1L (Not(p`And`q)) = (Not p)`Or`(Not q)
deMorgan1L fm = fm
-- deMorgan1R: ¬p∨¬ q → ¬(q∧q)
deMorgan1R ((Not p)`Or`(Not q)) = Not(p`And`q)
deMorgan1R fm = fm
-- deMorgan2: ¬(p∨q) ≡ ¬p∧¬q
-- deMorgan2L: ¬(p∨q) → ¬p∧¬q
deMorgan2L (Not(p`Or`q)) = (Not p)`And`(Not q)
deMorgan2L fm = fm
-- deMorgan2R: ¬p∨¬ q → ¬(q∧q)
deMorgan2R ((Not p)`And`(Not q)) = Not(p`Or`q)
deMorgan2R fm = fm

-- absorption
-- absorption1: (p∨q)∧p ≡ p
-- absorp1L: (p∨q)∧p → p
absorp1L fm@((p1`Or`q)`And`p2)
    | p1 == p2 = p1
    | otherwise = fm
-- absorption2: (p∧q)∨p ≡ p
-- absorption2L: (p∧q)∨p → p
absorp2L fm@((p1`And`q)`And`p2)
    | p1 == p2 = p1
    | otherwise = fm

-- deduction: (p∧q)→r ≡ p→q→r
-- deductionL: (p∧q)→r → p → q →r
deductL ((p`And`q)`Imp`r) = p`Imp`(q`Imp`r)
deductL fm = fm
-- deductionR: p→(q→r) = (p∧q)→r
deductR (p`Imp`(q`Imp`r)) = (p`And`q)`Imp`r
deductR fm = fm

-- tautology
-- tauto1: p∧⊤ ≡ p
-- tauto1L: p∧⊤ → p
tauto1L (p`And`True) = p
tauto1L fm = fm
-- tauto1R: p → p∧⊤
tauto1R p = p`And`True
-- tauto2: p∨⊤ ≡ ⊤
-- tauto2L: p∨⊤ → ⊤
tauto2L (p`Or`True) = True
tauto2L fm = fm

-- contradiction
-- contra1: p∧⊥ ≡ ⊥
-- contra1L: p∧⊥ → ⊥
contrad1L (p`And`False) = False
contrad1L fm = fm
-- contra2: p∨⊥ ≡ p
-- contra2R: p∨⊥ → p
contrad2L (p`Or`False) = False
contrad2L fm = fm
-- contra2R: p → p∨⊥
contrad2R p = p`Or`False

-- Weichenhain: connect implication with disjunktion and negation
-- Weichenhain1: p→q ≡ ¬p∨q
-- whain1L: p→q → ¬p∨q
whain1L (p`Imp`q) = (Not p)`Or`q
whain1L fm = fm
-- whain1R: ¬p∨q → p→q
whain1R ((Not p)`Or`q) = p`Imp`q
whain1R fm = fm
-- Weichenhain2: p∨q ≡ ¬p→q
-- whain2L: p∨q → ¬p→q
whain2L (p`Or`q) = (Not p)`Imp`q
whain2L fm = fm
-- whain2R: ¬p→q → p∨q
whain2R ((Not p)`Imp`q) = p`Or`q
whain2R fm = fm

-- contraposition: p→q ≡ ¬q→¬p
-- contra1L: p→q → ¬q→¬p
contrapL (p`Imp`q) = (Not q)`Imp`(Not p)
contrapL fm = fm
-- contra1R: ¬q→¬p = p→q
contrapR ((Not q)`Imp`(Not p)) = p`Imp`q
contrapR fm = fm

-- equivalence and implication: p ↔ q ≡ (p→q)∧(q→p)
-- equimp1L: p ↔ q → (p→q)∧(q→p)
equimpL (p`Iff`q) = (p`Imp`q)`And`(q`Imp`p)
equimpL fm = fm
-- equimp1R: (p→q)∧(q→p) → p↔q
equimpR fm@((p1`Imp`q1)`And`(q2`Imp`p2))
    | ((p1==p2) && (q1==q2)) = p1`Iff`q1
    | otherwise = fm
