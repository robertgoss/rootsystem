module SemiSimple where

data SemiSimple = Torus Int
                | A Int
                | B Int
                | C Int
                | D Int
                | G2 | F4 | E6 | E7 | E8
                | Product [SemiSimple]

