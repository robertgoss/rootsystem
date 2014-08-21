module DoubleQuotient where

import Data.Set as Set
import Weyl

doubleQuotient :: (SubWeylGroup2 w,WeylGroupElement g) => w -> [g]
doubleQuotient w = undefined
    where superElems = fromList . elements $ dualSuperGroup w
          sub1Elems = fromList . elements $ subGroup1 w
          sub2Elems = fromList . elements $ subGroup2 w
          quotient1 = quotient1' superElems
          quotient1' remaining | Set.null remaining = []
                               | otherwise = rep : quotient1' diff
              where rep = findMin remaining
                    repClass = Set.map (multiply rep) sub1Elems
                    diff = difference remaining repClass