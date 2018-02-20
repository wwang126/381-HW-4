module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- Wavelet Wang ONID:wangwav
-- David Okubo ONID:okubod
-- Sean Cramsey ONID:cramseys

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)
test (Facing c)
test (Clear d)
test Beeper
test Empty


-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt _ _ _ _ = undefined

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
