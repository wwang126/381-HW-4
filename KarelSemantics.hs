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
test (Not t) w r = not (test t w r)
test (Facing c) w r = getFacing r == c
test (Clear d) w (p, c, i) = isClear (neighbor (cardTurn d c) p) w
test Beeper w (p, _, _) = hasBeeper p w
test Empty _ r = isEmpty r


-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown   _ _ r = Done r
stmt PickBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move _ w r = let n = neighbor(getFacing r)(getPos r)
                  in if test(Clear Front) w r
                    then OK w (setPos n  r)
                    else Error ("Obstruction at: " ++ show n)

stmt PutBeeper _ w r = let p = getPos r
                        in if hasBeeper p w
                            then Error ("Beeper already present at:" ++ show p)
                            else if isEmpty r
                                then Error ("Beeper bag empty!")
                                else OK (incBeeper p w) (decBag r)
stmt (Turn d) _ w r = let c = getFacing r
                      in if c == cardTurn d c
                          then Error ("Already facing " ++ show c)
                          else OK w (setFacing (cardTurn d c) r)
stmt _ _ _ _ = undefined

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
