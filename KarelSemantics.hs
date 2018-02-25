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
                        in if isEmpty r
                            then Error ("Error: No beeper to put.")
                            else OK (incBeeper p w) (decBag r)
stmt (Turn d) _ w r = let c = getFacing r
                      in if c == cardTurn d c
                          then Error ("Already facing " ++ show c)
                          else OK w (setFacing (cardTurn d c) r)

stmt (Block []) d w r = OK w r
stmt (Block (s:ns)) d w r = case stmt s d w r of
                          OK nw nr -> stmt (Block ns) d nw nr
                          otherwise -> otherwise

stmt (If t x y) d w r = case (test t w r) of
                          True -> stmt x d w r
                          False -> stmt y d w r
stmt (Call m) d w r = case lookup m d of
                    Nothing -> Error ("Error: Undefined macro: " ++ m)
                    Just s -> stmt s d w r

stmt _ _ _ _ = undefined

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
