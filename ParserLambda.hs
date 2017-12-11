{-# OPTIONS_GHC -w #-}
module ParserLambda where
import Data.Char
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4

action_0 (5) = happyShift action_4
action_0 (6) = happyShift action_5
action_0 (8) = happyShift action_6
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (5) = happyShift action_2
action_1 _ = happyFail

action_2 (6) = happyShift action_11
action_2 _ = happyFail

action_3 (5) = happyShift action_4
action_3 (6) = happyShift action_5
action_3 (8) = happyShift action_6
action_3 (10) = happyAccept
action_3 (4) = happyGoto action_10
action_3 _ = happyFail

action_4 (6) = happyShift action_9
action_4 _ = happyFail

action_5 _ = happyReduce_7

action_6 (5) = happyShift action_8
action_6 (6) = happyShift action_5
action_6 (8) = happyShift action_6
action_6 (4) = happyGoto action_7
action_6 _ = happyFail

action_7 (5) = happyShift action_4
action_7 (6) = happyShift action_5
action_7 (8) = happyShift action_6
action_7 (9) = happyShift action_16
action_7 (4) = happyGoto action_15
action_7 _ = happyFail

action_8 (6) = happyShift action_14
action_8 _ = happyFail

action_9 (7) = happyShift action_13
action_9 _ = happyFail

action_10 (5) = happyShift action_4
action_10 (6) = happyShift action_5
action_10 (8) = happyShift action_6
action_10 (4) = happyGoto action_10
action_10 _ = happyReduce_4

action_11 (7) = happyShift action_12
action_11 _ = happyFail

action_12 (5) = happyShift action_4
action_12 (6) = happyShift action_5
action_12 (8) = happyShift action_6
action_12 (4) = happyGoto action_20
action_12 _ = happyFail

action_13 (5) = happyShift action_4
action_13 (6) = happyShift action_5
action_13 (8) = happyShift action_21
action_13 (4) = happyGoto action_20
action_13 _ = happyFail

action_14 (7) = happyShift action_19
action_14 _ = happyFail

action_15 (5) = happyShift action_4
action_15 (6) = happyShift action_5
action_15 (8) = happyShift action_6
action_15 (9) = happyShift action_18
action_15 (4) = happyGoto action_10
action_15 _ = happyFail

action_16 (8) = happyShift action_17
action_16 _ = happyFail

action_17 (5) = happyShift action_4
action_17 (6) = happyShift action_5
action_17 (8) = happyShift action_6
action_17 (4) = happyGoto action_24
action_17 _ = happyFail

action_18 _ = happyReduce_5

action_19 (5) = happyShift action_4
action_19 (6) = happyShift action_5
action_19 (8) = happyShift action_23
action_19 (4) = happyGoto action_20
action_19 _ = happyFail

action_20 (5) = happyShift action_4
action_20 (6) = happyShift action_5
action_20 (8) = happyShift action_6
action_20 (4) = happyGoto action_10
action_20 _ = happyReduce_1

action_21 (5) = happyShift action_8
action_21 (6) = happyShift action_5
action_21 (8) = happyShift action_6
action_21 (4) = happyGoto action_22
action_21 _ = happyFail

action_22 (5) = happyShift action_4
action_22 (6) = happyShift action_5
action_22 (8) = happyShift action_6
action_22 (9) = happyShift action_27
action_22 (4) = happyGoto action_15
action_22 _ = happyFail

action_23 (5) = happyShift action_8
action_23 (6) = happyShift action_5
action_23 (8) = happyShift action_6
action_23 (4) = happyGoto action_26
action_23 _ = happyFail

action_24 (5) = happyShift action_4
action_24 (6) = happyShift action_5
action_24 (8) = happyShift action_6
action_24 (9) = happyShift action_25
action_24 (4) = happyGoto action_10
action_24 _ = happyFail

action_25 _ = happyReduce_6

action_26 (5) = happyShift action_4
action_26 (6) = happyShift action_5
action_26 (8) = happyShift action_6
action_26 (9) = happyShift action_28
action_26 (4) = happyGoto action_15
action_26 _ = happyFail

action_27 (8) = happyShift action_17
action_27 _ = happyReduce_2

action_28 (8) = happyShift action_17
action_28 (9) = happyShift action_29
action_28 _ = happyReduce_2

action_29 _ = happyReduce_3

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Abs happy_var_2 ( happy_var_4 )
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Abs happy_var_2 ( happy_var_5 )
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 8 4 happyReduction_3
happyReduction_3 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (( Abs happy_var_3 ( happy_var_6 ) )
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (( App happy_var_1 happy_var_2 )
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 4 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (( App happy_var_2 happy_var_3 )
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 4 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (( App happy_var_2 happy_var_5 )
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  4 happyReduction_7
happyReduction_7 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn4
		 (Var happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 10 10 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLam -> cont 5;
	TokenVar happy_dollar_dollar -> cont 6;
	TokenPoint -> cont 7;
	TokenOB -> cont 8;
	TokenCB -> cont 9;
	_ -> happyError' (tk:tks)
	}

happyError_ 10 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parserlamb tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError b = error "Parse Error"

data NTlam
		 = NVar Int
         | NAbs Char NTlam
         | NApp NTlam NTlam
    deriving (Eq,Show)

data Tlam
		= Var Char
        | Abs Char Tlam
        | App Tlam Tlam
    deriving (Eq,Show)

data Token
		= TokenVar Char
		| TokenPoint
		| TokenOB
		| TokenCB
		| TokenLam
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
	| areEqual (c:(take 2 cs)) "lam" = TokenLam : lexer (tail(tail(tail(cs))))
	| isSpace c = lexer cs
	| isAlpha c = TokenVar c : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer ('.':cs) = TokenPoint : lexer cs

areEqual :: String -> String -> Bool
areEqual a b = a == b
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
