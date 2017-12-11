module Lam where
import ParserLambda

-- sintaxe abstrata do calculo lambda

-- Tipo NTlam: notação nameless
--data NTlam = NVar Int
--           | NAbs Char NTlam
--           | NApp NTlam NTlam
--    deriving (Eq,Show)

--data Tlam = Var Char
--          | Abs Char Tlam
--          | App Tlam Tlam
--          deriving (Eq,Show)

type Gamma = [(Char,Int)] -- Contexto das variáveis livres

-- Funcao que recebe um Tlam e retorna seu correspondente na notação nameless
removeNames :: Tlam -> Gamma -> NTlam
removeNames (Var x) g       = if(elem x (map fst g))
                              then NVar (snd (head ((filter (\(a,_) -> a == x) (reverse g)))))
                              else NVar (length(g))
removeNames (Abs c t) g     = NAbs '.' (let g' =  reverse((c, 0):(map (\(x,a) -> if(a>=0) then (x,a+1) else (x,a)) (reverse g)))
                              in (removeNames t g'))
removeNames (App t1 t2) g   = NApp (removeNames t1 g) (removeNames t2 g)

--Funcao que recebe o termo na notação nameless e recupera notação original
restoreNames :: NTlam -> Gamma -> Tlam
restoreNames (NVar x) g = Var (fst (head (filter (\(a,y) -> x == y) (reverse g))))
restoreNames (NAbs x t) g   = let x' = findVar g ['a','b'..'z']
                                  g' = reverse ((x',0): (map (\(x,a) -> if(a>=0) then (x,a+1) else (x,a)) (reverse g)))
                              in Abs x' (restoreNames t g')
restoreNames (NApp t1 t2) g = App (restoreNames t1 g) (restoreNames t2 g)

findVar :: Gamma -> [Char] -> Char
findVar x (l:ls) = let x' = (map fst x)
                   in if (elem l x')
                          then findVar x ls
                      else l

-- interpretador do termo lambda
interpret :: Tlam ->  Gamma -> Tlam
interpret t g = let t1 = removeNames t g
                    t2 = maxEval t1
                    t3 = restoreNames t2 g
                in t3

-- Função que busca o termo mais reduzido utilizando a estrategia call-by-value
maxEval :: NTlam -> NTlam
maxEval t = let t' = evalN (t)
            in
                if (t' == t) then t'
                else maxEval t'

--Função que verifica o termo nameless
isVal :: NTlam -> Bool
isVal (NVar x)       = True
isVal (NAbs x t)     = True
isVal _              = False

-- Termo Nameless
-- d indica o numero de unidades a se incrementar
-- c indica o numero de abstracoes externas
shifting :: NTlam -> Int -> Int -> NTlam
shifting (NVar k) d c     = if (k < c) then NVar k else (NVar (k+d))
shifting (NAbs x t1) d c  = NAbs x (shifting t1 d (c+1))
shifting (NApp t1 t2) d c = NApp (shifting t1 d c) (shifting t2 d c)

-- j : índice a ser substituído
-- s : termo quem substitui o índice
-- k : termo onde vai ser aplicada a substituição
subsN :: Int -> NTlam -> NTlam -> NTlam
subsN j s (NVar k)     = if (k == j) then s else (NVar k)
subsN j s (NAbs x t1)  = NAbs '.' (subsN (j+1) (shifting s 1 0) t1)                                                                        -- Substitui (j+1) pelo shifting de uma unidade em t1
subsN j s (NApp t1 t2) = NApp (subsN j s t1) (subsN j s t2)

-- avaliação segundo as regras do call-by-value
evalN :: NTlam -> NTlam
evalN (NVar x)              = NVar x
evalN (NAbs x t)            = NAbs x t
evalN (NApp (NAbs x t1) v2) = if (isVal(v2))                  --E-APP-ABS
                                  then shifting (subsN 0 (shifting v2 1 0) t1) (-1) 0
                              else
                                  let v2' = evalN v2
                                  in (NApp (NAbs x t1) v2')
evalN (NApp t1 v2)          = if(not(isVal(t1)))              --E-APP-1
                                  then
                                      let t1' = evalN(t1)
                                      in (NApp t1' v2)
                              else                            --E-APP-2
                                  let v2' = evalN(v2)
                                  in (NApp t1 v2')

-- main = getContents >>= print . interpret . parserlamb . lexer
