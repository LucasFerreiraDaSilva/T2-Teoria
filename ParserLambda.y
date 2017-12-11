{
module ParserLambda where
import Data.Char
}

%name parserlamb
%tokentype { Token }
%error { parseError }

%token
	lam { TokenLam }
	var { TokenVar $$ }
	'.' { TokenPoint }
	'(' { TokenOB }
	')' { TokenCB }

%%

-- regras de producao da gramatica

Tlam :  lam var '.' Tlam               { Abs $2 ( $4 ) }
         | lam var '.' '('Tlam')'         { Abs $2 ( $5 ) }
	   | '(' lam var'.' '('Tlam')' ')' { ( Abs $3 ( $6 ) ) }
	   | Tlam Tlam                  { ( App $1 $2 ) }
	   | '(' Tlam Tlam ')'          { ( App $2 $3 ) }
	   | '(' Tlam ')' '(' Tlam ')'  { ( App $2 $5 ) }
	   | var                                { Var $1 }

{

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

}
