module IMP.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

import IMP.Syntax

wrap :: String -> String
wrap = ('<' :) . (++ ">") -- this is why haskell is awesome

-- input
parseInput :: String -> String -> Either ParseError Construct
parseInput channel = parse (whitespace *> parseConstruct <* eof) $ wrap channel

-- program
parseProgram :: String -> String -> Either ParseError Stm
parseProgram channel = parse (whitespace *> parseStm <* eof) $ wrap channel

-- construct
parseConstruct :: Parser Construct
parseConstruct =
    choice . map try $
        [ Statement <$> parseStm
        , Bool <$> parseBexp
        , Arithm <$> parseAexp
        , Whitespace <$ whitespace
        ]

-- statement
parseStm :: Parser Stm
parseStm = buildExpressionParser table term
    where
        table =
            [ [Infix (NonDet <$ operator "||") AssocLeft]
            , [Infix (Par <$ reserved "par") AssocLeft]
            , [Infix (Seq <$ semi) AssocLeft]
            ]
        term =
            choice . map try $
                [ parens parseStm
                , parseSkip
                , parsePrint
                , parseVarDef
                , parseIf
                , parseWhile
                , parseLocal
                , parseProcDef
                , parseProcInvoc
                , parseRepeat
                , parseFor
                , parseDoTimes
                ]

parseSkip :: Parser Stm
parseSkip = Skip <$ reserved "skip"

parsePrint :: Parser Stm
parsePrint = Print <$ reserved "print" <*> parseAexp

parsePlaceholder :: Parser Ident
parsePlaceholder = "_" <$ reserved "_"

parseVar :: Parser Ident
parseVar = parsePlaceholder <|> identifier

parseVarDef :: Parser Stm
parseVarDef =
    (\x f e -> VarDef x $ f (Variable x) e)
        <$> parseVar
        <*> parseDefOp
        <*> parseAexp

parseDefOp :: Parser (Aexp -> Aexp -> Aexp)
parseDefOp =
    choice
        [ flip const <$ operator ":=" -- \_ e -> e
        , Bin Add <$ operator "+="
        , Bin Sub <$ operator "-="
        , Bin Mul <$ operator "*="
        ]

parseIf :: Parser Stm
parseIf =
    If
        <$ reserved "if"
        <*> parseBexp
        <* reserved "then"
        <*> parseStm
        <*> parseElse

parseElse :: Parser Stm
parseElse =
    choice . map try $
        [ reserved "end" *> pure Skip -- end
        , reserved "else" *> parseStm <* reserved "end" -- else .. end
        , reserved "else" *> parseIf -- else if .. end
        ]

parseWhile :: Parser Stm
parseWhile =
    While
        <$ reserved "while"
        <*> parseBexp
        <* reserved "do"
        <*> parseStm
        <* reserved "end"

parseLocal :: Parser Stm
parseLocal =
    Local
        <$ reserved "var"
        <*> parseVar
        <* parseDefOp
        <*> parseAexp
        <* reserved "in"
        <*> parseStm
        <* reserved "end"

parseProcDef :: Parser Stm
parseProcDef =
    ProcDef
        <$ reserved "procedure"
        <*> identifier
        <*> parseParamsRets
        <* reserved "begin"
        <*> parseStm
        <* reserved "end"

parseProcInvoc :: Parser Stm
parseProcInvoc =
    ProcInvoc
        <$> identifier
        <*> parseArgsRets

parseParamsRets :: Parser ([Ident], [Ident])
parseParamsRets =
    parens $
        (,)
            <$> sepBy identifier (symbol ",")
            <* operator ";"
            <*> sepBy identifier (symbol ",")

parseArgsRets :: Parser ([Aexp], [Ident])
parseArgsRets =
    parens $
        (,)
            <$> sepBy parseAexp (symbol ",")
            <* operator ";"
            <*> sepBy parseVar (symbol ",") -- allow placeholders in returns

parseRepeat :: Parser Stm
parseRepeat =
    (\s b -> Seq s $ While (Not b) s)
        <$ reserved "repeat"
        <*> parseStm
        <* reserved "until"
        <*> parseBexp

parseFor :: Parser Stm
parseFor =
    -- stop condition is evaluated every iteration
    (\x e1 e2 s -> Local x e1 $ While (Rel Lt (Variable x) e2) (Seq s (inc x)))
        <$ reserved "for"
        <*> identifier
        <* parseDefOp
        <*> parseAexp
        <* reserved "to"
        <*> parseAexp
        <* reserved "do"
        <*> parseStm
        <* reserved "end"

parseDoTimes :: Parser Stm
parseDoTimes =
    -- reserved keyword as counter variable prevents modification from body
    (\e s -> Local "times" e $ While (Rel Gt (Variable "times") (Numeral 0)) (Seq s (dec "times")))
        <$ reserved "do"
        <*> parseAexp
        <* reserved "times"
        <*> parseStm

-- arithmetic expression
parseAexp :: Parser Aexp
parseAexp = buildExpressionParser table term
    where
        table =
            [ [Infix (Bin Mul <$ operator "*") AssocLeft]
            ,
                [ Infix (Bin Add <$ operator "+") AssocLeft
                , Infix (Bin Sub <$ operator "-") AssocLeft
                ]
            ]
        term =
            choice
                [ parens parseAexp
                , Numeral <$> integer
                , Variable <$> identifier
                , Time <$ reserved "time" <*> parseStm
                ]

-- boolean expression
parseBexp :: Parser Bexp
parseBexp = buildExpressionParser table term
    where
        table =
            [ [Prefix (Not <$ reserved "not")]
            , [Infix (And <$ reserved "and") AssocLeft]
            , [Infix (Or <$ reserved "or") AssocLeft]
            ]
        term =
            choice
                [ parens parseBexp
                , parseRel
                , Boolean True <$ reserved "true"
                , Boolean False <$ reserved "false"
                ]

parseRel :: Parser Bexp
parseRel = flip Rel <$> parseAexp <*> parseRop <*> parseAexp -- constructor parameters differ from parse order

parseRop :: Parser Rop
parseRop =
    choice
        [ Eq <$ operator "="
        , Neq <$ operator "#"
        , Leq <$ operator "<="
        , Lt <$ operator "<"
        , Geq <$ operator ">="
        , Gt <$ operator ">"
        ]

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        ops =
            [ "+"
            , "-"
            , "*"
            , ":="
            , "="
            , "#"
            , "<"
            , "<="
            , ">"
            , ">="
            , "("
            , ")"
            , ";"
            , "not"
            , "and"
            , "or"
            , "||"
            , ","
            , "+="
            , "-="
            , "*="
            ]
        keywords =
            [ "if"
            , "then"
            , "else"
            , "end"
            , "while"
            , "do"
            , "skip"
            , "print"
            , "var"
            , "in"
            , "procedure"
            , "begin"
            , "par"
            , "true"
            , "false"
            , "_"
            , "for"
            , "to"
            , "repeat"
            , "until"
            , "time"
            ]
        style =
            emptyDef
                { Tok.commentLine = "//"
                , Tok.commentStart = "/*"
                , Tok.commentEnd = "*/"
                , Tok.reservedOpNames = ops
                , Tok.reservedNames = keywords
                , Tok.identStart = letter
                , Tok.identLetter = alphaNum
                }

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

operator :: String -> Parser ()
operator = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

integer :: Parser Val
integer = Tok.integer lexer

semi :: Parser String
semi = Tok.semi lexer

whitespace :: Parser ()
whitespace = Tok.whiteSpace lexer

symbol :: String -> Parser String
symbol = Tok.symbol lexer
