module IMP.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

import IMP.Syntax

wrap :: String -> String
wrap = ('<' :) . (++ ">") -- this is why haskell is awesome

parseInput :: String -> String -> Either ParseError Construct
parseInput channel = parse (whitespace *> parseConstruct <* eof) $ wrap channel

parseProgram :: String -> String -> Either ParseError Stm
parseProgram channel = parse (whitespace *> parseStm <* eof) $ wrap channel

parseConstruct :: Parser Construct
parseConstruct =
    choice . map try $
        [ Statement <$> parseStm
        , Bool <$> parseBexp
        , Arithm <$> parseAexp
        ]

parseStm :: Parser Stm
parseStm = buildExpressionParser table term
    where
        table =
            [ [Infix (reservedOp "||" >> return NonDet) AssocLeft]
            , [Infix (reserved "par" >> return Par) AssocLeft]
            , [Infix (semi >> return Seq) AssocLeft]
            ]
        term =
            choice . map try $
                [ parseSkip
                , parsePrint
                , parseVarDef
                , parseIfElse
                , parseWhile
                , parseLocal
                , parseProcDef
                , parseProcInvoc
                , parens parseStm
                ]

parseSkip :: Parser Stm
parseSkip = reserved "skip" >> return Skip

parsePrint :: Parser Stm
parsePrint = reserved "print" >> Print <$> parseAexp

parsePlaceholder :: Parser Var
parsePlaceholder = reserved "_" >> return "_"

parseVar :: Parser Var
parseVar = parsePlaceholder <|> identifier

parseVarDef :: Parser Stm
parseVarDef = do
    x <- parseVar
    reservedOp ":="
    e <- parseAexp
    return $ VarDef x e

parseIfElse :: Parser Stm
parseIfElse = do
    reserved "if"
    b <- parseBexp
    reserved "then"
    s <- parseStm
    s' <- option Skip (reserved "else" >> parseStm) -- else branch is optional
    reserved "end"
    return $ If b s s'

parseWhile :: Parser Stm
parseWhile = do
    reserved "while"
    b <- parseBexp
    reserved "do"
    s <- parseStm
    reserved "end"
    return $ While b s

parseLocal :: Parser Stm
parseLocal = do
    reserved "var"
    x <- identifier
    reservedOp ":="
    e <- parseAexp
    reserved "in"
    s <- parseStm
    reserved "end"
    return $ Local x e s

parseProcDef :: Parser Stm
parseProcDef = do
    reserved "procedure"
    p <- identifier
    (ps, rs) <- parseParamsRets
    reserved "begin"
    s <- parseStm
    reserved "end"
    return $ ProcDef p ps rs s

parseProcInvoc :: Parser Stm
parseProcInvoc = do
    p <- identifier
    (as, rs) <- parseArgsRets
    return $ ProcInvoc p as rs

parseParamsRets :: Parser ([Var], [Var])
parseParamsRets = parens $ do
    ps <- sepBy identifier (symbol ",")
    reservedOp ";"
    rs <- sepBy identifier (symbol ",")
    return (ps, rs)

parseArgsRets :: Parser ([Aexp], [Var])
parseArgsRets = parens $ do
    as <- sepBy parseAexp (symbol ",")
    reservedOp ";"
    rs <- sepBy parseVar (symbol ",") -- allow placeholders in invocation's returns
    return (as, rs)

parseAexp :: Parser Aexp
parseAexp = buildExpressionParser table term
    where
        table =
            [ [Infix (reservedOp "*" >> return (Bin Mul)) AssocLeft]
            ,
                [ Infix (reservedOp "+" >> return (Bin Add)) AssocLeft
                , Infix (reservedOp "-" >> return (Bin Sub)) AssocLeft
                ]
            ]
        term =
            parens parseAexp
                <|> (Numeral <$> integer)
                <|> (Variable <$> identifier)

parseBexp :: Parser Bexp
parseBexp = buildExpressionParser table term
    where
        table =
            [ [Prefix (reserved "not" >> return Not)]
            , [Infix (reserved "and" >> return And) AssocLeft]
            , [Infix (reserved "or" >> return Or) AssocLeft]
            ]
        term =
            parens parseBexp
                <|> parseRel
                <|> (reserved "true" >> return (Boolean True))
                <|> (reserved "false" >> return (Boolean False))

parseRel :: Parser Bexp
parseRel = do
    e1 <- parseAexp
    rop <- parseRop
    e2 <- parseAexp
    return $ Rel rop e1 e2

parseRop :: Parser Rop
parseRop =
    (reservedOp "=" >> return Eq)
        <|> (reservedOp "#" >> return Neq)
        <|> (reservedOp "<=" >> return Leq)
        <|> (reservedOp "<" >> return Lt)
        <|> (reservedOp ">=" >> return Geq)
        <|> (reservedOp ">" >> return Gt)

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
            ]
        names =
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
            ]
        style =
            emptyDef
                { Tok.commentLine = "//"
                , Tok.commentStart = "/*"
                , Tok.commentEnd = "*/"
                , Tok.reservedOpNames = ops
                , Tok.reservedNames = names
                , Tok.identStart = letter
                , Tok.identLetter = alphaNum
                }

identifier = Tok.identifier lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
parens = Tok.parens lexer
integer = Tok.integer lexer
semi = Tok.semi lexer
whitespace = Tok.whiteSpace lexer
symbol = Tok.symbol lexer
