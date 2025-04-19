module IMP.Parser where

import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token as Tok

import IMP.Syntax

parseConstruct :: Parser Construct
parseConstruct =
    choice
        [ try $ Statement <$> parseStm
        , try $ Arithm <$> parseAexp
        , Bool <$> parseBexp
        ]

parseInput :: String -> String -> Either ParseError Construct
parseInput = parse (whitespace *> parseConstruct <* eof)

parseProgram :: String -> String -> Either ParseError Stm
parseProgram = parse (whitespace *> parseStm <* eof)

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

parseStm :: Parser Stm
parseStm = buildExpressionParser table parseSeq
    where
        table =
            [ [Infix (reservedOp "||" >> return NonDet) AssocLeft]
            , [Infix (reserved "par" >> return Par) AssocLeft]
            ]

parseSeq :: Parser Stm
parseSeq = do
    singles <- sepBy1 parseSingle semi
    return $ foldr1 Seq singles

parseSingle :: Parser Stm
parseSingle =
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

parseVarDef :: Parser Stm
parseVarDef = try $ do
    x <- identifier
    reservedOp ":="
    e <- parseAexp
    return $ Def x e

parseIfElse :: Parser Stm
parseIfElse = do
    reserved "if"
    b <- parseBexp
    reserved "then"
    s <- parseStm
    s' <- option Skip (reserved "else" >> parseStm)
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

parsePar :: Parser Stm
parsePar = do
    s1 <- parseStm
    reserved "par"
    s2 <- parseStm
    return $ Par s1 s2

parseNonDet :: Parser Stm
parseNonDet = do
    s1 <- parseStm
    reservedOp "||"
    s2 <- parseStm
    return $ NonDet s1 s2

parseProcDef :: Parser Stm
parseProcDef = do
    reserved "procedure"
    name <- identifier
    (params, rets) <- parseParamsRets
    reserved "begin"
    body <- parseStm
    reserved "end"
    return $ ProcDef name params rets body

parseProcInvoc :: Parser Stm
parseProcInvoc = try $ do
    name <- identifier
    (args, rets) <- parseArgsRets
    return $ ProcInvoc name args rets

parseParamsRets :: Parser ([Var], [Var])
parseParamsRets = parens $ do
    params <- sepBy identifier (symbol ",")
    reservedOp ";"
    rets <- sepBy identifier (symbol ",")
    return (params, rets)

parseArgsRets :: Parser ([Aexp], [Var])
parseArgsRets = parens $ do
    args <- sepBy parseAexp (symbol ",")
    reservedOp ";"
    rets <- sepBy identifier (symbol ",")
    return (args, rets)

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
    where
        ops = ["+", "-", "*", ":=", "=", "#", "<", "<=", ">", ">=", "(", ")", ";", "not", "and", "or", "||", ","]
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
            ]
        style =
            emptyDef
                { Tok.commentLine = "--"
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
