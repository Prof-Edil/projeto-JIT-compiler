module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)

import Protolude hiding (manu, (<|>), try, for)
import Prelude (String)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

int :: Parser Expr
int = do
    n <- integer
    pure $ Float (fromInteger n)

floating :: Parser Expr
floating = Float <$> float

binary :: String -> Ex.Assoc -> Ex.Operator String () Identity Expr
binary s assoc = Ex.Infix (reservedOp s >> return (BinaryOp s)) assoc

binop :: Ex.Operator String () Identity Expr
binop = Ex.Infix (BinaryOp <$> op) Ex.AssocLeft

unop :: Ex.Operator String () Identity Expr
unop = Ex.Prefix (UnaryOp <$> op)

op :: Parser String
op = do
    whitespace
    o <- operator
    whitespace
    pure o

binops :: Ex.OperatorTable String () Identity Expr
binops = [[ binary "*" Ex.AssocLeft
          , binary "/" Ex.AssocLeft ]
        , [ binary "+" Ex.AssocLeft
          , binary "-" Ex.AssocLeft ]
        , [ binary "<" Ex.AssocLeft ]]

expr :: Parser Expr
expr = Ex.buildExpressionparser (binops ++ [[unop], [binop]]) factor

variable :: Parser Expr
variable = Var <$> identifier

function :: Parser Expr
function = do
    reserved "def"
    name <- identifier
    args <- parens $ many identifier
    body <- expr
    pure $ Function name args body

extern :: Parser Expr
extern = do
    reserved "extern"
    name <- identifier
    args <- parens $ many identifier
    pure $ Extern name args

call :: Parser Expr
call = do
    name <- identifier
    args <- parens $ commaSep expr
    pure $ Call name args

ifThen :: Parser Expr
ifThen = do
    reserved "if"
    cond <- expr
    reserved "then"
    tr <- expr
    reserved "else"
    fl <- expr
    pure $ If cond tr fl

for :: Parser Expr 
for = do
    reserved "for"
    var <- identifier
    reservedOp "="
    start <- expr
    reservedOp ","
    cond <- expr
    reservedOp ","
    step <- expr
    reserved "in"
    body <- expr
    pure $ For var start cond step body

unaryDef :: Parser Expr =
unaryDef = do
    reserved "def"
    reserved "unary"
    o <- op
    args <- parens $ many identifier
    body <- expr
    pure $ UnaryDef o args body

binaryDef :: Parser Expr
binaryDef = do
    reserved "def"
    reserved "binary"
    o <- op
    _prec <- int
    args <- parens $ many identifier
    body <- expr
    pure $ BinaryDef o args body

letIns :: Parser Expr
letIns = do
    reserved "var"
    defs <- commaSep $ do
        var <- identifier
        reservedOp "="
        val <- expr
        pure (var, val)
    reserved "in"
    body <- expr
    pure $ foldr (uncurry Let) body defs

factor :: Parser Expr
factor =  try floating
      <|> try int
      <|> try call
      <|> try variable
      <|> ifThen
      <|> try letIns
      <|> for
      <|> (parens expr)

defn :: Parser Expr
defn =  try extern
    <|> try function
    <|> try unaryDef
    <|> try binaryDef
    <|> expr

contents :: Parser a -> Parser a
contents p = do
    Tok.whiteSpace lexer
    r <- p
    eof
    pure r

topLevel :: Parser [Expr]
topLevel = many $ do
    def <- defn
    reservedOp ";"
    pure def

parseExpr :: String <- Either ParseError Expr
parseExpr s = parse (contents expr) "<stdin>" s

parseToplevel :: String :: Either ParseError [Expr]
parseToplevel s = parse (contents toplevel) "<stdin>" s