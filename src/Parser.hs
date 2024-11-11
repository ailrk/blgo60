{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use module export list" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# LANGUAGE OverloadedLabels #-}


module Parser where

import Data.Text qualified as Text
import Data.Set qualified as Set
import Text.Megaparsec (ParsecT, MonadParsec (try, getParserState), sepEndBy1, lookAhead, sepBy1, between, manyTill, sepBy, choice, manyTill_, anySingle, State (..))
import Text.Megaparsec.Char (letterChar, alphaNumChar, space1, char)
import Text.Megaparsec.Char.Lexer qualified as Lexer
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import AST
import Position (getPosition, Position)
import Symbol (Symbol, toSymbol, HasSymbolTable)
import UnliftIO (MonadUnliftIO)
import Prelude hiding (Type)
import Control.Lens ((%~), _1, _2, (^.), (?~))
import Data.Generics.Labels ()
import Text.Megaparsec.Debug (dbg)
import Text.Printf


-- Algol60 BNF
-- https://www.masswerk.at/algol60/algol60-syntaxversions.htm
-- http://www.bitsavers.org/pdf/sds/9xx/lang/900699C_Algol60_Ref_Nov66.pdf


----------------------------------------
-- basics
----------------------------------------


reservedWords :: [Text]
reservedWords =
  [ "begin"
  , "end"
  , "own"
  , "real"
  , "integer"
  , "bool"
  , "array"
  , "switch"
  , "procedure"
  , "value"
  , "string"
  , "label"
  , "goto"
  , "if"
  , "then"
  , "else"
  , "do"
  , "for"
  , "or"
  , "and"
  , "not"
  , "equiv"
  , "impl"
  , "step"
  , "while"
  , "comment"
  , "true"
  , "false"
  ]


type Parser m a = ParsecT Void Text m a

type CanParse ctx m = (HasSymbolTable ctx, MonadReader ctx m, MonadUnliftIO m)


lexeme :: Parser m a -> Parser m a
lexeme = Lexer.lexeme ignore


identifier :: CanParse ctx m => Parser m Symbol
identifier = (lexeme . try) (p >>= check) >>= lift . toSymbol
  where
    p = Text.pack <$> ((:) <$> (letterChar <|> char '_') <*> many (alphaNumChar <|> char '_'))
    check x =
     if x `elem` reservedWords
     then fail $ "keyword " ++ show x ++ " cannot be an identifier"
     else return x


-- keywords
reserved :: CanParse ctx m => Text -> Parser m ()
reserved w = symbol w *> pure ()


integer :: Parser m Expr
integer = IntExpr <$> lexeme Lexer.decimal <*> getPosition


float :: Parser m Expr
float = RealExpr <$> lexeme Lexer.float <*> getPosition


-- string literals
stringLiteral :: Parser m Expr
stringLiteral =
  StringExpr
  <$> Text.pack <$> (char '"' >> manyTill Lexer.charLiteral (char '"'))
  <*> getPosition


blockComment :: Parser m ()
blockComment = Lexer.skipBlockComment "comment" ";"


ignore :: Parser m ()
ignore = Lexer.space space1 empty blockComment


commaSep :: CanParse ctx m => Parser m a -> Parser m [a]
commaSep p = sepBy p (symbol ",")


commaSep1 :: CanParse ctx m => Parser m a -> Parser m [a]
commaSep1 p = sepBy1 p (symbol ",")


symbol :: CanParse ctx m => Text -> Parser m Symbol
symbol s = Lexer.symbol ignore s >>= lift . toSymbol


parens :: CanParse ctx m => Parser m a -> Parser m a
parens = between (symbol "(") (symbol ")")


brackets :: CanParse ctx m => Parser m a -> Parser m a
brackets = between (symbol "[") (symbol "]")


logicValue :: CanParse ctx m => Parser m Expr
logicValue = BoolExpr <$> c <*> getPosition
  where
    c = (symbol "true" *> pure True) <|> symbol "false" *> pure False


label :: CanParse ctx m => Parser m Symbol
label = do
  identifier


switchIdentifier :: CanParse ctx m => Parser m Symbol
switchIdentifier = identifier


arrayIdentifier :: CanParse ctx m => Parser m Symbol
arrayIdentifier = identifier


procedureIdentifier :: CanParse ctx m => Parser m Symbol
procedureIdentifier = identifier


ifClause :: CanParse ctx m => Parser m Expr
ifClause = symbol "if" *> booleanExpression <* symbol "then"


----------------------------------------
-- constituents of expressions
----------------------------------------


variable :: CanParse ctx m => Parser m Var
variable = do
  try subscriptedVariable
  <|> try functionDesignator
  <|> try simpleVariable


simpleVariable :: CanParse ctx m => Parser m Var
simpleVariable =
  SimpleVar <$> (SimpleVar_ <$> identifier <*> getPosition)


subscriptedVariable :: CanParse ctx m => Parser m Var
subscriptedVariable =
  SubscriptVar <$> (SubscriptVar_ <$> identifier <*> brackets subscriptList <*> getPosition)


subscriptList :: CanParse ctx m => Parser m [Expr]
subscriptList = commaSep1 subscriptExpression


subscriptExpression :: CanParse ctx m => Parser m Expr
subscriptExpression = arithmeticExpression


----------------------------------------
-- expressions
----------------------------------------


expression :: CanParse ctx m => Parser m Expr
expression = do
  try arithmeticExpression
  <|> try booleanExpression
  <|> try designationalExpression


arithmeticExpression :: CanParse ctx m => Parser m Expr
arithmeticExpression =
  try simpleArithmeticExpression
  <|> try (ifClause
          *> simpleArithmeticExpression
          *> symbol "else"
          *> arithmeticExpression)


binary :: CanParse ctx m => Text -> (a -> a -> a) -> Operator (ParsecT Void Text m) a
binary  name f = InfixL  (f <$ symbol name)


binaryR :: CanParse ctx m => Text -> (a -> a -> a) -> Operator (ParsecT Void Text m) a
binaryR  name f = InfixR  (f <$ symbol name)


prefix :: CanParse ctx m => Text -> (a -> a) -> Operator (ParsecT Void Text m) a
prefix  name f = Prefix  (f <$ symbol name)


simpleArithmeticExpression :: CanParse ctx m => Parser m Expr
simpleArithmeticExpression = do
  pos <- getPosition
  makeExprParser term
      [ [ binaryR "**" (BinopExpr ExpOp pos)]
      , [ prefix "+" (UnopExpr PositiveOp pos)
        , prefix "-" (UnopExpr NegativeOp pos)
        ]
      , [ binary "*" (BinopExpr TimesOp pos)
        , binary "//" (BinopExpr IntDivOp pos)
        , binary "/" (BinopExpr DivOp pos)
        ]
      , [ binary "+" (BinopExpr PlusOp pos)
        , binary "-" (BinopExpr MinusOp pos)
        ]
      ]
  where
    term = choice
      [ parens arithmeticExpression
      , try float
      , try integer
      , VarExpr <$> variable
      ]

booleanExpression :: CanParse ctx m => Parser m Expr
booleanExpression = do
  try simpleBooleanExpression
  <|> try (ifClause
        *> simpleBooleanExpression
        *> symbol "else"
        *> booleanExpression)


simpleBooleanExpression :: CanParse ctx m => Parser m Expr
simpleBooleanExpression = do
  pos <- getPosition
  makeExprParser term
    [ [ prefix "~" (UnopExpr NotOp pos) ]
     , [ binary "or" (BinopExpr OrOp pos) ]
     , [ binary "and" (BinopExpr AndOp pos) ]
     , [ binary "impl" (BinopExpr ImplOp pos) ]
     , [ binary "equiv" (BinopExpr EquivOp pos) ]
     ]

  where
    term =
      try logicValue
      <|> try relation
      <|> try (VarExpr <$> variable)
      <|> parens booleanExpression
    relation = do
      pos <- getPosition
      makeExprParser simpleArithmeticExpression
        [ [ binary "<=" (BinopExpr LeOp pos)
          , binary "~=" (BinopExpr NeqOp pos)
          , binary ">=" (BinopExpr GeOp pos)
          , binary ">" (BinopExpr GtOp pos)
          , binary "<" (BinopExpr LtOp pos)
          , binary "=" (BinopExpr EqOp pos)
          ]
        ]


designationalExpression :: CanParse ctx m => Parser m Expr
designationalExpression = do
  try simpleDesignationalExpression
  <|> try (ifClause
        *> simpleDesignationalExpression
        *> symbol "else"
        *> designationalExpression)


simpleDesignationalExpression :: CanParse ctx m => Parser m Expr
simpleDesignationalExpression = do
  try switchDesignator <|> try labeled <|> parens designationalExpression
  where
    labeled = LabelExpr <$> label <*> getPosition
    switchDesignator =
      SwitchExpr
      <$> switchIdentifier
      <*> brackets subscriptExpression
      <*> getPosition


actualParameter :: CanParse ctx m => Parser m Expr
actualParameter = do
  try stringLiteral <|> try expression


parameterDelimeter :: CanParse ctx m => Parser m ()
parameterDelimeter = do
  try (symbol ")" *> some (try letterChar) *> symbol ":(" *> pure ())
  <|> (symbol "," *> pure ())


actualParameterList :: CanParse ctx m => Parser m [Expr]
actualParameterList = do
  sepBy1 actualParameter parameterDelimeter


actualParameterPart :: CanParse ctx m => Parser m [Expr]
actualParameterPart = do
  parens actualParameterList


functionDesignator :: CanParse ctx m => Parser m Var
functionDesignator =
  FunctionDesignator <$> (FunctionDesignator_ <$> identifier <*> actualParameterPart <*> getPosition)


----------------------------------------
-- declarations
----------------------------------------


declaration :: CanParse ctx m => Parser m Dec
declaration = do
  try arrayDeclaration
  <|> try typeDeclaration
  <|> try switchDeclaration
  <|> procedureDeclaration


typeList :: CanParse ctx m => Parser m [Var]
typeList = commaSep1 simpleVariable


type_ :: CanParse ctx m => Parser m Type
type_ =
  try (symbol "real" *> pure RealT)
  <|> try (symbol "integer" *> pure IntegerT)
  <|> symbol "boolean" *> pure BooleanT


localOrOwn :: CanParse ctx m => Parser m Bool
localOrOwn = optional (symbol "own") <&> isJust


typeDeclaration :: CanParse ctx m => Parser m Dec
typeDeclaration = TypeDec <$> (TypeDec_ <$> localOrOwn <*> type_ <*> typeList <*> getPosition)


lowerBound :: CanParse ctx m => Parser m Expr
lowerBound = arithmeticExpression


upperBound :: CanParse ctx m => Parser m Expr
upperBound = arithmeticExpression


boundPair :: CanParse ctx m => Parser m (Expr, Expr)
boundPair = (,) <$> lowerBound <* symbol ":" <*> upperBound


boundPairList :: CanParse ctx m => Parser m [(Expr, Expr)]
boundPairList = commaSep1 boundPair


arraySegment :: CanParse ctx m => Parser m ArraySegment
arraySegment = ArraySegment <$> commaSep1 arrayIdentifier <*> brackets boundPairList


arrayList :: CanParse ctx m => Parser m [ArraySegment]
arrayList = commaSep1 arraySegment


arrayDeclaration :: CanParse ctx m => Parser m Dec
arrayDeclaration = ArrayDec <$> (try simple <|> full)
  where
    simple =
      reserved "array" *>
      (ArrayDec_ False Nothing <$> arrayList <*> getPosition)
    full =
      ArrayDec_
      <$> localOrOwn
      <*> (pure <$> type_)
      <*> (reserved "array" *> arrayList)
      <*> getPosition


switchList :: CanParse ctx m => Parser m [Expr]
switchList = commaSep1 designationalExpression


switchDeclaration :: CanParse ctx m => Parser m Dec
switchDeclaration =
  reserved "switch" *>
    (SwitchDec <$> (SwitchDec_ <$> switchIdentifier <* symbol ":="  <*> switchList <*> getPosition))


formalParameter :: CanParse ctx m => Parser m Symbol
formalParameter = identifier


formalParameterList :: CanParse ctx m => Parser m [Symbol]
formalParameterList = sepBy1 formalParameter parameterDelimeter


formalParameterPart :: CanParse ctx m => Parser m [Symbol]
formalParameterPart = optional (parens formalParameterList) <&> join . maybeToList


identifierList :: CanParse ctx m => Parser m [Symbol]
identifierList = commaSep1 identifier


valuePart :: CanParse ctx m => Parser m [Symbol]
valuePart = optional (reserved "value" *> identifierList <* symbol ";") <&> join . maybeToList


specifier :: CanParse ctx m => Parser m (Maybe Type)
specifier = do
  try (reserved "string" *> pure Nothing)
  <|> try (reserved "label" *> pure Nothing)
  <|> try (reserved "switch" *> pure Nothing)
  <|> try (type_ <* reserved "array" <&> pure)
  <|> try (reserved "array" *> pure Nothing)
  <|> try (type_ <* reserved "procedure" <&> pure)
  <|> try (reserved "procedure" *> pure Nothing)
  <|> (type_ <&> pure)


specificationPart :: CanParse ctx m => Parser m [(Maybe Type, [Symbol])]
specificationPart = do
  many (try ((,) <$> specifier <*> identifierList <* symbol ";"))


procedureHeading :: CanParse ctx m => Parser m (Stmt -> Position -> ProcedureDec)
procedureHeading = do
  (name, formals, values_, specifiers_) <-
    (,,,)
    <$> procedureIdentifier
    <*> formalParameterPart <* symbol ";"
    <*> valuePart
    <*> specificationPart

  let values = Set.fromList values_
  let specifiers = specifiers_ <&> _2 %~ Set.fromList
  let mkParam n =
        Parameter
          n
          (let p = Set.member n . (^. _2)
               mtype = find p specifiers >>= (^. _1)
            in fromMaybe UnknownT mtype)
          (if Set.member n values then CBV else CBN)
  let decC stmt pos =
        ProcedureDec_
          { name = name
          , parameters = mkParam <$> formals
          , returnType = Nothing
          , body = stmt
          , position = pos
          }
  pure decC


procedureBody :: CanParse ctx m => Parser m Stmt
procedureBody = statement


procedureDeclaration :: CanParse ctx m => Parser m Dec
procedureDeclaration = ProcedureDec <$> (try withType <|> withoutType)
  where
    withType = do
      t <- type_ <* reserved "procedure"
      procedureHeading <*> procedureBody <*> getPosition <&> (#returnType ?~ t)
    withoutType = reserved "procedure" *> procedureHeading <*> procedureBody <*> getPosition


----------------------------------------
-- statements and blocks
----------------------------------------

statement :: CanParse ctx m => Parser m Stmt
statement = try unconditionalStatement <|> try conditionalStatement <|> forStatement


unconditionalStatement :: CanParse ctx m => Parser m Stmt
unconditionalStatement = try basicStatement <|> try compoundStatement <|> block


unlabelledBasicStatement :: CanParse ctx m => Parser m Stmt
unlabelledBasicStatement = do
  try assignmentStatement
  <|> try gotoStatement
  <|> try procedureStatement
  <|> dummyStatement


basicStatement :: CanParse ctx m => Parser m Stmt
basicStatement = do
  try (LabelStmt <$> label <* symbol ":" <*> basicStatement) <|> unlabelledBasicStatement


leftPart :: CanParse ctx m => Parser m Var
leftPart = do
  try (variable <* symbol ":=")


leftPartList :: CanParse ctx m => Parser m [Var]
leftPartList = some (try leftPart)


assignmentStatement :: CanParse ctx m => Parser m Stmt
assignmentStatement = do
  AssignStmt <$> st
  where
    st = AssignStmt_
     <$> leftPartList
     <*> (try arithmeticExpression <|> booleanExpression)
     <*> getPosition


gotoStatement :: CanParse ctx m => Parser m Stmt
gotoStatement = GotoStmt <$> (reserved "goto" *> designationalExpression)


procedureStatement :: CanParse ctx m => Parser m Stmt
procedureStatement = CallStmt <$> (try withParams <|> withoutParams)
  where
    withParams = CallExpr <$> procedureIdentifier <*> actualParameterPart <*> getPosition
    withoutParams = CallExpr <$> procedureIdentifier <*> pure [] <*> getPosition


dummyStatement :: CanParse ctx m => Parser m Stmt
dummyStatement = do
  try (lookAhead (reserved "end")) <|> try (lookAhead (symbol ";" *> pure ()))
  pure DummyStmt


forListElement :: CanParse ctx m => Parser m ForListElement
forListElement = try step <|> try while <|> imm
  where
    step = Step
       <$> arithmeticExpression <* reserved "step"
       <*> arithmeticExpression <* reserved "until"
       <*> arithmeticExpression
    while = While
        <$> arithmeticExpression <* reserved "while"
        <*> booleanExpression
    imm = Immediate <$> arithmeticExpression


forList :: CanParse ctx m => Parser m [ForListElement]
forList = commaSep1 forListElement


forClause :: CanParse ctx m => Parser m (Stmt -> Position -> ForStmt)
forClause =
  ForStmt_ <$> (reserved "for" *> variable) <*> (symbol ":=" *> forList) <* reserved "do"


forStatement :: CanParse ctx m => Parser m Stmt
forStatement = do
  try (LabelStmt <$> label <* symbol ":" *> forStatement)
  <|> (ForStmt <$> (forClause <*> statement <*> getPosition))


conditionalStatement :: CanParse ctx m => Parser m Stmt
conditionalStatement = do
  try (LabelStmt <$> label <* symbol ":" *> conditionalStatement)
  <|> conditionalStatement'
  where
    conditionalStatement' = IfStmt <$> (try ifFor <|> try ifElse <|> if')
    ifFor = IfStmt_ <$> ifClause <*> forStatement <*> pure DummyStmt <*> getPosition
    ifElse = IfStmt_ <$> ifClause <*> unconditionalStatement <* reserved "else" <*> statement <*> getPosition
    if' = IfStmt_ <$> ifClause <*> unconditionalStatement <*> pure DummyStmt <*> getPosition


----------------------------------------
-- compound statements and blocks
----------------------------------------


compoundTail :: CanParse ctx m => Parser m Stmt
compoundTail = do
  SeqStmt <$> statement <*> next
  where
    next = do
      reserved "end"
      *> endComment
      *> pure DummyStmt
      <|> symbol ";" *> compoundTail

    endComment =
      manyTill anySingle
        (lookAhead . void $ (symbol ";" <|> symbol "end"))
      *> pure ()


blockHead :: CanParse ctx m => Parser m [Dec]
blockHead = do
  reserved "begin" *> sepEndBy1 declaration (symbol ";")


unlabeledCompoundStatement :: CanParse ctx m => Parser m Stmt
unlabeledCompoundStatement = reserved "begin" *> compoundTail


unlabeledBlock :: CanParse ctx m => Parser m Stmt
unlabeledBlock = BlockStmt <$> (BlockStmt_ <$> blockHead <*> compoundTail <*> getPosition)


compoundStatement :: CanParse ctx m => Parser m Stmt
compoundStatement =
  try (LabelStmt <$> label <* symbol ":" <*> compoundStatement)
  <|> unlabeledCompoundStatement


block :: CanParse ctx m => Parser m Stmt
block =
  try (label *> symbol ":" *> block) <|> unlabeledBlock


program :: CanParse ctx m => Parser m Stmt
program = ignore *> try block <|> compoundStatement

