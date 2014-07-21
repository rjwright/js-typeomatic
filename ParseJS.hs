
-- Copyright 2014 Google Inc. All rights reserved.

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.


-- This module parses a JavaScript source file using the Language.Javascript.Parser library (Version
-- 0.4.5), then simplifies the parse tree to produce a more useful abstract syntax tree (AST).
--
-- The doc for Language.Javascript.Parser is at hackage.haskell.org/package/language-javascript, but
-- it isn't particularly useful. The library's main data type - Node - has many constructors, and
-- the documentation doesn't tell you what javascript source code elements they correspond to. Best
-- approach is to look at the parser source.
--
-- TODO: Parser Node types that aren't handled yet are:
--      JSEmpty
--      JSHexInteger
--      JSPropertyAccessor
--      JSRegEx
--      JSWith
--
--
-- Top level function is (getASTWithSource (parseTree program file) file)
--


module ParseJS
( Index
, AST(..)
, ASTWithSourceSpan(..)
, Operator
, PropertyName(..)
, SourceFileName
, Value(..)
, Variable
, jsnGetNode
, parseTree
, getASTWithSource
) where


import Control.Monad.State
import Data.Functor.Identity
import Data.List
    ( delete
    , find
    , intercalate
    , nub
    ,(\\)
    )
import Data.Maybe
    ( catMaybes
    , fromJust
    , isJust
    , listToMaybe
    , mapMaybe
    )
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Parser.AST


type Index = Int
type Operator = String
type SourceFileName = String
type Variable = String


-- Represent an object property identifiers. Can be a string or an integer.
data PropertyName =
    --  Identifier is an integer literal.
      IndexProperty Index
    -- This is used in the TypeRules module when an object or array is indexed  with square brackets
    -- using a variable insead of a string/integer literal.
    --
    -- If the structure being indexed is an array then this will be resolved to an IntType. If the
    -- structure is an object, then the object is not type safe (so we no longer care about the type
    -- of this index)
    | UnknownProperty
    --  Identifier is a string literal.
    -- FIXME: This could have a better name.
    | VariableProperty Variable deriving (Show)


-- Represent literal values.
data Value =
      JSArray [ASTWithSourceSpan]
    | JSBool Bool
    -- TODO: Double quote strings are never treated differently to normal strings and should be
    -- merged with JSString when pipeline is complete.
    | JSDQString String
    | JSFloat Double
    | JSInt Int
    | JSNull
    -- Objects contain a list of PropNameValues.
    | JSObject [ASTWithSourceSpan]
    | JSString String
    | JSUndefined deriving (Show)


-- Represent a node in the AST
data AST =
      Block ASTWithSourceSpan
    | Case ASTWithSourceSpan ASTWithSourceSpan
    | Catch Variable (Maybe ASTWithSourceSpan) ASTWithSourceSpan
    | Default ASTWithSourceSpan
    | DoWhile ASTWithSourceSpan ASTWithSourceSpan
    | Finally ASTWithSourceSpan
    | For
        (Maybe ASTWithSourceSpan)
        (Maybe ASTWithSourceSpan)
        (Maybe ASTWithSourceSpan)
        ASTWithSourceSpan
    | ForIn [Variable] ASTWithSourceSpan ASTWithSourceSpan
    | ForVar
        [ASTWithSourceSpan]
        (Maybe ASTWithSourceSpan)
        (Maybe ASTWithSourceSpan)
        ASTWithSourceSpan
    | ForVarIn ASTWithSourceSpan ASTWithSourceSpan ASTWithSourceSpan
    | FunctionBody [ASTWithSourceSpan]
    | FunctionDeclaration Variable [Variable] ASTWithSourceSpan
    | If ASTWithSourceSpan ASTWithSourceSpan
    | IfElse ASTWithSourceSpan ASTWithSourceSpan ASTWithSourceSpan
    | Labelled Variable ASTWithSourceSpan
    | Return ASTWithSourceSpan
    | Switch ASTWithSourceSpan [ASTWithSourceSpan]
    | Try ASTWithSourceSpan [ASTWithSourceSpan]
    | While ASTWithSourceSpan ASTWithSourceSpan

    | Arguments [ASTWithSourceSpan]
    | Assignment Operator ASTWithSourceSpan ASTWithSourceSpan
    | Binary Operator ASTWithSourceSpan ASTWithSourceSpan
    | Break (Maybe Variable)
    | Call ASTWithSourceSpan ASTWithSourceSpan
    -- In Language.JavaScript, a call expression is an expression that calls - or accesses a
    -- property of - a function call. (E.g. foo()(); foo().bar;)
    --
    -- This program treats foo()() as a Call within a Call (I think that is a sufficient
    -- description for our purposes).
    | CallExpression ASTWithSourceSpan Operator ASTWithSourceSpan
    | Continue (Maybe Variable)
    -- TODO: Needs comment to explain what it is.
    -- TODO: New type. Needs to be handled in the Rules.
    -- Was called "List".
    | Expression [ASTWithSourceSpan]
    -- A function definition on the right hand side of some statement.
    | FunctionExpression (Maybe Variable) [Variable] ASTWithSourceSpan
    | Identifier Variable
    -- An index into a structure using square brackets.
    | Index ASTWithSourceSpan ASTWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    -- Was called "List".
    -- TODO: New type. Needs to be handled in the Rules.
    | StatementList [ASTWithSourceSpan]
    | New ASTWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    | ParenExpression ASTWithSourceSpan
    -- A property of an object.
    | PropNameValue PropertyName ASTWithSourceSpan
    -- A reference into a structure using a dot.
    | Reference ASTWithSourceSpan ASTWithSourceSpan
    | Ternary ASTWithSourceSpan ASTWithSourceSpan ASTWithSourceSpan
    | Throw ASTWithSourceSpan
    | UnaryPost Operator ASTWithSourceSpan
    | UnaryPre Operator ASTWithSourceSpan
    | Value Value
    | VarDeclaration Variable (Maybe ASTWithSourceSpan)
    deriving (Show)


data ASTWithSourceSpan = AWSS AST SrcSpan deriving (Show)
-- data ASTWithSourceSpan = AWSS Expression SrcSpan deriving (Show)


getASTWithSource :: JSNode -> SourceFileName -> (ASTWithSourceSpan, SourceFileName)
getASTWithSource jsn fileName = ((toAST jsn), fileName)


-- Parse JavaScript source code.
parseTree :: String -> SourceFileName -> JSNode
parseTree program fileName = (\(Right a) -> a) $ parse program fileName;


jsnGetNode :: JSNode -> Node
jsnGetNode (NS node _) = node


jsnGetSource :: JSNode -> SrcSpan
jsnGetSource (NS _ srcSpan) = srcSpan


identifierGetString :: Node -> String
identifierGetString (JSIdentifier jsid) = jsid


listToMaybeExpression :: [JSNode] -> Maybe ASTWithSourceSpan
listToMaybeExpression [] = Nothing
listToMaybeExpression jsn = Just $ listToASTExpression jsn


-- Some parser nodes contain lists of JSNodes that represent whole expressions. This function takes
-- such a list of Nodes and builds a single expression.
listToASTExpression :: [JSNode] -> ASTWithSourceSpan
listToASTExpression [item] = toAST item
listToASTExpression [(NS (JSUnary operator) srcSpan), (NS (JSDecimal x) _)]
    | (operator == "-") =
        if (elem '.' x) then
            AWSS (Value xFloat) srcSpan
        else
            AWSS (Value xInt) srcSpan
        where
            xFloat = JSFloat (-1 * (read x))
            xInt = JSInt (-1 * (read x))
listToASTExpression ((NS (JSUnary operator) srcSpan):x)
    | elem operator ["-", "+", "--", "++", "!", "typeof ", "delete ", "~"] =
        AWSS
            (UnaryPre
                operator
                (listToASTExpression x))
            srcSpan
listToASTExpression ((NS (JSLiteral "new ") srcSpan):x) =
    AWSS (New (listToASTExpression x)) srcSpan
listToASTExpression [x, (NS (JSArguments args) srcSpan)] =
    AWSS
        (Call
            (toAST x)
            (toASTArguments args srcSpan))
        (jsnGetSource x)
listToASTExpression list | (isCallExpression $ last list) =
    getASTCallExpression list
    where
        isCallExpression :: JSNode -> Bool
        isCallExpression (NS (JSCallExpression "." _) _) = True
        isCallExpression (NS (JSCallExpression "[]" _) _) = True
        isCallExpression _ = False
listToASTExpression list | (isParenCallExp $ last list) =
    getASTCall list
    where
        isParenCallExp :: JSNode -> Bool
        isParenCallExp (NS (JSCallExpression "()" _) _) = True
        isParenCallExp _ = False
-- FIXME: Anything else is assumed to be an assignment. Verify that that assumption is correct.
listToASTExpression list =
    getASTAssignment list []
    where
        isAssignmentOperator :: String -> Bool
        isAssignmentOperator op
            | elem op ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="] =
                True
            | otherwise = False
        getASTAssignment :: [JSNode] -> [JSNode] -> ASTWithSourceSpan
        getASTAssignment ((NS (JSOperator op) srcSpan):xs) preCurrent
            | (isAssignmentOperator op) =
                AWSS
                    (Assignment
                        op
                        (listToASTExpression preCurrent)
                        (listToASTExpression xs))
                    (jsnGetSource $ head preCurrent)
        getASTAssignment (x:xs) preCurrent = getASTAssignment xs (preCurrent ++ [x])


toASTArguments :: [[JSNode]] -> SrcSpan -> ASTWithSourceSpan
toASTArguments args srcSpan =
    AWSS (Arguments (map getASTArgument args)) srcSpan
    where
        getASTArgument :: [JSNode] -> ASTWithSourceSpan
        getASTArgument (item:[]) = toAST item
        getASTArgument nodes = listToASTExpression nodes


toASTVarDeclaration :: JSNode -> ASTWithSourceSpan
toASTVarDeclaration (NS (JSVarDecl name value) srcSpan) =
    AWSS
        (VarDeclaration
            (identifierGetString $ jsnGetNode name)
            (listToMaybeExpression value))
        srcSpan


-- To handle the case where the last element of a list is a (JSCallExpression "()" [JSArguments
-- _]) I don't know if the second field can be anything other than a singleton list containing a
-- JSArguments but for now I'm just going to hope not.
--
-- TODO: Find out what values the arguments list can have.
getASTCall :: [JSNode] -> ASTWithSourceSpan
getASTCall list =
    AWSS
        (Call
            (listToASTExpression (init list))
            (getArgs $ last list))
        (jsnGetSource $ head list)
    where
        getArgs :: JSNode -> ASTWithSourceSpan
        getArgs (NS (JSCallExpression _ [(NS (JSArguments args) srcSpan)]) _) =
            toASTArguments args srcSpan


-- To handle the case where the last element in the list is a (JSCallExpression "[]" exp) or a
-- (JSCallExpression "." exp).
getASTCallExpression :: [JSNode] -> ASTWithSourceSpan
getASTCallExpression list =
    AWSS
        (CallExpression
            (listToASTExpression (init list))
            (callExpOperator $ jsnGetNode $ last list)
            (callExpProperty $ jsnGetNode $ last list))
        (jsnGetSource $ head list)
    where
        callExpOperator :: Node -> Operator
        callExpOperator (JSCallExpression operator _) = operator
        -- FIXME: This assumes that the expression list can only ever be singleton. Verify.
        callExpProperty :: Node -> ASTWithSourceSpan
        callExpProperty (JSCallExpression "[]" [expr]) = toAST expr
        callExpProperty (JSCallExpression "." [expr]) =
            toAST expr


-- TODO: Remove all of the nested "Block" constructors etc.
toAST :: JSNode -> ASTWithSourceSpan
toAST (NS (JSBlock statements) srcSpan) =
    AWSS
        (Block
            (toAST statements))
        srcSpan
toAST (NS (JSFunctionBody bodyList) srcSpan) =
    AWSS
        (FunctionBody (map toAST bodyList))
        srcSpan
-- TODO: Make sure this is handled correctly when making Rules.
toAST (NS (JSSourceElements elementsList) srcSpan) =
    AWSS
        (StatementList (map toAST elementsList))
        srcSpan
-- TODO: Make sure this is handled correctly when making Rules.
toAST (NS (JSSourceElementsTop elementsList) srcSpan) =
    AWSS
        (StatementList (map toAST elementsList))
        srcSpan
toAST (NS (JSStatementBlock statements) srcSpan) =
    AWSS
        -- FIXME: Not sure if "Block" is the right thing here.
        (Block
            (toAST statements))
        srcSpan
toAST (NS (JSStatementList statements) srcSpan) =
    AWSS
        (StatementList (map toAST statements))
        srcSpan
-- TODO: Do I need to do anything with the first parameter?
toAST (NS (JSVariables _ varDecs) srcSpan) =
    -- FIXME: This doesn't actually come from a JSStatementList in the parser. Should make a
    -- "variables" type for this.
    AWSS
        (StatementList (map toASTVarDeclaration varDecs))
        srcSpan
toAST (NS (JSBreak label _) srcSpan) =
    AWSS
        (Break (liftM (identifierGetString . jsnGetNode) (listToMaybe label)))
        srcSpan
toAST (NS (JSCase cs body) srcSpan) =
    AWSS
        (Case
            (toAST cs)
            -- body is a JSStatementList.
            (toAST body))
        srcSpan
toAST (NS (JSCatch var test body) srcSpan) =
    AWSS
        (Catch
            (identifierGetString $ jsnGetNode var)
            (listToMaybeExpression test)
            -- body is a JSBlock.
            (toAST body))
        srcSpan
-- TODO: In a JSContinue we always have either a list contiaining just a literal semicolon, or list
-- with a value and a literal semicolon.
toAST (NS (JSContinue label) srcSpan) =
    AWSS
        (Continue (liftM (identifierGetString . jsnGetNode) (listToMaybe $ init label)))
        srcSpan
toAST (NS (JSDefault body) srcSpan) =
    AWSS
        (Default
            -- body is a JSStatementList.
            (toAST body))
        srcSpan
toAST (NS (JSDoWhile body test semi) srcSpan) =
    AWSS
        (DoWhile
            -- body is a JSStatementBlock.
            (toAST body)
            (toAST test))
        srcSpan
-- TODO: Comment on where JSExpressions come from.
toAST (NS (JSExpression jsnList) srcSpan) =
    AWSS
        (Expression (map listToASTExpression (getSublists jsnList [])))
        srcSpan
    where
        -- A JSExpression contains a list of JSNodes, separated by <JSLiteral ','>. These need to be
        -- seperated (basically the <JSLiteral ','>s need to be stripped).
        getSublists :: [JSNode] -> [JSNode] -> [[JSNode]]
        getSublists ((NS (JSLiteral ",") _):rest) current =
            [current] ++ (getSublists rest [])
        getSublists (node:rest) current = getSublists rest (current ++ [node])
        getSublists [] current = [current]
toAST (NS (JSFinally body) srcSpan) =
    AWSS
        (Finally
            -- body is a JSBlock.
            (toAST body))
        srcSpan
-- JSFor occurs when no varibles are declared in the loop (although they may be re-assigned).
toAST (NS (JSFor vars test count body) srcSpan) =
    AWSS
        (For
            (liftM toAST (listToMaybe vars))
            (liftM toAST (listToMaybe test))
            (liftM toAST (listToMaybe count))
            -- body is a JSStatementBlock.
            (toAST body))
        srcSpan
-- JSForIn occurs when no variables are declared inside the for statement, and the loop iterates
-- over some object (e.g. an array)
toAST (NS (JSForIn vars obj body) srcSpan) =
    AWSS
        (ForIn
            (map (identifierGetString . jsnGetNode) vars)
            (toAST obj)
            -- body is a JSStatementBlock.
            (toAST body))
        srcSpan
-- JSForVar occurs in the case that variables are declared in the loop statement.
toAST (NS (JSForVar vars test count body) srcSpan) =
    AWSS
        (ForVar
            (map toASTVarDeclaration vars)
            (liftM toAST (listToMaybe test))
            (liftM toAST (listToMaybe count))
            -- body is a JSStatementBlock.
            (toAST body))
        srcSpan
-- JSForVarIn occurs when variables are declared inside the for statement and the loop iterates over
-- some object (e.g. an array)
toAST (NS (JSForVarIn var obj body) srcSpan) =
    AWSS
        (ForVarIn
            (toASTVarDeclaration var)
            (toAST obj)
            -- body is a JSStatementBlock.
            (toAST body))
        srcSpan
toAST (NS (JSFunction name inputs body) srcSpan) =
    AWSS
        (FunctionDeclaration
            (identifierGetString $ jsnGetNode name)
            (map (identifierGetString . jsnGetNode) inputs)
            (toAST body))
        srcSpan
toAST (NS (JSIf test body) srcSpan) =
    AWSS
        (If
            (toAST test)
            -- body is a JSStatementBlock.
            (toAST body))
        srcSpan
toAST (NS (JSIfElse test trueBody falseBody) srcSpan) =
    AWSS
        (IfElse
            (toAST test)
            (toAST trueBody)
            (toAST falseBody))
        srcSpan
-- TODO: Comment on what a JSLabelled is.
toAST (NS (JSLabelled label body) srcSpan) =
    AWSS
        (Labelled
            (identifierGetString $ jsnGetNode label)
            -- Body can be anything.
            (toAST body))
        srcSpan
-- FIXME: Not 100% sure that this is safe.
-- TODO: Comment on where these come from.
-- FIXME: What do we do with this!!?? Do I need it?
-- toAST (NS (JSLiteral ";") srcSpan) = []
toAST (NS (JSReturn value) srcSpan) =
    AWSS
        (Return
            (returnValue value))
        srcSpan
    where
        -- The list in a JSReturn is always either a singleton list containing a semicolon, or a
        -- 2-list containing a JSNode (representing the value to be returned) and a semicolon.
        returnValue :: [JSNode] -> ASTWithSourceSpan
        returnValue [semi] = AWSS (Value JSUndefined) srcSpan
        returnValue [val, semi] = toAST val
toAST (NS (JSSwitch var cases) srcSpan) =
    AWSS
        (Switch
            (toAST var)
            (map toAST cases))
        srcSpan
toAST (NS (JSThrow expr) srcSpan) =
    AWSS
        (Throw (toASTExpression expr))
        srcSpan
    where
        toASTExpression :: JSNode -> ASTWithSourceSpan
        toASTExpression (NS (JSExpression ex) _) = listToASTExpression ex
toAST (NS (JSTry body catchClause) srcSpan) =
    AWSS
        (Try
            -- body is a JSBlock.
            (toAST body)
            -- Each of these is a JSCatch or a JSFinally.
            (map toAST catchClause))
        srcSpan
toAST (NS (JSWhile test body) srcSpan) =
    AWSS
        (While
            (toAST test)
            -- body is a JSStatementBlock.
            (toAST body))
        srcSpan
-- Anything else is assumed to be a statement.
-- toAST jsn =
--     [AWSS
--         (Statement
--             (toAST jsn))
--         (jsnGetSource jsn)
--     ]
-- toAST :: JSNode -> ASTWithSourceSpan
toAST (NS (JSArguments args) srcSpan) = toASTArguments args srcSpan
toAST (NS (JSExpressionBinary operator left right) srcSpan) =
    AWSS
        (Binary
            operator
            (listToASTExpression left)
            (listToASTExpression right))
        srcSpan
toAST (NS (JSExpressionParen expr) srcSpan) =
    AWSS
        (ParenExpression
            (toAST expr))
        srcSpan
toAST (NS (JSExpressionPostfix operator variable) srcSpan) =
    AWSS
        (UnaryPost
            operator
            (listToASTExpression variable))
        srcSpan
toAST (NS (JSExpressionTernary expr ifTrue ifFalse) srcSpan) =
    AWSS
        (Ternary
            (listToASTExpression expr)
            (listToASTExpression ifTrue)
            (listToASTExpression ifFalse))
        srcSpan
toAST (NS (JSFunctionExpression name args body) srcSpan) =
    AWSS
        (FunctionExpression
            (liftM (identifierGetString . jsnGetNode) (listToMaybe name))
            (map (identifierGetString . jsnGetNode) args)
            (toAST body))
        srcSpan
toAST (NS (JSIdentifier "undefined") srcSpan) =
    AWSS (Value JSUndefined) srcSpan
toAST (NS (JSIdentifier identifier) srcSpan) =
    AWSS (Identifier identifier) srcSpan
toAST (NS (JSLiteral "null") srcSpan) =
    AWSS (Value JSNull) srcSpan
toAST (NS (JSLiteral "this") srcSpan) =
    AWSS (Identifier "this") srcSpan
toAST (NS (JSMemberDot pre post) srcSpan) =
    AWSS
        (Reference
            (listToASTExpression pre)
            (toAST post))
        srcSpan
toAST (NS (JSMemberSquare pre post) srcSpan) =
    AWSS
        (Index
            (listToASTExpression pre)
            (toAST post))
        srcSpan
-- Anything left unmatched here is assumed to be a literal value.
toAST val =
    AWSS (Value (toASTValue val)) (jsnGetSource val)

-- These functions are used to process array literals.
--
-- The way that the parser (Grammar5.y) handles commas in arrays seems strange to me. A single comma
-- at the end of an array makes a (JSLiteral \",\"). If there is more than one comma at the end of
-- the array, then the last comma is ignored. A comma in any other scenario makes a (JSElision []);
-- two commas make two elisions, three commas three elisions, and so on;
--
-- The following are some examples of parser output.
--
-- []
-- JSArrayLiteral []
--
-- [1]
-- JSArrayLiteral [ JSDecimal \"1\" ]
--
-- [1, 2]
-- JSArrayLiteral [ JSDecimal \"1\", JSElision [], JSDecimal \"2\" ]
--
-- [,]
-- JSArrayLiteral [ JSElision [] ]
--
-- [,,]
-- JSArrayLiteral [ JSElision [], JSElision [] ]
--
-- [1,]
-- JSArrayLiteral [ JSDecimal \"1\", JSLiteral \",\" ]
--
-- [1,,]
-- JSArrayLiteral [ JSDecimal \"1\", JSElision [] ]
--
-- [,1]
-- JSArrayLiteral [ JSElision [], JSDecimal \"1\" ]
--
-- [,,1]
-- JSArrayLiteral [ JSElision [], JSElision [], JSDecimal \"1\" ]
--
-- [1,,2]
-- JSArrayLiteral [ JSDecimal \"1\", JSElision [], JSElision [], JSDecimal \"2\" ]


-- Some elisions have SpanEmpty so we need to use the last non-empty SpanPoint.
getNearestSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
getNearestSrcSpan (SpanEmpty) s = s
getNearestSrcSpan s _ = s


-- Takes the parse tree representation of an array literal, deals with elisions at the start of the
-- array, then processes what's left.
processArray :: [JSNode] -> [[JSNode]] -> SrcSpan -> [[JSNode]]
processArray [] current _ = current
processArray ((NS (JSElision es) srcSpan):rest) current nearestSpan =
    processArray
        rest
        (current ++ [[NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan nearestSpan)]])
        (getNearestSrcSpan srcSpan nearestSpan)
processArray jsArray current nearestSpan = arrayGetElements jsArray current nearestSpan


-- Process the remainder an array literal after any leading commas have been processed.
-- FIXME: Try to improve source spans.
arrayGetElements :: [JSNode] -> [[JSNode]] -> SrcSpan -> [[JSNode]]
-- Ignore one trailing comma at the end of the array.
arrayGetElements [(NS (JSLiteral ",") _)] current _ = current
-- A single elision at the end of the parsed array occurs when there are two commas at the end of
-- the array or when the array is equal to [,]
arrayGetElements [(NS (JSElision e) srcSpan)] current nearestSpan =
    current ++ [[(NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan nearestSpan))]]
arrayGetElements [item] current nearestSpan = current ++ [[item]]
-- Two elisions in a row (that aren't at the beginning of the array) indicates one undefined entry,
-- then a comma seperator, then the next entry.
arrayGetElements
    ((NS (JSElision _) srcSpan1):(NS (JSElision e) srcSpan2):rest) current nearestSpan  =
        arrayGetElements
            ((NS (JSElision e) srcSpan2):rest)
            (current
            ++ [[NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan1 nearestSpan)]])
            (getNearestSrcSpan srcSpan1 nearestSpan)
-- One elision and then a non-elision entry indicates a comma seperator and then the entry.
arrayGetElements ((NS (JSElision _) srcSpan):rest) current nearestSpan =
    arrayGetElements rest current (getNearestSrcSpan srcSpan nearestSpan)
arrayGetElements (jsn:rest) current nearestSpan =
    arrayGetElements
        rest
        (current ++ [[jsn]])
        (getNearestSrcSpan (jsnGetSource jsn) nearestSpan)


-- Takes a Node that represents a literal value and makes an AST node for that value.
toASTValue :: JSNode -> Value
toASTValue (NS (JSArrayLiteral arr) srcSpan) =
    JSArray
        (map listToASTExpression (processArray arr [] srcSpan))
toASTValue (NS (JSDecimal s) _) =
    if elem '.' s then
        JSFloat (read s)
    else
        JSInt (read s)
toASTValue (NS (JSLiteral "false") _) = JSBool False
toASTValue (NS (JSLiteral "true") _) = JSBool True
-- TODO: Refactor this? (already done once but still kinda crazy)
toASTValue (NS (JSObjectLiteral list) _) =
    JSObject
        (map toASTPropNameValue list)
    where
        -- Takes a JSNode that represents a property of an object and produdes a PropNameValue
        -- Expression.
        toASTPropNameValue :: JSNode -> ASTWithSourceSpan
        toASTPropNameValue
            (NS (JSPropertyNameandValue (NS (JSIdentifier name) _) value) srcSpan) =
                AWSS
                    (PropNameValue
                        (VariableProperty name)
                        (listToASTExpression value))
                    srcSpan
        toASTPropNameValue
            (NS (JSPropertyNameandValue (NS (JSDecimal index) _) value) srcSpan) =
                AWSS
                    (PropNameValue
                        (IndexProperty (read index))
                        (listToASTExpression value))
                    srcSpan
toASTValue (NS (JSStringLiteral '"' s) _) = JSDQString s
toASTValue (NS (JSStringLiteral _ s) _) = JSString s
