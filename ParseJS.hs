
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
( Expression(..)
, ExprWithSourceSpan(..)
, Index
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
      JSArray [ExprWithSourceSpan]
    | JSBool Bool
    -- TODO: Double quote strings are never treated differently to normal strings and should be
    -- merged with JSString when pipeline is complete.
    | JSDQString String
    | JSFloat Double
    | JSInt Int
    | JSNull
    -- Objects contain a list of PropNameValues.
    | JSObject [ExprWithSourceSpan]
    | JSString String
    | JSUndefined deriving (Show)


-- Represent, approximately, source elements that are expressions. None of these contain AST
-- fields except for FunctionExpression.
--
-- TODO: Can FunctionExpression be moved into AST? (probably not).
data Expression =
      Arguments [ExprWithSourceSpan]
    | Assignment Operator ExprWithSourceSpan ExprWithSourceSpan
    | Binary Operator ExprWithSourceSpan ExprWithSourceSpan
    | Break (Maybe Variable)
    | Call ExprWithSourceSpan ExprWithSourceSpan
    -- In Language.JavaScript, a call expression is an expression that calls - or accesses a
    -- property of - a function call. (E.g. foo()(); foo().bar;)
    --
    -- This program treats foo()() as a Call within a Call (I think that is a sufficient
    -- description for our purposes).
    | CallExpression ExprWithSourceSpan Operator ExprWithSourceSpan
    | Continue (Maybe Variable)
    -- A function definition on the right hand side of some statement.
    | FunctionExpression (Maybe Variable) [Variable] ASTWithSourceSpan
    | Identifier Variable
    -- An index into a structure using square brackets.
    | Index ExprWithSourceSpan ExprWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    | List [ExprWithSourceSpan]
    | New ExprWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    | ParenExpression ExprWithSourceSpan
    -- A property of an object.
    | PropNameValue PropertyName ExprWithSourceSpan
    -- A reference into a structure using a dot.
    | Reference ExprWithSourceSpan ExprWithSourceSpan
    | Ternary ExprWithSourceSpan ExprWithSourceSpan ExprWithSourceSpan
    | Throw ExprWithSourceSpan
    | UnaryPost Operator ExprWithSourceSpan
    | UnaryPre Operator ExprWithSourceSpan
    | Value Value
    | VarDeclaration Variable (Maybe ExprWithSourceSpan) deriving (Show)


-- Represent source elements which include a "block" or "body" and thus make logical non-terminal
-- nodes for an abstract syntax tree.
--
-- TODO: Also includes a type for return expressions (Return) and a wrapper for instances of the
-- Expression data type (Statement). Can they be moved into Expression? (probably not).
data AST =
      Block [ASTWithSourceSpan]
    | Case ExprWithSourceSpan ASTWithSourceSpan
    | Catch Variable (Maybe ExprWithSourceSpan) ASTWithSourceSpan
    | Default ASTWithSourceSpan
    | DoWhile ASTWithSourceSpan ExprWithSourceSpan
    | Finally ASTWithSourceSpan
    | For
        (Maybe ExprWithSourceSpan)
        (Maybe ExprWithSourceSpan)
        (Maybe ExprWithSourceSpan)
        ASTWithSourceSpan
    | ForIn [Variable] ExprWithSourceSpan ASTWithSourceSpan
    | ForVar
        [ExprWithSourceSpan]
        (Maybe ExprWithSourceSpan)
        (Maybe ExprWithSourceSpan)
        ASTWithSourceSpan
    | ForVarIn ExprWithSourceSpan ExprWithSourceSpan ASTWithSourceSpan
    | FunctionBody [ASTWithSourceSpan]
    | FunctionDeclaration Variable [Variable] ASTWithSourceSpan
    | If ExprWithSourceSpan ASTWithSourceSpan
    | IfElse ExprWithSourceSpan ASTWithSourceSpan ASTWithSourceSpan
    | Labelled Variable ASTWithSourceSpan
    | Return ExprWithSourceSpan
    | Statement ExprWithSourceSpan
    | Switch ExprWithSourceSpan ASTWithSourceSpan
    | Try ASTWithSourceSpan ASTWithSourceSpan
    | While ExprWithSourceSpan ASTWithSourceSpan deriving (Show)


data ASTWithSourceSpan = AWSS AST SrcSpan deriving (Show)
data ExprWithSourceSpan = EWSS Expression SrcSpan deriving (Show)


getASTWithSource :: JSNode -> SourceFileName -> ([ASTWithSourceSpan], SourceFileName)
getASTWithSource jsn fileName = ((toAST jsn), fileName)


-- Parse JavaScript source code.
parseTree :: String -> SourceFileName -> JSNode
parseTree program fileName = (\(Right a) -> a) $ parse program fileName;


jsnGetNode :: JSNode -> Node
jsnGetNode (NS node _) = node


jsnGetSource :: JSNode -> SrcSpan
jsnGetSource (NS _ srcSpan) = srcSpan


-- Statement is basically a AST wrapper for an Expression. If the original parse tree Node was a
-- JSExpression, then we will get a (Statement List [Expression]). Some of our data types want a
-- AST and some want an Expression. This function is for getting a List out of a (Statement List
-- [Expression]) AST node.
--
-- This is an artifact of trying to have Expressions as terminal (or near-terminal) nodes in the
-- AST, and thus wanting Expressions to contain other Expressions. I think AST and Expression
-- should be merged, however. Then, this function can probably go.
jsnToListExp :: JSNode -> ExprWithSourceSpan
jsnToListExp jsn =
    statementToListExp $ toAST jsn
    where
        statementToListExp :: [ASTWithSourceSpan] -> ExprWithSourceSpan
        statementToListExp [AWSS (Statement expr) _] = expr


identifierGetString :: Node -> String
identifierGetString (JSIdentifier jsid) = jsid


listToMaybeExpression :: [JSNode] -> Maybe ExprWithSourceSpan
listToMaybeExpression [] = Nothing
listToMaybeExpression jsn = Just $ listToASTExpression jsn


-- Some parser nodes contain lists of JSNodes that represent whole expressions. This function takes
-- such a list of Nodes and builds a single expression.
listToASTExpression :: [JSNode] -> ExprWithSourceSpan
listToASTExpression [item] = makeASTExpression item
listToASTExpression [(NS (JSUnary operator) srcSpan), (NS (JSDecimal x) _)]
    | (operator == "-") =
        if (elem '.' x) then
            EWSS (Value xFloat) srcSpan
        else
            EWSS (Value xInt) srcSpan
        where
            xFloat = JSFloat (-1 * (read x))
            xInt = JSInt (-1 * (read x))
listToASTExpression ((NS (JSUnary operator) srcSpan):x)
    | elem operator ["-", "+", "--", "++", "!", "typeof ", "delete ", "~"] =
        EWSS
            (UnaryPre
                operator
                (listToASTExpression x))
            srcSpan
listToASTExpression ((NS (JSLiteral "new ") srcSpan):x) =
    EWSS (New (listToASTExpression x)) srcSpan
listToASTExpression [x, (NS (JSArguments args) srcSpan)] =
    EWSS
        (Call
            (makeASTExpression x)
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
        getASTAssignment :: [JSNode] -> [JSNode] -> ExprWithSourceSpan
        getASTAssignment ((NS (JSOperator op) srcSpan):xs) preCurrent
            | (isAssignmentOperator op) =
                EWSS
                    (Assignment
                        op
                        (listToASTExpression preCurrent)
                        (listToASTExpression xs))
                    (jsnGetSource $ head preCurrent)
        getASTAssignment (x:xs) preCurrent = getASTAssignment xs (preCurrent ++ [x])


toASTArguments :: [[JSNode]] -> SrcSpan -> ExprWithSourceSpan
toASTArguments args srcSpan =
    EWSS (Arguments (map getASTArgument args)) srcSpan
    where
        getASTArgument :: [JSNode] -> ExprWithSourceSpan
        getASTArgument (item:[]) = makeASTExpression item
        getASTArgument nodes = listToASTExpression nodes


toASTVarDeclaration :: JSNode -> ExprWithSourceSpan
toASTVarDeclaration (NS (JSVarDecl name value) srcSpan) =
    EWSS
        (VarDeclaration
            (identifierGetString $ jsnGetNode name)
            (listToMaybeExpression value))
        srcSpan


-- To handle the case where the last element of a list is a (JSCallExpression "()" [JSArguments
-- _]) I don't know if the second field can be anything other than a singleton list containing a
-- JSArguments but for now I'm just going to hope not.
--
-- TODO: Find out what values the arguments list can have.
getASTCall :: [JSNode] -> ExprWithSourceSpan
getASTCall list =
    EWSS
        (Call
            (listToASTExpression (init list))
            (getArgs $ last list))
        (jsnGetSource $ head list)
    where
        getArgs :: JSNode -> ExprWithSourceSpan
        getArgs (NS (JSCallExpression _ [(NS (JSArguments args) srcSpan)]) _) =
            toASTArguments args srcSpan


-- To handle the case where the last element in the list is a (JSCallExpression "[]" exp) or a
-- (JSCallExpression "." exp).
getASTCallExpression :: [JSNode] -> ExprWithSourceSpan
getASTCallExpression list =
    EWSS
        (CallExpression
            (listToASTExpression (init list))
            (callExpOperator $ jsnGetNode $ last list)
            (callExpProperty $ jsnGetNode $ last list))
        (jsnGetSource $ head list)
    where
        callExpOperator :: Node -> Operator
        callExpOperator (JSCallExpression operator _) = operator
        -- FIXME: This assumes that the expression list can only ever be singleton. Verify.
        callExpProperty :: Node -> ExprWithSourceSpan
        callExpProperty (JSCallExpression "[]" [expr]) = jsnToListExp expr
        callExpProperty (JSCallExpression "." [expr]) =
            makeASTExpression expr


toAST :: JSNode -> [ASTWithSourceSpan]
-- These ones return a proper list of ASTs. (Haskell) Constructors which use one of these to
-- fill a field must have a [AST] for that field.
toAST (NS (JSBlock jsnode) _) = toAST jsnode
toAST (NS (JSFunctionBody bodyList) _) = concat $ map toAST bodyList
toAST (NS (JSSourceElements elementsList) _) = concat $ map toAST elementsList
toAST (NS (JSSourceElementsTop topList) _) = concat $ map toAST topList
toAST (NS (JSStatementBlock item) _) = toAST item
toAST (NS (JSStatementList statList) _) = concat $ map toAST statList
toAST (NS (JSVariables _ varDecs) srcSpan) =
    map (\jsn -> AWSS (Statement (toASTVarDeclaration jsn)) srcSpan) varDecs
-- These ones always return singleton lists. (Haskell) Constructors which use only these to
-- fill a field can have a AST for that field.
toAST (NS (JSBreak label _) srcSpan) =
    [AWSS
        (Statement
            (EWSS
                (Break (liftM (identifierGetString . jsnGetNode) (listToMaybe label)))
                srcSpan))
        srcSpan
    ]
toAST (NS (JSCase cs body) srcSpan) =
    [AWSS
        (Case
            (jsnToListExp cs)
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
toAST (NS (JSCatch var test body) srcSpan) =
    [AWSS
        (Catch
            (identifierGetString $ jsnGetNode var)
            (listToMaybeExpression test)
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- TODO: In a JSContinue we always have either a list contiaining just a literal semicolon, or list
-- with a value and a literal semicolon.
toAST (NS (JSContinue label) srcSpan) =
    [AWSS
        (Statement
            (EWSS (Continue (liftM (identifierGetString . jsnGetNode) (listToMaybe $ init label))) srcSpan))
        srcSpan
    ]
toAST (NS (JSDefault body) srcSpan) =
    [AWSS
        (Default
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
toAST (NS (JSDoWhile body test semi) srcSpan) =
    [AWSS
        (DoWhile
            (AWSS (Block (toAST body)) (jsnGetSource body))
            (jsnToListExp test))
        srcSpan
    ]
-- TODO: Comment on where JSExpressions come from.
toAST (NS (JSExpression jsnList) srcSpan) =
    [AWSS
        (Statement
            (EWSS
                (List (map listToASTExpression (getSublists jsnList [])))
                (jsnGetSource $ head jsnList)))
        srcSpan
    ]
    where
        -- A JSExpression contains a list of JSNodes, separated by <JSLiteral ','>. These need to be
        -- seperated (basically the <JSLiteral ','>s need to be stripped).
        getSublists :: [JSNode] -> [JSNode] -> [[JSNode]]
        getSublists ((NS (JSLiteral ",") _):rest) current =
            [current] ++ (getSublists rest [])
        getSublists (node:rest) current = getSublists rest (current ++ [node])
        getSublists [] current = [current]
toAST (NS (JSFinally body) srcSpan) =
    [AWSS
        (Finally
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSFor occurs when no varibles are declared in the loop (although they may be re-assigned).
toAST (NS (JSFor vars test count body) srcSpan) =
    [AWSS
        (For
            (liftM jsnToListExp (listToMaybe vars))
            (liftM jsnToListExp (listToMaybe test))
            (liftM jsnToListExp (listToMaybe count))
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSForIn occurs when no variables are declared inside the for statement, and the loop iterates
-- over some object (e.g. an array)
toAST (NS (JSForIn vars obj body) srcSpan) =
    [AWSS
        (ForIn
            (map (identifierGetString . jsnGetNode) vars)
            (jsnToListExp obj)
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSForVar occurs in the case that variables are declared in the loop statement.
toAST (NS (JSForVar vars test count body) srcSpan) =
    [AWSS
        (ForVar
            (map toASTVarDeclaration vars)
            (liftM jsnToListExp (listToMaybe test))
            (liftM jsnToListExp (listToMaybe count))
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSForVarIn occurs when variables are declared inside the for statement and the loop iterates over
-- some object (e.g. an array)
toAST (NS (JSForVarIn var obj body) srcSpan) =
    [AWSS
        (ForVarIn
            (toASTVarDeclaration var)
            (jsnToListExp obj)
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
toAST (NS (JSFunction name inputs body) srcSpan) =
    [AWSS
        (FunctionDeclaration
            (identifierGetString $ jsnGetNode name)
            (map (identifierGetString . jsnGetNode) inputs)
            (AWSS (FunctionBody (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
toAST (NS (JSIf test body) srcSpan) =
    [AWSS
        (If
            (jsnToListExp test)
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
toAST (NS (JSIfElse test trueBody falseBody) srcSpan) =
    [AWSS
        (IfElse
            (jsnToListExp test)
            (AWSS (Block (toAST trueBody)) (jsnGetSource trueBody))
            (AWSS (Block (toAST falseBody)) (jsnGetSource falseBody)))
        srcSpan
    ]
-- TODO: Comment on what a JSLabelled is.
toAST (NS (JSLabelled label body) srcSpan) =
    [AWSS
        (Labelled
            (identifierGetString $ jsnGetNode label)
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- FIXME: Not 100% sure that this is safe.
-- TODO: Comment on where these come from.
toAST (NS (JSLiteral ";") srcSpan) = []
toAST (NS (JSReturn value) srcSpan) =
    [AWSS
        (Return
            (returnValue value))
        srcSpan
    ]
    where
        -- The list in a JSReturn is always either a singleton list containing a semicolon, or a
        -- 2-list containing a JSNode (representing the value to be returned) and a semicolon.
        returnValue :: [JSNode] -> ExprWithSourceSpan
        returnValue [semi] = EWSS (Value JSUndefined) srcSpan
        returnValue [val, semi] = jsnToListExp val
toAST (NS (JSSwitch var cases) srcSpan) =
    [AWSS
        (Switch
            (jsnToListExp var)
            -- TODO: Check the source span for the block.
            (AWSS
                (Block
                    (concat $ map toAST cases))
                (jsnGetSource $ head cases)))
        srcSpan
    ]
toAST (NS (JSThrow expr) srcSpan) =
    [AWSS
        (Statement
            (EWSS (Throw (toASTExpression expr)) srcSpan))
        srcSpan
    ]
    where
        toASTExpression :: JSNode -> ExprWithSourceSpan
        toASTExpression (NS (JSExpression ex) _) = listToASTExpression ex
toAST (NS (JSTry body catchClause) srcSpan) =
    [AWSS
        (Try
            (AWSS
                (Block
                    (toAST body))
                (jsnGetSource body))
            -- TODO: Check the source span for the catch block.
            (AWSS (Block
                (concat $ map toAST catchClause))
                (jsnGetSource $ head catchClause)))
        srcSpan
    ]
toAST (NS (JSWhile test body) srcSpan) =
    [AWSS
        (While
            (jsnToListExp test)
            (AWSS (Block (toAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- Anything else is assumed to be a statement.
toAST jsn =
    [AWSS
        (Statement
            (makeASTExpression jsn))
        (jsnGetSource jsn)
    ]


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
        toASTPropNameValue :: JSNode -> ExprWithSourceSpan
        toASTPropNameValue
            (NS (JSPropertyNameandValue (NS (JSIdentifier name) _) value) srcSpan) =
                EWSS
                    (PropNameValue
                        (VariableProperty name)
                        (listToASTExpression value))
                    srcSpan
        toASTPropNameValue
            (NS (JSPropertyNameandValue (NS (JSDecimal index) _) value) srcSpan) =
                EWSS
                    (PropNameValue
                        (IndexProperty (read index))
                        (listToASTExpression value))
                    srcSpan
toASTValue (NS (JSStringLiteral '"' s) _) = JSDQString s
toASTValue (NS (JSStringLiteral _ s) _) = JSString s


makeASTExpression :: JSNode -> ExprWithSourceSpan
makeASTExpression (NS (JSArguments args) srcSpan) =
    toASTArguments args srcSpan
makeASTExpression (NS (JSExpressionBinary operator left right) srcSpan) =
    EWSS
        (Binary
            operator
            (listToASTExpression left)
            (listToASTExpression right))
        srcSpan
makeASTExpression (NS (JSExpressionParen expr) srcSpan) =
    EWSS
        (ParenExpression
            (jsnToListExp expr))
        srcSpan
makeASTExpression (NS (JSExpressionPostfix operator variable) srcSpan) =
    EWSS
        (UnaryPost
            operator
            (listToASTExpression variable))
        srcSpan
makeASTExpression (NS (JSExpressionTernary expr ifTrue ifFalse) srcSpan) =
    EWSS
        (Ternary
            (listToASTExpression expr)
            (listToASTExpression ifTrue)
            (listToASTExpression ifFalse))
        srcSpan
makeASTExpression (NS (JSFunctionExpression name args body) srcSpan) =
    EWSS
        (FunctionExpression
            (liftM (identifierGetString . jsnGetNode) (listToMaybe name))
            (map (identifierGetString . jsnGetNode) args)
            (AWSS (FunctionBody (toAST body)) (jsnGetSource body)))
        srcSpan
makeASTExpression (NS (JSIdentifier "undefined") srcSpan) =
    EWSS (Value JSUndefined) srcSpan
makeASTExpression (NS (JSIdentifier identifier) srcSpan) =
    EWSS (Identifier identifier) srcSpan
makeASTExpression (NS (JSLiteral "null") srcSpan) =
    EWSS (Value JSNull) srcSpan
makeASTExpression (NS (JSLiteral "this") srcSpan) =
    EWSS (Identifier "this") srcSpan
makeASTExpression (NS (JSMemberDot pre post) srcSpan) =
    EWSS
        (Reference
            (listToASTExpression pre)
            (makeASTExpression post))
        srcSpan
makeASTExpression (NS (JSMemberSquare pre post) srcSpan) =
    EWSS
        (Index
            (listToASTExpression pre)
            (jsnToListExp post))
        srcSpan
-- Anything left unmatched here is assumed to be a literal value.
makeASTExpression val =
    EWSS (Value (toASTValue val)) (jsnGetSource val)
