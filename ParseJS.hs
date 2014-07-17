
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
-- Module takes a JSAST and gives each vertex a unique integer label. The label counter is simply
-- threaded through the tree. Traversal is depth first. It's all fairly straight-forward.


-- This module parses a JavaScript source file using the Language.Javascript.Parser library (Version
-- 0.4.5), then simplifies the parse tree to produce a more useful abstract syntax tree (AST).
--
-- The doc for Language.Javascript.Parser is at hackage.haskell.org/package/language-javascript, but
-- it isn't particularly useful. The library's main data type - Node - has many constructors, and
-- the documentation doesn't tell you what javascript source code elements they correspond to. Best
-- approach is to look at the source.
--
-- Top level function is (getJSASTWithSource (parseTree program file) file)
--
--
-- TODO: Does this file need more comments?


module ParseJS
( Expression(..)
, ExprWithSourceSpan(..)
, Index
, JSAST(..)
, JSASTWithSourceSpan(..)
, Operator
, PropertyName(..)
, SourceFileName
, Value(..)
, Variable
, jsnGetNode
, parseTree
, getJSASTWithSource
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
    , mapMaybe
    )
import Language.JavaScript.Parser (parse)
import Language.JavaScript.Parser.AST


type Index = Int
type Operator = String
type SourceFileName = String
type Variable = String


-- Represent an identifier used to index an object property using square brackets. An object
-- property's identifier can be a string or an integer.
data PropertyName =
    --  Index is an integer literal.
      IndexProperty Index
    -- This is used in the TypeRules module when an object or array is indexed using a variable
    -- insead of a string/integer literal.
    --
    -- If the structure being indexed is an array then this will be resolved to an IntType. If the
    -- structure is an object, then the object is not type safe (so we no longer care about the type
    -- of this index)
    | UnknownProperty
    --  Index is a string literal.
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
    -- TODO: Comment on what the expressions can be.
    | JSObject [ExprWithSourceSpan]
    | JSString String
    | JSUndefined deriving (Show)


-- Represent, approximately, source elements that are expressions. Initially this was for elements
-- that appeared as expressions in the output from language.javascript.parser, but some other things
-- have been added where it made sense. (TODO: What things?)
--
-- None of these contain JSAST fields except for FunctionExpression.
--
-- TODO: Can FunctionExpression be moved into JSAST? (probably not).
data Expression =
      Arguments [ExprWithSourceSpan]
    | Assignment Operator ExprWithSourceSpan ExprWithSourceSpan
    | Binary Operator ExprWithSourceSpan ExprWithSourceSpan
    | Break (Maybe Variable)
    | Call ExprWithSourceSpan ExprWithSourceSpan
    -- In Language.JavaScript, a call expression is an expression that calls - or accesses a
    -- property of - a function call.
    --
    -- E.g. foo()(); Or foo().bar;
    --
    -- However, this program treats foo()() as a Call within a Call (I think that is a sufficient
    -- description for our purposes).
    | CallExpression ExprWithSourceSpan Operator ExprWithSourceSpan
    | Continue (Maybe Variable)
    -- A function expression occurs when a function definition is on the right hand side of some
    -- statement. TODO: Move into JSAST.
    | FunctionExpression (Maybe Variable) [Variable] JSASTWithSourceSpan
    | Identifier Variable
    -- Represents an index into a structure using square brackets.
    | Index ExprWithSourceSpan ExprWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    | List [ExprWithSourceSpan]
    | New ExprWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    | ParenExpression ExprWithSourceSpan
    -- Represents a property of an object.
    | PropNameValue PropertyName ExprWithSourceSpan
    -- Represents a reference to a structure member using a dot.
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
data JSAST =
      Block [JSASTWithSourceSpan]
    | Case ExprWithSourceSpan JSASTWithSourceSpan
    | Catch Variable (Maybe ExprWithSourceSpan) JSASTWithSourceSpan
    | Default JSASTWithSourceSpan
    | DoWhile JSASTWithSourceSpan ExprWithSourceSpan
    | Finally JSASTWithSourceSpan
    | For
        (Maybe ExprWithSourceSpan)
        (Maybe ExprWithSourceSpan)
        (Maybe ExprWithSourceSpan)
        JSASTWithSourceSpan
    | ForIn [Variable] ExprWithSourceSpan JSASTWithSourceSpan
    | ForVar
        [ExprWithSourceSpan]
        (Maybe ExprWithSourceSpan)
        (Maybe ExprWithSourceSpan)
        JSASTWithSourceSpan
    | ForVarIn ExprWithSourceSpan ExprWithSourceSpan JSASTWithSourceSpan
    | FunctionBody [JSASTWithSourceSpan]
    | FunctionDeclaration Variable [Variable] JSASTWithSourceSpan
    | If ExprWithSourceSpan JSASTWithSourceSpan
    | IfElse ExprWithSourceSpan JSASTWithSourceSpan JSASTWithSourceSpan
    | Labelled Variable JSASTWithSourceSpan
    | Return ExprWithSourceSpan
    | Statement ExprWithSourceSpan
    | Switch ExprWithSourceSpan JSASTWithSourceSpan
    | Try JSASTWithSourceSpan JSASTWithSourceSpan
    | While ExprWithSourceSpan JSASTWithSourceSpan deriving (Show)


data JSASTWithSourceSpan = AWSS JSAST SrcSpan deriving (Show)
data ExprWithSourceSpan = EWSS Expression SrcSpan deriving (Show)


getJSASTWithSource :: JSNode -> SourceFileName -> ([JSASTWithSourceSpan], SourceFileName)
getJSASTWithSource jsn fileName = ((toJSAST jsn), fileName)


-- Extract the Node from a JSNode.
jsnGetNode :: JSNode -> Node
jsnGetNode (NS a _) = a


jsnGetSource :: JSNode -> SrcSpan
jsnGetSource (NS _ s) = s


-- Parse JavaScript source code.
parseTree :: String -> SourceFileName -> JSNode
parseTree program fileName = (\(Right a) -> a) $ parse program fileName;


-- Make a List or a ParenExpression from a Statement JSNode.
jsnToListExp :: JSNode -> ExprWithSourceSpan
jsnToListExp jsn =
    statementToListExp $ toJSAST jsn
    where
        statementToListExp :: [JSASTWithSourceSpan] -> ExprWithSourceSpan
        statementToListExp [AWSS (Statement expr) _] = expr


-- Extract a (Maybe List), a (Maybe ParenExpression) or Nothing. Assumes that the first parameter is
-- always a singleton list or empty.
jsnToMaybeListExp :: [JSNode] -> Maybe ExprWithSourceSpan
jsnToMaybeListExp (jsn:[]) = Just $ jsnToListExp jsn
jsnToMaybeListExp [] = Nothing


-- Extract the String value from a JSIdentifier
identifierGetString :: Node -> String
identifierGetString (JSIdentifier jsid) = jsid


-- Make AST representation of a variable declaration.
toJSASTVarDeclaration :: JSNode -> ExprWithSourceSpan
toJSASTVarDeclaration (NS (JSVarDecl name value) srcSpan) =
    EWSS
        (VarDeclaration
            (identifierGetString $ jsnGetNode name)
            (maybeValue value))
        srcSpan
    where
        maybeValue :: [JSNode] -> Maybe ExprWithSourceSpan
        maybeValue [] = Nothing
        maybeValue val = Just $ listToJSASTExpression val


-- Takes a list of Nodes and builds a single expression.
--
-- TODO: Add a comment here explaining where these Node lists come from.
listToJSASTExpression :: [JSNode] -> ExprWithSourceSpan
listToJSASTExpression [item] = makeJSASTExpression item
listToJSASTExpression [(NS (JSUnary operator) srcSpan), (NS (JSDecimal x) _)]
    | (operator == "-") =
        if (elem '.' x) then
            EWSS (Value xFloat) srcSpan
        else
            EWSS (Value xInt) srcSpan
        where
            xFloat = JSFloat (-1 * (read x))
            xInt = JSInt (-1 * (read x))
listToJSASTExpression ((NS (JSUnary operator) srcSpan):x)
    | elem operator ["-", "+", "--", "++", "!", "typeof ", "delete ", "~"] =
        EWSS
            (UnaryPre
                operator
                (listToJSASTExpression x))
            srcSpan
listToJSASTExpression ((NS (JSLiteral "new ") srcSpan):x) =
    EWSS (New (listToJSASTExpression x)) srcSpan
listToJSASTExpression [x, (NS (JSArguments args) srcSpan)] =
    EWSS
        (Call
            (makeJSASTExpression x)
            (toJSASTArguments args srcSpan))
        (jsnGetSource x)
listToJSASTExpression list | (isCallExpression $ last list) =
    getJSASTCallExpression list
    where
        isCallExpression :: JSNode -> Bool
        isCallExpression (NS (JSCallExpression "." _) _) = True
        isCallExpression (NS (JSCallExpression "[]" _) _) = True
        isCallExpression _ = False
listToJSASTExpression list | (isParenCallExp $ last list) =
    getJSASTCall list
    where
        isParenCallExp :: JSNode -> Bool
        isParenCallExp (NS (JSCallExpression "()" _) _) = True
        isParenCallExp _ = False
-- FIXME: Anything else is assumed to be an assignment. Verify that this is correct.
listToJSASTExpression list =
    getJSASTAssignment list []
    where
        isAssignmentOperator :: String -> Bool
        isAssignmentOperator op
            | elem op ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="] =
                True
            | otherwise = False
        getJSASTAssignment :: [JSNode] -> [JSNode] -> ExprWithSourceSpan
        getJSASTAssignment ((NS (JSOperator op) srcSpan):xs) preCurrent
            | (isAssignmentOperator op) =
                EWSS
                    (Assignment
                        op
                        (listToJSASTExpression preCurrent)
                        (listToJSASTExpression xs))
                    (jsnGetSource $ head preCurrent)
        getJSASTAssignment (x:xs) preCurrent = getJSASTAssignment xs (preCurrent ++ [x])
        -- FIXME: Not used. Remove?
        isAssignment :: [JSNode] -> Bool
        isAssignment [] = False
        isAssignment ((NS (JSOperator op) _):ls) = (isAssignmentOperator op) || (isAssignment ls)
        isAssignment (_:ls) = isAssignment ls


-- Makes arguments from a list of lists of JSNodes that represent a list of arguments.
toJSASTArguments :: [[JSNode]] -> SrcSpan -> ExprWithSourceSpan
toJSASTArguments args srcSpan =
    EWSS (Arguments (map getJSASTArgument args)) srcSpan
    where
        getJSASTArgument :: [JSNode] -> ExprWithSourceSpan
        getJSASTArgument (item:[]) = makeJSASTExpression item
        getJSASTArgument nodes = listToJSASTExpression nodes


-- To handle the case where the last element of a list is a (JSCallExpression "()" [JSArguments
-- _]) I don't know if the second field can be anything other than a singleton list containing a
-- JSArguments but for now I'm just going to hope not.
--
-- TODO: Just look at the parser source to work this out
getJSASTCall :: [JSNode] -> ExprWithSourceSpan
getJSASTCall list =
    EWSS
        (Call
            (listToJSASTExpression (init list))
            (getArgs $ last list))
        (jsnGetSource $ head list)
    where
        getArgs :: JSNode -> ExprWithSourceSpan
        getArgs (NS (JSCallExpression _ [(NS (JSArguments args) srcSpan)]) _) =
            toJSASTArguments args srcSpan


-- To handle the case where the last element in the list is a (JSCallExpression "[]" exp) or a
-- (JSCallExpression "." exp).
getJSASTCallExpression :: [JSNode] -> ExprWithSourceSpan
getJSASTCallExpression list =
    EWSS
        (CallExpression
            (listToJSASTExpression (init list))
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
            makeJSASTExpression expr


-- Take a node in the parse tree and make an abstract syntax tree.
toJSAST :: JSNode -> [JSASTWithSourceSpan]
-- These ones return a proper list of JSASTs. (Haskell Land) Constructors which use one of these to
-- fill a field must have a [JSAST] for that field.
toJSAST (NS (JSBlock jsnode) _) = toJSAST jsnode
toJSAST (NS (JSFunctionBody bodyList) _) = concat $ map toJSAST bodyList
toJSAST (NS (JSSourceElements elementsList) _) = concat $ map toJSAST elementsList
toJSAST (NS (JSSourceElementsTop topList) _) = concat $ map toJSAST topList
toJSAST (NS (JSStatementBlock item) _) = toJSAST item
toJSAST (NS (JSStatementList statList) _) = concat $ map toJSAST statList
toJSAST (NS (JSVariables _ varDecs) srcSpan) =
    map (\jsn -> AWSS (Statement (toJSASTVarDeclaration jsn)) srcSpan) varDecs
-- These ones always return singleton lists. (Haskell Land) Constructors which use only these to
-- fill a field can have a JSAST for that field.
toJSAST (NS (JSBreak [] _) srcSpan) =
    [AWSS
        (Statement
            (EWSS (Break Nothing) srcSpan))
        srcSpan
    ]
toJSAST (NS (JSBreak [label] _) srcSpan) =
    [AWSS
        (Statement
            (EWSS (Break (Just $ identifierGetString $ jsnGetNode label)) srcSpan))
        srcSpan
    ]
toJSAST (NS (JSCase cs body) srcSpan) =
    [AWSS
        (Case
            (jsnToListExp cs)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
toJSAST (NS (JSCatch var test body) srcSpan) =
    [AWSS
        (Catch
            (identifierGetString $ jsnGetNode var)
            (listToMaybeExpression test)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
    where
        listToMaybeExpression :: [JSNode] -> Maybe ExprWithSourceSpan
        listToMaybeExpression [] = Nothing
        listToMaybeExpression jsn = Just $ listToJSASTExpression jsn
toJSAST (NS (JSContinue [item]) srcSpan) =
    [AWSS
        (Statement
            (EWSS (Continue Nothing) srcSpan))
        srcSpan
    ]
toJSAST (NS (JSContinue [label, semi]) srcSpan) =
    [AWSS
        (Statement
            (EWSS (Continue (Just $ identifierGetString $ jsnGetNode label)) srcSpan))
        srcSpan
    ]
toJSAST (NS (JSDefault body) srcSpan) =
    [AWSS
        (Default
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- TODO: Look at the parser source and find out what "something" actually is
toJSAST (NS (JSDoWhile body test something) srcSpan) =
    [AWSS
        (DoWhile
            (AWSS (Block (toJSAST body)) (jsnGetSource body))
            (jsnToListExp test))
        srcSpan
    ]
-- TODO: Comment on where JSExpressions come from.
toJSAST (NS (JSExpression jsnList) srcSpan) =
    [AWSS
        (Statement
            (EWSS
                (List (map listToJSASTExpression (getSublists jsnList [])))
                -- FIXME: Might not be the right source fragment for list.
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
toJSAST (NS (JSFinally body) srcSpan) =
    [AWSS
        (Finally
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSFor occurs when no varibles are declared in the loop (although they may be re-assigned).
toJSAST (NS (JSFor vars test count body) srcSpan) =
    [AWSS
        (For
            (jsnToMaybeListExp vars)
            (jsnToMaybeListExp test)
            (jsnToMaybeListExp count)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSForIn occurs when no variables are declared inside the for statement, and the loop iterates
-- over some object (e.g. an array)
toJSAST (NS (JSForIn vars obj body) srcSpan) =
    [AWSS
        (ForIn
            (map (identifierGetString . jsnGetNode) vars)
            (jsnToListExp obj)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSForVar occurs in the case that variables are declared in the loop statement.
toJSAST (NS (JSForVar vars test count body) srcSpan) =
    [AWSS
        (ForVar
            (map toJSASTVarDeclaration vars)
            (jsnToMaybeListExp test)
            (jsnToMaybeListExp count)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- JSForVarIn occurs when variables are declared inside the for statement and the loop iterates over
-- some object (e.g. an array)
toJSAST (NS (JSForVarIn var obj body) srcSpan) =
    [AWSS
        (ForVarIn
            (toJSASTVarDeclaration var)
            (jsnToListExp obj)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
toJSAST (NS (JSFunction name inputs body) srcSpan) =
    [AWSS
        (FunctionDeclaration
            (identifierGetString $ jsnGetNode name)
            (map (identifierGetString . jsnGetNode) inputs)
            (AWSS (FunctionBody (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
toJSAST (NS (JSIf test body) srcSpan) =
    [AWSS
        (If
            (jsnToListExp test)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
toJSAST (NS (JSIfElse test trueBody falseBody) srcSpan) =
    [AWSS
        (IfElse
            (jsnToListExp test)
            (AWSS (Block (toJSAST trueBody)) (jsnGetSource trueBody))
            (AWSS (Block (toJSAST falseBody)) (jsnGetSource falseBody)))
        srcSpan
    ]
-- TODO: Comment on what a JSLabelled is.
toJSAST (NS (JSLabelled label body) srcSpan) =
    [AWSS
        (Labelled
            (identifierGetString $ jsnGetNode label)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- TODO: Not 100% sure that this is safe.
-- TODO: Comment on where these come frome.
toJSAST (NS (JSLiteral ";") srcSpan) = []
toJSAST (NS (JSReturn [item]) srcSpan) =
    [AWSS
        (Return
            (EWSS (Value JSUndefined) srcSpan))
        srcSpan
    ]
toJSAST (NS (JSReturn [val, semi]) srcSpan) =
    [AWSS
        (Return
            (jsnToListExp val))
        srcSpan
    ]
toJSAST (NS (JSSwitch var cases) srcSpan) =
    [AWSS
        (Switch
            (jsnToListExp var)
            -- FIXME: Probably not the right source span for the block
            (AWSS
                (Block
                    (concat $ map toJSAST cases))
                (jsnGetSource $ head cases)))
        srcSpan
    ]
toJSAST (NS (JSThrow expr) srcSpan) =
    [AWSS
        (Statement
            (EWSS (Throw (toJSASTExpression expr)) srcSpan))
        srcSpan
    ]
    where
        toJSASTExpression :: JSNode -> ExprWithSourceSpan
        toJSASTExpression (NS (JSExpression ex) _) = listToJSASTExpression ex
toJSAST (NS (JSTry body catchClause) srcSpan) =
    [AWSS
        (Try
            (AWSS
                (Block
                    (toJSAST body))
                (jsnGetSource body))
            -- FIXME: Probably not the right source span for the block
            (AWSS (Block
                (concat $ map toJSAST catchClause))
                (jsnGetSource $ head catchClause)))
        srcSpan
    ]
toJSAST (NS (JSWhile test body) srcSpan) =
    [AWSS
        (While
            (jsnToListExp test)
            (AWSS (Block (toJSAST body)) (jsnGetSource body)))
        srcSpan
    ]
-- Anything else is assumed to be a statement.
-- TODO: Check that all parser output is covered.
toJSAST jsn =
    [AWSS
        (Statement
            (makeJSASTExpression jsn))
        (jsnGetSource jsn)
    ]


-- *************************************************************************************************
--
-- These functions are used to process array literals.
--
-- The way that the parser (Grammar5.y) handles commas in arrays seems strange to me. One comma at
-- the end of an array makes a (JSLiteral \",\"). A comma in any other scenario (that I have tried)
-- makes a (JSElision []). Two commas make two elisions, three commas three elisions, and so on,
-- EXCEPT when they occur at the end of the array; there, the last comma is ignored and (n - 1)
-- elisions are made. The following are some examples of parser output.
-- []
-- JSArrayLiteral []
--
-- [1,]
-- JSArrayLiteral [
--     JSDecimal \"1\",
--     JSLiteral \",\"]
--
-- [,]
-- JSArrayLiteral [
--     JSElision []]
--
-- [,,]
-- JSArrayLiteral [
--     JSElision [],
--     JSElision []]
--
-- [1,,]
-- JSArrayLiteral [
--     JSDecimal \"1\",
--     JSElision []]
--
-- [1]
-- JSArrayLiteral [
--     JSDecimal \"1\"]
--
-- [1,,2]
-- JSArrayLiteral [
--     JSDecimal \"1\",
--     JSElision [],
--     JSElision [],
--     JSDecimal \"2\"]
--
-- JSArrayLiteral [
--     JSElision [],
--     JSDecimal \"1\"]
-- [,1]
--
-- [1, 2]
-- JSArrayLiteral [
--     JSDecimal \"1\",
--     JSElision [],
--     JSDecimal \"2\"]
--
-- [,,1]
-- JSArrayLiteral [
--     JSElision [],
--     JSElision [],
--     JSDecimal \"1\"]
--
-- [,,,1]
-- JSArrayLiteral [
--     JSElision [],
--     JSElision [],
--     JSElision [],
--     JSDecimal \"1\"]
--
-- [1,,,]
-- JSArrayLiteral [
--     JSDecimal \"1\",
--     JSElision [],
--     JSElision []]
--
-- Takes a representation of a JS array and produces a singleton list containing the next element,
-- paired with the remainder of the array.
-- TODO: Try to fix source spans.
-- FIXME: Some of these might be redundant.
getElements :: [JSNode] -> [[JSNode]] -> SrcSpan -> [[JSNode]]
-- Ignore one trailing comma at the end of the array.
getElements [(NS (JSLiteral ",") _)] current _ = current
-- Single elisions at the end of the parsed array occur when there are two commas at the end of an
-- array or when the array is equal to [,]
getElements [(NS (JSElision e) srcSpan)] current nearestSpan =
    current ++ [[(NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan nearestSpan))]]
-- The last (or only) item in the array is not an elision.
getElements [item] current nearestSpan = current ++ [[item]]
-- Two elisions in a row that aren't at the beginning of the array indicates one undefined entry,
-- then a comma seperator, then the next entry.
--
-- (Two elisions at the beggining of the parsed array indicate two undefined entries, but that case
-- (is handled in jsArrayGetSubarray).
getElements ((NS (JSElision _) srcSpan1):(NS (JSElision e) srcSpan2):rest) current nearestSpan  =
    getElements
        ((NS (JSElision e) srcSpan2):rest)
        (current
        ++ [[NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan1 nearestSpan)]])
        (getNearestSrcSpan srcSpan2 (getNearestSrcSpan srcSpan1 nearestSpan))
-- One elision and then a non-elision entry indicates a comma seperator and then the entry.
getElements ((NS (JSElision _) srcSpan):rest) current nearestSpan =
    getElements rest current (getNearestSrcSpan srcSpan nearestSpan)
-- An entry and then an elision and then another entry. The elision is a seperator.
getElements (y:(NS (JSElision e) srcSpan):rest) current nearestSpan =
    getElements
        ((NS (JSElision e) srcSpan):rest)
        (current ++ [[y]])
        (getNearestSrcSpan srcSpan nearestSpan)
-- Not the last item in the list, and not an elision.
getElements (jsn:rest) current nearestSpan =
    getElements
        rest
        (current ++ [[jsn]])
        (getNearestSrcSpan (jsnGetSource jsn) nearestSpan)


-- Takes the parse tree representation of an array literal, deals with elisions at the start of the
-- array, then processes what's left.
processArray :: [JSNode] -> [[JSNode]] -> SrcSpan -> [[JSNode]]
processArray [] current _ = current
processArray ((NS (JSElision es) srcSpan):rest) current nearestSpan =
    processArray
        rest
        (current ++ [[NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan nearestSpan)]])
        (getNearestSrcSpan srcSpan nearestSpan)
processArray jsa current nearestSpan = (getElements jsa current nearestSpan)


jsArrayGetSubarrays :: [JSNode] -> SrcSpan -> [[JSNode]]
jsArrayGetSubarrays [] _ = []
jsArrayGetSubarrays jsa nearestSpan = (processArray jsa [] nearestSpan)

getNearestSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
getNearestSrcSpan (SpanEmpty) s = s
getNearestSrcSpan s _ = s


getNearestSpanNext :: [[JSNode]] -> SrcSpan -> SrcSpan
getNearestSpanNext [] nearestSpan = nearestSpan
getNearestSpanNext ls nearestSpan =
    if last ls == [] then
        nearestSpan
    else
        getNearestSrcSpan (jsnGetSource $ last $ last ls) nearestSpan

-- *************************************************************************************************

-- Takes a Node that represents a literal value and makes an AST node for that value.
toJSASTValue :: JSNode -> Value
toJSASTValue (NS (JSArrayLiteral arr) srcSpan) =
    JSArray
        -- (map listToJSASTExpression (jsArrayGetSubarrays arr [] srcSpan))
        (map listToJSASTExpression (jsArrayGetSubarrays arr srcSpan))
toJSASTValue (NS (JSDecimal s) _) =
    if elem '.' s then
        JSFloat (read s)
    else
        JSInt (read s)
toJSASTValue (NS (JSLiteral "false") _) = JSBool False
toJSASTValue (NS (JSLiteral "true") _) = JSBool True
-- FIXME: Refactor this? (already done but still kinda crazy)
toJSASTValue (NS (JSObjectLiteral list) _) =
    JSObject
        (concat $ map toJSASTPropNameValue list)
    where
        -- Takes a Node that represents a property of an object and produdes a singleton list
        -- containing a PropNameValue Expression.
        toJSASTPropNameValue :: JSNode -> [ExprWithSourceSpan]
        toJSASTPropNameValue
            (NS (JSPropertyNameandValue (NS (JSIdentifier name) _) value) srcSpan) =
                [EWSS
                    (PropNameValue
                        (VariableProperty name)
                        (listToJSASTExpression value))
                    srcSpan
                ]
        toJSASTPropNameValue
            (NS (JSPropertyNameandValue (NS (JSDecimal index) _) value) srcSpan) =
                [EWSS
                    (PropNameValue
                        (IndexProperty (read index))
                        (listToJSASTExpression value))
                    srcSpan
                ]
        toJSASTPropNameValue _ = []
toJSASTValue (NS (JSStringLiteral '"' s) _) = JSDQString s
toJSASTValue (NS (JSStringLiteral _ s) _) = JSString s


-- Takes a single Node and builds a single expression.
makeJSASTExpression :: JSNode -> ExprWithSourceSpan
makeJSASTExpression (NS (JSArguments args) srcSpan) =
    toJSASTArguments args srcSpan
makeJSASTExpression (NS (JSExpressionBinary operator left right) srcSpan) =
    EWSS
        (Binary
            operator
            (listToJSASTExpression left)
            (listToJSASTExpression right))
        srcSpan
makeJSASTExpression (NS (JSExpressionParen expr) srcSpan) =
    EWSS
        (ParenExpression
            (jsnToListExp expr))
        srcSpan
makeJSASTExpression (NS (JSExpressionPostfix operator variable) srcSpan) =
    EWSS
        (UnaryPost
            operator
            (listToJSASTExpression variable))
        srcSpan
makeJSASTExpression (NS (JSExpressionTernary expr ifTrue ifFalse) srcSpan) =
    EWSS
        (Ternary
            (listToJSASTExpression expr)
            (listToJSASTExpression ifTrue)
            (listToJSASTExpression ifFalse))
        srcSpan
makeJSASTExpression (NS (JSFunctionExpression [] args body) srcSpan) =
    EWSS
        (FunctionExpression
            Nothing
            (map (identifierGetString . jsnGetNode) args)
            (AWSS (FunctionBody (toJSAST body)) (jsnGetSource body)))
        srcSpan
makeJSASTExpression (NS (JSFunctionExpression [name] args body) srcSpan) =
    EWSS
        (FunctionExpression
            (Just $ identifierGetString $ jsnGetNode name)
            (map (identifierGetString . jsnGetNode) args)
            (AWSS (FunctionBody (toJSAST body)) (jsnGetSource body)))
        srcSpan
makeJSASTExpression (NS (JSIdentifier "undefined") srcSpan) =
    EWSS (Value JSUndefined) srcSpan
makeJSASTExpression (NS (JSIdentifier identifier) srcSpan) =
    EWSS (Identifier identifier) srcSpan
makeJSASTExpression (NS (JSLiteral "null") srcSpan) =
    EWSS (Value JSNull) srcSpan
makeJSASTExpression (NS (JSLiteral "this") srcSpan) =
    EWSS (Identifier "this") srcSpan
makeJSASTExpression (NS (JSMemberDot pre post) srcSpan) =
    EWSS
        (Reference
            (listToJSASTExpression pre)
            (makeJSASTExpression post))
        srcSpan
makeJSASTExpression (NS (JSMemberSquare pre post) srcSpan) =
    EWSS
        (Index
            (listToJSASTExpression pre)
            (jsnToListExp post))
        srcSpan
-- Anything left unmatched here is assumed to be a literal value.
makeJSASTExpression val =
    EWSS (Value (toJSASTValue val)) (jsnGetSource val)
