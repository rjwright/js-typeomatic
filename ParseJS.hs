
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


-- This module parses a JavaScript source file using the Language.Javascript.Parser library, then
-- simplifies the parse tree to produce a more useful abstract syntax tree (AST).
--
-- The doc for Language.Javascript.Parser is at hackage.haskell.org/package/language-javascript, but
-- it isn't particularly useful. The library's main data type - Node - has many constructors, and
-- the documentation doesn't tell you what javascript source code elements they correspond to. Best
-- approach is to look at the source.
--
-- Top level function is (toJSAST (parseTree program file) file)
--
-- FIXME: fileName is threaded throughout because the parser doesn't actually use the file name -_-
-- This threading can probably be avoided.
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
, toJSAST
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
-- None of these contain JSAST fields except for FunctionExpression. TODO: Should FunctionExpression
-- be moved into JSAST?
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
-- Expression data type (Statement). They should be moved into Expression.
data JSAST =
      Block [JSASTWithSourceSpan]
    | Case ExprWithSourceSpan JSASTWithSourceSpan
    | Catch Variable (Maybe ExprWithSourceSpan) JSASTWithSourceSpan
    | Default JSASTWithSourceSpan
    | DoWhile JSASTWithSourceSpan ExprWithSourceSpan
    | Finally JSASTWithSourceSpan
    | For (Maybe ExprWithSourceSpan) (Maybe ExprWithSourceSpan) (Maybe ExprWithSourceSpan) JSASTWithSourceSpan
    | ForIn [Variable] ExprWithSourceSpan JSASTWithSourceSpan
    | ForVar [ExprWithSourceSpan] (Maybe ExprWithSourceSpan) (Maybe ExprWithSourceSpan) JSASTWithSourceSpan
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


-- These two data types were added later, when I decided I wanted to keep source code information in
-- the tree. The approach might not be the best. An oppinion from a fresh reviewer could be helpful.
data JSASTWithSourceSpan = AWSS JSAST SrcSpan SourceFileName deriving (Show)
data ExprWithSourceSpan = EWSS Expression SrcSpan SourceFileName deriving (Show)


-- Extract the Node from a JSNode.
jsnGetNode :: JSNode -> Node
jsnGetNode (NS a _) = a


jsnGetSource :: JSNode -> SrcSpan
jsnGetSource (NS _ s) = s


-- Parse JavaScript source code.
parseTree :: String -> SourceFileName -> JSNode
parseTree program fileName = (\(Right a) -> a) $ parse program fileName;


-- TODO: Is this still needed?
-- FIXME: Expecially won't be needed if the fileName argument is removed from toJSAST.
astMap :: [JSNode] -> SourceFileName -> [JSASTWithSourceSpan]
astMap jsnList fileName = concat $ map (\jsn -> toJSAST jsn fileName) jsnList


-- Make a List or a ParenExpression from a Statement JSNode.
jsnToListExp :: JSNode -> SourceFileName -> ExprWithSourceSpan
jsnToListExp jsn fileName =
    statementToListExp $ toJSAST jsn fileName
    where
        statementToListExp [AWSS (Statement expr) _ _] = expr


-- Extract a (Maybe List), a (Maybe ParenExpression) or Nothing. Assumes that the first parameter is
-- always a singleton list or empty.
jsnToMaybeListExp :: [JSNode] -> SourceFileName -> Maybe ExprWithSourceSpan
jsnToMaybeListExp (jsn:[]) fileName = Just $ jsnToListExp jsn fileName
jsnToMaybeListExp [] _ = Nothing


-- Extract the String value from a JSIdentifier
identifierGetString :: Node -> String
identifierGetString (JSIdentifier jsid) = jsid


-- Make AST representation of a variable declaration.
toJSASTVarDeclaration :: JSNode -> SourceFileName -> ExprWithSourceSpan
toJSASTVarDeclaration (NS (JSVarDecl name value) srcSpan) fileName =
    EWSS
        (VarDeclaration
            (identifierGetString $ jsnGetNode name)
            (maybeValue value))
        srcSpan
        fileName
    where
        maybeValue [] = Nothing
        maybeValue val = Just $ listToJSASTExpression val fileName


-- Take a node in the parse tree and make an abstract syntax tree.
toJSAST :: JSNode -> SourceFileName -> [JSASTWithSourceSpan]
-- These ones return a proper list of JSASTs. (Haskell Land) Constructors which use one of these to
-- fill a field must have a [JSAST] for that field.
toJSAST (NS (JSSourceElementsTop topList) _) fileName = astMap topList fileName
toJSAST (NS (JSSourceElements elementsList) _) fileName = astMap elementsList fileName
toJSAST (NS (JSFunctionBody bodyList) _) fileName = astMap bodyList fileName
toJSAST (NS (JSStatementList statList) _) fileName = astMap statList fileName
toJSAST (NS (JSBlock jsnode) _) fileName = toJSAST jsnode fileName
toJSAST (NS (JSStatementBlock item) _) fileName = toJSAST item fileName
toJSAST (NS (JSVariables _ varDecs) srcSpan) fileName =
    map (\jsn -> AWSS (Statement (toJSASTVarDeclaration jsn fileName)) srcSpan fileName) varDecs
-- These ones always return singleton lists. (Haskell Land) Constructors which use only these to
-- fill a field can have a JSAST for that field.

-- TODO: Comment on where these come from!
toJSAST (NS (JSExpression jsnList) srcSpan) fileName =
    [AWSS
        (Statement
            (EWSS
                (List (map (\l -> listToJSASTExpression l fileName) (getSublists jsnList [])))
                -- FIXME: Might not be the right source fragment for list.
                (jsnGetSource $ head jsnList)
                fileName))
        srcSpan
        fileName
    ]
    where
        -- A JSExpression contains a list of JSNodes, separated by <JSLiteral ','>. These need to be
        -- seperated (basically the <JSLiteral ','>s need to be stripped).
        getSublists :: [JSNode] -> [JSNode] -> [[JSNode]]
        getSublists ((NS (JSLiteral ",") _):rest) current =
            [current] ++ (getSublists rest [])
        getSublists (node:rest) current = getSublists rest (current ++ [node])
        getSublists [] current = [current]
toJSAST (NS (JSFunction name inputs body) srcSpan) fileName =
    [AWSS
        (FunctionDeclaration
            (identifierGetString $ jsnGetNode name)
            (map (identifierGetString . jsnGetNode) inputs)
            (AWSS (FunctionBody (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSLabelled label body) srcSpan) fileName =
    [AWSS
        (Labelled
            (identifierGetString $ jsnGetNode label)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSBreak [] _) srcSpan) fileName =
    [AWSS
        (Statement
            (EWSS (Break Nothing) srcSpan fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSBreak [label] _) srcSpan) fileName =
    [AWSS
        (Statement
            (EWSS (Break (Just $ identifierGetString $ jsnGetNode label)) srcSpan fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSContinue [item]) srcSpan) fileName =
    [AWSS
        (Statement
            (EWSS (Continue Nothing) srcSpan fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSContinue [label, semi]) srcSpan) fileName =
    [AWSS
        (Statement
            (EWSS (Continue (Just $ identifierGetString $ jsnGetNode label)) srcSpan fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSThrow expr) srcSpan) fileName =
    [AWSS
        (Statement
            (EWSS (Throw (toJSASTExpression (jsnGetNode expr) fileName)) srcSpan fileName))
        srcSpan
        fileName
    ]
    where
        toJSASTExpression (JSExpression ex) = listToJSASTExpression ex
-- JSForVar occurs in the case that variables are declared in the loop statement.
toJSAST (NS (JSForVar vars test count body) srcSpan) fileName =
    [AWSS
        (ForVar
            (map (\v -> toJSASTVarDeclaration v fileName) vars)
            (jsnToMaybeListExp test fileName)
            (jsnToMaybeListExp count fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
-- JSFor occurs when no varibles are declared in the loop (although they may be re-assigned).
toJSAST (NS (JSFor vars test count body) srcSpan) fileName =
    [AWSS
        (For
            (jsnToMaybeListExp vars fileName)
            (jsnToMaybeListExp test fileName)
            (jsnToMaybeListExp count fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
-- JSForIn occurs when no variables are declared inside the for statement, and the loop iterates
-- over some object (e.g. an array)
toJSAST (NS (JSForIn vars obj body) srcSpan) fileName =
    [AWSS
        (ForIn
            (map (identifierGetString . jsnGetNode) vars)
            (jsnToListExp obj fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
-- JSForVarIn occurs when variables are declared inside the for statement and the loop iterates over
-- some object (e.g. an array)
toJSAST (NS (JSForVarIn var obj body) srcSpan) fileName =
    [AWSS
        (ForVarIn
            (toJSASTVarDeclaration var fileName)
            (jsnToListExp obj fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSWhile test body) srcSpan) fileName =
    [AWSS
        (While
            (jsnToListExp test fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
-- TODO: Look at the parser source and find out what "something" actually is
toJSAST (NS (JSDoWhile body test something) srcSpan) fileName =
    [AWSS
        (DoWhile
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName)
            (jsnToListExp test fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSIf test body) srcSpan) fileName =
    [AWSS
        (If
            (jsnToListExp test fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSIfElse test trueBody falseBody) srcSpan) fileName =
    [AWSS
        (IfElse
            (jsnToListExp test fileName)
            (AWSS (Block (toJSAST trueBody fileName)) (jsnGetSource trueBody) fileName)
            (AWSS (Block (toJSAST falseBody fileName)) (jsnGetSource falseBody) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSSwitch var cases) srcSpan) fileName =
    [AWSS
        (Switch
            (jsnToListExp var fileName)
            -- FIXME: Probably not the right source span for the block
            (AWSS
                (Block
                    (astMap cases fileName))
                (jsnGetSource $ head cases)
                fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSCase cs body) srcSpan) fileName =
    [AWSS
        (Case
            (jsnToListExp cs fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSDefault body) srcSpan) fileName =
    [AWSS
        (Default
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSTry body catchClause) srcSpan) fileName =
    [AWSS
        (Try
            (AWSS
                (Block
                    (toJSAST body fileName))
                (jsnGetSource body)
                fileName)
            -- FIXME: Probably not the right source span for the block
            (AWSS (Block
                (astMap catchClause fileName))
                (jsnGetSource $ head catchClause)
                fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSCatch var test body) srcSpan) fileName =
    [AWSS
        (Catch
            (identifierGetString $ jsnGetNode var)
            (listToMaybeExpression test fileName)
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
    where
        listToMaybeExpression [] _ = Nothing
        listToMaybeExpression jsn fileName = Just $ listToJSASTExpression jsn fileName
toJSAST (NS (JSFinally body) srcSpan) fileName =
    [AWSS
        (Finally
            (AWSS (Block (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSReturn [item]) srcSpan) fileName =
    [AWSS
        (Return
            (EWSS (Value JSUndefined) srcSpan fileName))
        srcSpan
        fileName
    ]
toJSAST (NS (JSReturn [val, semi]) srcSpan) fileName =
    [AWSS
        (Return
            (jsnToListExp val fileName))
        srcSpan
        fileName
    ]
-- TODO: Not 100% sure that this is safe.
toJSAST (NS (JSLiteral ";") srcSpan) fileName = []
toJSAST x fileName =
    [AWSS
        (Statement
            (makeJSASTExpression x fileName))
        (jsnGetSource x)
        fileName
    ]

----------------------------------------------------------------------------------------------------
-- *************************************************************************************************
--
-- These functions are used to process array literals. Elisions in array literals are a pain in the
-- butt. This code is kind of bad and took me an insane amount of time to write.
--
-- *************************************************************************************************

-- Takes a representation of a JS array and produces a singleton list containing the next element,
-- paired with the remainder of the array.
--
-- TODO: This is similar to getSublists (in toJSAST). Have a look at that.
--
-- FIXME: This has been changed from [Node] -> ([Node], [Node]) to [JSNode] -> ([JSNode], JSNode]).
-- Some errors may have arisen in the process. Check this, especially the source spans.
getSubarray :: [JSNode] -> ([JSNode], [JSNode])
-- If there is nothing left in the input array then return nothing.
getSubarray [] = ([], [])
-- Ignore one trailing comma at the end of the array.
getSubarray [(NS (JSLiteral ",") _)] = ([], [])
-- I don't remember how we end up with a single elision, but apparently it can happen.
getSubarray [(NS (JSElision e) srcSpan)] = ([(NS (JSIdentifier "undefined") srcSpan)], [])
-- Process the last (or only) item in the array.
getSubarray [item] = ([item], [])
-- Two elisions in a row that aren't at the beginning of the array indicates one undefined entry,
-- then a comma seperator, then the next entry.
getSubarray ((NS (JSElision _) srcSpan1):(NS (JSElision e) srcSpan2):rest) =
    ([(NS (JSIdentifier "undefined") srcSpan1)], ((NS (JSElision e) srcSpan2):rest))
-- One elision and then a non-elision entry indicates a comma seperator and then the entry.
getSubarray ((NS (JSElision _) _):rest) = getSubarray rest
-- An entry and then an elision and then another entry. The elision is a seperator.
getSubarray (y:(NS (JSElision e) srcSpan):rest) = ([y], ((NS (JSElision e) srcSpan):rest))
getSubarray (y:ys) = let (next, rest) = getSubarray ys in (y:(next), rest)


-- Takes an array and makes a (Haskell Land) 2D list, one sublist per element of the JS array.
arrayToSubarrays :: [JSNode] -> [[JSNode]]
arrayToSubarrays [] = []
arrayToSubarrays ls = let (nextSub, rest) = getSubarray ls in ([nextSub] ++ (arrayToSubarrays rest))


-- Takes the parse tree representation of an array literal, deals with elisions at the start of the
-- array, then processes what's left.
jsArrayGetSubarray :: [JSNode] -> [[JSNode]]
jsArrayGetSubarray jsa =
    let (el, rest) = getLeadingElisions jsa in (el ++ (arrayToSubarrays rest))
    where
        getLeadingElisions [] = ([], [])
        getLeadingElisions ((NS (JSElision es) srcSpan):x) =
            let (e, rest) = getLeadingElisions x
                in (([(NS (JSIdentifier "undefined") srcSpan)]:e), rest)
        getLeadingElisions lst = ([], lst)
----------------------------------------------------------------------------------------------------



-- Takes a list of Nodes and builds a single expression.
--
-- TODO: Add a comment here explaining where these Node lists come from.
listToJSASTExpression :: [JSNode] -> SourceFileName -> ExprWithSourceSpan
listToJSASTExpression [item] fileName = makeJSASTExpression item fileName
-- FIXME: This is very ugly
listToJSASTExpression [(NS (JSUnary operator) srcSpan1), (NS (JSDecimal x) srcSpan2)] fileName
    | elem operator ["-"] =
        let y = if elem '.' x then
                    JSFloat (-1 * (read x))
                else
                    -- FIXME: Might not be the right source span for the expression
                    JSInt (-1 * (read x)) in EWSS (Value y) srcSpan1 fileName
listToJSASTExpression ((NS (JSUnary operator) srcSpan):x) fileName
    | elem operator ["-", "+", "--", "++", "!", "typeof ", "delete ", "~"] =
        EWSS
            (UnaryPre
                operator
                (listToJSASTExpression x fileName))
            srcSpan
            fileName
listToJSASTExpression ((NS (JSLiteral "new ") srcSpan):x) fileName =
    EWSS (New (listToJSASTExpression x fileName)) srcSpan fileName
listToJSASTExpression [x, (NS (JSArguments args) srcSpan)] fileName =
    EWSS
        (Call
            (makeJSASTExpression x fileName)
            (toJSASTArguments args fileName srcSpan))
        (jsnGetSource x)
        fileName
listToJSASTExpression list fileName | (isCallExpression $ last list) =
    getJSASTCallExpression list fileName
    where
        isCallExpression :: JSNode -> Bool
        isCallExpression (NS (JSCallExpression "." _) _) = True
        isCallExpression (NS (JSCallExpression "[]" _) _) = True
        isCallExpression _ = False
listToJSASTExpression list fileName | (isParenCallExp $ last list) =
    getJSASTCall list fileName
    where
        isParenCallExp :: JSNode -> Bool
        isParenCallExp (NS (JSCallExpression "()" _) _) = True
        isParenCallExp _ = False
-- FIXME: Anything else is assumed to be an assignment. Verify that this is correct.
listToJSASTExpression list fileName =
    getJSASTAssignment list []
    where
        isAssignmentOperator :: String -> Bool
        isAssignmentOperator op
            | elem op ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="] =
                True
            | otherwise = False
        getJSASTAssignment :: [JSNode] -> [JSNode] -> ExprWithSourceSpan
        getJSASTAssignment ((NS (JSOperator op) srcSpan):xs) preCurrent | (isAssignmentOperator op) =
                EWSS
                    (Assignment
                        op
                        (listToJSASTExpression preCurrent fileName)
                        (listToJSASTExpression xs fileName))
                    (jsnGetSource $ head preCurrent)
                    fileName
        getJSASTAssignment (x:xs) preCurrent = getJSASTAssignment xs (preCurrent ++ [x])
        -- FIXME: Not used. Remove?
        isAssignment :: [JSNode] -> Bool
        isAssignment [] = False
        isAssignment ((NS (JSOperator op) _):ls) = (isAssignmentOperator op) || (isAssignment ls)
        isAssignment (_:ls) = isAssignment ls


-- Makes arguments from a list of lists of JSNodes that represent a list of arguments.
toJSASTArguments :: [[JSNode]] -> SourceFileName -> SrcSpan -> ExprWithSourceSpan
toJSASTArguments args fileName srcSpan =
    -- FIXME: Might not be the right source span for args
    EWSS (Arguments (map getJSASTArgument args)) srcSpan fileName
    where
        getJSASTArgument (item:[]) = makeJSASTExpression item fileName
        getJSASTArgument nodes = listToJSASTExpression nodes fileName


-- To handle the case where the last element of the list is a (JSCallExpression "()" [JSArguments
-- _]) I don't know if the second field can be anything other than a singleton list containing a
-- JSArguments but for now I'm just going to hope not.
--
-- TODO: Just look at the parser source to work this out
getJSASTCall :: [JSNode] -> SourceFileName -> ExprWithSourceSpan
getJSASTCall list fileName =
    EWSS
        (Call
            (listToJSASTExpression (init list) fileName)
            (getArgs (jsnGetNode $ last list) fileName))
        (jsnGetSource $ head list)
        fileName
    where
        getArgs (JSCallExpression _ [(NS (JSArguments args) srcSpan)]) fileName =
            toJSASTArguments args fileName srcSpan


-- To handle the case where the last element in the list is a (JSCallExpression "[]" exp) or a
-- (JSCallExpression "." exp).
getJSASTCallExpression :: [JSNode] -> SourceFileName -> ExprWithSourceSpan
getJSASTCallExpression list fileName =
    EWSS
        (CallExpression
            (listToJSASTExpression (init list) fileName)
            (callExpOperator $ jsnGetNode $ last list)
            (callExpProperty $ jsnGetNode $ last list))
        (jsnGetSource $ head list)
        fileName
    where
        callExpOperator (JSCallExpression operator _) = operator
        -- FIXME: This assumes that the expression list can only ever be singleton. Try to verify.
        callExpProperty (JSCallExpression "[]" [expr]) = jsnToListExp expr fileName
        callExpProperty (JSCallExpression "." [expr]) =
            makeJSASTExpression expr fileName


-- Takes a Node that represents a property of an object and produdes a singleton list containing a
-- PropNameValue Expression.
toJSASTPropNameValue :: JSNode -> SourceFileName -> [ExprWithSourceSpan]
toJSASTPropNameValue (NS (JSPropertyNameandValue (NS (JSIdentifier name) _) value) srcSpan) fileName =
    [EWSS
        (PropNameValue
            (VariableProperty name)
            (listToJSASTExpression value fileName))
        srcSpan
        fileName
    ]
toJSASTPropNameValue (NS (JSPropertyNameandValue (NS (JSDecimal index) _) value) srcSpan) fileName =
    [EWSS
        (PropNameValue
            (IndexProperty (read index))
            (listToJSASTExpression value fileName))
        srcSpan
        fileName
    ]
toJSASTPropNameValue _ _ = []


-- Takes a Node that represents a literal value and makes an AST node for that value.
toJSASTValue :: Node -> SourceFileName -> Value
toJSASTValue (JSDecimal s) _ =
    if elem '.' s then
        JSFloat (read s)
    else
        JSInt (read s)
toJSASTValue (JSLiteral "false") _ = JSBool False
toJSASTValue (JSLiteral "true") _ = JSBool True
toJSASTValue (JSStringLiteral '"' s) _ = JSDQString s
toJSASTValue (JSStringLiteral _ s) _ = JSString s
toJSASTValue (JSObjectLiteral list) fileName =
    JSObject
        (concat $ map (\n -> toJSASTPropNameValue n fileName) list)
toJSASTValue (JSArrayLiteral arr) fileName =
    JSArray
        (map (\sa -> listToJSASTExpression sa fileName) (jsArrayGetSubarray arr))


-- Takes a single Node and builds a single expression.
makeJSASTExpression :: JSNode -> SourceFileName -> ExprWithSourceSpan
makeJSASTExpression (NS (JSExpressionBinary operator left right) srcSpan) fileName =
    EWSS
        (Binary
            operator
            -- FIXME: Needs to use the real file name
            (listToJSASTExpression left fileName)
            (listToJSASTExpression right fileName))
        srcSpan
        fileName
makeJSASTExpression (NS (JSIdentifier "undefined") srcSpan) fileName =
    EWSS (Value JSUndefined) srcSpan fileName
makeJSASTExpression (NS (JSLiteral "null") srcSpan) fileName =
    EWSS (Value JSNull) srcSpan fileName
makeJSASTExpression (NS (JSLiteral "this") srcSpan) fileName =
    EWSS (Identifier "this") srcSpan fileName
makeJSASTExpression (NS (JSIdentifier identifier) srcSpan) fileName =
    EWSS (Identifier identifier) srcSpan fileName
makeJSASTExpression (NS (JSExpressionPostfix operator variable) srcSpan) fileName =
    EWSS
        (UnaryPost
            operator
            (listToJSASTExpression variable fileName))
        srcSpan
        fileName
makeJSASTExpression (NS (JSExpressionParen expr) srcSpan) fileName =
    EWSS
        (ParenExpression
            (jsnToListExp expr fileName))
        srcSpan
        fileName
makeJSASTExpression (NS (JSExpressionTernary expr ifTrue ifFalse) srcSpan) fileName =
    EWSS
        (Ternary
            (listToJSASTExpression expr fileName)
            (listToJSASTExpression ifTrue fileName)
            (listToJSASTExpression ifFalse fileName))
        srcSpan
        fileName
makeJSASTExpression (NS (JSMemberDot pre post) srcSpan) fileName =
    EWSS
        (Reference
            (listToJSASTExpression pre fileName)
            (makeJSASTExpression post fileName))
        srcSpan
        fileName
makeJSASTExpression (NS (JSMemberSquare pre post) srcSpan) fileName =
    EWSS
        (Index
            (listToJSASTExpression pre fileName)
            (jsnToListExp post fileName))
        srcSpan
        fileName
makeJSASTExpression (NS (JSArguments args) srcSpan) fileName = toJSASTArguments args fileName srcSpan
makeJSASTExpression (NS (JSFunctionExpression [name] args body) srcSpan) fileName =
    EWSS
        (FunctionExpression
            (Just $ identifierGetString $ jsnGetNode name)
            (map (identifierGetString . jsnGetNode) args)
            (AWSS (FunctionBody (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
makeJSASTExpression (NS (JSFunctionExpression [] args body) srcSpan) fileName =
    EWSS
        (FunctionExpression
            Nothing
            (map (identifierGetString . jsnGetNode) args)
            (AWSS (FunctionBody (toJSAST body fileName)) (jsnGetSource body) fileName))
        srcSpan
        fileName
-- Anything left unmatched here is assumed to be a literal value.
makeJSASTExpression val fileName =
    EWSS (Value (toJSASTValue (jsnGetNode val) fileName)) (jsnGetSource val) fileName
