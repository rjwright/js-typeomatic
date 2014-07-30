
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
    , deleteBy
    , dropWhileEnd
    , find
    , groupBy
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
-- TODO: Rename these from JSX to ASTX
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
    | New ASTWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    | ParenExpression ASTWithSourceSpan
    -- A property of an object.
    | PropNameValue PropertyName ASTWithSourceSpan
    -- A reference into a structure using a dot.
    | Reference ASTWithSourceSpan ASTWithSourceSpan
    -- TODO: Needs comment to explain what it is.
    -- TODO: New type. Needs to be handled in the Rules.
    -- Was called "List".
    | StatementList [ASTWithSourceSpan]
    | Ternary ASTWithSourceSpan ASTWithSourceSpan ASTWithSourceSpan
    | Throw ASTWithSourceSpan
    | UnaryPost Operator ASTWithSourceSpan
    | UnaryPre Operator ASTWithSourceSpan
    | Value Value
    | VarDeclaration Variable (Maybe ASTWithSourceSpan)
    deriving (Show)


data ASTWithSourceSpan = AWSS AST SrcSpan deriving (Show)


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
-- FIXME: Probably semicolons can come through here, or empty lists.
listToASTExpression :: [JSNode] -> ASTWithSourceSpan
listToASTExpression [item] = toAST item
listToASTExpression [(NS (JSUnary operator) srcSpan), (NS (JSDecimal x) _)]
    | (operator == "-") =
        if (elem '.' x)
            then
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
            (toAST (NS (JSArguments args) srcSpan)))
        (jsnGetSource x)
-- To handle the case where the last element in the list is a (JSCallExpression "[]" exp) or a
-- (JSCallExpression "." exp).
listToASTExpression list | (isCallExpression $ last list) =
    AWSS
        (CallExpression
            (listToASTExpression (init list))
            (callExpOperator $ jsnGetNode $ last list)
            (callExpProperty $ jsnGetNode $ last list))
        (jsnGetSource $ head list)
    where
        isCallExpression :: JSNode -> Bool
        isCallExpression (NS (JSCallExpression "." _) _) = True
        isCallExpression (NS (JSCallExpression "[]" _) _) = True
        isCallExpression _ = False
        callExpOperator :: Node -> Operator
        callExpOperator (JSCallExpression operator _) = operator
        -- FIXME: This assumes that the expression list can only ever be singleton. Verify.
        callExpProperty :: Node -> ASTWithSourceSpan
        callExpProperty (JSCallExpression _ [member]) = toAST member
-- To handle the case where the last element of a list is a (JSCallExpression "()" [JSArguments
-- _]) I don't know if the second field can be anything other than a singleton list containing a
-- JSArguments but for now I'm just going to hope not.
--
-- TODO: Find out what values the arguments list can have.
listToASTExpression list | (isParenCallExp $ last list) =
    AWSS
        (Call
            (listToASTExpression (init list))
            (getArgs $ jsnGetNode $ last list))
        (jsnGetSource $ head list)
    where
        isParenCallExp :: JSNode -> Bool
        isParenCallExp (NS (JSCallExpression "()" _) _) = True
        isParenCallExp _ = False
        getArgs :: Node -> ASTWithSourceSpan
        getArgs (JSCallExpression _ [args]) = toAST args
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


toASTVarDeclaration :: JSNode -> ASTWithSourceSpan
toASTVarDeclaration (NS (JSVarDecl name value) srcSpan) =
    AWSS
        (VarDeclaration
            (identifierGetString $ jsnGetNode name)
            (listToMaybeExpression value))
        srcSpan


-- TODO: Check use of this function. Probably used in more places than needed.
filterSemicolons :: [JSNode] -> [JSNode]
filterSemicolons jsnList =
    filter isNotSemi jsnList
    where
        isNotSemi jsn = not ((jsnGetNode jsn) == (JSLiteral ";"))


-- TODO: Remove all of the nested constructors.
-- TODO: Alpha order.
-- TODO: All semicolons have been filtered out. Is that OK?
toAST :: JSNode -> ASTWithSourceSpan
toAST (NS (JSBlock statements) srcSpan) =
    AWSS
        (Block
            (toAST statements))
        srcSpan
toAST (NS (JSFunctionBody bodyList) srcSpan) =
    AWSS
        (FunctionBody (map toAST (filterSemicolons bodyList)))
        srcSpan
-- TODO: Make sure this is handled correctly when making Rules.
toAST (NS (JSSourceElements elementsList) srcSpan) =
    AWSS
        (StatementList (map toAST (filterSemicolons elementsList)))
        srcSpan
-- TODO: Make sure this is handled correctly when making Rules.
toAST (NS (JSSourceElementsTop elementsList) srcSpan) =
    AWSS
        (StatementList (map toAST (filterSemicolons elementsList)))
        srcSpan
toAST (NS (JSStatementBlock statements) srcSpan) =
    AWSS
        -- FIXME: Not sure if "Block" is the right thing here.
        (Block
            (toAST statements))
        srcSpan
toAST (NS (JSStatementList statements) srcSpan) =
    AWSS
        (StatementList (map toAST (filterSemicolons statements)))
        srcSpan
-- TODO: Do I need to do anything with the first parameter?
toAST (NS (JSVariables _ varDecs) srcSpan) =
    -- FIXME: This doesn't actually come from a JSStatementList in the parser. Should make a
    -- "variables" type for this.
    AWSS
        (StatementList (map toASTVarDeclaration (filterSemicolons varDecs)))
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
        (Continue (liftM (identifierGetString . jsnGetNode) (listToMaybe $ filterSemicolons label)))
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
        getSublists ((NS (JSLiteral ";") _):[]) current = [current]
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
toAST (NS (JSFunction name args body) srcSpan) =
    AWSS
        (FunctionDeclaration
            (identifierGetString $ jsnGetNode name)
            (map (identifierGetString . jsnGetNode) args)
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
toAST (NS (JSPropertyNameandValue name value) srcSpan) =
    AWSS
        (PropNameValue
            (getPropertyName $ jsnGetNode name)
            -- Value can be a proper list of JSNodes, eg. var o = { p: x = 2 };
            (listToASTExpression value))
        srcSpan
    where
        getPropertyName :: Node -> PropertyName
        getPropertyName (JSIdentifier n) = VariableProperty n
        getPropertyName (JSDecimal n) = IndexProperty (read n)
toAST (NS (JSReturn value) srcSpan) =
    AWSS
        (Return
            (returnValue $ filterSemicolons value))
        srcSpan
    where
        -- The list in a JSReturn is always either a singleton list containing a semicolon, or a
        -- 2-list containing a JSNode (representing the value to be returned) and a semicolon.
        returnValue :: [JSNode] -> ASTWithSourceSpan
        returnValue [] = AWSS (Value JSUndefined) srcSpan
        returnValue [val] = toAST val
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
            -- FIXME: Can there every be semicolons?
            (map toAST (filterSemicolons catchClause)))
        srcSpan
toAST (NS (JSWhile test body) srcSpan) =
    AWSS
        (While
            (toAST test)
            -- body is a JSStatementBlock.
            (toAST body))
        srcSpan
toAST (NS (JSArguments args) srcSpan) =
    AWSS
        (Arguments
            (map listToASTExpression args))
        srcSpan
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


-- -- Takes the parse tree representation of an array literal, deals with elisions at the start of the
-- -- array, then processes what's left.
-- processArray :: [JSNode] -> [[JSNode]] -> SrcSpan -> [[JSNode]]
-- processArray [] current _ = current
-- processArray ((NS (JSElision es) srcSpan):rest) current nearestSpan =
--     processArray
--         rest
--         (current ++ [[NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan nearestSpan)]])
--         (getNearestSrcSpan srcSpan nearestSpan)
-- -- processArray jsArray current nearestSpan = arrayGetElements jsArray current nearestSpan
-- processArray jsArray current nearestSpan = current ++ (arrayGetElements jsArray [] nearestSpan)


-- Process the remainder an array literal after any leading commas have been processed.
-- FIXME: Try to improve source spans.
-- arrayGetElements :: [JSNode] -> [[JSNode]] -> SrcSpan -> [[JSNode]]
-- arrayGetElements [] current nearestSpan = current
-- -- Ignore one trailing comma at the end of the array.
-- arrayGetElements [(NS (JSLiteral ",") _)] current _ = current
-- -- A single elision at the end of the parsed array occurs when there are two commas at the end of
-- -- the array or when the array is equal to [,]
-- arrayGetElements [(NS (JSElision e) srcSpan)] current nearestSpan =
--     current ++ [[(NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan nearestSpan))]]
-- -- Two elisions in a row (that aren't at the beginning of the array) indicates one undefined entry,
-- -- then a comma seperator, then the next entry.
-- arrayGetElements
--     ((NS (JSElision _) srcSpan1):(NS (JSElision e) srcSpan2):rest) current nearestSpan  =
--         arrayGetElements
--             ((NS (JSElision e) srcSpan2):rest)
--             (current
--             ++ [[NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan1 nearestSpan)]])
--             (getNearestSrcSpan srcSpan1 nearestSpan)
-- -- One elision and then a non-elision entry indicates a comma seperator and then the entry.
-- arrayGetElements ((NS (JSElision _) srcSpan):(jsn):rest) current nearestSpan =
--     arrayGetElements rest (current ++ [[jsn]]) (getNearestSrcSpan srcSpan nearestSpan)
-- arrayGetElements (jsn:rest) [] nearestSpan =
--     arrayGetElements
--         rest
--         [[jsn]]
--         (getNearestSrcSpan (jsnGetSource jsn) nearestSpan)
-- arrayGetElements (jsn:rest) current nearestSpan =
--     arrayGetElements
--         rest
--         ((init current) ++ [(last current) ++ [jsn]])
--         (getNearestSrcSpan (jsnGetSource jsn) nearestSpan)


-- Takes the parse tree representation of an array literal, deals with elisions at the start of the
-- array, then processes what's left.
-- processArray :: [JSNode] -> [[JSNode]] -> SrcSpan -> [[JSNode]]
-- processArray [] current _ = current
-- processArray ((NS (JSElision es) srcSpan):rest) current nearestSpan =
--     processArray
--         rest
--         (current ++ [[NS (JSIdentifier "undefined") (getNearestSrcSpan srcSpan nearestSpan)]])
--         (getNearestSrcSpan srcSpan nearestSpan)
-- processArray jsArray current nearestSpan = current ++ (sublists jsArray)

isElision (NS (JSElision _) _) = True
isElision _ = False

-- FIXME: Source spans could be better
processArray :: [JSNode] -> SrcSpan -> [[JSNode]]
processArray [] _ = []
processArray list nearestSpan
    | null leadingElisions =
        (sublists (drop (length $ leadingElisions) list))
    -- FIXME: remove this case (or the empty list case)?
    | (length leadingElisions) == (length list) =
        leadingElisions
    | otherwise =
        leadingElisions ++ (sublists (drop (length $ leadingElisions) list))
        where
            leadingElisions =
                [[(NS (JSIdentifier "undefined") (getNearestSrcSpan (jsnGetSource el) nearestSpan))] | el <- (takeWhile isElision list)]

sublists :: [JSNode] -> [[JSNode]]
sublists list =
    breakAtElisions
        $ dropWhileEnd (((==) (JSLiteral ",")) . jsnGetNode) list
    -- elisionsToUndefineds
    --     $ breakAtElisions
    --         $ dropWhileEnd (((==) (JSLiteral ",")) . jsnGetNode) list

-- Seperate elisions from non-elisions.
breakAtElisions :: [JSNode] -> [[JSNode]]
breakAtElisions [] = []
breakAtElisions list =
    filter (not . null) (separateElisions $ groupBy compareElements list)
    where
        compareElements l r
            | (isElision l) && (isElision r) = True
            | (not $ isElision l) && (not $ isElision r) = True
            | otherwise = False
        -- We delete one elision per group of elisions, except at the end of the array. Then break
        -- the groups up into singleton lists.
        -- [[a, b], [el, el, el], [c], [el]] -> [[a, b], [el, el], [c], [el]]
        -- separateElisions [ls] =
        --     if (isElision $ head ls)
        --         then [[el] | el <- ls]
        --         else [ls]
        -- separateElisions (ls:others) =
        --     if (isElision $ head ls)
        --         then [[el] | el <- drop 1 ls] ++ (separateElisions others)
        --         else [ls] ++ (separateElisions others)
        separateElisions [ls] =
            if (isElision $ head ls)
                then [[NS (JSIdentifier "undefined") (jsnGetSource el)] | el <- ls]
                else [ls]
        separateElisions (ls:others) =
            if (isElision $ head ls)
                then [[NS (JSIdentifier "undefined") (jsnGetSource el)] | el <- drop 1 ls] ++ (separateElisions others)
                else [ls] ++ (separateElisions others)

-- walking through the broken-up list, replace any remaining elisions with "undefined"
-- elisionsToUndefineds :: [[JSNode]] -> [[JSNode]]
-- elisionsToUndefineds list =
--     map (map elToUndefined) list
--     where
--         elToUndefined n = if isElision n then NS (JSIdentifier "undefined") (jsnGetSource n) else n


-- Takes a Node that represents a literal value and makes an AST node for that value.
toASTValue :: JSNode -> Value
toASTValue (NS (JSArrayLiteral arr) srcSpan) =
    -- We call listToASTExpression on the sublists of the output from processArray because a
    -- sublist of the parsed array isn't necessarily a sublist of the actual array (in the JS
    -- source code). For example
    --
    -- [y = 10, 3];
    --
    -- Produces
    --
    -- JSArrayLiteral
    --     [
    --     JSIdentifier \"y\",
    --     JSOperator \"=\",
    --     JSDecimal \"10\",
    --     JSElision [],
    --     JSDecimal \"3\"
    --     ]
    --
    -- Which is processed to
    -- TODO: Check this.
    -- [[JSIdentifier \"y\", JSOperator \"=\", JSDecimal \"10\"], [JSDecimal \"3\"]]
    JSArray
        (map listToASTExpression (processArray arr srcSpan))
        -- (map listToASTExpression (processArray arr [] srcSpan))
toASTValue (NS (JSDecimal s) _) =
    if elem '.' s
        then
            JSFloat (read s)
        else
            JSInt (read s)
toASTValue (NS (JSLiteral "false") _) = JSBool False
toASTValue (NS (JSLiteral "true") _) = JSBool True
-- FIXME: Can there ever be semicolons in the list?
toASTValue (NS (JSObjectLiteral list) _) = JSObject (map toAST (filterSemicolons list))
toASTValue (NS (JSStringLiteral '"' s) _) = JSDQString s
toASTValue (NS (JSStringLiteral _ s) _) = JSString s
