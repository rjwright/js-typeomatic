
-- This module parses a JavaScript source file using the language.javascript.Parser library for
-- Haskell, then simplifies the parse tree to produce a more useful abstract syntax tree (AST).
--
-- The documentation for the Haskell library is on the web, but it isn't particularly useful. Most
-- of its data types contain a lot of different (and meaningful) constructors that don't show up in
-- the doc. Best approach is to look at the source.
--
-- Top level function is (toJSAST . parseTree)
--
-- TODO: Does this file need more comments?


module ParseJS
( Expression(..)
, Index
, JSAST(..)
, Operator
, PropertyName(..)
, SourceFragment
, Value(..)
, Variable
, getSourceFragments
, nodeGetSpan
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
import System.Environment
import System.IO


type Variable = String
type Index = Int
type Operator = String
type Row = Int
type Col = Int
-- (FileName, StartRow, StartCol, EndRow, EndCol)
type SourceFragment = (String, Row, Col, Row, Col)

-- Represent an identifier used to index an object property using square branchets. An object
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
      JSArray [Expression]
    | JSBool Bool
    -- Double quote strings are never treated differently to normal strings.
    -- TODO: Should be merged with JSString
    | JSDQString String
    | JSFloat Double
    | JSInt Int
    | JSNull
    -- TODO: Comment on what the expressions can be.
    | JSObject [Expression]
    | JSString String
    | JSUndefined deriving (Show)


-- Represent source elements that can approximately be described as expressions. Initially this was
-- for elements that appeared as expressions in the output from language.javascript.parser, but some
-- other things have been added where it made sense.
--
-- None of these contain JSAST fields except for FunctionExpression.
--
-- TODO: Each of these should contain a SourceFragment.
data Expression =
      Arguments [Expression]
    | Assignment Operator Expression Expression
    | Binary Operator Expression Expression
    | Break (Maybe Variable)
    | Call Expression Expression
    -- In Language.JavaScript, a call expression is an expression that calls - or accesses a
    -- property of - a function call.
    --
    -- E.g. foo()(); Or foo().bar;
    --
    -- However, this program treats foo()() as a Call within a Call (as I believe that is a
    -- sufficient description for our purposes).
    | CallExpression Expression Operator Expression
    | Continue (Maybe Variable)
    -- A function expression occurs when a function definition is on the right hand side of some
    -- statement.
    | FunctionExpression (Maybe Variable) [Variable] JSAST
    | Identifier Variable
    -- Represents an index into a structure using square brackets.
    | Index Expression Expression
    -- TODO: Needs comment to explain what it is
    | List [Expression]
    | New Expression
    -- TODO: Needs comment to explain what it is.
    | ParenExpression Expression
    -- Represents a property of an object.
    | PropNameValue PropertyName Expression
    -- Represents a reference to a structure member using a dot.
    | Reference Expression Expression
    | Ternary Expression Expression Expression
    | Throw Expression
    | UnaryPost Operator Expression
    | UnaryPre Operator Expression
    | Value Value
    | VarDeclaration Variable (Maybe Expression) deriving (Show)


-- Represent source elements which include a "block" or "body" and thus make logical non-terminal
-- nodes for an abstract syntax tree.
--
-- FIXME: Also includes a type for return expressions (Return) and a wrapper for instances of the
-- Expression data type (Statement). I'm not sure how or why that happened, but I'm sure there was
-- an excellent reason.
--
-- TODO: Each of these should contain a SourceFragment.
data JSAST =
      Block [JSAST]
    | Case Expression JSAST
    | Catch Variable (Maybe Expression) JSAST
    | Default JSAST
    | DoWhile JSAST Expression
    | Finally JSAST
    | For (Maybe Expression) (Maybe Expression) (Maybe Expression) JSAST
    | ForIn [Variable] Expression JSAST
    | ForVar [Expression] (Maybe Expression) (Maybe Expression) JSAST
    | ForVarIn Expression Expression JSAST
    | FunctionBody [JSAST]
    | FunctionDeclaration Variable [Variable] JSAST
    | If Expression JSAST
    | IfElse Expression JSAST JSAST
    | Labelled Variable JSAST
    | Return Expression
    | Statement Expression
    | Switch Expression JSAST
    | Try JSAST JSAST
    | While Expression JSAST deriving (Show)


-- Extract the Node from a JSNode.
jsnGetNode :: JSNode -> Node
jsnGetNode (NS a _) = a


-- Parse JavaScript source code.
parseTree :: String -> Node
parseTree program = jsnGetNode $ (\(Right a) -> a) $ parse program "";


nodeGetSpan :: Node -> [SrcSpan]
nodeGetSpan (JSSourceElementsTop elements) =
    map jsNodeGetSpan elements
    where
        jsNodeGetSpan (NS node srcSpan) = srcSpan


getSourceFragments :: [SrcSpan] -> String -> [SourceFragment] -> [SourceFragment]
getSourceFragments (s1:s2:[]) file result =
    (getSourceFragment s1 s2 file):result
getSourceFragments (s1:s2:sx) file result =
    (getSourceFragments (s2:sx) file result) ++ (getSourceFragment s1 s2 file):result


getSourceFragment :: SrcSpan -> SrcSpan -> String -> SourceFragment
getSourceFragment (SpanPoint _ row1 col1) (SpanPoint _ row2 col2) fileName =
    (fileName, row1, col1, row2, col2)



-- Extract the Node from a JSNode and apply toJSAST.
jsnToJSAST :: JSNode -> [JSAST]
jsnToJSAST jsn = toJSAST $ jsnGetNode jsn


-- Most Nodes have a [JSNode] field. astMap applies toJSAST to the Node field of each JSNode in such
-- a list, putting the results in a new list
astMap :: [JSNode] -> [JSAST]
astMap jsnList = concat $ map jsnToJSAST jsnList


-- Extract a list or an expression in parens from a Statement.
statementToListExp :: [JSAST] -> Expression
statementToListExp [Statement (List list)] = List list
statementToListExp [Statement (ParenExpression exp)] = ParenExpression exp


statementToMaybeListExp :: [JSAST] -> Maybe Expression
statementToMaybeListExp [] = Nothing
statementToMaybeListExp list = Just $ statementToListExp list


-- Make an expression list or paren expression Statement from a JSNode and then extract the
-- expression.
jsnToListExp :: JSNode -> Expression
jsnToListExp jsn = statementToListExp $ jsnToJSAST jsn


jsnToMaybeListExp :: [JSNode] -> Maybe Expression
jsnToMaybeListExp jsnList = statementToMaybeListExp $ astMap jsnList


-- Extract the String value from a JSIdentifier
identifierGetString :: Node -> String
identifierGetString (JSIdentifier jsid) = jsid


-- Make representations of variable declarations in AST.
toJSASTVarDeclaration :: Node -> Expression
toJSASTVarDeclaration (JSVarDecl name []) =
    VarDeclaration
        (identifierGetString $ jsnGetNode name)
        Nothing
toJSASTVarDeclaration (JSVarDecl name value) =
    VarDeclaration
        (identifierGetString $ jsnGetNode name)
        (Just $ mapListToExpression value)


-- Take a node in the parse tree output from language.javascript.Parser and make an abstract syntax
-- tree.
toJSAST :: Node -> [JSAST]
-- These ones return a proper list of JSASTs. (Haskell Land) Constructors which use one of these to
-- fill a field must have a [JSAST] for that field.
toJSAST (JSSourceElementsTop topList) = astMap topList
toJSAST (JSSourceElements elementsList) = astMap elementsList
toJSAST (JSFunctionBody bodyList) = astMap bodyList
toJSAST (JSStatementList statList) = astMap statList
toJSAST (JSBlock jsnode) = jsnToJSAST jsnode
toJSAST (JSStatementBlock item) = jsnToJSAST item
toJSAST (JSVariables _ varDecs) = map (Statement . toJSASTVarDeclaration . jsnGetNode) varDecs
-- These ones always return singleton lists. (Haskell Land) Constructors which use only these to
-- fill a field can have a JSAST for that field.
--
-- A JSExpression contains a list of JSNodes, separated by <JSLiteral ','>. These need to be
-- seperated (basically the <JSLiteral ','>s need to be stripped. My code to do that is kind of
-- disgusting.
toJSAST (JSExpression jsnList) =
    [Statement
        (List (map listToJSASTExpression (jsExpGetSublists $ map jsnGetNode jsnList)))
    ]
    where
        -- TODO: This could be improved.
        jsExpGetSublists [] = []
        jsExpGetSublists [item] = [[item]]
        jsExpGetSublists ls =
            let (nextSub, rest) = getSublist ls in ([nextSub] ++ (jsExpGetSublists rest))
        getSublist [] = ([], [])
        getSublist [item] = ([item], [])
        getSublist ((JSLiteral ","):rest) = ([], rest)
        getSublist (y:ys) = let (next, rest) = getSublist ys in (y:(next), rest)
toJSAST (JSFunction name inputs body) =
    [FunctionDeclaration
        (identifierGetString $ jsnGetNode name)
        (map (identifierGetString . jsnGetNode) inputs)
        (FunctionBody (jsnToJSAST body))
    ]
toJSAST (JSLabelled label body) =
    [Labelled
        (identifierGetString $ jsnGetNode label)
        (Block (jsnToJSAST body))
    ]
toJSAST (JSBreak [] _) =
    [Statement
        (Break Nothing)
    ]
toJSAST (JSBreak [label] _) =
    [Statement
        (Break (Just $ identifierGetString $ jsnGetNode label))
    ]
toJSAST (JSContinue [item]) =
    [Statement
        (Continue Nothing)
    ]
toJSAST (JSContinue [label, semi]) =
    [Statement
        (Continue (Just $ identifierGetString $ jsnGetNode label))
    ]
toJSAST (JSThrow expr) =
    [Statement
        (Throw (toJSASTExpression $ jsnGetNode expr))
    ]
    where
        toJSASTExpression (JSExpression ex) = mapListToExpression ex
-- JSForVar occurs in the case that variables are declared in the loop statement, multiple
-- predicates and multiple counter operations.
toJSAST (JSForVar vars test count body) =
    [ForVar
        (map (toJSASTVarDeclaration . jsnGetNode) vars)
        (jsnToMaybeListExp test)
        (jsnToMaybeListExp count)
        (Block (jsnToJSAST body))
    ]
-- JSFor occurs when no varibles are declared in the loop (although they may be re-assigned),
-- multiple predicates and multiple counter operations
toJSAST (JSFor vars test count body) =
    [For
        (jsnToMaybeListExp vars)
        (jsnToMaybeListExp test)
        (jsnToMaybeListExp count)
        (Block (jsnToJSAST body))
    ]
-- JSForIn occurs when no variables are declared inside the for statement, and the loop iterates
-- over some object (e.g. an array)
toJSAST (JSForIn vars obj body) =
    [ForIn
        (map (identifierGetString . jsnGetNode) vars)
        (jsnToListExp obj)
        (Block (jsnToJSAST body))
    ]
-- JSForVarIn occurs when variables are declared inside the for statement and the loop iterates over
-- some object (e.g. an array)
toJSAST (JSForVarIn var obj body) =
    [ForVarIn
        (toJSASTVarDeclaration $ jsnGetNode var)
        (jsnToListExp obj)
        (Block (jsnToJSAST body))
    ]
toJSAST (JSWhile test body) =
    [While
        (jsnToListExp test)
        (Block (jsnToJSAST body))
    ]
-- TODO: Look at the parser source and find out what "something" actually is
toJSAST (JSDoWhile body test something) =
    [DoWhile
        (Block (jsnToJSAST body))
        (jsnToListExp test)
    ]
toJSAST (JSIf test body) =
    [If
        (jsnToListExp test)
        (Block (jsnToJSAST body))
    ]
toJSAST (JSIfElse test trueBody falseBody) =
    [IfElse
        (jsnToListExp test)
        (Block (jsnToJSAST trueBody))
        (Block (jsnToJSAST falseBody))
    ]
toJSAST (JSSwitch var cases) =
    [Switch
        (jsnToListExp var)
        (Block (concat $ map jsnToJSAST cases))
    ]
toJSAST (JSCase cs body) =
    [Case
        (jsnToListExp cs)
        (Block (jsnToJSAST body))
    ]
toJSAST (JSDefault body) =
    [Default
        (Block (jsnToJSAST body))
    ]
toJSAST (JSTry body catchClause) =
    [Try
        (Block (jsnToJSAST body))
        (Block (astMap catchClause))
    ]
toJSAST (JSCatch var test body) =
    [Catch
        (identifierGetString $ jsnGetNode var)
        (mapListToMaybeExpression test)
        (Block (jsnToJSAST body))
    ]
toJSAST (JSFinally body) =
    [Finally
        (Block (jsnToJSAST body))
    ]
toJSAST (JSReturn [item]) =
    [Return
        (Value JSUndefined)
    ]
toJSAST (JSReturn [val, semi]) =
    [Return
        (jsnToListExp val)
    ]
-- TODO: Not 100% sure that this is safe.
toJSAST (JSLiteral ";") = []
toJSAST x =
    [Statement
        (makeJSASTExpression x)
    ]

----------------------------------------------------------------------------------------------------
-- *************************************************************************************************
-- These functions are used to process array literals. Elisions in array literals are a bit of a
-- pain in the butt. This code is kind of bad and took me an insane amount of time to write. Good
-- luck.
-- *************************************************************************************************
-- TODO: Find a better way to single these out (horizontal lines in code suck)

-- Takes a representation of a JS array and produces a singleton list containing the next element,
-- paired with the remainder of the array.
getSubarray :: [Node] -> ([Node], [Node])
-- If there is nothing left in the input array then return nothing.
getSubarray [] = ([], [])
-- Ignore one trailing comma at the end of the array.
getSubarray [(JSLiteral ",")] = ([], [])
-- I don't remember how we end up with a single elision, but apparently it can happen.
getSubarray [(JSElision e)] = ([(JSIdentifier "undefined")], [])
-- Process the last (or only) item in the array.
getSubarray [item] = ([item], [])
-- Two elisions in a row that aren't at the beginning of the array indicates one undefined entry,
-- then a comma seperator, then the next entry.
getSubarray ((JSElision _):(JSElision e):rest) =
    ([(JSIdentifier "undefined")], ((JSElision e):rest))
-- One elision and then a non-elision entry indicates a comma seperator and then the entry.
getSubarray ((JSElision _):rest) = getSubarray rest
-- An entry and then an elision and then another entry. The elision is a seperator.
getSubarray (y:(JSElision e):rest) = ([y], ((JSElision e):rest))
getSubarray (y:ys) = let (next, rest) = getSubarray ys in (y:(next), rest)


-- Takes an array and makes a (Haskell Land) 2D array, one subarray per element of the (JS) array.
arrayToSubarrays :: [Node] -> [[Node]]
arrayToSubarrays [] = []
arrayToSubarrays ls = let (nextSub, rest) = getSubarray ls in ([nextSub] ++ (arrayToSubarrays rest))


-- Takes a representation of a literal array from the output of language.javascript.Parser, deals
-- with elisions at the start of the array, then processes what's left.
jsArrayGetSubarray :: [Node] -> [[Node]]
jsArrayGetSubarray jsa =
    let (el, rest) = getLeadingElisions jsa in (el ++ (arrayToSubarrays rest))
    where
        getLeadingElisions [] = ([], [])
        getLeadingElisions ((JSElision es):x) =
            let (e, rest) = getLeadingElisions x in (([JSIdentifier "undefined"]:e), rest)
        getLeadingElisions lst = ([], lst)
----------------------------------------------------------------------------------------------------


-- Takes a list of JSNodes and makes an expression.
mapListToExpression :: [JSNode] -> Expression
mapListToExpression jsn = listToJSASTExpression $ map jsnGetNode jsn


mapListToMaybeExpression :: [JSNode] -> Maybe Expression
mapListToMaybeExpression [] = Nothing
mapListToMaybeExpression jsn = Just $ mapListToExpression jsn


-- Takes a list of Nodes and builds a single expression.
--
-- TODO: Add a comment here explaining where these Node lists come from.
listToJSASTExpression :: [Node] -> Expression
listToJSASTExpression [item] = makeJSASTExpression item
-- FIXME: This is very ugly
listToJSASTExpression [(JSUnary operator), (JSDecimal x)]
    | elem operator ["-"] =
        let y = if elem '.' x then
                    JSFloat (-1 * (read x))
                else
                    JSInt (-1 * (read x)) in Value  y
listToJSASTExpression ((JSUnary operator):x)
    | elem operator ["-", "+", "--", "++", "!", "typeof ", "delete ", "~"] =
        UnaryPre
            operator
            (listToJSASTExpression x)
listToJSASTExpression ((JSLiteral "new "):x) = New (listToJSASTExpression x)
listToJSASTExpression [x, (JSArguments args)] =
    Call
        (makeJSASTExpression x)
        (toJSASTArguments args)
-- FIXME: This is SUPER ugly and isCallExpression isn't being used.
listToJSASTExpression list =
    if (isAssignment list) then
        getJSASTAssignment list
    else if (isParenCallExp $ last list) then
        getJSASTCall list
    else
        getJSASTCallExpression list

-- Makes arguments from a list of lists of JSNodes that represent a list of arguments.
toJSASTArguments :: [[JSNode]] -> Expression
toJSASTArguments args =
    Arguments (map getJSASTArgument args)
    where
        getJSASTArgument [item] = makeJSASTExpression $ jsnGetNode item
        getJSASTArgument nodes = mapListToExpression nodes

-- Determine if a Node is an assignment operator
isAssignmentOperator :: String -> Bool
isAssignmentOperator op
    | elem op ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="] = True
    | otherwise = False

-- Determine whether the list of Nodes is an assignment
isAssignment :: [Node] -> Bool
isAssignment [] = False
isAssignment ((JSOperator op):ls) = (isAssignmentOperator op) || (isAssignment ls)
isAssignment (_:ls) = isAssignment ls


-- To handle messy assignments
getJSASTAssignment :: [Node] -> Expression
getJSASTAssignment list =
    -- FIXME: This whole function is a brain drain. Refactor.
    let (pre, op, post) = getPrePost list in
        Assignment
            op
            (listToJSASTExpression pre)
            (listToJSASTExpression post)
    where
        getPrePost ((JSOperator op):xs)
            | (isAssignmentOperator op) = ([], op, xs)
        getPrePost (x:xs) = let (l, o, u) = getPrePost xs in (x:l, o, u)


-- Determine whether a node is JSCallExpression with parentheses
isParenCallExp :: Node -> Bool
isParenCallExp (JSCallExpression "()" _) = True
isParenCallExp _ = False


-- To handle the case where the last element of the list is a (JSCallExpression "()" [JSArguments
-- _]) I don't know if the second field can be anything other than a singleton list containing a
-- JSArguments but for now I'm just going to hope not.
--
-- TODO: Just look at the parser source to work this out
getJSASTCall :: [Node] -> Expression
getJSASTCall list =
    Call
        (listToJSASTExpression $ init list)
        (lastGetArgs $ argsNode $ last list)
    where
        argsNode (JSCallExpression _ [args]) = jsnGetNode args
        lastGetArgs (JSArguments ar) = toJSASTArguments ar


-- Determine whether a node is a JSCallExpression with a dot or with square brackets.
--
-- FIXME: Should be called by listToJSASTExpression but currently isn't called.
isCallExpression :: Node -> Bool
isCallExpression (JSCallExpression "." _) = True
isCallExpression (JSCallExpression "[]" _) = True
isCallExpression _ = False

-- To handle the case where the last element in the list is a (JSCallExpression "[]" exp) or a
-- (JSCallExpression "." exp).
getJSASTCallExpression :: [Node] -> Expression
getJSASTCallExpression list =
    CallExpression
        (listToJSASTExpression $ init list)
        (callExpOperator $ last list)
        (callExpProperty $ last list)
    where
        callExpOperator (JSCallExpression operator _) = operator
        callExpProperty (JSCallExpression "[]" exp) = statementToListExp $ astMap exp
        callExpProperty (JSCallExpression "." [exp]) = makeJSASTExpression $ jsnGetNode exp


-- Takes a Node that represents a property of an object and produdes a singleton list containing a
-- PropNameValue Expression.
toJSASTPropNameValue :: Node -> [Expression]
toJSASTPropNameValue (JSPropertyNameandValue (NS (JSIdentifier name) _) value) =
    [PropNameValue
        (VariableProperty name)
        (listToJSASTExpression $ map jsnGetNode value)
    ]
toJSASTPropNameValue (JSPropertyNameandValue (NS (JSDecimal index) _) value) =
    [PropNameValue
        (IndexProperty (read index))
        (listToJSASTExpression $ map jsnGetNode value)
    ]
toJSASTPropNameValue _ = []


-- Takes a Node that represents a literal value and makes an AST node for that value.
toJSASTValue :: Node -> Value
toJSASTValue (JSDecimal s) =
    if elem '.' s then
        JSFloat (read s)
    else
        JSInt (read s)
toJSASTValue (JSLiteral "false") = JSBool False
toJSASTValue (JSLiteral "true") = JSBool True
toJSASTValue (JSStringLiteral '"' s) = JSDQString s
toJSASTValue (JSStringLiteral _ s) = JSString s
toJSASTValue (JSObjectLiteral list) =
    JSObject
        (concat $ map (toJSASTPropNameValue . jsnGetNode) list)
toJSASTValue (JSArrayLiteral arr) =
    JSArray
        (map listToJSASTExpression (jsArrayGetSubarray $ map jsnGetNode arr))


-- Takes a single Node and builds a single expression.
makeJSASTExpression :: Node -> Expression
makeJSASTExpression (JSExpressionBinary operator left right) =
    Binary
        operator
        (mapListToExpression left)
        (mapListToExpression right)
makeJSASTExpression (JSIdentifier "undefined") = Value JSUndefined
makeJSASTExpression (JSLiteral "null") = Value JSNull
makeJSASTExpression (JSLiteral "this") = Identifier "this"
makeJSASTExpression (JSIdentifier identifier) = Identifier identifier
makeJSASTExpression (JSExpressionPostfix operator variable) =
    UnaryPost
        operator
        (mapListToExpression variable)
makeJSASTExpression (JSExpressionParen expr) =
    ParenExpression
        (jsnToListExp expr)
makeJSASTExpression (JSExpressionTernary expr iftrue iffalse) =
    Ternary
        (mapListToExpression expr)
        (mapListToExpression iftrue)
        (mapListToExpression iffalse)
makeJSASTExpression (JSMemberDot pre post) =
    Reference
        (mapListToExpression pre)
        (makeJSASTExpression $ jsnGetNode post)
makeJSASTExpression (JSMemberSquare pre post) =
    Index
        (mapListToExpression pre)
        (jsnToListExp post)
makeJSASTExpression (JSArguments args) = toJSASTArguments args
makeJSASTExpression (JSFunctionExpression [name] args body) =
    FunctionExpression
        (Just $ identifierGetString $ jsnGetNode name)
        (map (identifierGetString . jsnGetNode) args)
        (FunctionBody (jsnToJSAST body))
makeJSASTExpression (JSFunctionExpression [] args body) =
    FunctionExpression
        Nothing
        (map (identifierGetString . jsnGetNode) args)
        (FunctionBody (jsnToJSAST body))
-- Anything left unmatched here is assumed to be a literal value.
makeJSASTExpression val = Value (toJSASTValue val)
