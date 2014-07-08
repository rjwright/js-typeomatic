
-- This module parses a JavaScript source file using the language.javascript.Parser library for
-- Haskell, then simplifies the parse tree to produce a more useful abstract syntax tree (AST).
--
-- The documentation for the Haskell library is on the web, but it isn't particularly useful. Most
-- of its data types contain a lot of different (and meaningful) constructors that don't show up in
-- the doc. Best approach is to look at the source.
--
-- Top level function is (toJSAST . parseTree)
--
-- FIXME: File name is threaded throughout because the parser doesn't actually use the file name -_-
-- This threading can be removed if the parser is forked.
--
-- TODO: Does this file need more comments?


module ParseJS
( Expression(..)
, Index
, JSAST(..)
, Operator
, PropertyName(..)
, SourceFileName
, SourceFragment
, Value(..)
, Variable
, getSourceFragments
, parseTree
, toJSAST
, topNodeGetSpan
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
type SourceFileName = String


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
      Arguments [Expression] SrcSpan SourceFileName
    | Assignment Operator Expression Expression SrcSpan SourceFileName
    | Binary Operator Expression Expression SrcSpan SourceFileName
    | Break (Maybe Variable) SrcSpan SourceFileName
    | Call Expression Expression SrcSpan SourceFileName
    -- In Language.JavaScript, a call expression is an expression that calls - or accesses a SrcSpan SourceFileName
    -- property of - a function call. SrcSpan SourceFileName
    -- SrcSpan SourceFileName
    -- E.g. foo()(); Or foo().bar; SrcSpan SourceFileName
    -- SrcSpan SourceFileName
    -- However, this program treats foo()() as a Call within a Call (as I believe that is a SrcSpan SourceFileName
    -- sufficient description for our purposes). SrcSpan SourceFileName
    | CallExpression Expression Operator Expression SrcSpan SourceFileName
    | Continue (Maybe Variable) SrcSpan SourceFileName
    -- A function expression occurs when a function definition is on the right hand side of some SrcSpan SourceFileName
    -- statement. SrcSpan SourceFileName
    | FunctionExpression (Maybe Variable) [Variable] JSAST SrcSpan SourceFileName
    | Identifier Variable SrcSpan SourceFileName
    -- Represents an index into a structure using square brackets. SrcSpan SourceFileName
    | Index Expression Expression SrcSpan SourceFileName
    -- TODO: Needs comment to explain what it is SrcSpan SourceFileName
    | List [Expression] SrcSpan SourceFileName
    | New Expression SrcSpan SourceFileName
    -- TODO: Needs comment to explain what it is. SrcSpan SourceFileName
    | ParenExpression Expression SrcSpan SourceFileName
    -- Represents a property of an object. SrcSpan SourceFileName
    | PropNameValue PropertyName Expression SrcSpan SourceFileName
    -- Represents a reference to a structure member using a dot. SrcSpan SourceFileName
    | Reference Expression Expression SrcSpan SourceFileName
    | Ternary Expression Expression Expression SrcSpan SourceFileName
    | Throw Expression SrcSpan SourceFileName
    | UnaryPost Operator Expression SrcSpan SourceFileName
    | UnaryPre Operator Expression SrcSpan SourceFileName
    | Value Value SrcSpan SourceFileName
    | VarDeclaration Variable (Maybe Expression) SrcSpan SourceFileName deriving (Show)


-- Represent source elements which include a "block" or "body" and thus make logical non-terminal
-- nodes for an abstract syntax tree.
--
-- FIXME: Also includes a type for return expressions (Return) and a wrapper for instances of the
-- Expression data type (Statement). I'm not sure how or why that happened, but I'm sure there was
-- an excellent reason.
--
-- TODO: Each of these should contain a SourceFragment.
data JSAST =
      Block [JSAST] SrcSpan SourceFileName
    | Case Expression JSAST SrcSpan SourceFileName
    | Catch Variable (Maybe Expression) JSAST SrcSpan SourceFileName
    | Default JSAST SrcSpan SourceFileName
    | DoWhile JSAST Expression SrcSpan SourceFileName
    | Finally JSAST SrcSpan SourceFileName
    | For (Maybe Expression) (Maybe Expression) (Maybe Expression) JSAST SrcSpan SourceFileName
    | ForIn [Variable] Expression JSAST SrcSpan SourceFileName
    | ForVar [Expression] (Maybe Expression) (Maybe Expression) JSAST SrcSpan SourceFileName
    | ForVarIn Expression Expression JSAST SrcSpan SourceFileName
    | FunctionBody [JSAST] SrcSpan SourceFileName
    | FunctionDeclaration Variable [Variable] JSAST SrcSpan SourceFileName
    | If Expression JSAST SrcSpan SourceFileName
    | IfElse Expression JSAST JSAST SrcSpan SourceFileName
    | Labelled Variable JSAST SrcSpan SourceFileName
    | Return Expression SrcSpan SourceFileName
    | Statement Expression SrcSpan SourceFileName
    | Switch Expression JSAST SrcSpan SourceFileName
    | Try JSAST JSAST SrcSpan SourceFileName
    | While Expression JSAST SrcSpan SourceFileName deriving (Show)

type JSASTWithSourceSpan = WS JSAST SrcSpan SourceFileName
type ExprWithSourceSpan = WS Expression SrcSpan SourceFileName
-- data jsastWithSource = NSF JSAST SourceFragment

-- Extract the Node from a JSNode.
jsnGetNode :: JSNode -> Node
jsnGetNode (NS a _) = a

jsnGetSpan :: JSNode -> SrcSpan
jsnGetSpan (NS _ s) = s

-- jsnToNodeWithSource :: JSNode -> NodeWithSource
-- jsnToNodeWithSource (NS node srcSpan) = NSF node

-- Parse JavaScript source code.
-- parseTree :: String -> String -> Node
-- parseTree program fileName = jsnGetNode $ (\(Right a) -> a) $ parse program fileName;
parseTree :: String -> SourceFileName -> JSNode
parseTree program fileName = (\(Right a) -> a) $ parse program fileName;


-- Extract the Node from a JSNode and apply toJSAST.
-- jsnToJSAST :: JSNode -> [JSAST]
-- jsnToJSAST jsn = toJSAST $ jsnGetNode jsn
-- jsnToJSAST :: JSNode -> SourceFragment -> [JSAST]
-- jsnToJSAST jsn nextFragment = toJSAST (jsnGetNode jsn) (jsnGetSpan jsn) nextFragment


-- Most Nodes have a [JSNode] field. astMap applies toJSAST to the Node field of each JSNode in such
-- a list, putting the results in a new list
astMap :: [JSNode] -> SourceFileName -> [JSAST]
astMap jsnList fileName = concat $ map (\jsn -> toJSAST jsn fileName) jsnList
-- astMap :: [JSNode] -> SourceFragment -> [JSAST]
-- astMap jsnList nextFragment =
--     let reversed = reverse jsnList in
--     reverse $ makeFragments reversed nextFragment
--     where
--         makeFragments (f:fx) next =
--             let n = makeNextFragment (jsnGetSpan f) next in
--             (jsnToJSAST f n) ++ (makeFragments fx n)
--         makeFragments [] _ = []


-- Extract a list or an expression in parens from a Statement.
statementToListExp :: [JSAST] -> Expression
statementToListExp [Statement (List ls srcSpan fileName) _ _] = List ls srcSpan fileName
statementToListExp [Statement (ParenExpression expr srcSpan fileName) _ _] =
    ParenExpression expr srcSpan fileName


statementToMaybeListExp :: [JSAST] -> Maybe Expression
statementToMaybeListExp [] = Nothing
statementToMaybeListExp list = Just $ statementToListExp list


-- Make an expression list or paren expression Statement from a JSNode and then extract the
-- expression.
-- FIXME: Is this needed?
jsnToListExp :: JSNode -> SourceFileName -> Expression
jsnToListExp jsn fileName = statementToListExp $ toJSAST jsn fileName


jsnToMaybeListExp :: [JSNode] -> SourceFileName -> Maybe Expression
jsnToMaybeListExp jsnList fileName = statementToMaybeListExp $ astMap jsnList fileName


-- Extract the String value from a JSIdentifier
identifierGetString :: Node -> String
identifierGetString (JSIdentifier jsid) = jsid


-- Make representations of variable declarations in AST.
toJSASTVarDeclaration :: JSNode -> SourceFileName -> Expression
toJSASTVarDeclaration (NS (JSVarDecl name []) srcSpan) fileName =
    VarDeclaration
        (identifierGetString $ jsnGetNode name)
        Nothing
        srcSpan
        fileName
toJSASTVarDeclaration (NS (JSVarDecl name value) srcSpan) fileName =
    VarDeclaration
        (identifierGetString $ jsnGetNode name)
        (Just $ mapListToExpression value fileName)
        srcSpan
        fileName


-- Take a node in the parse tree output from language.javascript.Parser and make an abstract syntax
-- tree. toJSAST :: (Node, SourceFragment) -> [JSAST] Takes a node, the node's SrcSpan, and either
-- the node's next sibling's SrcSpan OR the node's parent's next sibling's SrcSpan (i.e. the end
-- point of the nodes source)
-- toJSAST :: Node -> [JSAST]
-- toJSAST :: Node -> SrcSpan -> SourceFragment -> [JSAST]
toJSAST :: JSNode -> SourceFileName -> [JSAST]
-- These ones return a proper list of JSASTs. (Haskell Land) Constructors which use one of these to
-- fill a field must have a [JSAST] for that field.
toJSAST (NS (JSSourceElementsTop topList) srcSpan) fileName = astMap topList fileName
toJSAST (NS (JSSourceElements elementsList) srcSpan) fileName = astMap elementsList fileName
toJSAST (NS (JSFunctionBody bodyList) srcSpan) fileName = astMap bodyList fileName
toJSAST (NS (JSStatementList statList) srcSpan) fileName = astMap statList fileName
toJSAST (NS (JSBlock jsnode) srcSpan) fileName = toJSAST jsnode fileName
toJSAST (NS (JSStatementBlock item) srcSpan) fileName = toJSAST item fileName
toJSAST (NS (JSVariables _ varDecs) srcSpan) fileName =
    map makeStatement varDecs
    where
        makeStatement jsn =
            Statement (toJSASTVarDeclaration jsn fileName) srcSpan fileName
-- These ones always return singleton lists. (Haskell Land) Constructors which use only these to
-- fill a field can have a JSAST for that field.
--
-- A JSExpression contains a list of JSNodes, separated by <JSLiteral ','>. These need to be
-- seperated (basically the <JSLiteral ','>s need to be stripped. My code to do that is kind of
-- disgusting.
toJSAST (NS (JSExpression jsnList) srcSpan) fileName =
    [Statement
        (List
            (map
                (\l -> listToJSASTExpression l fileName)
                (jsExpGetSublists jsnList))
            -- FIXME: Might not be the right source fragment for list.
            (jsnGetSpan $ head jsnList)
            fileName)
        srcSpan
        fileName
    ]
    where
        -- TODO: This could be improved.
        jsExpGetSublists [] = []
        jsExpGetSublists [item] = [[item]]
        jsExpGetSublists ls =
            let (nextSub, rest) = getSublist ls in ([nextSub] ++ (jsExpGetSublists rest))
        getSublist [] = ([], [])
        getSublist [item] = ([item], [])
        getSublist ((NS (JSLiteral ",") _):rest) = ([], rest)
        getSublist (y:ys) = let (next, rest) = getSublist ys in (y:(next), rest)
toJSAST (NS (JSFunction name inputs body) srcSpan) fileName =
    [FunctionDeclaration
        (identifierGetString $ jsnGetNode name)
        (map (identifierGetString . jsnGetNode) inputs)
        (FunctionBody (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSLabelled label body) srcSpan) fileName =
    [Labelled
        (identifierGetString $ jsnGetNode label)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSBreak [] _) srcSpan) fileName =
    [Statement
        (Break Nothing srcSpan fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSBreak [label] _) srcSpan) fileName =
    [Statement
        (Break (Just $ identifierGetString $ jsnGetNode label) srcSpan fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSContinue [item]) srcSpan) fileName =
    [Statement
        (Continue Nothing srcSpan fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSContinue [label, semi]) srcSpan) fileName =
    [Statement
        (Continue (Just $ identifierGetString $ jsnGetNode label) srcSpan fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSThrow expr) srcSpan) fileName =
    [Statement
        (Throw (toJSASTExpression (jsnGetNode expr) fileName) srcSpan fileName)
        srcSpan
        fileName
    ]
    where
        toJSASTExpression (JSExpression ex) = mapListToExpression ex
-- JSForVar occurs in the case that variables are declared in the loop statement, multiple
-- predicates and multiple counter operations.
toJSAST (NS (JSForVar vars test count body) srcSpan) fileName =
    [ForVar
        (map (\v -> toJSASTVarDeclaration v fileName) vars)
        (jsnToMaybeListExp test fileName)
        (jsnToMaybeListExp count fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
-- JSFor occurs when no varibles are declared in the loop (although they may be re-assigned),
-- multiple predicates and multiple counter operations
toJSAST (NS (JSFor vars test count body) srcSpan) fileName =
    [For
        (jsnToMaybeListExp vars fileName)
        (jsnToMaybeListExp test fileName)
        (jsnToMaybeListExp count fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
-- JSForIn occurs when no variables are declared inside the for statement, and the loop iterates
-- over some object (e.g. an array)
toJSAST (NS (JSForIn vars obj body) srcSpan) fileName =
    [ForIn
        (map (identifierGetString . jsnGetNode) vars)
        (jsnToListExp obj fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
-- JSForVarIn occurs when variables are declared inside the for statement and the loop iterates over
-- some object (e.g. an array)
toJSAST (NS (JSForVarIn var obj body) srcSpan) fileName =
    [ForVarIn
        (toJSASTVarDeclaration var fileName)
        (jsnToListExp obj fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSWhile test body) srcSpan) fileName =
    [While
        (jsnToListExp test fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
-- TODO: Look at the parser source and find out what "something" actually is
toJSAST (NS (JSDoWhile body test something) srcSpan) fileName =
    [DoWhile
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        (jsnToListExp test fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSIf test body) srcSpan) fileName =
    [If
        (jsnToListExp test fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSIfElse test trueBody falseBody) srcSpan) fileName =
    [IfElse
        (jsnToListExp test fileName)
        (Block (toJSAST trueBody fileName) (jsnGetSpan trueBody) fileName)
        (Block (toJSAST falseBody fileName) (jsnGetSpan falseBody) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSSwitch var cases) srcSpan) fileName =
    [Switch
        (jsnToListExp var fileName)
        -- FIXME: Probably not the right source span for the block
        (Block
            (concat $ map (\c -> toJSAST c fileName) cases)
            (jsnGetSpan $ head cases)
            fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSCase cs body) srcSpan) fileName =
    [Case
        (jsnToListExp cs fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSDefault body) srcSpan) fileName =
    [Default
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSTry body catchClause) srcSpan) fileName =
    [Try
        (Block
            (toJSAST body fileName)
            (jsnGetSpan body)
            fileName)
        -- FIXME: Probably not the right source span for the block
        (Block
            (astMap catchClause fileName)
            (jsnGetSpan $ head catchClause)
            fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSCatch var test body) srcSpan) fileName =
    [Catch
        (identifierGetString $ jsnGetNode var)
        (mapListToMaybeExpression test fileName)
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSFinally body) srcSpan) fileName =
    [Finally
        (Block (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSReturn [item]) srcSpan) fileName =
    [Return
        (Value JSUndefined srcSpan fileName)
        srcSpan
        fileName
    ]
toJSAST (NS (JSReturn [val, semi]) srcSpan) fileName =
    [Return
        (jsnToListExp val fileName)
        srcSpan
        fileName
    ]
-- TODO: Not 100% sure that this is safe.
toJSAST (NS (JSLiteral ";") srcSpan) fileName = []
toJSAST x fileName =
    [Statement
        (makeJSASTExpression x fileName)
        (jsnGetSpan x)
        fileName
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


-- Takes an array and makes a (Haskell Land) 2D array, one subarray per element of the (JS) array.
arrayToSubarrays :: [JSNode] -> [[JSNode]]
arrayToSubarrays [] = []
arrayToSubarrays ls = let (nextSub, rest) = getSubarray ls in ([nextSub] ++ (arrayToSubarrays rest))


-- Takes a representation of a literal array from the output of language.javascript.Parser, deals
-- with elisions at the start of the array, then processes what's left.
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


-- Takes a list of JSNodes and makes an expression.
mapListToExpression :: [JSNode] -> SourceFileName -> Expression
mapListToExpression jsn fileName = listToJSASTExpression jsn fileName


mapListToMaybeExpression :: [JSNode] -> SourceFileName -> Maybe Expression
mapListToMaybeExpression [] _ = Nothing
mapListToMaybeExpression jsn fileName = Just $ mapListToExpression jsn fileName


-- Takes a list of Nodes and builds a single expression.
--
-- TODO: Add a comment here explaining where these Node lists come from.
listToJSASTExpression :: [JSNode] -> SourceFileName -> Expression
listToJSASTExpression [item] fileName = makeJSASTExpression item fileName
-- FIXME: This is very ugly
listToJSASTExpression [(NS (JSUnary operator) srcSpan1), (NS (JSDecimal x) srcSpan2)] fileName
    | elem operator ["-"] =
        let y = if elem '.' x then
                    JSFloat (-1 * (read x))
                else
                    -- FIXME: Might not be the right source span for the expression
                    JSInt (-1 * (read x)) in Value y srcSpan1 fileName
listToJSASTExpression ((NS (JSUnary operator) srcSpan):x) fileName
    | elem operator ["-", "+", "--", "++", "!", "typeof ", "delete ", "~"] =
        UnaryPre
            operator
            (listToJSASTExpression x fileName)
            srcSpan
            fileName
listToJSASTExpression ((NS (JSLiteral "new ") srcSpan):x) fileName =
    New (listToJSASTExpression x fileName) srcSpan fileName
listToJSASTExpression [x, (NS (JSArguments args) srcSpan)] fileName =
    Call
        (makeJSASTExpression x fileName)
        (toJSASTArguments args fileName)
        (jsnGetSpan x)
        fileName
-- FIXME: This is SUPER ugly and isCallExpression isn't being used.
listToJSASTExpression list fileName =
    if (isAssignment list) then
        getJSASTAssignment list fileName
    else if (isParenCallExp $ last list) then
        getJSASTCall list fileName
    else
        getJSASTCallExpression list fileName

-- Makes arguments from a list of lists of JSNodes that represent a list of arguments.
toJSASTArguments :: [[JSNode]] -> SourceFileName -> Expression
toJSASTArguments args fileName =
    -- FIXME: Might not be the right source span for args
    Arguments (map getJSASTArgument args) (jsnGetSpan $ head $ head args) fileName
    where
        getJSASTArgument [item] = makeJSASTExpression item fileName
        getJSASTArgument nodes = mapListToExpression nodes fileName

-- Determine if a Node is an assignment operator
isAssignmentOperator :: String -> Bool
isAssignmentOperator op
    | elem op ["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "&=", "^=", "|="] = True
    | otherwise = False

-- Determine whether the list of Nodes is an assignment
isAssignment :: [JSNode] -> Bool
isAssignment [] = False
isAssignment ((NS (JSOperator op) _):ls) = (isAssignmentOperator op) || (isAssignment ls)
isAssignment (_:ls) = isAssignment ls


-- To handle messy assignments
getJSASTAssignment :: [JSNode] -> SourceFileName -> Expression
getJSASTAssignment list fileName =
    -- FIXME: This whole function is a brain drain. Refactor.
    let (pre, op, post) = getPrePost list in
        Assignment
            op
            (listToJSASTExpression pre fileName)
            (listToJSASTExpression post fileName)
            -- FIXME: Might not be the right source span for assignment list
            (jsnGetSpan $ head list)
            fileName
    where
        getPrePost ((NS (JSOperator op) _):xs)
            | (isAssignmentOperator op) = ([], op, xs)
        getPrePost (x:xs) = let (l, o, u) = getPrePost xs in (x:l, o, u)


-- Determine whether a node is JSCallExpression with parentheses
isParenCallExp :: JSNode -> Bool
isParenCallExp (NS (JSCallExpression "()" _) _) = True
isParenCallExp _ = False


-- To handle the case where the last element of the list is a (JSCallExpression "()" [JSArguments
-- _]) I don't know if the second field can be anything other than a singleton list containing a
-- JSArguments but for now I'm just going to hope not.
--
-- TODO: Just look at the parser source to work this out
getJSASTCall :: [JSNode] -> SourceFileName -> Expression
getJSASTCall list fileName =
    Call
        (listToJSASTExpression (init list) fileName)
        (lastGetArgs (argsNode $ jsnGetNode $ last list) fileName)
        (jsnGetSpan $ head list)
        fileName
    where
        argsNode (JSCallExpression _ [args]) = jsnGetNode args
        lastGetArgs (JSArguments ar) fileName = toJSASTArguments ar fileName


-- Determine whether a node is a JSCallExpression with a dot or with square brackets.
--
-- FIXME: Should be called by listToJSASTExpression but currently isn't called.
isCallExpression :: Node -> Bool
isCallExpression (JSCallExpression "." _) = True
isCallExpression (JSCallExpression "[]" _) = True
isCallExpression _ = False

-- To handle the case where the last element in the list is a (JSCallExpression "[]" exp) or a
-- (JSCallExpression "." exp).
getJSASTCallExpression :: [JSNode] -> SourceFileName -> Expression
getJSASTCallExpression list fileName =
    CallExpression
        (listToJSASTExpression (init list) fileName)
        (callExpOperator $ jsnGetNode $ last list)
        (callExpProperty $ jsnGetNode $ last list)
        (jsnGetSpan $ head list)
        fileName
    where
        callExpOperator (JSCallExpression operator _) = operator
        callExpProperty (JSCallExpression "[]" expr) = statementToListExp $ astMap expr fileName
        callExpProperty (JSCallExpression "." [expr]) =
            makeJSASTExpression expr fileName


-- Takes a Node that represents a property of an object and produdes a singleton list containing a
-- PropNameValue Expression.
toJSASTPropNameValue :: JSNode -> SourceFileName -> [Expression]
toJSASTPropNameValue (NS (JSPropertyNameandValue (NS (JSIdentifier name) _) value) srcSpan) fileName =
    [PropNameValue
        (VariableProperty name)
        (listToJSASTExpression value fileName)
        srcSpan
        fileName
    ]
toJSASTPropNameValue (NS (JSPropertyNameandValue (NS (JSDecimal index) _) value) srcSpan) fileName =
    [PropNameValue
        (IndexProperty (read index))
        (listToJSASTExpression value fileName)
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
makeJSASTExpression :: JSNode -> SourceFileName -> Expression
makeJSASTExpression (NS (JSExpressionBinary operator left right) srcSpan) fileName =
    Binary
        operator
        -- FIXME: Needs to use the real file name
        (mapListToExpression left fileName)
        (mapListToExpression right fileName)
        srcSpan
        fileName
makeJSASTExpression (NS (JSIdentifier "undefined") srcSpan) fileName =
    Value JSUndefined srcSpan fileName
makeJSASTExpression (NS (JSLiteral "null") srcSpan) fileName =
    Value JSNull srcSpan fileName
makeJSASTExpression (NS (JSLiteral "this") srcSpan) fileName =
    Identifier "this" srcSpan fileName
makeJSASTExpression (NS (JSIdentifier identifier) srcSpan) fileName =
    Identifier identifier srcSpan fileName
makeJSASTExpression (NS (JSExpressionPostfix operator variable) srcSpan) fileName =
    UnaryPost
        operator
        (mapListToExpression variable fileName)
        srcSpan
        fileName
makeJSASTExpression (NS (JSExpressionParen expr) srcSpan) fileName =
    ParenExpression
        (jsnToListExp expr fileName)
        srcSpan
        fileName
makeJSASTExpression (NS (JSExpressionTernary expr ifTrue ifFalse) srcSpan) fileName =
    Ternary
        (mapListToExpression expr fileName)
        (mapListToExpression ifTrue fileName)
        (mapListToExpression ifFalse fileName)
        srcSpan
        fileName
makeJSASTExpression (NS (JSMemberDot pre post) srcSpan) fileName =
    Reference
        (mapListToExpression pre fileName)
        (makeJSASTExpression post fileName)
        srcSpan
        fileName
makeJSASTExpression (NS (JSMemberSquare pre post) srcSpan) fileName =
    Index
        (mapListToExpression pre fileName)
        (jsnToListExp post fileName)
        srcSpan
        fileName
makeJSASTExpression (NS (JSArguments args) srcSpan) fileName = toJSASTArguments args fileName
makeJSASTExpression (NS (JSFunctionExpression [name] args body) srcSpan) fileName =
    FunctionExpression
        (Just $ identifierGetString $ jsnGetNode name)
        (map (identifierGetString . jsnGetNode) args)
        (FunctionBody (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
makeJSASTExpression (NS (JSFunctionExpression [] args body) srcSpan) fileName =
    FunctionExpression
        Nothing
        (map (identifierGetString . jsnGetNode) args)
        (FunctionBody (toJSAST body fileName) (jsnGetSpan body) fileName)
        srcSpan
        fileName
-- Anything left unmatched here is assumed to be a literal value.
makeJSASTExpression val fileName =
    Value (toJSASTValue (jsnGetNode val) fileName) (jsnGetSpan val) fileName
