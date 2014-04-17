
--------------------------------------------------------------------------------

-- This module maps the scope tree of the input JavaScript and assigns
-- unambiguous labels to all identifiers. Each node in the scope tree represents
-- a function or a function expression and includes the function's identifier,
-- the type constraints generated by that function, a list of subtrees for any
-- inner functions declared in that function, subtrees for any function
-- expressions declared in that function, and a list of identifiers that are in
-- scope inside that function's body. You can uncomment some of the other
-- functions in the main function to get some different output. There's code for
-- the scope tree with the AST subree that generates each node included in that
-- node, for a scope tree with the rules field stripped, and for all the rules
-- pulled out of the tree and lumped into a list. Type rules are extracted and
-- added to the tree as the tree is built.

-- module DeclarationGraph
module Main
( ParentFunction(..)
, IdentifierLabel
, FunctionIdentifier(..)
, FunctionRules(..)
, FunctionExpressionRules(..)
, getDeclarationGraph
, main
, graphGetAllRules
) where

--------------------------------------------------------------------------------

import ParseJS
import LabelJSAST
import TypeRules
import System.Environment

-- The main reason for carting one of these around for every node in the
-- declaration graph was so that I could use it to make the call graph.
-- It is no longer necessary at this layer. Remove it? Will it be useful
-- when it comes time to compile? Should I alter it to use the unique
-- identifiers?
data ParentFunction = ParentFunDecl ASTChild
            | ParentFunExpr ExprChild
            | ParentGlobal [ASTChild]
            | TopLevel deriving (Show)

-- Identifier for functions which have a declared identifier or a dummy
-- "global function".
data FunctionIdentifier = 
            FunctionID DeclaredIdentifier
            | GlobalID deriving (Show)

-- Represents a function delcaration with an identifier, a set of rules
-- generated by the function, a list of functions delcared inside the
-- body of the function, a list of function expressions created in the
-- body of the function, a list of all identifiers visible to the
-- function (for use in generating rules with fully defined identifiers)
-- and the chunk of AST where the function is being declared (the parent).
--
-- Carrying the parent AST around seems like a waste of memory, but I
-- don't know how Haskell actually does it. I guess lazy eval means that
-- it probably doesn't actually exist all the time.
data FunctionRules = FunctionRules FunctionIdentifier [Rule] [FunctionRules]
                        [FunctionExpressionRules] [DeclaredIdentifier] ParentFunction deriving (Show)
-- Pretty much the same as FunctionRules, but for a function expression instead
-- of a function declaration.
data FunctionExpressionRules = FunctionExpressionRules (Maybe FunctionIdentifier) [Rule]
                        [FunctionRules] [FunctionExpressionRules] [DeclaredIdentifier] ParentFunction deriving (Show)

-- A FunctionRules with the parent field removed.
data CleanedFunctionRules = CleanedFunctionRules FunctionIdentifier [Rule] [CleanedFunctionRules]
                        [CleanedFunctionExpressionRules] [DeclaredIdentifier] deriving (Show)
-- A FunctionExpressionRules with the parent field removed.
data CleanedFunctionExpressionRules = CleanedFunctionExpressionRules (Maybe FunctionIdentifier) [Rule]
                        [CleanedFunctionRules] [CleanedFunctionExpressionRules] [DeclaredIdentifier] deriving (Show)

-- A FunctionRules with the parent and rules fields removed.
data CleanedFunction = CleanedFunction FunctionIdentifier [CleanedFunction]
                        [CleanedFunctionExpression] [DeclaredIdentifier] deriving (Show)
-- A FunctionExpressionRules with the parent and rules fields removed.
data CleanedFunctionExpression = CleanedFunctionExpression (Maybe FunctionIdentifier)
                        [CleanedFunction] [CleanedFunctionExpression] [DeclaredIdentifier] deriving (Show)


main :: IO ()
main = do
    (infile:[]) <- getArgs
    pr <- readFile infile
    putStrLn . show . toJSAST . parseTree $ pr
    putStrLn . show . label . toJSAST . parseTree $ pr
--    putStrLn . show . getDeclarationGraph . label . toJSAST . parseTree $ pr
    putStrLn . show . cleanFunctionRules . getDeclarationGraph . label . toJSAST . parseTree $ pr
--    putStrLn . show . cleanFunction . cleanFunctionRules . getDeclarationGraph . label . toJSAST . parseTree $ pr
--    putStrLn . show . graphGetAllRules . getDeclarationGraph . label . toJSAST . parseTree $ pr


-- Remove the parent field from a FunctionRules so that the tree is more legible
-- when printed.
cleanFunctionRules :: FunctionRules -> CleanedFunctionRules
cleanFunctionRules (FunctionRules id rules fRules feRules dIDs parent) = CleanedFunctionRules id rules
        (map cleanFunctionRules fRules) (map cleanFunctionExpressionRules feRules) dIDs
-- Remove the parent field from a FunctionExpressionRules so that the tree is
-- more legible when printed.
cleanFunctionExpressionRules :: FunctionExpressionRules -> CleanedFunctionExpressionRules
cleanFunctionExpressionRules (FunctionExpressionRules mid rules fRules feRules dIDs parent) =
        CleanedFunctionExpressionRules mid rules (map cleanFunctionRules fRules)
        (map cleanFunctionExpressionRules feRules) dIDs

-- Remove the rules field from a CleanedFunctionRules so that the tree is more
-- legible when printed.
cleanFunction :: CleanedFunctionRules -> CleanedFunction
cleanFunction (CleanedFunctionRules id rules cfRules cfeRules dIDs) = CleanedFunction id (map cleanFunction cfRules)
        (map cleanFunctionExpression cfeRules) dIDs
-- Remove the rules field from a CleanedFunctionExpressionRules so that the tree
-- is more legible when printed.
cleanFunctionExpression :: CleanedFunctionExpressionRules -> CleanedFunctionExpression
cleanFunctionExpression (CleanedFunctionExpressionRules mid rules cfRules cfeRules dIDs) =
        CleanedFunctionExpression mid (map cleanFunction cfRules) (map cleanFunctionExpression cfeRules) dIDs

-- Take all of the rules in the scope tree and put then in one list.
graphGetAllRules :: FunctionRules -> [Rule]
graphGetAllRules funrl = funGetAllRules funrl []

-- Get all the rules out of a subtree rooted at a FunctionRules node and add
-- them to the provided list of rules ("old" parameter).
funGetAllRules :: FunctionRules -> [Rule] -> [Rule]
funGetAllRules (FunctionRules id rules funs funExs dIDs parent) old = funExsRules
        where
        -- Add the rules in the parameter node to the old rules.
        currentRules = old ++ rules
        -- Map funGetAllRules over the functions declared in the parameter
        -- node and add them to currentRules.
        funsRules = mapFunGetAllRules funs currentRules
        -- Map funExprGetAllRules over the function expressions declared
        -- in the parameter node and add them to funsRules
        funExsRules = mapFunExprGetAllRules funExs funsRules


-- Map funGetAllRules over the a list of FunctionRules and add them to the
-- provided list of Rules ("rules" parameter).
mapFunGetAllRules :: [FunctionRules] -> [Rule] -> [Rule]
mapFunGetAllRules (f:fx) rules = mapFunGetAllRules fx (funGetAllRules f rules)
mapFunGetAllRules [] rules = rules

-- Get all the rules out of a subtree that is rooted at a
-- FunctionExpressionRules node and add them to the provided list of rules
-- ("old" parameter).
funExprGetAllRules :: FunctionExpressionRules -> [Rule] -> [Rule]
funExprGetAllRules (FunctionExpressionRules id rules funs funExs dIDs parent) old = funExsRules
        where
        -- Add the rules in the parameter node to the old rules.
        currentRules = old ++ rules
        -- Map funGetAllRules over the functions declared in the parameter
        -- node and add them to currentRules.
        funsRules = mapFunGetAllRules funs currentRules
        -- Map funExprGetAllRules over the function expressions declared
        -- in the parameter node and add them to funsRules
        funExsRules = mapFunExprGetAllRules funExs funsRules

-- Map funExprGetAllRules over the a list of FunctionExpressionRules and add
-- them to the provided list of Rules ("rules" parameter).
mapFunExprGetAllRules :: [FunctionExpressionRules] -> [Rule] -> [Rule]
mapFunExprGetAllRules (f:fx) rules = mapFunExprGetAllRules fx (funExprGetAllRules f rules)
mapFunExprGetAllRules [] rules = rules

-- The crux of this module. Take a labelled abstract syntax tree and produce a
-- tree with FunctionRule and FunctionExpression vertices. Each vertex
-- represents a block that will spawn a new scope when entered. Each node
-- includes the type rules gathered from within that block. The rules are
-- expressed with unique identifiers so that they can later be extracted
-- from the tree and placed in a set without identifier collisions occuring.
getDeclarationGraph :: [ASTChild] -> FunctionRules
-- Make a dummy global function, with all global level functions and variables
-- declared in the "body" of the global function.
getDeclarationGraph jsastLab = FunctionRules GlobalID (mapASTChildRules jsastLab dIDs)
        (mapASTGetFR jsastLab (ParentGlobal jsastLab) dIDs) (mapASTGetFER jsastLab (ParentGlobal jsastLab) dIDs)
        (concat . map astGetVarDecs $ jsastLab) TopLevel
        where
        -- Get identifiers for everything declared at the global level.
        dIDs = (concat . map astGetVarDecs $ jsastLab)


-- Find all indentifiers declared in the signature and body of a function.
funDecGetVarDecs :: ASTChild -> [DeclaredIdentifier]
funDecGetVarDecs (LabFunctionDeclaration var args body, n) =
        -- Add the arguments.
        (map argMakeLabel args) ++
        -- Add any variables declared in the body.
        (astGetVarDecs $ body) ++
        -- Add the 'this' identifier.
        [DeclaredIdentifier "this" (IDLabel n)]

-- Find all indentifiers declared in the signature and body of a function expression.
funExprGetVarDecs :: ExprChild -> [DeclaredIdentifier]
funExprGetVarDecs (LabFunctionExpression mv args body, n) =
        -- Add the name of the function expression, if it has one.
        (listID (funExprMakeLabel thisFun)) ++
        -- Add the arguments.
        (map argMakeLabel args) ++
        -- Add any variables declared in the body.
        (astGetVarDecs $ body) ++
        -- Add the 'this' identifier.
        [DeclaredIdentifier "this" (IDLabel n)]
        where
        -- Give a name to the (Haskell Land) argument.
        thisFun = (LabFunctionExpression mv args body, n)
        -- Find the name of this function expression.
        listID Nothing = []
        listID (Just id) = [id]

-- Add two lists of identifiers. If an identifier occurs in both lists, keep the
-- element from the second list.
addDeclarationLists :: [DeclaredIdentifier] -> [DeclaredIdentifier] -> [DeclaredIdentifier]
-- Remove any over-written identifiers from the old list and add the remaining
-- identifiers to the new list.
addDeclarationLists old new = (addDec old new) ++ new
        where
        addDec [] _ = []
        -- Add identifiers to the combined list one at a time.
        addDec (o:os) n = (addDec' o n) ++ (addDec os n)
        -- Add the identifier from the old list if the new
        -- list doesn't contain it.
        addDec' o n = if listContains o n then [] else [o]
        -- Check if the new list contains an identifier.
        listContains _ [] = False
        listContains o (n:ns) = (identifiersMatch o n) || (listContains o ns)
        -- Check if two identifiers are the same (they will have unique labels,
        -- but we added those. We're looking for variables with the same name.)
        identifiersMatch  (DeclaredIdentifier id1 x) (DeclaredIdentifier id2 y) = id1 == id2


-- Map astGetFunRules over a list of labelled AST nodes.
mapASTGetFR :: [ASTChild] -> ParentFunction -> [DeclaredIdentifier] -> [FunctionRules]
mapASTGetFR ast parent dIDs = concat . map mfr $ ast
        where
        mfr a = astGetFunRules a parent dIDs

-- Map astGetFunExprRules over a list of labelled AST nodes.
mapASTGetFER :: [ASTChild] -> ParentFunction -> [DeclaredIdentifier] -> [FunctionExpressionRules]
mapASTGetFER ast parent dIDs = concat . map mfr $ ast
        where
        mfr a = astGetFunExprRules a parent dIDs

-- Map exprGetFunExprRules over a list of labelled Expression nodes.
mapExpGetFER :: [ExprChild] -> ParentFunction -> [DeclaredIdentifier] -> [FunctionExpressionRules]
mapExpGetFER exList parent dIDs = concat . map gfe $ exList
        where
        gfe ex = exprGetFunExprRules ex parent dIDs


-- Make FunctionRules from an labelled AST node. For most inputs, with the
-- exception of LabFunctionDeclaration, LabReturn and LabStatement, just process
-- the body recursively.
astGetFunRules :: ASTChild -> ParentFunction -> [DeclaredIdentifier] -> [FunctionRules]
astGetFunRules (LabBlock astList, n) parent dIDs = (mapASTGetFR astList parent dIDs)
astGetFunRules (LabFunctionBody astList, n) parent dIDs = (mapASTGetFR astList parent dIDs)
-- For a FunctionDeclaration we add a new FunctionRules.
astGetFunRules (LabFunctionDeclaration (id, x) args body, n) parent dIDs =
        -- Make a new FunctionRules.
        [
        FunctionRules (FunctionID (funDecMakeLabel thisFunction))
        -- Find the type rules generated by this function's body.
        (astChildRules body declaredIDs)
        -- Find any function declarations in this function's body and process
        -- recursively.
        (astGetFunRules body newParent declaredIDs)
        -- Find any function expressions in this function's body and process
        -- recursively.
        (astGetFunExprRules body newParent declaredIDs)
        -- Declared identifiers visible inside this function's body, plus this
        -- function's parent AST.
        declaredIDs parent
        ]
        where
        -- Gives a name to the argument. I find myself doing this a lot when
        -- pattern matching on different sorts of inputs to a function. It's
        -- quite inelegant, but I don't think there's really any way around it.
        -- I think haskell should automatically bind a varible to function
        -- parameters.
        thisFunction = (LabFunctionDeclaration (id, x) args body, n)
        -- Take the input list of declared identifiers and add any identifiers
        -- declared in this function declaration, overwriting identifiers as
        -- neccessary. Resulting list of declared identifiers is passed into
        -- calls to astChildRules, astGetFunRule and astGetFunExprRules for this
        -- function's body.
        declaredIDs = (addDeclarationLists dIDs (funDecGetVarDecs thisFunction))
        -- The parent AST for this function declaration.
        newParent = ParentFunDecl thisFunction
astGetFunRules (LabLabelled label body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabForVar varEx test count body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabFor varEx test count body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabForIn varList obj body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabForVarIn varEx obj body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabWhile test body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabDoWhile body test, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabIf test body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabIfElse test bodyT bodyF, n) parent dIDs = (astGetFunRules bodyT parent dIDs) ++
        (astGetFunRules bodyF parent dIDs)
astGetFunRules (LabSwitch id cases, n) parent dIDs = astGetFunRules cases parent dIDs
astGetFunRules (LabCase ex body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabDefault body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabTry body catches, n) parent dIDs = (astGetFunRules body parent dIDs) ++
        (astGetFunRules catches parent dIDs)
astGetFunRules (LabCatch var mTest body, n) parent dIDs = astGetFunRules body parent dIDs
astGetFunRules (LabFinally body, n) parent dIDs = astGetFunRules body parent dIDs
-- LabReturn has only one field, an expression. And expression can't (deirectly)
-- contain a function declaration. Return nothing.
astGetFunRules (LabReturn ex, n) parent dIDs = []
-- LabStatement has only one field, an expression. And expression can't (deirectly)
-- contain a function declaration. Return nothing.
astGetFunRules (LabStatement ex, n) parent dIDs = []


-- Make FunctionExpressionRules from an AST. An ASTChild can't immediately
-- represent a function expression. Recursively process all child ASTs and
-- expressions.
astGetFunExprRules :: ASTChild -> ParentFunction -> [DeclaredIdentifier] -> [FunctionExpressionRules]
astGetFunExprRules (LabBlock astList, n) parent dIDs = (mapASTGetFER astList parent dIDs)
astGetFunExprRules (LabFunctionBody astList, n) parent dIDs = (mapASTGetFER astList parent dIDs)
astGetFunExprRules (LabFunctionDeclaration id args body, n) parent dIDs = []
astGetFunExprRules (LabLabelled label body, n) parent dIDs = astGetFunExprRules body parent dIDs
astGetFunExprRules (LabForVar varEx test count body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (mapExpGetFER varEx parent dIDs) ++ (getMaybeFunExprRules test parent dIDs) ++
        (getMaybeFunExprRules count parent dIDs)
astGetFunExprRules (LabFor varEx test count body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (getMaybeFunExprRules varEx parent dIDs) ++ (getMaybeFunExprRules test parent dIDs) ++
        (getMaybeFunExprRules count parent dIDs)
astGetFunExprRules (LabForIn varList obj body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (exprGetFunExprRules obj parent dIDs)
astGetFunExprRules (LabForVarIn varEx obj body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (exprGetFunExprRules obj parent dIDs) ++ (exprGetFunExprRules varEx parent dIDs)
astGetFunExprRules (LabWhile test body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (exprGetFunExprRules test parent dIDs)
astGetFunExprRules (LabDoWhile body test, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (exprGetFunExprRules test parent dIDs)
astGetFunExprRules (LabIf test body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (exprGetFunExprRules test parent dIDs)
astGetFunExprRules (LabIfElse test bodyT bodyF, n) parent dIDs = (astGetFunExprRules bodyT parent dIDs) ++
        (astGetFunExprRules bodyF parent dIDs) ++ (exprGetFunExprRules test parent dIDs)
astGetFunExprRules (LabSwitch id cases, n) parent dIDs = (astGetFunExprRules cases parent dIDs) ++
        (exprGetFunExprRules id parent dIDs)
astGetFunExprRules (LabCase ex body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (exprGetFunExprRules ex parent dIDs)
astGetFunExprRules (LabDefault body, n) parent dIDs = astGetFunExprRules body parent dIDs
astGetFunExprRules (LabTry body catches, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (astGetFunExprRules catches parent dIDs)
astGetFunExprRules (LabCatch var mTest body, n) parent dIDs = (astGetFunExprRules body parent dIDs) ++
        (getMaybeFunExprRules mTest parent dIDs)
astGetFunExprRules (LabFinally body, n) parent dIDs = astGetFunExprRules body parent dIDs
astGetFunExprRules (LabReturn ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs
astGetFunExprRules (LabStatement ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs


-- Does this make sense with the rules code that handles members of objects and
-- arrays? TODO
-- Search object and array literals for function expressions.
valueGetFunExprRules :: ValueChild -> ParentFunction -> [DeclaredIdentifier] -> [FunctionExpressionRules]
valueGetFunExprRules (LabObject ex, n) parent dIDs = mapExpGetFER ex parent dIDs
valueGetFunExprRules (LabArray els, n) parent dIDs = mapExpGetFER els parent dIDs
valueGetFunExprRules _ _ _ = []


-- Make FunctionExpressionRules from an Expression. All of these, with the
-- exception of LabFunctionExpression, either return nothing (when they don't
-- contain any fields that could contain a function expression), or recursively
-- process any expression or value fields. 
exprGetFunExprRules :: ExprChild -> ParentFunction -> [DeclaredIdentifier] -> [FunctionExpressionRules]
exprGetFunExprRules (LabList expList, n) parent dIDs = mapExpGetFER expList parent dIDs
exprGetFunExprRules (LabBinary op ex1 ex2, n) parent dIDs = (exprGetFunExprRules ex1 parent dIDs) ++
        (exprGetFunExprRules ex2 parent dIDs)
exprGetFunExprRules (LabUnaryPost op ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs
exprGetFunExprRules (LabUnaryPre op ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs
exprGetFunExprRules (LabTernary ex1 ex2 ex3, n) parent dIDs = (exprGetFunExprRules ex1 parent dIDs) ++
        (exprGetFunExprRules ex2 parent dIDs) ++ (exprGetFunExprRules ex3 parent dIDs)
exprGetFunExprRules (LabAssignment op ex1 ex2, n) parent dIDs = (exprGetFunExprRules ex1 parent dIDs) ++
        (exprGetFunExprRules ex2 parent dIDs)
exprGetFunExprRules (LabIdentifier var, n) parent dIDs = []
exprGetFunExprRules (LabReference ex1 ex2, n) parent dIDs = (exprGetFunExprRules ex1 parent dIDs) ++
        (exprGetFunExprRules ex2 parent dIDs)
exprGetFunExprRules (LabIndex ex1 ex2, n) parent dIDs = (exprGetFunExprRules ex1 parent dIDs) ++
        (exprGetFunExprRules ex2 parent dIDs)
-- Needs to deal with objects and arrays, whose members can be function expressions. TODO
-- Is this already done in the code that handles values? TODO
-- exprGetFunExprRules (LabValue (LabOjbect ex, r), n) parent dIDs = []
-- exprGetFunExprRules (LabValue (LabArray ex, r), n) parent dIDs = []
exprGetFunExprRules (LabValue val, n) parent dIDs = valueGetFunExprRules val parent dIDs
exprGetFunExprRules (LabPropNameValue var ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs
exprGetFunExprRules (LabCall ex1 ex2, n) parent dIDs = (exprGetFunExprRules ex1 parent dIDs) ++
        (exprGetFunExprRules ex2 parent dIDs)
exprGetFunExprRules (LabArguments args, n) parent dIDs = mapExpGetFER args parent dIDs
exprGetFunExprRules (LabParenExpression ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs
exprGetFunExprRules (LabBreak var, n) parent dIDs = []
exprGetFunExprRules (LabContinue var, n) parent dIDs = []
exprGetFunExprRules (LabThrow ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs
exprGetFunExprRules (LabCallExpression ex1 op ex2, n) parent dIDs = (exprGetFunExprRules ex1 parent dIDs) ++
        (exprGetFunExprRules ex2 parent dIDs)
-- For a LabFunctionExpression we add a new FunctionExpressionRules.
exprGetFunExprRules (LabFunctionExpression mv vls body, n) parent dIDs =
        -- Make a new FunctionExpressionRules.
        [
        FunctionExpressionRules
        -- Include the name of the function expression, if it has one.
        (getMaybeID (funExprMakeLabel thisExpr))
        -- Find type rules generated in this function expression's body.
        (astChildRules body declaredIDs)
        -- Find all function declarations in this function expression's body and
        -- process recursively.
        (astGetFunRules body newParent declaredIDs)
        -- Find all function expressions in this function expression's body and
        -- process recursively.
        (astGetFunExprRules body newParent declaredIDs)
        -- Declared identifiers visible inside this function expression's body,
        -- plus this function expression's parent AST.
        declaredIDs parent
        ]
        where
        -- Give a name to the (Haskell Land) argument.
        thisExpr = (LabFunctionExpression mv vls body, n)
        -- Take the input list of declared identifiers and add any identifiers
        -- declared in this function expression, overwriting identifiers as
        -- neccessary. Resulting list of declared identifiers is passed into
        -- calls to astChildRules, astGetFunRule and astGetFunExprRules for this
        -- function expression's body.
        declaredIDs = addDeclarationLists dIDs (funExprGetVarDecs thisExpr)
        -- Get the function expression's name, if it has one.
        getMaybeID Nothing = Nothing
        getMaybeID (Just id) = Just (FunctionID id)
        -- This function expression's parent AST.
        newParent = ParentFunExpr thisExpr
exprGetFunExprRules (LabVarDeclaration var mex, n) parent dIDs = getMaybeFunExprRules mex parent dIDs
exprGetFunExprRules (LabNew ex, n) parent dIDs = exprGetFunExprRules ex parent dIDs

-- Make FunctionExpressionRules from Maybe ExprChild.
getMaybeFunExprRules :: (Maybe ExprChild) -> ParentFunction -> [DeclaredIdentifier] -> [FunctionExpressionRules]
getMaybeFunExprRules Nothing parent dIDs = []
getMaybeFunExprRules (Just ex) parent dIDs = exprGetFunExprRules ex parent dIDs


-- Find all identifiers declared in an Expression. All of these, with the
-- exception of LabVarDeclaration, return nothing or recursively process any
-- expression fields recursively.
--
-- NOTE: Argument lists declare variables (see testargscope.js). DONE - see funDecGetVarDecs.
-- Possibly the same for ForIn. TODO
exprGetVarDecs :: ExprChild -> [DeclaredIdentifier]
exprGetVarDecs (LabList ex, n) = concat . map exprGetVarDecs $ ex
exprGetVarDecs (LabBinary op ex1 ex2, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2)
exprGetVarDecs (LabUnaryPost op ex, n) = exprGetVarDecs $ ex
exprGetVarDecs (LabUnaryPre op ex, n) = exprGetVarDecs $ ex
exprGetVarDecs (LabTernary ex1 ex2 ex3, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2) ++ (exprGetVarDecs $ ex3)
exprGetVarDecs (LabAssignment op ex1 ex2, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2)
exprGetVarDecs (LabIdentifier var, n) = []
exprGetVarDecs (LabReference ex1 ex2, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2)
exprGetVarDecs (LabIndex ex1 ex2, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2)
exprGetVarDecs (LabValue val, n) = []
exprGetVarDecs (LabPropNameValue var ex, n) = exprGetVarDecs $ ex
exprGetVarDecs (LabCall ex1 ex2, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2)
exprGetVarDecs (LabArguments ex, n) = concat . map exprGetVarDecs $ ex
exprGetVarDecs (LabParenExpression ex, n) = exprGetVarDecs $ ex
exprGetVarDecs (LabBreak mv, n) = []
exprGetVarDecs (LabContinue mv, n) = []
exprGetVarDecs (LabThrow ex, n) = exprGetVarDecs $ ex
exprGetVarDecs (LabCallExpression ex1 op ex2, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2)
-- A function expression doesn't declare anything that is relevant at this scope.
exprGetVarDecs (LabFunctionExpression mv var body, n)  = []
-- Return the unique identifier for the variable being declared and recursively
-- process the expression field.
exprGetVarDecs (LabVarDeclaration (var, x) mex, n) = [varDecMakeLabel $ thisVarDec] ++ (maybeExprGetVarDecs $ mex)
        where 
        -- Name the (Haskell Land) argument.
        thisVarDec = (LabVarDeclaration (var, x) mex, n)
exprGetVarDecs (LabNew ex, n) = exprGetVarDecs $ ex


-- Find variable declarations in a Maybe ExprChild.
maybeExprGetVarDecs :: (Maybe ExprChild) -> [DeclaredIdentifier]
maybeExprGetVarDecs Nothing = []
maybeExprGetVarDecs (Just ex) = exprGetVarDecs $ ex


-- Find all identifiers declared in a JSAST. All of these, with the exception of
-- LabFunctionDeclaration and LabLabelled, return nothing or recursively process
-- any AST or expression fields.
astGetVarDecs :: ASTChild -> [DeclaredIdentifier]
astGetVarDecs (LabBlock body, n) = concat . map astGetVarDecs $ body
astGetVarDecs (LabFunctionBody body, n) = concat . map astGetVarDecs $ body
-- Add the name of the function. Definitions of functions don't declare any
-- variables that are relevant at this scope.
astGetVarDecs (LabFunctionDeclaration (var, x) args body, n) = [funDecMakeLabel thisFun]
        where
        thisFun = (LabFunctionDeclaration (var, x) args body, n)
-- Not really sure about this. TODO
astGetVarDecs (LabLabelled (var, x) body, n) = [labelledMakeLabel thisLabelled] ++ (astGetVarDecs $ body)
        where
        thisLabelled = (LabLabelled (var, x) body, n)
astGetVarDecs (LabForVar ex mex1 mex2 body, n) = (concat . map exprGetVarDecs $ ex) ++ (maybeExprGetVarDecs $ mex1) ++
        (maybeExprGetVarDecs $ mex2) ++ (astGetVarDecs $ body)
astGetVarDecs (LabFor mex1 mex2 mex3 body, n) = (maybeExprGetVarDecs $ mex1) ++ (maybeExprGetVarDecs $ mex2) ++
        (maybeExprGetVarDecs $ mex3) ++ (astGetVarDecs $ body)
astGetVarDecs (LabForIn vars ex body, n) = (exprGetVarDecs $ ex) ++ (astGetVarDecs $ body)
astGetVarDecs (LabForVarIn ex1 ex2 body, n) = (exprGetVarDecs $ ex1) ++ (exprGetVarDecs $ ex2) ++
        (astGetVarDecs $ body)
astGetVarDecs (LabWhile ex body, n) = (exprGetVarDecs $ ex) ++ (astGetVarDecs $ body)
astGetVarDecs (LabDoWhile body ex, n) = (astGetVarDecs $ body) ++ (exprGetVarDecs $ ex)
astGetVarDecs (LabIf ex body, n) = (exprGetVarDecs $ ex) ++ (astGetVarDecs $ body)
astGetVarDecs (LabIfElse ex bodyT bodyF, n) = (exprGetVarDecs $ ex) ++ (astGetVarDecs $ bodyT) ++
        (astGetVarDecs $ bodyF)
astGetVarDecs (LabSwitch ex cases, n) = (exprGetVarDecs $ ex) ++ (astGetVarDecs $ cases)
astGetVarDecs (LabCase ex body, n) = (exprGetVarDecs $ ex) ++ (astGetVarDecs $ body)
astGetVarDecs (LabDefault body, n) = astGetVarDecs $ body
astGetVarDecs (LabTry body catch, n) = (astGetVarDecs $ body) ++ (astGetVarDecs $ catch)
astGetVarDecs (LabCatch var mex body, n) = (maybeExprGetVarDecs $ mex) ++ (astGetVarDecs $ body)
astGetVarDecs (LabFinally body, n) = astGetVarDecs $ body
astGetVarDecs (LabReturn ex, n) = exprGetVarDecs $ ex
astGetVarDecs (LabStatement ex, n) = exprGetVarDecs $ ex
