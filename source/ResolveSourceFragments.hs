
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


-- This module takes an AST containing SrcSpans from the original AST (output from
-- Language.Javascript.Parser), and produces an AST with SourceFragments. SourceFragments are a
-- source file name and a range. The range is described by a start row and start column, and an end
-- row and end column. I chose this approach over carrying actual source code as strings, because I
-- would need to do unsafe IO to get the source fragments and turn then into Strings. This way we
-- just carry the range, and read the actual source code any time that we need its contents. The
-- only problem with this (other than being a bit weird) is that the underlying source file might
-- change or be removed in the course of the program, so that retrieved source code will no longer
-- match what's expected. However, this program is intended to be short-running and infrequently
-- run, so I doubt this issue will be a problem in real-world usage.
--
-- Top level function is
-- (jsastListWSMakeSourceFragments (getJSASTWithSource (parseTree program file) file) span)

module ResolveSourceFragments
( ExprWithSourceFragment(..)
, ExprWSF(..)
, JSASTWithSourceFragment(..)
, JSASTWSF(..)
, SourceFragment(..)
, ValueWithSourceFragment(..)
, jsastListWSMakeSourceFragments
) where


import Language.JavaScript.Parser
import ParseJS
import System.Environment


type Row = Int
type Col = Int

-- (FileName, StartRow, StartCol, EndRow, EndCol)
type SourceFragment = (SourceFileName, Row, Col, Row, Col)

-- Represent literal values.
data ValueWithSourceFragment =
      WSArray [ExprWithSourceFragment]
    | WSBool Bool
    -- Double quote strings are never treated differently to normal strings.
    -- TODO: Should be merged with JSString
    | WSDQString String
    | WSFloat Double
    | WSInt Int
    | WSNull
    -- TODO: Comment on what the expressions can be.
    | WSObject [ExprWithSourceFragment]
    | WSString String
    | WSUndefined deriving (Show)

data ExprWSF =
      WSArguments [ExprWithSourceFragment]
    | WSAssignment Operator ExprWithSourceFragment ExprWithSourceFragment
    | WSBinary Operator ExprWithSourceFragment ExprWithSourceFragment
    | WSBreak (Maybe Variable)
    | WSCall ExprWithSourceFragment ExprWithSourceFragment
    | WSCallExpression ExprWithSourceFragment Operator ExprWithSourceFragment
    | WSContinue (Maybe Variable)
    | WSFunctionExpression (Maybe Variable) [Variable] JSASTWithSourceFragment
    | WSIdentifier Variable
    | WSIndex ExprWithSourceFragment ExprWithSourceFragment
    | WSList [ExprWithSourceFragment]
    | WSNew ExprWithSourceFragment
    | WSParenExpression ExprWithSourceFragment
    | WSPropNameValue PropertyName ExprWithSourceFragment
    | WSReference ExprWithSourceFragment ExprWithSourceFragment
    | WSTernary ExprWithSourceFragment ExprWithSourceFragment ExprWithSourceFragment
    | WSThrow ExprWithSourceFragment
    | WSUnaryPost Operator ExprWithSourceFragment
    | WSUnaryPre Operator ExprWithSourceFragment
    | WSValue ValueWithSourceFragment
    | WSVarDeclaration Variable (Maybe ExprWithSourceFragment) deriving (Show)

data JSASTWSF =
      WSBlock [JSASTWithSourceFragment]
    | WSCase ExprWithSourceFragment JSASTWithSourceFragment
    | WSCatch Variable (Maybe ExprWithSourceFragment) JSASTWithSourceFragment
    | WSDefault JSASTWithSourceFragment
    | WSDoWhile JSASTWithSourceFragment ExprWithSourceFragment
    | WSFinally JSASTWithSourceFragment
    | WSFor (Maybe ExprWithSourceFragment) (Maybe ExprWithSourceFragment) (Maybe ExprWithSourceFragment) JSASTWithSourceFragment
    | WSForIn [Variable] ExprWithSourceFragment JSASTWithSourceFragment
    | WSForVar [ExprWithSourceFragment] (Maybe ExprWithSourceFragment) (Maybe ExprWithSourceFragment) JSASTWithSourceFragment
    | WSForVarIn ExprWithSourceFragment ExprWithSourceFragment JSASTWithSourceFragment
    | WSFunctionBody [JSASTWithSourceFragment]
    | WSFunctionDeclaration Variable [Variable] JSASTWithSourceFragment
    | WSIf ExprWithSourceFragment JSASTWithSourceFragment
    | WSIfElse ExprWithSourceFragment JSASTWithSourceFragment JSASTWithSourceFragment
    | WSLabelled Variable JSASTWithSourceFragment
    | WSReturn ExprWithSourceFragment
    | WSStatement ExprWithSourceFragment
    | WSSwitch ExprWithSourceFragment JSASTWithSourceFragment
    | WSTry JSASTWithSourceFragment JSASTWithSourceFragment
    | WSWhile ExprWithSourceFragment JSASTWithSourceFragment deriving (Show)


data JSASTWithSourceFragment =
      AWSF JSASTWSF SourceFragment deriving (Show)
data ExprWithSourceFragment =
      EWSF ExprWSF SourceFragment deriving (Show)


-- nextSpan is the list's parent's next sibling (or the end of the file, if the parent has no next
-- sibling)
jsastListWSMakeSourceFragments :: ([JSASTWithSourceSpan], SourceFileName) -> SrcSpan -> [JSASTWithSourceFragment]
jsastListWSMakeSourceFragments (list, fileName) nextSpan =
    jsastListMakeSourceFragments list fileName nextSpan


-- nextSpan is the list's parent's next sibling (or the end of the file, if the parent has no next
-- sibling)
jsastListMakeSourceFragments :: [JSASTWithSourceSpan] -> SourceFileName -> SrcSpan -> [JSASTWithSourceFragment]
jsastListMakeSourceFragments (x:y:z) fileName nextSpan =
    (jsastMakeSourceFragment x fileName (jsastGetSource y)):(jsastListMakeSourceFragments (y:z) fileName nextSpan)
jsastListMakeSourceFragments (x:[]) fileName nextSpan = [jsastMakeSourceFragment x fileName nextSpan]
jsastListMakeSourceFragments [] _ nextSpan = []


jsastGetSource :: JSASTWithSourceSpan -> SrcSpan
jsastGetSource (AWSS _ srcSpan) = srcSpan


exprGetSource :: ExprWithSourceSpan -> SrcSpan
exprGetSource (EWSS _ srcSpan) = srcSpan


maybeExprGetSource :: Maybe ExprWithSourceSpan -> Maybe SrcSpan
maybeExprGetSource (Just (EWSS _ srcSpan)) = Just srcSpan
maybeExprGetSource Nothing = Nothing


exprListMakeSourceFragments :: [ExprWithSourceSpan] -> SourceFileName -> SrcSpan -> [ExprWithSourceFragment]
exprListMakeSourceFragments (x:y:z) fileName nextSpan =
    (exprMakeSourceFragment x fileName (exprGetSource y)):(exprListMakeSourceFragments (y:z) fileName nextSpan)
exprListMakeSourceFragments (x:[]) fileName nextSpan = [exprMakeSourceFragment x fileName nextSpan]
exprListMakeSourceFragments [] _ _ = []


makeSourceFragment :: SrcSpan -> SrcSpan -> SourceFileName -> SourceFragment
makeSourceFragment (SpanPoint _ startRow startCol) (SpanPoint _ nextRow nextCol) fileName =
    (fileName, startRow, startCol, nextRow, nextCol)


-- Here nextSpan is just the end of this fragment
-- Still to do
--      WSCase ExprWithSourceFragment JSASTWithSourceFragment
--      WSCatch Variable (Maybe ExprWithSourceFragment) JSASTWithSourceFragment
--      WSDefault JSASTWithSourceFragment
--      WSDoWhile JSASTWithSourceFragment ExprWithSourceFragment
--      WSFinally JSASTWithSourceFragment
--      WSForIn [Variable] ExprWithSourceFragment JSASTWithSourceFragment
--      WSIf ExprWithSourceFragment JSASTWithSourceFragment
--      WSIfElse ExprWithSourceFragment JSASTWithSourceFragment JSASTWithSourceFragment
--      WSLabelled Variable JSASTWithSourceFragment
--      WSSwitch ExprWithSourceFragment JSASTWithSourceFragment
--      WSTry JSASTWithSourceFragment JSASTWithSourceFragment
--      WSWhile ExprWithSourceFragment JSASTWithSourceFragment
jsastMakeSourceFragment :: JSASTWithSourceSpan -> SourceFileName -> SrcSpan -> JSASTWithSourceFragment
-- jsastMakeSourceFragment (AWSS () srcSpan) fileName nextSpan =
--     AWSF
--         (WS...)
--         (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (Block list) srcSpan) fileName nextSpan =
    AWSF
        (WSBlock (jsastListMakeSourceFragments list fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
-- FIXME: These two share a lot of code. Merge them?
jsastMakeSourceFragment (AWSS (For vars cond expr body) srcSpan) fileName nextSpan =
    AWSF
        (WSFor
            (maybeExprMakeSourceFragment vars fileName varsNextSpan)
            (maybeExprMakeSourceFragment cond fileName condNextSpan)
            (maybeExprMakeSourceFragment expr fileName (jsastGetSource body))
            (jsastMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
    where
        justSpanGetSpan (Just ss) = ss
        condSrcSpan = maybeExprGetSource cond
        exprSrcSpan = maybeExprGetSource expr
        varsNextSpan =
            if (not (condSrcSpan == Nothing)) then
                justSpanGetSpan condSrcSpan
            else if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSource body
        condNextSpan =
            if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSource body
jsastMakeSourceFragment (AWSS (ForVar vars cond expr body) srcSpan) fileName nextSpan =
    AWSF
        (WSForVar
            (exprListMakeSourceFragments vars fileName varsNextSpan)
            (maybeExprMakeSourceFragment cond fileName condNextSpan)
            (maybeExprMakeSourceFragment expr fileName (jsastGetSource body))
            (jsastMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
    where
        justSpanGetSpan (Just ss) = ss
        condSrcSpan = maybeExprGetSource cond
        exprSrcSpan = maybeExprGetSource expr
        varsNextSpan =
            if (not (condSrcSpan == Nothing)) then
                justSpanGetSpan condSrcSpan
            else if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSource body
        condNextSpan =
            if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSource body
jsastMakeSourceFragment (AWSS (ForVarIn var obj body) srcSpan) fileName nextSpan =
    AWSF
        (WSForVarIn
            (exprMakeSourceFragment var fileName (exprGetSource obj))
            (exprMakeSourceFragment obj fileName (jsastGetSource body))
            (jsastMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (FunctionBody list) srcSpan) fileName nextSpan =
    AWSF
        (WSFunctionBody
        (jsastListMakeSourceFragments list fileName nextSpan))
    (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (FunctionDeclaration var args body) srcSpan) fileName nextSpan =
    AWSF
        (WSFunctionDeclaration
            var
            args
            -- The body is the last child of the function declaration,so it has the same end point.
            (jsastMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (Return expr) srcSpan) fileName nextSpan =
    AWSF
        (WSReturn (exprMakeSourceFragment expr fileName nextSpan))
    (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (Statement expr) srcSpan) fileName nextSpan =
    AWSF
        (WSStatement (exprMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)


valueMakeSourceFragment :: Value -> SourceFileName -> SrcSpan -> ValueWithSourceFragment
valueMakeSourceFragment (JSArray list) fileName nextSpan =
    WSArray (exprListMakeSourceFragments list fileName nextSpan)
valueMakeSourceFragment (JSBool val) _ _ = WSBool val
    -- Double quote strings are never treated differently to normal strings.
    -- TODO: Should be merged with JSString
valueMakeSourceFragment (JSDQString val) _ _ = WSDQString val
valueMakeSourceFragment (JSFloat val) _ _ = WSFloat val
valueMakeSourceFragment (JSInt val) _ _ = WSInt val
valueMakeSourceFragment JSNull _ _ = WSNull
valueMakeSourceFragment (JSObject list) fileName nextSpan =
    WSObject (exprListMakeSourceFragments list fileName nextSpan)
valueMakeSourceFragment (JSString val) _ _ = WSString val
valueMakeSourceFragment  JSUndefined _ _ = WSUndefined


maybeExprMakeSourceFragment :: Maybe ExprWithSourceSpan -> SourceFileName -> SrcSpan -> Maybe ExprWithSourceFragment
maybeExprMakeSourceFragment (Just exprWithSourceSpan) fileName srcSpan =
    Just (exprMakeSourceFragment exprWithSourceSpan fileName srcSpan)
maybeExprMakeSourceFragment Nothing _ _ = Nothing


-- Still to do
--      WSBreak (Maybe Variable)
--      WSContinue (Maybe Variable)
--      WSNew ExprWithSourceFragment
--      WSParenExpression ExprWithSourceFragment
--      WSTernary ExprWithSourceFragment ExprWithSourceFragment ExprWithSourceFragment
--      WSThrow ExprWithSourceFragment
exprMakeSourceFragment :: ExprWithSourceSpan -> SourceFileName -> SrcSpan -> ExprWithSourceFragment
-- exprMakeSourceFragment (EWSS () srcSpan) fileName nextSpan =
--     EWSF
--         (WS...)
--         (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Arguments list) srcSpan) fileName nextSpan =
    EWSF
        (WSArguments
            (exprListMakeSourceFragments list fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Assignment op expr1 expr2) srcSpan) fileName nextSpan =
    EWSF
        (WSAssignment
            op
            (exprMakeSourceFragment expr1 fileName (exprGetSource expr2))
            (exprMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Binary op expr1 expr2) srcSpan) fileName nextSpan =
    EWSF
        (WSBinary
            op
            (exprMakeSourceFragment expr1 fileName (exprGetSource expr2))
            (exprMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Call expr1 expr2) srcSpan) fileName nextSpan =
    EWSF
        (WSCall
            (exprMakeSourceFragment expr1 fileName (exprGetSource expr2))
            (exprMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (CallExpression expr op callExpr) srcSpan) fileName nextSpan =
    EWSF
        (WSCallExpression
            (exprMakeSourceFragment expr fileName (exprGetSource callExpr))
            op
            (exprMakeSourceFragment callExpr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (FunctionExpression name args body) srcSpan) fileName nextSpan =
    EWSF
        (WSFunctionExpression
            name
            args
            (jsastMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Identifier var) srcSpan) fileName nextSpan =
    EWSF
        (WSIdentifier var)
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Index expr1 expr2) srcSpan) fileName nextSpan =
    EWSF
        (WSIndex
            (exprMakeSourceFragment expr1 fileName (exprGetSource expr2))
            (exprMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (List list) srcSpan) fileName nextSpan =
    EWSF
        (WSList
            (exprListMakeSourceFragments list fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (PropNameValue name expr) srcSpan) fileName nextSpan =
    EWSF
        (WSPropNameValue
            name
            (exprMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Reference expr1 expr2) srcSpan) fileName nextSpan =
    EWSF
        (WSReference
            (exprMakeSourceFragment expr1 fileName (exprGetSource expr2))
            (exprMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (UnaryPost op expr) srcSpan) fileName nextSpan =
    EWSF
        (WSUnaryPost
            op
            (exprMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (UnaryPre op expr) srcSpan) fileName nextSpan =
    EWSF
        (WSUnaryPre
            op
            (exprMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Value val) srcSpan) fileName nextSpan =
    EWSF
        (WSValue (valueMakeSourceFragment val fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (VarDeclaration var expr) srcSpan) fileName nextSpan =
    EWSF
        (WSVarDeclaration
            var
            (maybeExprMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
