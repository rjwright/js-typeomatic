
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
-- (astListWSMakeSourceFragments (getASTWithSource (parseTree program file) file) span)

module ResolveSourceFragments
( ASTWithSourceFragment(..)
, ASTWithSF(..)
, SourceFragment(..)
, ValueWithSourceFragment(..)
, astWSMakeSourceFragment
, astMakeSourceFragment
) where


import Control.Monad.State
import Language.JavaScript.Parser
import ParseJS
import System.Environment


type Row = Int
type Col = Int

-- (FileName, StartRow, StartCol, EndRow, EndCol)
type SourceFragment = (String, Row, Col, Row, Col)

data ValueWithSourceFragment =
      WSArray [ASTWithSourceFragment]
    | WSBool Bool
    | WSDQString String
    | WSFloat Double
    | WSInt Int
    | WSNull
    | WSObject [ASTWithSourceFragment]
    | WSString String
    | WSUndefined deriving (Show)


data ASTWithSF =
      WSBlock ASTWithSourceFragment
    | WSCase ASTWithSourceFragment ASTWithSourceFragment
    | WSCatch Variable (Maybe ASTWithSourceFragment) ASTWithSourceFragment
    | WSDefault ASTWithSourceFragment
    | WSDoWhile ASTWithSourceFragment ASTWithSourceFragment
    | WSFinally ASTWithSourceFragment
    | WSFor (Maybe ASTWithSourceFragment) (Maybe ASTWithSourceFragment) (Maybe ASTWithSourceFragment) ASTWithSourceFragment
    | WSForIn [Variable] ASTWithSourceFragment ASTWithSourceFragment
    | WSForVar [ASTWithSourceFragment] (Maybe ASTWithSourceFragment) (Maybe ASTWithSourceFragment) ASTWithSourceFragment
    | WSForVarIn ASTWithSourceFragment ASTWithSourceFragment ASTWithSourceFragment
    | WSFunctionBody [ASTWithSourceFragment]
    | WSFunctionDeclaration Variable [Variable] ASTWithSourceFragment
    | WSIf ASTWithSourceFragment ASTWithSourceFragment
    | WSIfElse ASTWithSourceFragment ASTWithSourceFragment ASTWithSourceFragment
    | WSLabelled Variable ASTWithSourceFragment
    | WSReturn ASTWithSourceFragment
    -- TODO: Delete this. It's just here to make things compile so I can test something.
    | WSStatement ASTWithSourceFragment
    -- TODO: Delete this. It's just here to make things compile so I can test something.
    | WSList [ASTWithSourceFragment]
    | WSSwitch ASTWithSourceFragment [ASTWithSourceFragment]
    | WSTry ASTWithSourceFragment [ASTWithSourceFragment]
    | WSWhile ASTWithSourceFragment ASTWithSourceFragment

    | WSArguments [ASTWithSourceFragment]
    | WSAssignment Operator ASTWithSourceFragment ASTWithSourceFragment
    | WSBinary Operator ASTWithSourceFragment ASTWithSourceFragment
    | WSBreak (Maybe Variable)
    | WSCall ASTWithSourceFragment ASTWithSourceFragment
    | WSCallExpression ASTWithSourceFragment Operator ASTWithSourceFragment
    | WSContinue (Maybe Variable)
    -- Was called "WSList".
    | WSExpression [ASTWithSourceFragment]
    | WSFunctionExpression (Maybe Variable) [Variable] ASTWithSourceFragment
    | WSIdentifier Variable
    | WSIndex ASTWithSourceFragment ASTWithSourceFragment
    | WSNew ASTWithSourceFragment
    | WSParenExpression ASTWithSourceFragment
    | WSPropNameValue PropertyName ASTWithSourceFragment
    | WSReference ASTWithSourceFragment ASTWithSourceFragment
    -- Was called "List".
    | WSStatementList [ASTWithSourceFragment]
    | WSTernary ASTWithSourceFragment ASTWithSourceFragment ASTWithSourceFragment
    | WSThrow ASTWithSourceFragment
    | WSUnaryPost Operator ASTWithSourceFragment
    | WSUnaryPre Operator ASTWithSourceFragment
    | WSValue ValueWithSourceFragment
    | WSVarDeclaration Variable (Maybe ASTWithSourceFragment) deriving (Show)


data ASTWithSourceFragment =
      AWSF ASTWithSF SourceFragment deriving (Show)


astGetSpan :: ASTWithSourceSpan -> SrcSpan
astGetSpan (AWSS _ srcSpan) = srcSpan


-- nextSpan is the list's parent's next sibling (or the end of the file, if the parent has no next
-- sibling)
-- astListWSMakeSourceFragments :: ([ASTWithSourceSpan], SourceFileName) -> SrcSpan -> [ASTWithSourceFragment]
-- astListWSMakeSourceFragments (list, fileName) nextSpan =
--     astListMakeSourceFragments list fileName nextSpan

astWSMakeSourceFragment :: (ASTWithSourceSpan, SourceFileName) -> SrcSpan -> ASTWithSourceFragment
astWSMakeSourceFragment (ast, fileName) nextSpan =
    astMakeSourceFragment ast fileName nextSpan


-- nextSpan is the list's parent's next sibling (or the end of the file, if the parent has no next
-- sibling)
astListMakeSourceFragments :: [ASTWithSourceSpan] -> SourceFileName -> SrcSpan -> [ASTWithSourceFragment]
astListMakeSourceFragments (x:y:z) fileName nextSpan =
    (astMakeSourceFragment x fileName (astGetSpan y)):(astListMakeSourceFragments (y:z) fileName nextSpan)
astListMakeSourceFragments (x:[]) fileName nextSpan = [astMakeSourceFragment x fileName nextSpan]
astListMakeSourceFragments [] _ _ = []


valueMakeSourceFragment :: Value -> SourceFileName -> SrcSpan -> ValueWithSourceFragment
valueMakeSourceFragment (JSArray list) fileName nextSpan =
    WSArray (astListMakeSourceFragments list fileName nextSpan)
valueMakeSourceFragment (JSBool val) _ _ = WSBool val
valueMakeSourceFragment (JSDQString val) _ _ = WSDQString val
valueMakeSourceFragment (JSFloat val) _ _ = WSFloat val
valueMakeSourceFragment (JSInt val) _ _ = WSInt val
valueMakeSourceFragment JSNull _ _ = WSNull
valueMakeSourceFragment (JSObject list) fileName nextSpan =
    WSObject (astListMakeSourceFragments list fileName nextSpan)
valueMakeSourceFragment (JSString val) _ _ = WSString val
valueMakeSourceFragment  JSUndefined _ _ = WSUndefined


makeSourceFragment :: SrcSpan -> SrcSpan -> SourceFileName -> SourceFragment
makeSourceFragment (SpanPoint _ startRow startCol) (SpanPoint _ nextRow nextCol) fileName =
    (fileName, startRow, startCol, nextRow, nextCol)


maybeASTMakeSourceFragment :: Maybe ASTWithSourceSpan -> SourceFileName -> SrcSpan -> Maybe ASTWithSourceFragment
maybeASTMakeSourceFragment maybeAST fileName srcSpan =
    liftM (\m -> astMakeSourceFragment m fileName srcSpan) maybeAST


-- Here nextSpan is just the end of this fragment
-- Still to do
--      WSCase ASTWithSourceFragment ASTWithSourceFragment
--      WSCatch Variable (Maybe ASTWithSourceFragment) ASTWithSourceFragment
--      WSDefault ASTWithSourceFragment
--      WSDoWhile ASTWithSourceFragment ASTWithSourceFragment
--      WSFinally ASTWithSourceFragment
--      WSForIn [Variable] ASTWithSourceFragment ASTWithSourceFragment
--      WSIfElse ASTWithSourceFragment ASTWithSourceFragment ASTWithSourceFragment
--      WSLabelled Variable ASTWithSourceFragment
--      WSSwitch ASTWithSourceFragment ASTWithSourceFragment
--      WSTry ASTWithSourceFragment ASTWithSourceFragment
--      WSWhile ASTWithSourceFragment ASTWithSourceFragment
--
--      WSBreak (Maybe Variable)
--      WSContinue (Maybe Variable)
--      WSNew ASTWithSourceFragment
--      WSParenExpression ASTWithSourceFragment
--      WSTernary ASTWithSourceFragment ASTWithSourceFragment ASTWithSourceFragment
--      WSThrow ASTWithSourceFragment
astMakeSourceFragment :: ASTWithSourceSpan -> SourceFileName -> SrcSpan -> ASTWithSourceFragment
-- astMakeSourceFragment (AWSS () srcSpan) fileName nextSpan =
--     AWSF
--         (WS...)
--         (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Block body) srcSpan) fileName nextSpan =
    AWSF
        (WSBlock (astMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Expression body) srcSpan) fileName nextSpan =
    AWSF
        (WSExpression (astListMakeSourceFragments body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
-- FIXME: These two share a lot of code. Merge them?
astMakeSourceFragment (AWSS (For vars cond expr body) srcSpan) fileName nextSpan =
    AWSF
        (WSFor
            (maybeASTMakeSourceFragment vars fileName varsNextSpan)
            (maybeASTMakeSourceFragment cond fileName condNextSpan)
            (maybeASTMakeSourceFragment expr fileName (astGetSpan body))
            (astMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
    where
        justSpanGetSpan (Just ss) = ss
        condSrcSpan = liftM astGetSpan cond
        exprSrcSpan = liftM astGetSpan expr
        varsNextSpan =
            if (not (condSrcSpan == Nothing)) then
                justSpanGetSpan condSrcSpan
            else if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                astGetSpan body
        condNextSpan =
            if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                astGetSpan body
astMakeSourceFragment (AWSS (ForVar vars cond expr body) srcSpan) fileName nextSpan =
    AWSF
        (WSForVar
            (astListMakeSourceFragments vars fileName varsNextSpan)
            (maybeASTMakeSourceFragment cond fileName condNextSpan)
            (maybeASTMakeSourceFragment expr fileName (astGetSpan body))
            (astMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
    where
        justSpanGetSpan (Just ss) = ss
        condSrcSpan = liftM astGetSpan cond
        exprSrcSpan = liftM astGetSpan expr
        varsNextSpan =
            if (not (condSrcSpan == Nothing)) then
                justSpanGetSpan condSrcSpan
            else if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                astGetSpan body
        condNextSpan =
            if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                astGetSpan body
astMakeSourceFragment (AWSS (ForVarIn var obj body) srcSpan) fileName nextSpan =
    AWSF
        (WSForVarIn
            (astMakeSourceFragment var fileName (astGetSpan obj))
            (astMakeSourceFragment obj fileName (astGetSpan body))
            (astMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (FunctionBody list) srcSpan) fileName nextSpan =
    AWSF
        (WSFunctionBody
        (astListMakeSourceFragments list fileName nextSpan))
    (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (FunctionDeclaration var args body) srcSpan) fileName nextSpan =
    AWSF
        (WSFunctionDeclaration
            var
            args
            -- The body is the last child of the function declaration,so it has the same end point.
            (astMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (If expr body) srcSpan) fileName nextSpan =
    AWSF
        (WSIf
            (astMakeSourceFragment expr fileName (astGetSpan body))
            (astMakeSourceFragment body fileName nextSpan))
    (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Return expr) srcSpan) fileName nextSpan =
    AWSF
        (WSReturn (astMakeSourceFragment expr fileName nextSpan))
    (makeSourceFragment srcSpan nextSpan fileName)
--      WSStatementList [ASTWithSourceFragment]
astMakeSourceFragment (AWSS (StatementList body) srcSpan) fileName nextSpan =
    AWSF
        (WSStatementList (astListMakeSourceFragments body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Arguments list) srcSpan) fileName nextSpan =
    AWSF
        (WSArguments
            (astListMakeSourceFragments list fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Assignment op expr1 expr2) srcSpan) fileName nextSpan =
    AWSF
        (WSAssignment
            op
            (astMakeSourceFragment expr1 fileName (astGetSpan expr2))
            (astMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Binary op expr1 expr2) srcSpan) fileName nextSpan =
    AWSF
        (WSBinary
            op
            (astMakeSourceFragment expr1 fileName (astGetSpan expr2))
            (astMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Call expr1 expr2) srcSpan) fileName nextSpan =
    AWSF
        (WSCall
            (astMakeSourceFragment expr1 fileName (astGetSpan expr2))
            (astMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (CallExpression expr op callExpr) srcSpan) fileName nextSpan =
    AWSF
        (WSCallExpression
            (astMakeSourceFragment expr fileName (astGetSpan callExpr))
            op
            (astMakeSourceFragment callExpr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (FunctionExpression name args body) srcSpan) fileName nextSpan =
    AWSF
        (WSFunctionExpression
            name
            args
            (astMakeSourceFragment body fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Identifier var) srcSpan) fileName nextSpan =
    AWSF
        (WSIdentifier var)
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Index expr1 expr2) srcSpan) fileName nextSpan =
    AWSF
        (WSIndex
            (astMakeSourceFragment expr1 fileName (astGetSpan expr2))
            (astMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (PropNameValue name expr) srcSpan) fileName nextSpan =
    AWSF
        (WSPropNameValue
            name
            (astMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Reference expr1 expr2) srcSpan) fileName nextSpan =
    AWSF
        (WSReference
            (astMakeSourceFragment expr1 fileName (astGetSpan expr2))
            (astMakeSourceFragment expr2 fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (UnaryPost op expr) srcSpan) fileName nextSpan =
    AWSF
        (WSUnaryPost
            op
            (astMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (UnaryPre op expr) srcSpan) fileName nextSpan =
    AWSF
        (WSUnaryPre
            op
            (astMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (Value val) srcSpan) fileName nextSpan =
    AWSF
        (WSValue (valueMakeSourceFragment val fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
astMakeSourceFragment (AWSS (VarDeclaration var expr) srcSpan) fileName nextSpan =
    AWSF
        (WSVarDeclaration
            var
            (maybeASTMakeSourceFragment expr fileName nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
