
module ResolveJSASTSourceFragments
( SourceFragment(..)
, getSourceFragments
, resolveJSASTSource
) where


import Language.JavaScript.Parser
import ParseJS
import System.Environment


type Row = Int
type Col = Int


-- (FileName, StartRow, StartCol, EndRow, EndCol)
type SourceFragment = (String, Row, Col, Row, Col)


data ExprWS =
      WSArguments [ExprWithSource]
    | WSAssignment Operator ExprWithSource ExprWithSource
    | WSBinary Operator ExprWithSource ExprWithSource
    | WSBreak (Maybe Variable)
    | WSCall ExprWithSource ExprWithSource
    | WSCallExpression ExprWithSource Operator ExprWithSource
    | WSContinue (Maybe Variable)
    | WSFunctionExpression (Maybe Variable) [Variable] JSASTWithSource
    | WSIdentifier Variable
    | WSIndex ExprWithSource ExprWithSource
    | WSList [ExprWithSource]
    | WSNew ExprWithSource
    | WSParenExpression ExprWithSource
    | WSPropNameValue PropertyName ExprWithSource
    | WSReference ExprWithSource ExprWithSource
    | WSTernary ExprWithSource ExprWithSource ExprWithSource
    | WSThrow ExprWithSource
    | WSUnaryPost Operator ExprWithSource
    | WSUnaryPre Operator ExprWithSource
    | WSValue Value
    | WSVarDeclaration Variable (Maybe ExprWithSource) deriving (Show)

data JSASTWS =
      WSBlock [JSASTWithSource]
    | WSCase ExprWithSource JSASTWithSource
    | WSCatch Variable (Maybe ExprWithSource) JSASTWithSource
    | WSDefault JSASTWithSource
    | WSDoWhile JSASTWithSource ExprWithSource
    | WSFinally JSASTWithSource
    | WSFor (Maybe ExprWithSource) (Maybe ExprWithSource) (Maybe ExprWithSource) JSASTWithSource
    | WSForIn [Variable] ExprWithSource JSASTWithSource
    | WSForVar [ExprWithSource] (Maybe ExprWithSource) (Maybe ExprWithSource) JSASTWithSource
    | WSForVarIn ExprWithSource ExprWithSource JSASTWithSource
    | WSFunctionBody [JSASTWithSource]
    | WSFunctionDeclaration Variable [Variable] JSASTWithSource
    | WSIf ExprWithSource JSASTWithSource
    | WSIfElse ExprWithSource JSASTWithSource JSASTWithSource
    | WSLabelled Variable JSASTWithSource
    | WSReturn ExprWithSource
    | WSStatement ExprWithSource
    | WSSwitch ExprWithSource JSASTWithSource
    | WSTry JSASTWithSource JSASTWithSource
    | WSWhile ExprWithSource JSASTWithSource deriving (Show)

data JSASTWithSource =
      AWSR JSASTWS SourceFragment deriving (Show)
data ExprWithSource =
      EWSR ExprWS SourceFragment deriving (Show)

-- resolveJSASTTopLevelSources :: [JSAST] -> [JSASTWithSource]
sfGetFileName :: SourceFragment -> SourceFileName
sfGetFileName (fileName, _, _, _, _) = fileName

sfGetStartRow :: SourceFragment -> Row
sfGetStartRow (_, startRow, _, _, _) = startRow

sfGetStartCol :: SourceFragment -> Col
sfGetStartCol (_, _, startCol, _, _) = startCol

sfGetEndRow :: SourceFragment -> Row
sfGetEndRow (_, _, _, endRow, _) = endRow

sfGetEndCol :: SourceFragment -> Col
sfGetEndCol (_, _, _, _, endCol) = endCol

jsastGetSpan :: JSASTWithSourceSpan -> SrcSpan
jsastGetSpan (AWS _ srcSpan _) = srcSpan

exprGetSpan :: ExprWithSourceSpan -> SrcSpan
exprGetSpan (EWS _ srcSpan _) = srcSpan

jsastGetFile :: JSASTWithSourceSpan -> SourceFileName
jsastGetFile (AWS _ _ fileName) = fileName

exprGetFile :: ExprWithSourceSpan -> SourceFileName
exprGetFile (EWS _ _ fileName) = fileName


-- FROM OLD STUFF
-- FIXME: Currently for the last code point we just make the start equal to the end and process it
-- as a special case. Should find a better solution.
getSourceFragments :: [SrcSpan] -> SourceFileName -> [SourceFragment] -> [SourceFragment]
getSourceFragments (s1:[]) fileName result =
    (getSourceFragment s1 s1 fileName):result
getSourceFragments (s1:s2:[]) fileName result =
    -- (getSourceFragments (s2:[]) file result) ++ (getSourceFragment s1 s2 file):result
    (getSourceFragment s1 s2 fileName):result ++ (getSourceFragments (s2:[]) fileName result)
getSourceFragments (s1:s2:sx) fileName result =
    (getSourceFragment s1 s2 fileName):result ++ (getSourceFragments (s2:sx) fileName result)

-- FROM OLD STUFF
getSourceFragment :: SrcSpan -> SrcSpan -> SourceFileName -> SourceFragment
getSourceFragment (SpanPoint _ row1 col1) (SpanPoint _ row2 col2) fileName =
    (fileName, row1, col1, row2, col2)

-- FROM OLD STUFF
makeNextFragment :: SrcSpan -> SourceFragment -> SourceFragment
makeNextFragment (SpanPoint _ startRow startCol) (fileName, nextRow, nextCol, _, _) =
    (fileName, startRow, startCol, nextRow, nextCol)

-- resolveJSASTSiblingSources :: [JSAST] -> SourceFragment -> [JSASTWithSource]
-- resolveJSASTSiblingSources siblings parentSource =
--     let reversed = reverse siblings in
--     head reversed


resolveJSASTSource :: JSASTWithSourceSpan -> SourceFragment -> JSASTWithSource
resolveJSASTSource (AWS (FunctionDeclaration var args body) srcSpan fileName) parentFragment =
    AWSR
        (WSFunctionDeclaration
            var
            args
            (resolveJSASTSource body fragment))
        fragment
    where
        fragment = makeNextFragment srcSpan parentFragment
