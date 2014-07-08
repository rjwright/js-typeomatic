
module ResolveJSASTSourceFragments
(
    resolveJSASTSource
) where


import ParseJS
import System.Environment

-- data ExprWithSource =
--       WSArguments [ExprWithSource] SourceFragment
--     | WSAssignment Operator ExprWithSource ExprWithSource SourceFragment
--     | WSBinary Operator ExprWithSource ExprWithSource SourceFragment
--     | WSBreak (Maybe Variable) SourceFragment
--     | WSCall ExprWithSource ExprWithSource SourceFragment
--     | WSCallExpression ExprWithSource Operator ExprWithSource SourceFragment
--     | WSContinue (Maybe Variable) SourceFragment
--     | WSFunctionExpression (Maybe Variable) [Variable] JSASTWithSource SourceFragment
--     | WSIdentifier Variable SourceFragment
--     | WSIndex ExprWithSource ExprWithSource SourceFragment
--     | WSList [ExprWithSource] SourceFragment
--     | WSNew ExprWithSource SourceFragment
--     | WSParenExpression ExprWithSource SourceFragment
--     | WSPropNameValue PropertyName ExprWithSource SourceFragment
--     | WSReference ExprWithSource ExprWithSource SourceFragment
--     | WSTernary ExprWithSource ExprWithSource ExprWithSource SourceFragment
--     | WSThrow ExprWithSource SourceFragment
--     | WSUnaryPost Operator ExprWithSource SourceFragment
--     | WSUnaryPre Operator ExprWithSource SourceFragment
--     | WSValue Value SourceFragment
--     | WSVarDeclaration Variable (Maybe ExprWithSource) SourceFragment deriving (Show)

-- data JSASTWithSource =
--       WSBlock [JSASTWithSource] SourceFragment
--     | WSCase ExprWithSource JSASTWithSource SourceFragment
--     | WSCatch Variable (Maybe ExprWithSource) JSASTWithSource SourceFragment
--     | WSDefault JSASTWithSource SourceFragment
--     | WSDoWhile JSASTWithSource ExprWithSource SourceFragment
--     | WSFinally JSASTWithSource SourceFragment
--     | WSFor (Maybe ExprWithSource) (Maybe ExprWithSource) (Maybe ExprWithSource) JSASTWithSource SourceFragment
--     | WSForIn [Variable] ExprWithSource JSASTWithSource SourceFragment
--     | WSForVar [ExprWithSource] (Maybe ExprWithSource) (Maybe ExprWithSource) JSASTWithSource SourceFragment
--     | WSForVarIn ExprWithSource ExprWithSource JSASTWithSource SourceFragment
--     | WSFunctionBody [JSASTWithSource] SourceFragment
--     | WSFunctionDeclaration Variable [Variable] JSASTWithSource SourceFragment
--     | WSIf ExprWithSource JSASTWithSource SourceFragment
--     | WSIfElse ExprWithSource JSASTWithSource JSASTWithSource SourceFragment
--     | WSLabelled Variable JSASTWithSource SourceFragment
--     | WSReturn ExprWithSource SourceFragment
--     | WSStatement ExprWithSource SourceFragment
--     | WSSwitch ExprWithSource JSASTWithSource SourceFragment
--     | WSTry JSASTWithSource JSASTWithSource SourceFragment
--     | WSWhile ExprWithSource JSASTWithSource SourceFragment deriving (Show)

-- resolveJSASTTopLevelSources :: [JSAST] -> [JSASTWithSource]
sfGetFileName :: SourceFragment -> SourceFileName
sfGetFileName (fileName _ _ _ _) = fileName

sfGetStartRow :: SourceFragment -> Row
sfGetStartRow (_ startRow _ _ _) = startRow

sfGetStartCol :: SourceFragment -> Col
sfGetStartCol (_ _ startCol _ _) = startCol

sfGetEndRow :: SourceFragment -> Row
sfGetEndRow (_ _ _ endRow _) = endRow

sfGetEndCol :: SourceFragment -> Col
sfGetEndCol (_ _ _ _ endCol) = endCol

topNodeGetSpan :: JSNode -> [SrcSpan]
topNodeGetSpan (NS (JSSourceElementsTop elements) _) =
    map jsnGetSpan elements

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


getSourceFragment :: SrcSpan -> SrcSpan -> SourceFileName -> SourceFragment
getSourceFragment (SpanPoint _ row1 col1) (SpanPoint _ row2 col2) fileName =
    (fileName, row1, col1, row2, col2)

makeNextFragment :: SrcSpan -> SourceFragment -> SourceFragment
makeNextFragment (SpanPoint _ startRow startCol) (fileName, nextRow, nextCol, _, _) =
    (fileName, startRow, startCol, nextRow, nextCol)

resolveJSASTSiblingSources :: [JSAST] -> SourceFragment -> [JSASTWithSource]
resolveJSASTSiblingSources siblings parentSource =
    let reversed = reverse siblings in
    head reversed


resolveJSASTSource :: JSAST -> JSASTWithSource
resolveJSASTSource (FunctionDeclaration Variable [Variable] JSAST SrcSpan SourceFileName
resolveJSASTSource (FunctionDeclaration var args body srcSpan fileName) =
    WSFunctionDeclaration
        var
        args
        (resolveJSASTSource body)

-- FIXME: This is awful! Rewrite the JSAST tree so that
-- the source file and span are easy to get out.
jsastGetSpanAndFile :: JSAST -> (SrcSpan, SourceFileName)
jsastGetSpanAndFile (Block _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Case _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Catch _ _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Default _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (DoWhile _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Finally _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (For _ _ _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (ForIn _ _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (ForVar _ _ _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (ForVarIn _ _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (FunctionBody _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (FunctionDeclaration _ _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (If _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (IfElse _ _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Labelled _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Return _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Statement _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Switch _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (Try _ _ srcSpan fileName) = (srcSpan, fileName)
jsastGetSpanAndFile (While _ _ srcSpan fileName) = (srcSpan, fileName)