
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
    | WSValue Value
    | WSVarDeclaration Variable (Maybe ExprWithSourceFragment) deriving (Show)

data JSASTWS =
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
      AWSF JSASTWS SourceFragment deriving (Show)
data ExprWithSourceFragment =
      EWSF ExprWS SourceFragment deriving (Show)

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


resolveJSASTSource :: JSASTWithSourceSpan -> SourceFragment -> JSASTWithSourceFragment
resolveJSASTSource (AWS (FunctionDeclaration var args body) srcSpan fileName) parentFragment =
    AWSF
        (WSFunctionDeclaration
            var
            args
            (resolveJSASTSource body fragment))
        fragment
    where
        fragment = makeNextFragment srcSpan parentFragment


fa :: [JSASTWithSourceSpan] -> SourceFragment -> [JSASTWithSourceFragment]
fa (x:y:z) parentFragment =
    [fb x (makeSourceFragment (jsastGetSpan x) (jsastGetSpan y))]:(fa (y:z) parentFragment)
fa (x:[]) parentFragment = [fb x (makeSourceFragment (jsastGetSpan x) (parentFragment))]

fb :: JSASTWithSourceSpan -> SourceFragment -> JSASTWithSourceFragment
fb (AWS (FunctionDeclaration var args body) srcSpan fileName) myFragment =
    AWSF
        (WSFunctionDeclaration
            var
            args
            (fb body (makeSourceFragment myFragment (getSpan body))))
        myFragment

fb (AWS (FunctionBody list) srcSpan fileName) myFragment =
    AWSF
        (WSFunctionBody
            (reverse $ fa (reverse list) myFragment))
    myFragment
-- And so on

-- nextSpan is the list's parent's next sibling (or the end of the file, if the parent has no next sibling)
fa :: [JSASTWithSourceSpan] -> SrcSpan -> [JSASTWithSourceFragment]
fa (x:y:z) nextSpan =
    [fb x (jsastGetSpan y)]:(fa (y:z) nextSpan)
fa (x:[]) nextSpan = [fb x nextSpan]

-- Here nextSpan is just the end of my fragment
fb :: JSASTWithSourceSpan -> SrcSpan -> JSASTWithSourceFragment
fb (AWS (FunctionDeclaration var args body) srcSpan fileName) nextSpan =
    AWSF
        (WSFunctionDeclaration
            var
            args
            -- The body is the last child of the function declaration,so it has the same end point.
            (fb body nextSpan))
        (makeSourceFragment srcSpan nextSpan)

fb (AWS (FunctionBody list) srcSpan fileName) nextSpan =
    AWSF
        (WSFunctionBody
            (reverse $ fa (reverse list) nextSpan))
    (makeSourceFragment srcSpan nextSpan)
-- And so on