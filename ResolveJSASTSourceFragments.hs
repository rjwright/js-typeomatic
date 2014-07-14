
module ResolveJSASTSourceFragments
( ExprWithSourceFragment(..)
, ExprWSF(..)
, JSASTWithSourceFragment(..)
, JSASTWSF(..)
, SourceFragment(..)
, ValueWithSourceFragment(..)
, getSourceFragments
, jsastListMakeSourceFragments
, jsastMakeSourceFragment
) where


import Language.JavaScript.Parser
import ParseJS
import System.Environment


type Row = Int
type Col = Int


-- (FileName, StartRow, StartCol, EndRow, EndCol)
type SourceFragment = (String, Row, Col, Row, Col)

-- Represent literal values.
-- data ValueWSF =
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

-- data ValueWithSourceFragment =
--       VWSF ValueWSF SourceFragment deriving (Show)
data JSASTWithSourceFragment =
      AWSF JSASTWSF SourceFragment deriving (Show)
data ExprWithSourceFragment =
      EWSF ExprWSF SourceFragment deriving (Show)


jsastGetSpan :: JSASTWithSourceSpan -> SrcSpan
jsastGetSpan (AWSS _ srcSpan _) = srcSpan

exprGetSpan :: ExprWithSourceSpan -> SrcSpan
exprGetSpan (EWSS _ srcSpan _) = srcSpan

maybeExprGetSpan :: Maybe ExprWithSourceSpan -> Maybe SrcSpan
maybeExprGetSpan (Just (EWSS _ srcSpan _)) = Just srcSpan
maybeExprGetSpan Nothing = Nothing


-- FROM OLD STUFF
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


---------------------------------------------------------------------------------------------------

makeSourceFragment :: SrcSpan -> SrcSpan -> SourceFileName -> SourceFragment
makeSourceFragment (SpanPoint _ startRow startCol) (SpanPoint _ nextRow nextCol) fileName =
    (fileName, startRow, startCol, nextRow, nextCol)


valueMakeSourceFragment :: Value -> SrcSpan -> ValueWithSourceFragment
valueMakeSourceFragment (JSArray list) nextSpan =
    WSArray (exprListMakeSourceFragments list nextSpan)
valueMakeSourceFragment (JSBool val) _ = WSBool val
    -- Double quote strings are never treated differently to normal strings.
    -- TODO: Should be merged with JSString
valueMakeSourceFragment (JSDQString val) _ = WSDQString val
valueMakeSourceFragment (JSFloat val) _ = WSFloat val
valueMakeSourceFragment (JSInt val) _ = WSInt val
valueMakeSourceFragment JSNull _ = WSNull
valueMakeSourceFragment (JSObject list) nextSpan =
    WSObject (exprListMakeSourceFragments list nextSpan)
valueMakeSourceFragment (JSString val) _ = WSString val
valueMakeSourceFragment  JSUndefined _ = WSUndefined

-- nextSpan is the list's parent's next sibling (or the end of the file, if the parent has no next
-- sibling)
jsastListMakeSourceFragments :: [JSASTWithSourceSpan] -> SrcSpan -> [JSASTWithSourceFragment]
jsastListMakeSourceFragments (x:y:z) nextSpan =
    (jsastMakeSourceFragment x (jsastGetSpan y)):(jsastListMakeSourceFragments (y:z) nextSpan)
jsastListMakeSourceFragments (x:[]) nextSpan = [jsastMakeSourceFragment x nextSpan]
jsastListMakeSourceFragments [] nextSpan = []


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
jsastMakeSourceFragment :: JSASTWithSourceSpan -> SrcSpan -> JSASTWithSourceFragment
-- jsastMakeSourceFragment (AWSS () srcSpan fileName) nextSpan =
--     AWSF
--         (WS...)
--         (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (Block list) srcSpan fileName) nextSpan =
    AWSF
        (WSBlock (jsastListMakeSourceFragments list nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (For vars cond expr body) srcSpan fileName) nextSpan =
    AWSF
        (WSFor
            (maybeExprMakeSourceFragment vars varsNextSpan)
            (maybeExprMakeSourceFragment cond condNextSpan)
            (maybeExprMakeSourceFragment expr (jsastGetSpan body))
            (jsastMakeSourceFragment body nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
    where
        justSpanGetSpan (Just ss) = ss
        condSrcSpan = maybeExprGetSpan cond
        exprSrcSpan = maybeExprGetSpan expr
        varsNextSpan =
            if (not (condSrcSpan == Nothing)) then
                justSpanGetSpan condSrcSpan
            else if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSpan body
        condNextSpan =
            if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSpan body
jsastMakeSourceFragment (AWSS (ForVar vars cond expr body) srcSpan fileName) nextSpan =
    AWSF
        (WSForVar
            (exprListMakeSourceFragments vars varsNextSpan)
            (maybeExprMakeSourceFragment cond condNextSpan)
            (maybeExprMakeSourceFragment expr (jsastGetSpan body))
            (jsastMakeSourceFragment body nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
    where
        justSpanGetSpan (Just ss) = ss
        condSrcSpan = maybeExprGetSpan cond
        exprSrcSpan = maybeExprGetSpan expr
        varsNextSpan =
            if (not (condSrcSpan == Nothing)) then
                justSpanGetSpan condSrcSpan
            else if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSpan body
        condNextSpan =
            if (not (exprSrcSpan == Nothing)) then
                justSpanGetSpan exprSrcSpan
            else
                jsastGetSpan body
jsastMakeSourceFragment (AWSS (ForVarIn var obj body) srcSpan fileName) nextSpan =
    AWSF
        (WSForVarIn
            (exprMakeSourceFragment var (exprGetSpan obj))
            (exprMakeSourceFragment obj (jsastGetSpan body))
            (jsastMakeSourceFragment body nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (FunctionBody list) srcSpan fileName) nextSpan =
    AWSF
        (WSFunctionBody
        (jsastListMakeSourceFragments list nextSpan))
    (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (FunctionDeclaration var args body) srcSpan fileName) nextSpan =
    AWSF
        (WSFunctionDeclaration
            var
            args
            -- The body is the last child of the function declaration,so it has the same end point.
            (jsastMakeSourceFragment body nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (Return expr) srcSpan fileName) nextSpan =
    AWSF
        (WSReturn (exprMakeSourceFragment expr nextSpan))
    (makeSourceFragment srcSpan nextSpan fileName)
jsastMakeSourceFragment (AWSS (Statement expr) srcSpan fileName) nextSpan =
    AWSF
        (WSStatement (exprMakeSourceFragment expr nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)


exprListMakeSourceFragments :: [ExprWithSourceSpan] -> SrcSpan -> [ExprWithSourceFragment]
exprListMakeSourceFragments (x:y:z) nextSpan =
    (exprMakeSourceFragment x (exprGetSpan y)):(exprListMakeSourceFragments (y:z) nextSpan)
exprListMakeSourceFragments (x:[]) nextSpan = [exprMakeSourceFragment x nextSpan]
exprListMakeSourceFragments [] nextSpan = []

maybeExprMakeSourceFragment :: Maybe ExprWithSourceSpan -> SrcSpan -> Maybe ExprWithSourceFragment
maybeExprMakeSourceFragment (Just exprWithSourceSpan) srcSpan =
    Just (exprMakeSourceFragment exprWithSourceSpan srcSpan)
maybeExprMakeSourceFragment Nothing _ = Nothing


-- Still to do
--      WSBreak (Maybe Variable)
--      WSContinue (Maybe Variable)
--      WSNew ExprWithSourceFragment
--      WSParenExpression ExprWithSourceFragment
--      WSTernary ExprWithSourceFragment ExprWithSourceFragment ExprWithSourceFragment
--      WSThrow ExprWithSourceFragment
exprMakeSourceFragment :: ExprWithSourceSpan -> SrcSpan -> ExprWithSourceFragment
-- exprMakeSourceFragment (EWSS () srcSpan fileName) nextSpan =
--     EWSF
--         (WS...)
--         (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Arguments list) srcSpan fileName) nextSpan =
    EWSF
        (WSArguments
            (exprListMakeSourceFragments list nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Assignment op expr1 expr2) srcSpan fileName) nextSpan =
    EWSF
        (WSAssignment
            op
            (exprMakeSourceFragment expr1 (exprGetSpan expr2))
            (exprMakeSourceFragment expr2 nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Binary op expr1 expr2) srcSpan fileName) nextSpan =
    EWSF
        (WSBinary
            op
            (exprMakeSourceFragment expr1 (exprGetSpan expr2))
            (exprMakeSourceFragment expr2 nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Call expr1 expr2) srcSpan fileName) nextSpan =
    EWSF
        (WSCall
            (exprMakeSourceFragment expr1 (exprGetSpan expr2))
            (exprMakeSourceFragment expr2 nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (CallExpression expr op callExpr) srcSpan fileName) nextSpan =
    EWSF
        (WSCallExpression
            (exprMakeSourceFragment expr (exprGetSpan callExpr))
            op
            (exprMakeSourceFragment callExpr nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (FunctionExpression name args body) srcSpan fileName) nextSpan =
    EWSF
        (WSFunctionExpression
            name
            args
            (jsastMakeSourceFragment body nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Identifier var) srcSpan fileName) nextSpan =
    EWSF
        (WSIdentifier var)
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Index expr1 expr2) srcSpan fileName) nextSpan =
    EWSF
        (WSIndex
            (exprMakeSourceFragment expr1 (exprGetSpan expr2))
            (exprMakeSourceFragment expr2 nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (List list) srcSpan fileName) nextSpan =
    EWSF
        (WSList
            (exprListMakeSourceFragments list nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (PropNameValue name expr) srcSpan fileName) nextSpan =
    EWSF
        (WSPropNameValue
            name
            (exprMakeSourceFragment expr nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Reference expr1 expr2) srcSpan fileName) nextSpan =
    EWSF
        (WSReference
            (exprMakeSourceFragment expr1 (exprGetSpan expr2))
            (exprMakeSourceFragment expr2 nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (UnaryPost op expr) srcSpan fileName) nextSpan =
    EWSF
        (WSUnaryPost
            op
            (exprMakeSourceFragment expr nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (UnaryPre op expr) srcSpan fileName) nextSpan =
    EWSF
        (WSUnaryPre
            op
            (exprMakeSourceFragment expr nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (Value val) srcSpan fileName) nextSpan =
    EWSF
        (WSValue (valueMakeSourceFragment val nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
exprMakeSourceFragment (EWSS (VarDeclaration var expr) srcSpan fileName) nextSpan =
    EWSF
        (WSVarDeclaration
            var
            (maybeExprMakeSourceFragment expr nextSpan))
        (makeSourceFragment srcSpan nextSpan fileName)
