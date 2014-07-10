
-- Module takes a JSAST and gives each vertex a unique integer label. The label counter is simply
-- threaded through the tree. Traversal is depth first. It's all fairly straight-forward.
--
-- Top level function is (label . toJSAST . parseTree).

module LabelJSAST
( ASTChild
, ExprChild
, IndexChild
, JSASTLabel
, LabelledExpression(..)
, LabelledJSAST(..)
, LabelledPropertyName(..)
, LabelledValue(..)
, OpChild
, PropertyNameChild
, ValueChild
, VarChild
, childGetLabel
, label
) where


import ParseJS
import ResolveJSASTSourceFragments
import System.Environment

-- A type for the labels.
type JSASTLabel = Int


-- A Variable and a label.
type VarChild = (Variable, JSASTLabel)


-- An Operator and a label.
type OpChild = (Operator, JSASTLabel)


-- An Index and a label.
type IndexChild = (Index, JSASTLabel)


-- A VarChild or IndexChild wrapped in a LabelledPropertyName, and a label. Added long after the
-- original code was written.
--
-- TODO: Test.
type PropertyNameChild = (LabelledPropertyName, JSASTLabel)


-- A value wrapped as a LabelledValue, and a label. Most LabelledValues contain only the value and
-- no label.
type ValueChild = (LabelledValue, JSASTLabel)


-- A LabelledExpression (which is a labelled subtree) and a label.
type ExprChild = (LabelledExpression, JSASTLabel)


-- A LabelledJSAST (which is a labelled subree) an a label.
type ASTChild = (LabelledJSAST, JSASTLabel, SourceFragment)


-- A wrapper for a VarChild or IndexChild that identifies it as a name for a an object property.
data LabelledPropertyName =
      LabIndexProperty IndexChild
    | LabVariableProperty VarChild deriving (Show)


-- A labelled representation of a literal. LabelledValues representing primitives contain only the
-- value and no label. LabelledValues representing objects and arrays are labelled recursively.
-- LabUndefined and LabNull have no value or label.
data LabelledValue =
      LabArray [ExprChild]
    | LabBool Bool
    | LabDQString String
    | LabFloat Double
    | LabInt Int
    | LabNull
    | LabObject [ExprChild]
    | LabString String
    | LabUndefined deriving (Show)


-- FIXME: Some of these contain Maybe *Child values. "Nothing" has no label. Is that a problem?
--
-- A recursively labelled subtree, rooted at a LabelledExpression.
data LabelledExpression =
      LabArguments [ExprChild]
    | LabAssignment OpChild ExprChild ExprChild
    | LabBinary OpChild ExprChild ExprChild
    | LabBreak (Maybe VarChild)
    | LabCall ExprChild ExprChild
    | LabCallExpression ExprChild OpChild ExprChild
    | LabContinue (Maybe VarChild)
    | LabFunctionExpression (Maybe VarChild) [VarChild] ASTChild
    | LabIdentifier VarChild
    | LabIndex ExprChild ExprChild
    | LabList [ExprChild]
    | LabNew ExprChild
    | LabParenExpression ExprChild
    | LabPropNameValue PropertyNameChild ExprChild
    | LabReference ExprChild ExprChild
    | LabTernary ExprChild ExprChild ExprChild
    | LabThrow ExprChild
    | LabUnaryPost OpChild ExprChild
    | LabUnaryPre OpChild ExprChild
    | LabValue ValueChild
    | LabVarDeclaration VarChild (Maybe ExprChild) deriving (Show)


-- A recursively labelled subrtree, rooted at a LabelledJSAST.
data LabelledJSAST =
      LabBlock [ASTChild]
    | LabCase ExprChild ASTChild
    | LabCatch VarChild (Maybe ExprChild) ASTChild
    | LabDefault ASTChild
    | LabDoWhile ASTChild ExprChild
    | LabFinally ASTChild
    | LabFor (Maybe ExprChild) (Maybe ExprChild) (Maybe ExprChild) ASTChild
    | LabForIn [VarChild] ExprChild ASTChild
    | LabForVar [ExprChild] (Maybe ExprChild) (Maybe ExprChild) ASTChild
    | LabForVarIn ExprChild ExprChild ASTChild
    | LabFunctionBody [ASTChild]
    | LabFunctionDeclaration VarChild [VarChild] ASTChild
    | LabIf ExprChild ASTChild
    | LabIfElse ExprChild ASTChild ASTChild
    | LabLabelled VarChild ASTChild
    | LabReturn ExprChild
    | LabStatement ExprChild
    | LabSwitch ExprChild ASTChild
    | LabTry ASTChild ASTChild
    | LabWhile ExprChild ASTChild deriving (Show)


-- Takes an unlabelled AST and labels the whole thing.
label :: [JSASTWithSourceFragment] -> [ASTChild]
label list = labelJSASTList list 0


-- Extract the JSASTLabel from a VarChild, ExprChild etc.
childGetLabel :: (a, JSASTLabel) -> JSASTLabel
childGetLabel (child, lab) = lab


-- Extract the labels from a list of VarChild, ExprChild etc.
listGetLabels :: [(a, JSASTLabel)] -> [JSASTLabel]
listGetLabels [] = []
listGetLabels (c:cs) = ((childGetLabel c):(listGetLabels cs))


-- Find the greater of the label on a Maybe *Child and a given value.
maxMaybeLabel :: (Maybe (a, JSASTLabel)) -> JSASTLabel -> JSASTLabel
-- If the Maybe *Child is nothing then the given value is the greatest.
maxMaybeLabel Nothing v = v
maxMaybeLabel (Just e) v = max (childGetLabel e) v


-- Label a list of Varialbes.
labelVarList :: [Variable] -> JSASTLabel -> [VarChild]
labelVarList [] _ = []
labelVarList (v:vx) n = (v, n + 1):(labelVarList vx (n + 1))


-- Label a list of Expressions.
labelExpressionList :: [ExprWithSourceSpan] -> JSASTLabel -> [ExprChild]
labelExpressionList [] _ = []
labelExpressionList (e:ex) n =
    let (le, m) = labelExpression e n in ((le, m):(labelExpressionList ex m))


-- Label a list of JSASTs.
labelJSASTList :: [JSASTWithSourceSpan] -> JSASTLabel -> [ASTChild]
labelJSASTList [] _ = []
labelJSASTList (a:ax) n =
    let (la, m) = labelJSAST a n in ((la, m):(labelJSASTList ax m))


-- Label a Varialble.
labelVariable :: Variable -> JSASTLabel -> VarChild
labelVariable var n = (var, n + 1)


-- Label a Maybe Variable if it is not Nothing.
labelMaybeVar :: (Maybe Variable) -> JSASTLabel -> (Maybe VarChild)
labelMaybeVar Nothing n = Nothing
labelMaybeVar (Just var) n = Just (labelVariable var n)


-- Label an Operator.
labelOperator :: Operator -> JSASTLabel -> OpChild
labelOperator op n = (op, n + 1)


-- Label an Index.
labelIndex :: Index -> JSASTLabel -> IndexChild
labelIndex ix n = (ix, n + 1)


-- Label a PropertyName.
--
-- TODO: Unit test?
labelPropertyName :: PropertyName -> JSASTLabel -> PropertyNameChild
labelPropertyName (IndexProperty ix) n =
    (LabIndexProperty field1, (childGetLabel field1) + 1)
    where
        field1 = labelIndex ix n
labelPropertyName (VariableProperty var) n =
    (LabVariableProperty field1, (childGetLabel field1) + 1)
    where
        field1 = labelVariable var n


-- Label a Value. Recursively process any child fields.
labelValue :: Value -> JSASTLabel -> ValueChild
labelValue (JSInt i) n = (LabInt i, n + 1)
labelValue (JSFloat x) n = (LabFloat x, n + 1)
labelValue (JSString s) n = (LabString s, n + 1)
labelValue (JSBool b) n = (LabBool b, n + 1)
labelValue (JSDQString s) n = (LabDQString s, n + 1)
labelValue (JSObject props) n =
    (LabObject field1, (maximum ((listGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelExpressionList props n
labelValue (JSArray el) n =
    (LabArray field1, (maximum ((listGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelExpressionList el n
labelValue (JSUndefined) n = (LabUndefined, n + 1)
labelValue (JSNull) n = (LabNull, n + 1)


-- Label an Expression. Recursively process any child fields.
labelExpression :: ExprWithSourceSpan -> JSASTLabel -> ExprChild
labelExpression (EWSS (List ex) _ _) n =
    ((LabList (field1)), (maximum ((listGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelExpressionList ex n
labelExpression (EWSS (Binary op ex1 ex2) _ _) n =
    ((LabBinary field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex1 (childGetLabel field1)
        field3 = labelExpression ex2 (childGetLabel field2)
labelExpression (EWSS (UnaryPost op ex) _ _) n =
    ((LabUnaryPost field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex (childGetLabel field1)
labelExpression (EWSS (UnaryPre op ex) _ _) n =
    ((LabUnaryPre field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex (childGetLabel field1)
labelExpression (EWSS (Ternary ex1 ex2 ex3) _ _) n =
    ((LabTernary field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childGetLabel field1)
        field3 = labelExpression ex3 (childGetLabel field2)
labelExpression (EWSS (Assignment op ex1 ex2) _ _) n =
    ((LabAssignment field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex1 (childGetLabel field1)
        field3 = labelExpression ex2 (childGetLabel field2)
labelExpression (EWSS (Identifier ident) _ _) n =
    ((LabIdentifier field1), (childGetLabel field1) + 1)
    where
        field1 = labelVariable ident n
labelExpression (EWSS (Reference ex1 ex2) _ _) n =
    ((LabReference field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childGetLabel field1)
labelExpression (EWSS (Index ex1 ex2) _ _) n =
    ((LabIndex field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childGetLabel field1)
labelExpression (EWSS (Value val) _ _) n =
    ((LabValue field1), (childGetLabel field1) + 1)
    where
        field1 = labelValue val n
labelExpression (EWSS (PropNameValue name ex) _ _) n =
    ((LabPropNameValue field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelPropertyName name n
        field2 = labelExpression ex (childGetLabel field1)
labelExpression (EWSS (Call ex1 ex2) _ _) n =
    ((LabCall field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childGetLabel field1)
labelExpression (EWSS (Arguments args) _ _) n =
    ((LabArguments (field1)), (maximum ((listGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelExpressionList args n
labelExpression (EWSS (ParenExpression ex) _ _) n =
    ((LabParenExpression field1), (childGetLabel field1) + 1)
    where
        field1 = labelExpression ex n
labelExpression (EWSS (Break vars) _ _) n =
    ((LabBreak field1), (maxMaybeLabel field1 n) + 1)
    where
        field1 = labelMaybeVar vars n
labelExpression (EWSS (Continue vars) _ _) n =
    ((LabContinue field1), (maxMaybeLabel field1 n) + 1)
    where
        field1 = labelMaybeVar vars n
labelExpression (EWSS (Throw ex) _ _) n =
    ((LabThrow field1), (childGetLabel field1) + 1)
    where
        field1 = labelExpression ex n
labelExpression (EWSS (CallExpression ex1 op ex2) _ _) n =
    ((LabCallExpression field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelExpression ex1 n
        field2 = labelOperator op (childGetLabel field1)
        field3 = labelExpression ex2 (childGetLabel field2)
labelExpression (EWSS (FunctionExpression var vars ast) _ _) n =
    ((LabFunctionExpression field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelMaybeVar var n
        field2 = labelVarList vars (maxMaybeLabel field1 n)
        field3 = labelJSAST ast (maximum ((listGetLabels field2) ++ [n]))
labelExpression (EWSS (VarDeclaration var ex) _ _) n =
    ((LabVarDeclaration field1 field2), (maxMaybeLabel field2 (childGetLabel field1)) + 1)
    where
        field1 = labelVariable var n
        field2 = labelMaybeExpression ex (childGetLabel field1)
labelExpression (EWSS (New ex) _ _) n =
    ((LabNew field1), (childGetLabel field1) + 1)
    where
        field1 = labelExpression ex n


-- Label a Maybe Expression if it is not Nothing.
labelMaybeExpression :: (Maybe ExprWithSourceSpan) -> JSASTLabel -> (Maybe ExprChild)
labelMaybeExpression Nothing n = Nothing
labelMaybeExpression (Just ex) n = Just $ labelExpression ex n


-- Label a JSAST. Recursively process any child fields.
labelJSAST :: JSASTWithSourceSpan -> JSASTLabel -> ASTChild
labelJSAST (AWSS (Block jsastLs) srcSpan fileName) n =
    ((LabBlock field1), (maximum ((listGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelJSASTList jsastLs n
labelJSAST (AWSS (FunctionBody jsastLs) srcSpan fileName) n =
    ((LabFunctionBody field1), (maximum ((listGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelJSASTList jsastLs n
labelJSAST (AWSS (FunctionDeclaration var args body) srcSpan fileName) n =
    ((LabFunctionDeclaration field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelVariable var n
        field2 = labelVarList args (childGetLabel field1)
        field3 = labelJSAST body $ maximum ((listGetLabels field2) ++ [childGetLabel field1])
labelJSAST (AWSS (Labelled var body) srcSpan fileName) n =
    ((LabLabelled field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelVariable var n
        field2 = labelJSAST body (childGetLabel field1)
labelJSAST (AWSS (ForVar ex1 ex2 ex3 body) srcSpan fileName) n =
    ((LabForVar field1 field2 field3 field4), (childGetLabel field4) + 1)
    where
        field1 = labelExpressionList ex1 n
        field2 = labelMaybeExpression ex2 $ maximum ((listGetLabels field1) ++ [n])
        field3 =
            labelMaybeExpression ex3 $ maximum ((listGetLabels field1) ++ [maxMaybeLabel field2 n])
        field4 =
            labelJSAST
                body
                $ maximum
                    ((listGetLabels field1)
                    ++ [maxMaybeLabel field2 n]
                    ++ [maxMaybeLabel field3 n])
labelJSAST (AWSS (For ex1 ex2 ex3 body) srcSpan fileName) n =
    ((LabFor field1 field2 field3 field4), (childGetLabel field4) + 1)
    where
        field1 = labelMaybeExpression ex1 n
        field2 = labelMaybeExpression ex2 (maxMaybeLabel field1 n)
        field3 =
            labelMaybeExpression ex3 $ max (maxMaybeLabel field1 n) (maxMaybeLabel field2 n)
        field4 =
            labelJSAST
                body
                $ maximum
                    ([maxMaybeLabel field1 n]
                    ++ [maxMaybeLabel field2 n]
                    ++ [maxMaybeLabel field3 n])
labelJSAST (AWSS (ForIn vars ex body) srcSpan fileName) n =
    ((LabForIn field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelVarList vars n
        field2 = labelExpression ex $ maximum ((listGetLabels field1) ++ [n])
        field3 = labelJSAST body (childGetLabel field2)
labelJSAST (AWSS (ForVarIn ex1 ex2 body) srcSpan fileName) n =
    ((LabForVarIn field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childGetLabel field1)
        field3 = labelJSAST body (childGetLabel field2)
labelJSAST (AWSS (While ex body) srcSpan fileName) n =
    ((LabWhile field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST body (childGetLabel field1)
labelJSAST (AWSS (DoWhile body ex )srcSpan fileName) n  =
    ((LabDoWhile field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelJSAST body n
        field2 = labelExpression ex (childGetLabel field1)
labelJSAST (AWSS (If ex body) srcSpan fileName) n =
    ((LabIf field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST body (childGetLabel field1)
labelJSAST (AWSS (IfElse ex bodyT bodyF) srcSpan fileName) n =
    ((LabIfElse field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST bodyT (childGetLabel field1)
        field3 = labelJSAST bodyF (childGetLabel field2)
labelJSAST (AWSS (Switch ex cs) srcSpan fileName) n =
    ((LabSwitch field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST cs (childGetLabel field1)
labelJSAST (AWSS (Case ex body) srcSpan fileName) n =
    ((LabCase field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST body (childGetLabel field1)
labelJSAST (AWSS (Default body) srcSpan fileName) n =
    ((LabDefault field1), (childGetLabel field1) + 1)
    where
        field1 = labelJSAST body n
labelJSAST (AWSS (Try body ctch) srcSpan fileName) n =
    ((LabTry field1 field2), (childGetLabel field2) + 1)
    where
        field1 = labelJSAST body n
        field2 = labelJSAST ctch (childGetLabel field1)
labelJSAST (AWSS (Catch var ex body) srcSpan fileName) n =
    ((LabCatch field1 field2 field3), (childGetLabel field3) + 1)
    where
        field1 = labelVariable var n
        field2 = labelMaybeExpression ex (childGetLabel field1)
        field3 = labelJSAST body (maxMaybeLabel field2 (childGetLabel field1))
labelJSAST (AWSS (Finally body) srcSpan fileName) n =
    ((LabFinally field1), (childGetLabel field1) + 1)
    where
        field1 = labelJSAST body n
labelJSAST (AWSS (Return ex) srcSpan fileName) n =
    ((LabReturn field1), (childGetLabel field1) + 1)
    where
        field1 = labelExpression ex n
labelJSAST (AWSS (Statement ex) srcSpan fileName) n =
    ((LabStatement field1), (childGetLabel field1) + 1)
    where
        field1 = labelExpression ex n
