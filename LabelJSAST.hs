
---------------------------------------------------------------------------------
-- Module takes a JSAST and gives each vertex a unique integer label. The label
-- counter is simply threaded through the tree. Traversal is depth first. It's
-- all fairly straight-forward.

module LabelJSAST
( label
, childGetLabel
, JSASTLabel
, VarChild
, OpChild
, IndexChild
, PropertyNameChild
, ValueChild
, ExprChild
, ASTChild
, LabelledPropertyName(..)
, LabelledValue(..)
, LabelledExpression(..)
, LabelledJSAST(..)
) where

---------------------------------------------------------------------------------

import System.Environment
import ParseJS

-- A type for the labels.
type JSASTLabel = Int

-- A Variable and a label.
type VarChild = (Variable, JSASTLabel)
-- An Operator and a label.
type OpChild = (Operator, JSASTLabel)
-- An Index and a label.
type IndexChild = (Index, JSASTLabel)
-- A VarChild or IndexChild wrapped in a LabelledPropertyName, and a label.
-- Added long after the original code was written. Probably needs testing TODO
type PropertyNameChild = (LabelledPropertyName, JSASTLabel)
-- A value wrapped as a LabelledValue, and a label.
-- Most LabelledValues contain only the value and no label.
type ValueChild = (LabelledValue, JSASTLabel)
-- A LabelledExpression (which is a labelled subtree) and a label.
type ExprChild = (LabelledExpression, JSASTLabel)
-- A LabelledJSAST (which is a labelled subree) an a label.
type ASTChild = (LabelledJSAST, JSASTLabel)

-- A wrapper for a VarChild or IndexChild that identifies it as a name for a an
-- object property.
data LabelledPropertyName = LabVariableProperty VarChild
            | LabIndexProperty IndexChild deriving (Show)

-- A labelled representation of a literal.
-- LabelledValues representing primitives contain only the value and no label.
-- LabelledValues representing objects and arrays are labelled recursively.
-- LabUndefined and LabNull have no value or label.
data LabelledValue = LabInt Int
            | LabFloat Double
            | LabString String
            | LabBool Bool
            | LabDQString String
            | LabObject [ExprChild]
            | LabArray [ExprChild]
            | LabUndefined
            | LabNull deriving (Show)

-- FIXME: Some of these contain Maybe *Child values.
-- "Nothing" has no label. Is that a problem?

-- A recursively labelled subtree, rooted at a LabelledExpression.
data LabelledExpression = LabList [ExprChild]
            | LabBinary OpChild ExprChild ExprChild
            | LabUnaryPost OpChild ExprChild
            | LabUnaryPre OpChild ExprChild
            | LabTernary ExprChild ExprChild ExprChild
            | LabAssignment OpChild ExprChild ExprChild
            | LabIdentifier VarChild
            | LabReference ExprChild ExprChild
            | LabIndex ExprChild ExprChild
            | LabValue ValueChild
            | LabPropNameValue PropertyNameChild ExprChild
            | LabCall ExprChild ExprChild
            | LabArguments [ExprChild]
            | LabParenExpression ExprChild
            | LabBreak (Maybe VarChild)
            | LabContinue (Maybe VarChild)
            | LabThrow ExprChild
            | LabCallExpression ExprChild OpChild ExprChild
            -- FIXME: Can a function expression have a name (first arg)?
            | LabFunctionExpression (Maybe VarChild) [VarChild] ASTChild
            | LabVarDeclaration VarChild (Maybe ExprChild)
            | LabNew ExprChild deriving (Show)

-- A recursively labelled subrtree, rooted at a LabelledJSAST.
data LabelledJSAST = LabBlock [ASTChild]
            | LabFunctionBody [ASTChild]
            | LabFunctionDeclaration VarChild [VarChild] ASTChild
            | LabLabelled VarChild ASTChild
            | LabForVar [ExprChild] (Maybe ExprChild) (Maybe ExprChild) ASTChild
            | LabFor (Maybe ExprChild) (Maybe ExprChild) (Maybe ExprChild) ASTChild
            | LabForIn [VarChild] ExprChild ASTChild
            | LabForVarIn ExprChild ExprChild ASTChild
            | LabWhile ExprChild ASTChild
            | LabDoWhile ASTChild ExprChild
            | LabIf ExprChild ASTChild
            | LabIfElse ExprChild ASTChild ASTChild
            | LabSwitch ExprChild ASTChild
            | LabCase ExprChild ASTChild
            | LabDefault ASTChild
            | LabTry ASTChild ASTChild
            | LabCatch VarChild (Maybe ExprChild) ASTChild
            | LabFinally ASTChild
            | LabReturn ExprChild
            | LabStatement ExprChild deriving (Show)


-- main :: IO ()
-- main = do
--    (infile:[]) <- getArgs
--    pr <- readFile infile
--  putStrLn . show . parseTree $ pr
--    putStrLn . show . toJSAST . parseTree $ pr
--    putStrLn . show . label . toJSAST . parseTree $ pr


-- Takes an unlabelled AST and labels the whole thing.
label :: [JSAST] -> [ASTChild]
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
labelExpressionList :: [Expression] -> JSASTLabel -> [ExprChild]
labelExpressionList [] _ = []
labelExpressionList (e:ex) n = let (le, m) = labelExpression e n in ((le, m):(labelExpressionList ex m))


-- Label a list of JSASTs.
labelJSASTList :: [JSAST] -> JSASTLabel -> [ASTChild]
labelJSASTList [] _ = []
labelJSASTList (a:ax) n = let (la, m) = labelJSAST a n in ((la, m):(labelJSASTList ax m))


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
-- Needs to be tested. TODO
labelPropertyName :: PropertyName -> JSASTLabel -> PropertyNameChild
labelPropertyName (IndexProperty ix) n = (LabIndexProperty field1, (childGetLabel $ field1) + 1)
        where
        field1 = labelIndex ix n
labelPropertyName (VariableProperty var) n = (LabVariableProperty field1, (childGetLabel $ field1) + 1)
        where
        field1 = labelVariable var n


-- Label a Value. Recursively process any child fields.
labelValue :: Value -> JSASTLabel -> ValueChild
labelValue (JSInt i) n = (LabInt i, n + 1)
labelValue (JSFloat x) n = (LabFloat x, n + 1)
labelValue (JSString s) n = (LabString s, n + 1)
labelValue (JSBool b) n = (LabBool b, n + 1)
labelValue (JSDQString s) n = (LabDQString s, n + 1)
labelValue (JSObject props) n = (LabObject field1, (maximum ((listGetLabels field1) ++ [n])) + 1)
            where
            field1 = labelExpressionList props n
labelValue (JSArray el) n = (LabArray field1, (maximum ((listGetLabels field1) ++ [n])) + 1)
            where
            field1 = labelExpressionList el n
labelValue (JSUndefined) n = (LabUndefined, n + 1)
labelValue (JSNull) n = (LabNull, n + 1)


-- Label an Expression. Recursively process any child fields.
labelExpression :: Expression -> JSASTLabel -> ExprChild
labelExpression (List ex) n = ((LabList (field1)), (maximum ((listGetLabels field1) ++ [n])) + 1)
            where
            field1 = labelExpressionList ex n
labelExpression (Binary op ex1 ex2) n = ((LabBinary field1 field2 field3), (childGetLabel field3) + 1)
            where
            field1 = labelOperator op n
            field2 = labelExpression ex1 (childGetLabel field1)
            field3 = labelExpression ex2 (childGetLabel field2)
labelExpression (UnaryPost op ex) n = ((LabUnaryPost field1 field2), (childGetLabel field2) + 1)
            where
            field1 = labelOperator op n
            field2 = labelExpression ex (childGetLabel field1)
labelExpression (UnaryPre op ex) n = ((LabUnaryPre field1 field2), (childGetLabel field2) + 1)
            where
            field1 = labelOperator op n
            field2 = labelExpression ex (childGetLabel field1)
labelExpression (Ternary ex1 ex2 ex3) n = ((LabTernary field1 field2 field3), (childGetLabel field3) + 1)
            where
            field1 = labelExpression ex1 n
            field2 = labelExpression ex2 (childGetLabel field1)
            field3 = labelExpression ex3 (childGetLabel field2)
labelExpression (Assignment op ex1 ex2) n = ((LabAssignment field1 field2 field3), (childGetLabel field3) + 1)
            where
            field1 = labelOperator op n
            field2 = labelExpression ex1 (childGetLabel field1)
            field3 = labelExpression ex2 (childGetLabel field2)
labelExpression (Identifier ident) n = ((LabIdentifier field1), (childGetLabel field1) + 1)
            where
            field1 = labelVariable ident n
labelExpression (Reference ex1 ex2) n = ((LabReference field1 field2), (childGetLabel field2) + 1)
            where
            field1 = labelExpression ex1 n
            field2 = labelExpression ex2 (childGetLabel field1)
labelExpression (Index ex1 ex2) n = ((LabIndex field1 field2), (childGetLabel field2) + 1)
            where
            field1 = labelExpression ex1 n
            field2 = labelExpression ex2 (childGetLabel field1)
labelExpression (Value val) n = ((LabValue field1), (childGetLabel field1) + 1)
            where
            field1 = labelValue val n
labelExpression (PropNameValue name ex) n = ((LabPropNameValue field1 field2), (childGetLabel field2) + 1)
            where
            field1 = labelPropertyName name n
            field2 = labelExpression ex (childGetLabel field1)
labelExpression (Call ex1 ex2) n = ((LabCall field1 field2), (childGetLabel field2) + 1)
            where
            field1 = labelExpression ex1 n
            field2 = labelExpression ex2 (childGetLabel field1)
labelExpression (Arguments args) n = ((LabArguments (field1)), (maximum ((listGetLabels field1) ++ [n])) + 1)
            where
            field1 = labelExpressionList args n
labelExpression (ParenExpression ex) n = ((LabParenExpression field1), (childGetLabel field1) + 1)
            where
            field1 = labelExpression ex n
labelExpression (Break vars) n = ((LabBreak field1), (maxMaybeLabel field1 n) + 1)
            where
            field1 = labelMaybeVar vars n
labelExpression (Continue vars) n = ((LabContinue field1), (maxMaybeLabel field1 n) + 1)
            where
            field1 = labelMaybeVar vars n
labelExpression (Throw ex) n = ((LabThrow field1), (childGetLabel field1) + 1)
            where
            field1 = labelExpression ex n
labelExpression (CallExpression ex1 op ex2) n = ((LabCallExpression field1 field2 field3), (childGetLabel field3) + 1)
            where
            field1 = labelExpression ex1 n
            field2 = labelOperator op (childGetLabel field1)
            field3 = labelExpression ex2 (childGetLabel field2)
labelExpression (FunctionExpression var vars ast) n = ((LabFunctionExpression field1 field2 field3),
            (childGetLabel field3) + 1)
            where
            field1 = labelMaybeVar var n
            field2 = labelVarList vars (maxMaybeLabel field1 n)
            field3 = labelJSAST ast (maximum ((listGetLabels field2) ++ [n]))
labelExpression (VarDeclaration var ex) n = ((LabVarDeclaration field1 field2),
            (maxMaybeLabel field2 (childGetLabel field1)) + 1)
            where
            field1 = labelVariable var n
            field2 = labelMaybeExpression ex (childGetLabel field1)
labelExpression (New ex) n = ((LabNew field1), (childGetLabel field1) + 1)
            where
            field1 = labelExpression ex n

-- Label a Maybe Expression if it is not Nothing.
labelMaybeExpression :: (Maybe Expression) -> JSASTLabel -> (Maybe ExprChild)
labelMaybeExpression Nothing n = Nothing
labelMaybeExpression (Just ex) n = Just (labelExpression ex n)


-- Label a JSAST. Recursively process any child fields.
labelJSAST :: JSAST -> JSASTLabel -> ASTChild
labelJSAST (Block jsastLs) n = ((LabBlock field1), (maximum ((listGetLabels $ field1) ++ [n])) + 1)
            where
            field1 = labelJSASTList jsastLs n
labelJSAST (FunctionBody jsastLs) n = ((LabFunctionBody field1), (maximum ((listGetLabels $ field1) ++ [n])) + 1)
            where
            field1 = labelJSASTList jsastLs n
labelJSAST (FunctionDeclaration var args body) n = ((LabFunctionDeclaration field1 field2 field3),
            (childGetLabel $ field3) + 1)
            where
            field1 = labelVariable var n
            field2 = labelVarList args (childGetLabel field1)
            field3 = labelJSAST body (maximum ((listGetLabels field2) ++ [childGetLabel field1]))
labelJSAST (Labelled var body) n = ((LabLabelled field1 field2), (childGetLabel $ field2) + 1)
            where
            field1 = labelVariable var n
            field2 = labelJSAST body (childGetLabel field1)
labelJSAST (ForVar ex1 ex2 ex3 body) n = ((LabForVar field1 field2 field3 field4), (childGetLabel $ field4) + 1)
            where
            field1 = labelExpressionList ex1 n
            field2 = labelMaybeExpression ex2 (maximum ((listGetLabels field1) ++ [n]))
            field3 = labelMaybeExpression ex3 (maximum ((listGetLabels field1) ++ [maxMaybeLabel field2 n]))
            field4 = labelJSAST body
                    (maximum ((listGetLabels field1) ++ [maxMaybeLabel field2 n] ++ [maxMaybeLabel field3 n]))
labelJSAST (For ex1 ex2 ex3 body) n = ((LabFor field1 field2 field3 field4),
            (childGetLabel $ field4) + 1)
            where
            field1 = labelMaybeExpression ex1 n
            field2 = labelMaybeExpression ex2 (maxMaybeLabel field1 n)
            field3 = labelMaybeExpression ex3 (max (maxMaybeLabel field1 n) (maxMaybeLabel field2 n))
            field4 = labelJSAST body
                    (maximum ([maxMaybeLabel field1 n] ++ [maxMaybeLabel field2 n] ++ [maxMaybeLabel field3 n]))
labelJSAST (ForIn vars ex body) n = ((LabForIn field1 field2 field3),
            (childGetLabel $ field3) + 1)
            where
            field1 = labelVarList vars n
            field2 = labelExpression ex (maximum ((listGetLabels field1) ++ [n]))
            field3 = labelJSAST body (childGetLabel field2)
labelJSAST (ForVarIn ex1 ex2 body) n = ((LabForVarIn field1 field2 field3),
            (childGetLabel $ field3) + 1)
            where
            field1 = labelExpression ex1 n
            field2 = labelExpression ex2 (childGetLabel field1)
            field3 = labelJSAST body (childGetLabel field2)
labelJSAST (While ex body) n = ((LabWhile field1 field2),
            (childGetLabel $ field2) + 1)
            where
            field1 = labelExpression ex n
            field2 = labelJSAST body (childGetLabel field1)
labelJSAST (DoWhile body ex) n  = ((LabDoWhile field1 field2), (childGetLabel field2) + 1)
            where
            field1 = labelJSAST body n
            field2 = labelExpression ex (childGetLabel $ field1)
labelJSAST (If ex body) n = ((LabIf field1 field2), (childGetLabel $ field2) + 1)
            where
            field1 = labelExpression ex n
            field2 = labelJSAST body (childGetLabel field1)
labelJSAST (IfElse ex bodyT bodyF) n = ((LabIfElse field1 field2 field3),
            (childGetLabel $ field3) + 1)
            where
            field1 = labelExpression ex n
            field2 = labelJSAST bodyT (childGetLabel $ field1)
            field3 = labelJSAST bodyF (childGetLabel $ field2)
labelJSAST (Switch ex cs) n = ((LabSwitch field1 field2),
            (childGetLabel $ field2) + 1)
            where
            field1 = labelExpression ex n
            field2 = labelJSAST cs (childGetLabel field1)
labelJSAST (Case ex body) n = ((LabCase field1 field2),
            (childGetLabel $ field2) + 1)
            where
            field1 = labelExpression ex n
            field2 = labelJSAST body (childGetLabel field1)
labelJSAST (Default body) n = ((LabDefault field1), (childGetLabel $ field1) + 1)
            where
            field1 = labelJSAST body n
labelJSAST (Try body ctch) n = ((LabTry field1 field2),
            (childGetLabel $ field2) + 1)
            where
            field1 = labelJSAST body n
            field2 = labelJSAST ctch (childGetLabel $ field1)
labelJSAST (Catch var ex body) n = ((LabCatch field1 field2 field3),
            (childGetLabel $ field3) + 1)
            where
            field1 = labelVariable var n
            field2 = labelMaybeExpression ex (childGetLabel field1)
            field3 = labelJSAST body (maxMaybeLabel field2 (childGetLabel field1))
labelJSAST (Finally body) n = ((LabFinally field1), (childGetLabel $ field1) + 1)
            where
            field1 = labelJSAST body n
labelJSAST (Return ex) n = ((LabReturn field1), (childGetLabel field1) + 1)
            where
            field1 = labelExpression ex n
labelJSAST (Statement ex) n = ((LabStatement field1), (childGetLabel field1) + 1)
            where
            field1 = labelExpression ex n

