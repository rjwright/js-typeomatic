
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


-- This module takes a JSAST and gives each vertex a unique integer label. The label counter is simply
-- threaded through the tree. Traversal is depth first. It's all fairly straight-forward.
--
-- Top level function is
-- (label (jsastListWSMakeSourceFragments (getJSASTWithSource (parseTree program file) file) span))


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
, childGetSource
, childWSGetLabel
, label
) where


import ParseJS
import ResolveSourceFragments
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
type ExprChild = (LabelledExpression, JSASTLabel, SourceFragment)


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


-- Extract the JSASTLabel from a VarChild, IndexChild etc.
childGetLabel :: (a, JSASTLabel) -> JSASTLabel
childGetLabel (child, lab) = lab

-- Extract the JSASTLabel from a ASTChild, ExprChild etc.
childWSGetLabel :: (a, JSASTLabel, b) -> JSASTLabel
childWSGetLabel (_, lab, _) = lab

childGetSource :: (a, b, SourceFragment) -> SourceFragment
childGetSource (_, _, sf) = sf

-- Extract the labels from a list of VarChild, IndexChild etc.
listGetLabels :: [(a, JSASTLabel)] -> [JSASTLabel]
listGetLabels [] = []
listGetLabels (c:cs) = ((childGetLabel c):(listGetLabels cs))

-- Extract the labels from a list of ASTChild, ExprChild etc.
listWSGetLabels :: [(a, JSASTLabel, b)] -> [JSASTLabel]
listWSGetLabels [] = []
listWSGetLabels (c:cs) = ((childWSGetLabel c):(listWSGetLabels cs))


-- Find the greater of the label on a Maybe *Child and a given value.
maxMaybeLabel :: (Maybe (a, JSASTLabel)) -> JSASTLabel -> JSASTLabel
-- If the Maybe *Child is nothing then the given value is the greatest.
maxMaybeLabel Nothing v = v
maxMaybeLabel (Just e) v = max (childGetLabel e) v


-- Find the greater of the label on a Maybe *Child and a given value.
maxMaybeWSLabel :: (Maybe (a, JSASTLabel, b)) -> JSASTLabel -> JSASTLabel
-- If the Maybe *Child is nothing then the given value is the greatest.
maxMaybeWSLabel Nothing v = v
maxMaybeWSLabel (Just e) v = max (childWSGetLabel e) v

-- Label a list of Varialbes.
labelVarList :: [Variable] -> JSASTLabel -> [VarChild]
labelVarList [] _ = []
labelVarList (v:vx) n = (v, n + 1):(labelVarList vx (n + 1))


-- Label a list of Expressions.
labelExpressionList :: [ExprWithSourceFragment] -> JSASTLabel -> [ExprChild]
labelExpressionList [] _ = []
labelExpressionList (e:ex) n =
    let (le, m, sf) = labelExpression e n in ((le, m, sf):(labelExpressionList ex m))


-- Label a list of JSASTs.
labelJSASTList :: [JSASTWithSourceFragment] -> JSASTLabel -> [ASTChild]
labelJSASTList [] _ = []
labelJSASTList (a:ax) n =
    let (la, m, sf) = labelJSAST a n in ((la, m, sf):(labelJSASTList ax m))


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
labelValue :: ValueWithSourceFragment -> JSASTLabel -> ValueChild
labelValue (WSInt i) n = (LabInt i, n + 1)
labelValue (WSFloat x) n = (LabFloat x, n + 1)
labelValue (WSString s) n = (LabString s, n + 1)
labelValue (WSBool b) n = (LabBool b, n + 1)
labelValue (WSDQString s) n = (LabDQString s, n + 1)
labelValue (WSObject props) n =
    (LabObject field1, (maximum ((listWSGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelExpressionList props n
labelValue (WSArray el) n =
    (LabArray field1, (maximum ((listWSGetLabels field1) ++ [n])) + 1)
    where
        field1 = labelExpressionList el n
labelValue (WSUndefined) n = (LabUndefined, n + 1)
labelValue (WSNull) n = (LabNull, n + 1)


-- Label an Expression. Recursively process any child fields.
labelExpression :: ExprWithSourceFragment -> JSASTLabel -> ExprChild
labelExpression (EWSF (WSList ex) sourceFragment) n =
    ((LabList (field1)), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelExpressionList ex n
labelExpression (EWSF (WSBinary op ex1 ex2) sourceFragment) n =
    ((LabBinary field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex1 (childGetLabel field1)
        field3 = labelExpression ex2 (childWSGetLabel field2)
labelExpression (EWSF (WSUnaryPost op ex) sourceFragment) n =
    ((LabUnaryPost field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex (childGetLabel field1)
labelExpression (EWSF (WSUnaryPre op ex) sourceFragment) n =
    ((LabUnaryPre field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex (childGetLabel field1)
labelExpression (EWSF (WSTernary ex1 ex2 ex3) sourceFragment) n =
    ((LabTernary field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childWSGetLabel field1)
        field3 = labelExpression ex3 (childWSGetLabel field2)
labelExpression (EWSF (WSAssignment op ex1 ex2) sourceFragment) n =
    ((LabAssignment field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelExpression ex1 (childGetLabel field1)
        field3 = labelExpression ex2 (childWSGetLabel field2)
labelExpression (EWSF (WSIdentifier ident) sourceFragment) n =
    ((LabIdentifier field1), (childGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelVariable ident n
labelExpression (EWSF (WSReference ex1 ex2) sourceFragment) n =
    ((LabReference field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childWSGetLabel field1)
labelExpression (EWSF (WSIndex ex1 ex2) sourceFragment) n =
    ((LabIndex field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childWSGetLabel field1)
labelExpression (EWSF (WSValue val) sourceFragment) n =
    ((LabValue field1), (childGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelValue val n
labelExpression (EWSF (WSPropNameValue name ex) sourceFragment) n =
    ((LabPropNameValue field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelPropertyName name n
        field2 = labelExpression ex (childGetLabel field1)
labelExpression (EWSF (WSCall ex1 ex2) sourceFragment) n =
    ((LabCall field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childWSGetLabel field1)
labelExpression (EWSF (WSArguments args) sourceFragment) n =
    ((LabArguments (field1)), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelExpressionList args n
labelExpression (EWSF (WSParenExpression ex) sourceFragment) n =
    ((LabParenExpression field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
labelExpression (EWSF (WSBreak vars) sourceFragment) n =
    ((LabBreak field1), (maxMaybeLabel field1 n) + 1, sourceFragment)
    where
        field1 = labelMaybeVar vars n
labelExpression (EWSF (WSContinue vars) sourceFragment) n =
    ((LabContinue field1), (maxMaybeLabel field1 n) + 1, sourceFragment)
    where
        field1 = labelMaybeVar vars n
labelExpression (EWSF (WSThrow ex) sourceFragment) n =
    ((LabThrow field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
labelExpression (EWSF (WSCallExpression ex1 op ex2) sourceFragment) n =
    ((LabCallExpression field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelExpression ex1 n
        field2 = labelOperator op (childWSGetLabel field1)
        field3 = labelExpression ex2 (childGetLabel field2)
labelExpression (EWSF (WSFunctionExpression var vars ast) sourceFragment) n =
    ((LabFunctionExpression field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelMaybeVar var n
        field2 = labelVarList vars (maxMaybeLabel field1 n)
        field3 = labelJSAST ast (maximum ((listGetLabels field2) ++ [n]))
labelExpression (EWSF (WSVarDeclaration var ex) sourceFragment) n =
    ((LabVarDeclaration field1 field2), (maxMaybeWSLabel field2 (childGetLabel field1)) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelMaybeExpression ex (childGetLabel field1)
labelExpression (EWSF (WSNew ex) sourceFragment) n =
    ((LabNew field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelExpression ex n


-- Label a Maybe Expression if it is not Nothing.
labelMaybeExpression :: (Maybe ExprWithSourceFragment) -> JSASTLabel -> (Maybe ExprChild)
labelMaybeExpression Nothing n = Nothing
labelMaybeExpression (Just ex) n = Just $ labelExpression ex n


-- Label a JSAST. Recursively process any child fields.
labelJSAST :: JSASTWithSourceFragment -> JSASTLabel -> ASTChild
labelJSAST (AWSF (WSBlock jsastLs) sourceFragment) n =
    ((LabBlock field1), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelJSASTList jsastLs n
labelJSAST (AWSF (WSFunctionBody jsastLs) sourceFragment) n =
    ((LabFunctionBody field1), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelJSASTList jsastLs n
labelJSAST (AWSF (WSFunctionDeclaration var args body) sourceFragment) n =
    ((LabFunctionDeclaration field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelVarList args (childGetLabel field1)
        field3 = labelJSAST body $ maximum ((listGetLabels field2) ++ [childGetLabel field1])
labelJSAST (AWSF (WSLabelled var body) sourceFragment) n =
    ((LabLabelled field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelJSAST body (childGetLabel field1)
labelJSAST (AWSF (WSForVar ex1 ex2 ex3 body) sourceFragment) n =
    ((LabForVar field1 field2 field3 field4), (childWSGetLabel field4) + 1, sourceFragment)
    where
        field1 = labelExpressionList ex1 n
        field2 = labelMaybeExpression ex2 $ maximum ((listWSGetLabels field1) ++ [n])
        field3 =
            labelMaybeExpression ex3 $ maximum ((listWSGetLabels field1) ++ [maxMaybeWSLabel field2 n])
        field4 =
            labelJSAST
                body
                $ maximum
                    ((listWSGetLabels field1)
                    ++ [maxMaybeWSLabel field2 n]
                    ++ [maxMaybeWSLabel field3 n])
labelJSAST (AWSF (WSFor ex1 ex2 ex3 body) sourceFragment) n =
    ((LabFor field1 field2 field3 field4), (childWSGetLabel field4) + 1, sourceFragment)
    where
        field1 = labelMaybeExpression ex1 n
        field2 = labelMaybeExpression ex2 (maxMaybeWSLabel field1 n)
        field3 =
            labelMaybeExpression ex3 $ max (maxMaybeWSLabel field1 n) (maxMaybeWSLabel field2 n)
        field4 =
            labelJSAST
                body
                $ maximum
                    ([maxMaybeWSLabel field1 n]
                    ++ [maxMaybeWSLabel field2 n]
                    ++ [maxMaybeWSLabel field3 n])
labelJSAST (AWSF (WSForIn vars ex body) sourceFragment) n =
    ((LabForIn field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelVarList vars n
        field2 = labelExpression ex $ maximum ((listGetLabels field1) ++ [n])
        field3 = labelJSAST body (childWSGetLabel field2)
labelJSAST (AWSF (WSForVarIn ex1 ex2 body) sourceFragment) n =
    ((LabForVarIn field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelExpression ex1 n
        field2 = labelExpression ex2 (childWSGetLabel field1)
        field3 = labelJSAST body (childWSGetLabel field2)
labelJSAST (AWSF (WSWhile ex body) sourceFragment) n =
    ((LabWhile field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST body (childWSGetLabel field1)
labelJSAST (AWSF (WSDoWhile body ex) sourceFragment) n  =
    ((LabDoWhile field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelJSAST body n
        field2 = labelExpression ex (childWSGetLabel field1)
labelJSAST (AWSF (WSIf ex body) sourceFragment) n =
    ((LabIf field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST body (childWSGetLabel field1)
labelJSAST (AWSF (WSIfElse ex bodyT bodyF) sourceFragment) n =
    ((LabIfElse field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST bodyT (childWSGetLabel field1)
        field3 = labelJSAST bodyF (childWSGetLabel field2)
labelJSAST (AWSF (WSSwitch ex cs) sourceFragment) n =
    ((LabSwitch field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST cs (childWSGetLabel field1)
labelJSAST (AWSF (WSCase ex body) sourceFragment) n =
    ((LabCase field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
        field2 = labelJSAST body (childWSGetLabel field1)
labelJSAST (AWSF (WSDefault body) sourceFragment) n =
    ((LabDefault field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelJSAST body n
labelJSAST (AWSF (WSTry body ctch) sourceFragment) n =
    ((LabTry field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelJSAST body n
        field2 = labelJSAST ctch (childWSGetLabel field1)
labelJSAST (AWSF (WSCatch var ex body) sourceFragment) n =
    ((LabCatch field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelMaybeExpression ex (childGetLabel field1)
        field3 = labelJSAST body (maxMaybeWSLabel field2 (childGetLabel field1))
labelJSAST (AWSF (WSFinally body) sourceFragment) n =
    ((LabFinally field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelJSAST body n
labelJSAST (AWSF (WSReturn ex) sourceFragment) n =
    ((LabReturn field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
labelJSAST (AWSF (WSStatement ex) sourceFragment) n =
    ((LabStatement field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelExpression ex n
