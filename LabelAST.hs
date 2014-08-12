
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


-- This module takes a AST and gives each vertex a unique integer label. The label counter is simply
-- threaded through the tree. Traversal is depth first. It's all fairly straight-forward.
--
-- Top level function is
-- (label (astListWSMakeSourceFragments (getASTWithSource (parseTree program file) file) span))


module LabelAST
( ASTChild
, ASTLabel
, IndexChild
, LabelledAST(..)
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
type ASTLabel = Int


-- A Variable and a label.
type VarChild = (Variable, ASTLabel)


-- An Operator and a label.
type OpChild = (Operator, ASTLabel)


-- An Index and a label.
type IndexChild = (Index, ASTLabel)


-- A VarChild or IndexChild wrapped in a LabelledPropertyName, and a label. Added long after the
-- original code was written.
--
-- TODO: Test.
type PropertyNameChild = (LabelledPropertyName, ASTLabel)


-- A value wrapped as a LabelledValue, and a label. Most LabelledValues contain only the value and
-- no label.
type ValueChild = (LabelledValue, ASTLabel)


-- A LabelledExpression (which is a labelled subtree) and a label.
-- type ASTChild = (LabelledExpression, ASTLabel, SourceFragment)


-- A LabelledAST (which is a labelled subree) an a label.
-- TODO: Rename to LabelledAST && rename LabelledAST to LabAST (or LAST).
type ASTChild = (LabelledAST, ASTLabel, SourceFragment)


-- A wrapper for a VarChild or IndexChild that identifies it as a name for a an object property.
data LabelledPropertyName =
      LabIndexProperty IndexChild
    | LabVariableProperty VarChild deriving (Show)


-- A labelled representation of a literal. LabelledValues representing primitives contain only the
-- value and no label. LabelledValues representing objects and arrays are labelled recursively.
-- LabUndefined and LabNull have no value or label.
data LabelledValue =
      LabArray [ASTChild]
    | LabBool Bool
    | LabDQString String
    | LabFloat Double
    | LabInt Int
    | LabNull
    | LabObject [ASTChild]
    | LabString String
    | LabUndefined deriving (Show)


-- A recursively labelled subrtree, rooted at a LabelledAST.
data LabelledAST =
      LabBlock ASTChild
    | LabCase ASTChild ASTChild
    | LabCatch VarChild (Maybe ASTChild) ASTChild
    | LabDefault ASTChild
    | LabDoWhile ASTChild ASTChild
    | LabFinally ASTChild
    | LabFor (Maybe ASTChild) (Maybe ASTChild) (Maybe ASTChild) ASTChild
    | LabForIn [VarChild] ASTChild ASTChild
    | LabForVar [ASTChild] (Maybe ASTChild) (Maybe ASTChild) ASTChild
    | LabForVarIn ASTChild ASTChild ASTChild
    | LabFunctionBody [ASTChild]
    | LabFunctionDeclaration VarChild [VarChild] ASTChild
    | LabIf ASTChild ASTChild
    | LabIfElse ASTChild ASTChild ASTChild
    | LabLabelled VarChild ASTChild
    | LabReturn ASTChild
    | LabStatement ASTChild
    | LabSwitch ASTChild ASTChild
    | LabTry ASTChild ASTChild
    | LabWhile ASTChild ASTChild

    | LabArguments [ASTChild]
    | LabAssignment OpChild ASTChild ASTChild
    | LabBinary OpChild ASTChild ASTChild
    | LabBreak (Maybe VarChild)
    | LabCall ASTChild ASTChild
    | LabCallExpression ASTChild OpChild ASTChild
    | LabContinue (Maybe VarChild)
    | LabExpression [ASTChild]
    | LabFunctionExpression (Maybe VarChild) [VarChild] ASTChild
    | LabIdentifier VarChild
    | LabIndex ASTChild ASTChild
    | LabList [ASTChild]
    | LabNew ASTChild
    | LabParenExpression ASTChild
    | LabPropNameValue PropertyNameChild ASTChild
    | LabReference ASTChild ASTChild
    | LabStatementList [ASTChild]
    | LabTernary ASTChild ASTChild ASTChild
    | LabThrow ASTChild
    | LabUnaryPost OpChild ASTChild
    | LabUnaryPre OpChild ASTChild
    | LabValue ValueChild
    | LabVarDeclaration VarChild (Maybe ASTChild) deriving (Show)


-- Takes an unlabelled AST and labels the whole thing.
label :: ASTWithSourceFragment -> ASTChild
-- label list = labelASTList list 0
label ast = labelAST ast 0


-- Extract the ASTLabel from a VarChild, IndexChild etc.
childGetLabel :: (a, ASTLabel) -> ASTLabel
childGetLabel (child, lab) = lab

-- Extract the ASTLabel from a ASTChild, ASTChild etc.
childWSGetLabel :: (a, ASTLabel, b) -> ASTLabel
childWSGetLabel (_, lab, _) = lab

childGetSource :: (a, b, SourceFragment) -> SourceFragment
childGetSource (_, _, sf) = sf

-- Extract the labels from a list of VarChild, IndexChild etc.
listGetLabels :: [(a, ASTLabel)] -> [ASTLabel]
listGetLabels [] = []
listGetLabels (c:cs) = ((childGetLabel c):(listGetLabels cs))

-- Extract the labels from a list of ASTChild, ASTChild etc.
listWSGetLabels :: [(a, ASTLabel, b)] -> [ASTLabel]
listWSGetLabels [] = []
listWSGetLabels (c:cs) = ((childWSGetLabel c):(listWSGetLabels cs))


-- Find the greater of the label on a Maybe *Child and a given value.
maxMaybeLabel :: (Maybe (a, ASTLabel)) -> ASTLabel -> ASTLabel
-- If the Maybe *Child is nothing then the given value is the greatest.
maxMaybeLabel maybeASTChild label =
    maybe label (\astChild -> max (childGetLabel astChild) label) maybeASTChild


-- Find the greater of the label on a Maybe *Child and a given value.
maxMaybeWSLabel :: (Maybe (a, ASTLabel, b)) -> ASTLabel -> ASTLabel
-- If the Maybe *Child is nothing then the given value is the greatest.
maxMaybeWSLabel maybeASTChild label =
    maybe label (\astChild -> max (childWSGetLabel astChild) label) maybeASTChild

-- Label a list of Varialbes.
labelVarList :: [Variable] -> ASTLabel -> [VarChild]
labelVarList [] _ = []
labelVarList (v:vx) n = (v, n + 1):(labelVarList vx (n + 1))


-- Label a list of Expressions.
labelExpressionList :: [ASTWithSourceFragment] -> ASTLabel -> [ASTChild]
labelExpressionList [] _ = []
labelExpressionList (e:ex) n =
    let (le, m, sf) = labelAST e n in ((le, m, sf):(labelExpressionList ex m))


-- Label a list of ASTs.
labelASTList :: [ASTWithSourceFragment] -> ASTLabel -> [ASTChild]
labelASTList [] _ = []
labelASTList (a:ax) n =
    let (la, m, sf) = labelAST a n in ((la, m, sf):(labelASTList ax m))


-- Label a Varialble.
labelVariable :: Variable -> ASTLabel -> VarChild
labelVariable var n = (var, n + 1)


-- -- Label a Maybe Variable if it is not Nothing.
-- labelMaybeVar :: (Maybe Variable) -> ASTLabel -> (Maybe VarChild)
-- labelMaybeVar maybeVar label = maybe Nothing (\var -> Just $ labelVariable var label) maybeVar

-- Label a Maybe Variable if it is not Nothing.
labelMaybeVar :: (Maybe Variable) -> ASTLabel -> (Maybe VarChild)
labelMaybeVar maybeVar label = maybeVar >>= (\var -> Just $ labelVariable var label)

-- Label an Operator.
labelOperator :: Operator -> ASTLabel -> OpChild
labelOperator op n = (op, n + 1)


-- Label an Index.
labelIndex :: Index -> ASTLabel -> IndexChild
labelIndex ix n = (ix, n + 1)


-- Label a PropertyName.
--
-- TODO: Unit test?
labelPropertyName :: PropertyName -> ASTLabel -> PropertyNameChild
labelPropertyName (IndexProperty ix) n =
    (LabIndexProperty field1, (childGetLabel field1) + 1)
    where
        field1 = labelIndex ix n
labelPropertyName (VariableProperty var) n =
    (LabVariableProperty field1, (childGetLabel field1) + 1)
    where
        field1 = labelVariable var n


-- Label a Value. Recursively process any child fields.
labelValue :: ValueWithSourceFragment -> ASTLabel -> ValueChild
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


-- Label a Maybe Expression if it is not Nothing.
-- FIXME: Replace with monad operations.
labelMaybeAST :: (Maybe ASTWithSourceFragment) -> ASTLabel -> (Maybe ASTChild)
labelMaybeAST maybeAST label = maybeAST >>= (\ast -> Just $ labelAST ast label)


-- Label a AST. Recursively process any child fields.
labelAST :: ASTWithSourceFragment -> ASTLabel -> ASTChild
-- FIXME: WSBlock handler changed in a rush after child changed from a list to just one node. Double
-- check it.
labelAST (AWSF (WSBlock body) sourceFragment) n =
    ((LabBlock field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST body n
labelAST (AWSF (WSFunctionBody astList) sourceFragment) n =
    ((LabFunctionBody field1), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelASTList astList n
labelAST (AWSF (WSFunctionDeclaration var args body) sourceFragment) n =
    ((LabFunctionDeclaration field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelVarList args (childGetLabel field1)
        field3 = labelAST body $ maximum ((listGetLabels field2) ++ [childGetLabel field1])
labelAST (AWSF (WSLabelled var body) sourceFragment) n =
    ((LabLabelled field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelAST body (childGetLabel field1)
labelAST (AWSF (WSForVar ex1 ex2 ex3 body) sourceFragment) n =
    ((LabForVar field1 field2 field3 field4), (childWSGetLabel field4) + 1, sourceFragment)
    where
        field1 = labelExpressionList ex1 n
        field2 = labelMaybeAST ex2 $ maximum ((listWSGetLabels field1) ++ [n])
        field3 =
            labelMaybeAST ex3 $ maximum ((listWSGetLabels field1) ++ [maxMaybeWSLabel field2 n])
        field4 =
            labelAST
                body
                $ maximum
                    ((listWSGetLabels field1)
                    ++ [maxMaybeWSLabel field2 n]
                    ++ [maxMaybeWSLabel field3 n])
labelAST (AWSF (WSFor ex1 ex2 ex3 body) sourceFragment) n =
    ((LabFor field1 field2 field3 field4), (childWSGetLabel field4) + 1, sourceFragment)
    where
        field1 = labelMaybeAST ex1 n
        field2 = labelMaybeAST ex2 (maxMaybeWSLabel field1 n)
        field3 =
            labelMaybeAST ex3 $ max (maxMaybeWSLabel field1 n) (maxMaybeWSLabel field2 n)
        field4 =
            labelAST
                body
                $ maximum
                    ([maxMaybeWSLabel field1 n]
                    ++ [maxMaybeWSLabel field2 n]
                    ++ [maxMaybeWSLabel field3 n])
labelAST (AWSF (WSForIn vars ex body) sourceFragment) n =
    ((LabForIn field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelVarList vars n
        field2 = labelAST ex $ maximum ((listGetLabels field1) ++ [n])
        field3 = labelAST body (childWSGetLabel field2)
labelAST (AWSF (WSForVarIn ex1 ex2 body) sourceFragment) n =
    ((LabForVarIn field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelAST ex1 n
        field2 = labelAST ex2 (childWSGetLabel field1)
        field3 = labelAST body (childWSGetLabel field2)
labelAST (AWSF (WSWhile ex body) sourceFragment) n =
    ((LabWhile field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelAST ex n
        field2 = labelAST body (childWSGetLabel field1)
labelAST (AWSF (WSDoWhile body ex) sourceFragment) n  =
    ((LabDoWhile field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelAST body n
        field2 = labelAST ex (childWSGetLabel field1)
labelAST (AWSF (WSIf ex body) sourceFragment) n =
    ((LabIf field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelAST ex n
        field2 = labelAST body (childWSGetLabel field1)
labelAST (AWSF (WSIfElse ex bodyT bodyF) sourceFragment) n =
    ((LabIfElse field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelAST ex n
        field2 = labelAST bodyT (childWSGetLabel field1)
        field3 = labelAST bodyF (childWSGetLabel field2)
-- FIXME: Uncomment this and fix it (second field is a list now)
-- labelAST (AWSF (WSSwitch ex cs) sourceFragment) n =
--     ((LabSwitch field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
--     where
--         field1 = labelAST ex n
--         field2 = labelAST cs (childWSGetLabel field1)
labelAST (AWSF (WSCase ex body) sourceFragment) n =
    ((LabCase field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelAST ex n
        field2 = labelAST body (childWSGetLabel field1)
labelAST (AWSF (WSDefault body) sourceFragment) n =
    ((LabDefault field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST body n
-- FIXME: Uncomment this and fix it (second field is a list now)
-- labelAST (AWSF (WSTry body ctch) sourceFragment) n =
--     ((LabTry field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
--     where
--         field1 = labelAST body n
--         field2 = labelAST ctch (childWSGetLabel field1)
labelAST (AWSF (WSCatch var ex body) sourceFragment) n =
    ((LabCatch field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelMaybeAST ex (childGetLabel field1)
        field3 = labelAST body (maxMaybeWSLabel field2 (childGetLabel field1))
labelAST (AWSF (WSFinally body) sourceFragment) n =
    ((LabFinally field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST body n
labelAST (AWSF (WSReturn ex) sourceFragment) n =
    ((LabReturn field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST ex n
labelAST (AWSF (WSStatement ex) sourceFragment) n =
    ((LabStatement field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST ex n
labelAST (AWSF (WSList ex) sourceFragment) n =
    ((LabList (field1)), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelExpressionList ex n
labelAST (AWSF (WSExpression ex) sourceFragment) n =
    ((LabExpression (field1)), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelExpressionList ex n
labelAST (AWSF (WSStatementList ex) sourceFragment) n =
    ((LabStatementList (field1)), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelExpressionList ex n
labelAST (AWSF (WSBinary op ex1 ex2) sourceFragment) n =
    ((LabBinary field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelAST ex1 (childGetLabel field1)
        field3 = labelAST ex2 (childWSGetLabel field2)
labelAST (AWSF (WSUnaryPost op ex) sourceFragment) n =
    ((LabUnaryPost field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelAST ex (childGetLabel field1)
labelAST (AWSF (WSUnaryPre op ex) sourceFragment) n =
    ((LabUnaryPre field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelAST ex (childGetLabel field1)
labelAST (AWSF (WSTernary ex1 ex2 ex3) sourceFragment) n =
    ((LabTernary field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelAST ex1 n
        field2 = labelAST ex2 (childWSGetLabel field1)
        field3 = labelAST ex3 (childWSGetLabel field2)
labelAST (AWSF (WSAssignment op ex1 ex2) sourceFragment) n =
    ((LabAssignment field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelOperator op n
        field2 = labelAST ex1 (childGetLabel field1)
        field3 = labelAST ex2 (childWSGetLabel field2)
labelAST (AWSF (WSIdentifier ident) sourceFragment) n =
    ((LabIdentifier field1), (childGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelVariable ident n
labelAST (AWSF (WSReference ex1 ex2) sourceFragment) n =
    ((LabReference field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelAST ex1 n
        field2 = labelAST ex2 (childWSGetLabel field1)
labelAST (AWSF (WSIndex ex1 ex2) sourceFragment) n =
    ((LabIndex field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelAST ex1 n
        field2 = labelAST ex2 (childWSGetLabel field1)
labelAST (AWSF (WSValue val) sourceFragment) n =
    ((LabValue field1), (childGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelValue val n
labelAST (AWSF (WSPropNameValue name ex) sourceFragment) n =
    ((LabPropNameValue field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelPropertyName name n
        field2 = labelAST ex (childGetLabel field1)
labelAST (AWSF (WSCall ex1 ex2) sourceFragment) n =
    ((LabCall field1 field2), (childWSGetLabel field2) + 1, sourceFragment)
    where
        field1 = labelAST ex1 n
        field2 = labelAST ex2 (childWSGetLabel field1)
labelAST (AWSF (WSArguments args) sourceFragment) n =
    ((LabArguments (field1)), (maximum ((listWSGetLabels field1) ++ [n])) + 1, sourceFragment)
    where
        field1 = labelExpressionList args n
labelAST (AWSF (WSParenExpression ex) sourceFragment) n =
    ((LabParenExpression field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST ex n
labelAST (AWSF (WSBreak vars) sourceFragment) n =
    ((LabBreak field1), (maxMaybeLabel field1 n) + 1, sourceFragment)
    where
        field1 = labelMaybeVar vars n
labelAST (AWSF (WSContinue vars) sourceFragment) n =
    ((LabContinue field1), (maxMaybeLabel field1 n) + 1, sourceFragment)
    where
        field1 = labelMaybeVar vars n
labelAST (AWSF (WSThrow ex) sourceFragment) n =
    ((LabThrow field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST ex n
labelAST (AWSF (WSCallExpression ex1 op ex2) sourceFragment) n =
    ((LabCallExpression field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelAST ex1 n
        field2 = labelOperator op (childWSGetLabel field1)
        field3 = labelAST ex2 (childGetLabel field2)
labelAST (AWSF (WSFunctionExpression var vars ast) sourceFragment) n =
    ((LabFunctionExpression field1 field2 field3), (childWSGetLabel field3) + 1, sourceFragment)
    where
        field1 = labelMaybeVar var n
        field2 = labelVarList vars (maxMaybeLabel field1 n)
        field3 = labelAST ast (maximum ((listGetLabels field2) ++ [n]))
labelAST (AWSF (WSVarDeclaration var ex) sourceFragment) n =
    ((LabVarDeclaration field1 field2), (maxMaybeWSLabel field2 (childGetLabel field1)) + 1, sourceFragment)
    where
        field1 = labelVariable var n
        field2 = labelMaybeAST ex (childGetLabel field1)
labelAST (AWSF (WSNew ex) sourceFragment) n =
    ((LabNew field1), (childWSGetLabel field1) + 1, sourceFragment)
    where
        field1 = labelAST ex n
