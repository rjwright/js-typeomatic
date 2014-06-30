
--------------------------------------------------------------------------------

-- This module generates type constraints (also called "type rules" or just
-- "rules" throughout this program) from sections of a labelled AST that
-- correspond to functions (i.e. scope levels). Type constraints are
-- "equalities" between types, where most types appear as type variables.
-- Although in general they are not symmetric. For constraints that are
-- symmetric, two versions of that constraint should be generated. However the
-- constraints are transitive, so they actually define a partial order but not
-- an equivalence relation on the set of types that appear in the AST.
--
-- The output from this module will be input to the unification module. I have
-- written this constraint-generating code without fully understanding the
-- unificaion algorithm in the context of type inference, although I have written
-- unification for the purpose of computing sufficientcies in a more abstract
-- context. I'm just hoping they will be in a convenient format for unification.

module TypeRules
( Rule(..)
, Type(..)
, IdentifierLabel(..)
, DeclaredIdentifier(..)
, funExprMakeLabel
, varDecMakeLabel
, funDecMakeLabel
, labelledMakeLabel
, argMakeLabel
, mapASTChildRules
, astChildRules
) where

--------------------------------------------------------------------------------

import LabelJSAST
import ParseJS

-- NOTE FOR THIS MODULE:
-- 1)
-- Try to preserve info about when a variable is an integer. E.g. if we have
-- var a = 1; var b = 5; var c = a * b;
-- we know that a and b are still ints and c is an int. Use three numerical
-- types: IntType, NumType and FloatType. Note that normally FloatType will
-- unify with IntType to give FloatType (we can use ints as floats). But in some
-- contexts we require a variable to be strictly an integer (e.g. array
-- indexing). Possibly handle that in the compiler layer.


-- The types that can appear in the AST.
--
-- Some of them are second-order types which contain other types as fields. The
-- values of some second-order types depend of on the types of their fields.
-- E.g:
--              (Meta 11) = (IntAndInt (Meta 9) (Meta 8))
--              (Meta 7) = IntType
--              (Meta 6) = IntType
--              (Meta 9) = (IntAndInt (Meta 7) (Meta 6))
--              (Meta 8) = IntType
--
-- In this case (Meta 11) has type IntType if both (Meta 9) and (Meta 8) have
-- type IntType. As it turns out, they do.
data Type = 
            -- The type for an identifier, including a label to distinguish
            -- variables with the same name.
            IdentifierType Variable IdentifierLabel
            -- FunctionType contains and AgumentsIDType and a ReturnType and a
            -- ConstructorType.
            | FunctionType Type Type Type
            -- ConstructorType contains the type variable for the body of the
            -- function.
            | ConstructorType Type
            -- ReturnType contains the type variable for the body of the
            -- function.
            | ReturnType Type
            -- ArgumentsIDType just contains a FunctionType (i.e. a reference
            -- back to the function it belongs to). I can't remember the exact
            -- details at this moment, but I'm pretty sure that rules containing
            -- this type tend to cause a cycle in the constraints which may
            -- cause problems. Check it out. TODO
            | ArgumentsIDType Type
            -- A list of types, one for each argument that a particular function
            -- takes. Used to equate an ArgumentsIDType to some actual types.
            | ArgumentsType [Type]
            -- For the evaluation of a function. Contains a FunctionType.
            | EvaluationType Type
            -- For the instantiation of a function. Contains a FunctionType.
            | InstantiationType Type
            -- For expressions that contain + or +=.
            | PlusType Type Type
            -- Used for - * and % operators, where the value of the expression
            -- is an int if both operands ints and is a float otherwise.
            | IntAndInt Type Type
            -- Fairly self-explanatory.
            | IntType
            | FloatType
            | NumType
            | StringType
            | BoolType
            | UndefType
            | NullType
            -- An ObjectType contains a list of properties.
            -- TODO: add more comments on this when you remember WTF is going on.
            | ObjectType [(PropertyName, Type)]
            -- When a member of an object is referenced it implies that the
            -- Object has at least the filed being referenced.
            | AtLeastObjectType [PropertyName]
            -- A ReferenceType contains an ObjectType or ArrayType (possibly
            -- indirectly) and a Variable/Index.
            | ReferenceType Type PropertyName
            -- An array has a list of properties (Elements! Not to be confused
            -- with other properties of the array object, which I am ignoring
            -- the existence of for now.)
            -- TODO: add more comments on this when you remember WTF is going on.
            | ArrayType [(PropertyName, Type)]
            -- For use when an object or array is indexed using a variable
            -- instead of a string literal or int literal. If the type of
            -- the field is found to be an object then that object has
            -- AmbiguousType. If the field is found to be an array then
            -- we do nothing with this information and proceed.
            | CorruptIfObjectType Type
            -- For use when an object or array is indexed using a variable or
            -- expression. If the field turns out to be an array then this is
            -- equivalent to IntType. If the field turns out to be an object
            -- then we do nothing with this information and proceed.
            | IntIfArrayType Type
            -- A type variable, basically.
            | Meta JSASTLabel
            -- For things that have been found to be untypeable.
            | AmbiguousType deriving (Show)


-- A type rule. Huzzah!
data Rule = Rule Type Type deriving (Show)

-- GlobalLabel is for identifiers that are assigned without being
-- declared, as doing so declares a global variable. IDLabel is for
-- any identifier that can be assigned a unique label when it is
-- declared.
data IdentifierLabel = IDLabel Int | GlobalLabel deriving (Show)
-- An identifier with a name and a label
data DeclaredIdentifier = DeclaredIdentifier Variable IdentifierLabel deriving (Show)

----------------------------------------------------------------------------------------------------------------------
-- These functions ensure that the label used in making a unique
-- DeclaredIdentifier is consistent, as the DeclaredIdentifiers need to be made
-- more than once in some cases.
funExprMakeLabel :: ExprChild -> (Maybe DeclaredIdentifier)
funExprMakeLabel (LabFunctionExpression mv vls body, n) = maybeID mv n
        where
        maybeID Nothing _ = Nothing
        maybeID (Just (id, _)) x = Just (DeclaredIdentifier id (IDLabel x))

varDecMakeLabel :: ExprChild -> DeclaredIdentifier
varDecMakeLabel (LabVarDeclaration (var, x) mex, n) = DeclaredIdentifier var (IDLabel n)

funDecMakeLabel :: ASTChild -> DeclaredIdentifier
funDecMakeLabel (LabFunctionDeclaration (id, x) args body, n) = DeclaredIdentifier id (IDLabel n)

labelledMakeLabel :: ASTChild -> DeclaredIdentifier
labelledMakeLabel (LabLabelled (var, x) body, n) = DeclaredIdentifier var (IDLabel n)

argMakeLabel :: VarChild -> DeclaredIdentifier
argMakeLabel (var, n) = DeclaredIdentifier var (IDLabel n)
----------------------------------------------------------------------------------------------------------------------

-- Extract the value (strip label) from a VarChild, ExprChild,
-- ValueChild or ASTChild.
childGetValue :: (a, JSASTLabel) -> a
childGetValue (val, lab) = val

-- Create a Meta type from the label on a VarChild, ExprChild,
-- ValueChild or ASTChild.
childToMeta :: (a, JSASTLabel) -> Type
childToMeta ch = Meta . childGetLabel $ ch


-- Generate rules from a Maybe VarChild
maybeVarChildRules :: (Maybe VarChild) -> [DeclaredIdentifier] -> [Rule]
maybeVarChildRules (Just vc) dIDs = varChildRules vc $ dIDs
maybeVarChildRules Nothing _ = []

-- Generate rules from a Maybe ExprChild
maybeExprChildRules :: (Maybe ExprChild) -> [DeclaredIdentifier] -> [Rule]
maybeExprChildRules (Just ec) dIDs = exprChildRules ec dIDs
maybeExprChildRules Nothing _ = []


-- Generate rules from a VarChild list
mapVarChildRules :: [VarChild] -> [DeclaredIdentifier] -> [Rule]
mapVarChildRules var dIDs = concat . map mapVarChildRules' $ var
    where
    mapVarChildRules' v = varChildRules v dIDs
-- Generate rules from an ExprChild list
mapExprChildRules :: [ExprChild] -> [DeclaredIdentifier] -> [Rule]
mapExprChildRules ex dIDs = concat . map mapExprChildRules' $ ex
    where
    mapExprChildRules' e = exprChildRules e dIDs
-- Gernerate rules from an ASTChild list
mapASTChildRules :: [ASTChild] -> [DeclaredIdentifier] -> [Rule]
mapASTChildRules ast dIDs = concat . map mapASTChildRules' $ ast
    where
    mapASTChildRules' a = astChildRules a dIDs

-- Generate rules from a VarChild.
varChildRules :: VarChild -> [DeclaredIdentifier] -> [Rule]
varChildRules (var, x) dIDs = [Rule (Meta x) (IdentifierType var (idGetLabel var dIDs))] ++
        [Rule (IdentifierType var (idGetLabel var dIDs)) (Meta x)]

-- Take a variable name and a list of declared identifiers and search the list
-- for the variable name. If the name appears in the list, return the label it
-- appears with, otherwise return GlobalLabel.
idGetLabel :: Variable -> [DeclaredIdentifier] -> IdentifierLabel
idGetLabel variable [] = GlobalLabel
idGetLabel variable ((DeclaredIdentifier v label):ds) = if (variable == v) then label else (idGetLabel variable ds)
 
-- Generate rules from a value.
valueChildRules :: ValueChild -> [DeclaredIdentifier] -> [Rule]
-- For primitive literals.
valueChildRules (LabInt _, x) dIDs = [Rule (Meta x) IntType]
valueChildRules (LabFloat _, x) dIDs = [Rule (Meta x) FloatType]
valueChildRules (LabString _, x) dIDs = [Rule (Meta x) StringType]
valueChildRules (LabBool _, x) dIDs = [Rule (Meta x) BoolType]
valueChildRules (LabDQString _, x) dIDs = [Rule (Meta x) StringType]
-- Objects! Make a rule for the object, the properties of the object and the
-- values of the properties of the object.
valueChildRules (LabObject members, x) dIDs = [Rule (Meta x) thisObjectType] ++
        (concat (map propTypeRules members))
        where
        -- A name for the type of this object.
        thisObjectType = (ObjectType (map prop members))
        -- Strip labels from labelled index property name.
        getPropName (LabIndexProperty (ix, p), q) = IndexProperty ix
        -- Strip labels from labelled variable property name.
        getPropName (LabVariableProperty (var, p), q) = VariableProperty var
        -- Pair the name of a property with the type of its value.
        prop (LabPropNameValue name ex, n) = ((getPropName $ name), (childToMeta $ ex))
        -- Make rules for the properties of this object.
        propTypeRules (LabPropNameValue name ex, n) =
                -- Each property is a reference type.
                [Rule (ReferenceType thisObjectType (getPropName $ name)) (Meta n)] ++
                -- The whole reference expression has the same type as the value
                -- of the property.
                [Rule (Meta n) (childToMeta $ ex)] ++
                -- Recursively process value of property.
                (exprChildRules ex dIDs)
-- Make a rule for the array. Make rules for the elements in the array. Make rules for the
-- values of the elements in the array. I need to make sure that exprChildRules always binds
-- n to something meaningfull (n is the bridge from the element to the expression that is its
-- value). TODO
valueChildRules (LabArray elements, x) dIDs = [Rule (Meta x) (ArrayType (elemTypes elements [] 0))] ++
        (mapExprChildRules elements dIDs) ++ (elemTypeRules elements [] 0)
        where
        -- We know that the property name is an index because this is an array
        -- (not actually true but let's pretend that it is).
        -- Pair the property name (i.e. element index) to the type of its value.
        elemType ex i = ((IndexProperty i), (childToMeta $ ex))
        -- Take a list of PropertyNameValues and make a list of indicies paired
        -- with the types of their values.
        elemTypes [] elTypes _ = elTypes
        elemTypes (e:els) elTypes i = elemTypes (els) ((elemType e i):elTypes) (i + 1)
        -- Each property (i.e. element) generates a rule relating the reference
        -- type to the type of its value.
        elemTypeRule ex i =
            Rule (ReferenceType (ArrayType (elemTypes elements [] 0)) (IndexProperty i)) (childToMeta $ ex)
        -- Find all rules generated by a list of array elements (properties).
        elemTypeRules [] elRules _ = elRules
        elemTypeRules (e:els) elRules i = elemTypeRules els ((elemTypeRule e i):elRules) (i + 1)
valueChildRules (LabUndefined, x) dIDs = [Rule (Meta x) UndefType]
valueChildRules (LabNull, x) dIDs = [Rule (Meta x) NullType]

-- Generate rules from an expression.
-- Go through and give the parameters for exprChildRules meaningfull names! TODO
exprChildRules :: ExprChild -> [DeclaredIdentifier] -> [Rule]
-- The type of a list of expressions is the same as the type of the last
-- expression in the list.
-- I am not actually 100% sure that that is accurate. TODO
exprChildRules (LabList expList, n) dIDs = [Rule (Meta n) (childToMeta . last $ expList)] ++
        (mapExprChildRules expList dIDs)
-- The '+' operator has unique behavior. The type of the expression depends on
-- the types of both operands. There is a custom type - PlusType - for this
-- operator and the '+=' operator.
exprChildRules (LabBinary ("+", _) ex1 ex2, n) dIDs =
        [Rule (Meta n) (PlusType (childToMeta $ ex1) (childToMeta $ ex2))] ++
        [Rule (PlusType (childToMeta $ ex1) (childToMeta $ ex2)) (Meta n)] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- These operators only act on numbers. If both operands are of integer type
-- then the expression is of integer type. If both operands are of the weaker
-- NumType then the expression has type NumType. If either of the operands has
-- type float then the expression has type float.
exprChildRules (LabBinary (op, _) ex1 ex2, n) dIDs | elem op ["-", "%", "*"] =
        [Rule (childToMeta $ ex1) NumType] ++ [Rule (childToMeta $ ex2) NumType] ++
        [Rule (Meta n) (IntAndInt (childToMeta $ ex1) (childToMeta $ ex2))] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- '/' only operates on numbers. The whole expression has type float.
exprChildRules (LabBinary (op, _) ex1 ex2, n) dIDs | elem op ["/"] =
        [Rule (childToMeta $ ex1) NumType] ++ [Rule (childToMeta $ ex2) NumType] ++ [Rule (Meta n) FloatType] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- Bitwise binary operators act only on numbers (which are cast to integers). The type of the expression is integer.
exprChildRules (LabBinary (op, _) ex1 ex2, n) dIDs | elem op ["&", "|", "^"] = [Rule (Meta n) IntType] ++
        [Rule (childToMeta $ ex1) NumType] ++ [Rule (childToMeta $ ex2) NumType] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- Bitwise shift operators act only one numbers (which are cast to integers). The type of the expression is integer.
exprChildRules (LabBinary (op, _) ex1 ex2, n) dIDs | elem op ["<<", ">>", ">>>"] = [Rule (Meta n) IntType] ++
        [Rule (childToMeta $ ex1) NumType] ++ [Rule (childToMeta $ ex2) NumType] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- The type of a comparison expression is Bool.
exprChildRules (LabBinary (op, _) ex1 ex2, n) dIDs | elem op ["==", "!=", "===", "!==", ">", "<", ">=", "<="] =
    [Rule (Meta n) BoolType] ++ (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- The type of a binary logic expression is Bool. JavaScript's interpretation of
-- various expressions when cast to boolean is complex. What we want to do with
-- it depends on what we want to do with the compiler.
exprChildRules (LabBinary (op, _) ex1 ex2, n) dIDs | elem op ["&&", "||"] = [Rule (Meta n) BoolType] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- Postfix '++' or '--' only operate on numbers. They type of the expression is
-- number (integer if ex is integer and float if ex is float).
exprChildRules (LabUnaryPost op ex, n) dIDs = [Rule (childToMeta $ ex) NumType] ++
        [Rule (Meta n) (childToMeta $ ex)] ++ (exprChildRules ex dIDs)
-- These only operate on numbers. The type of the expression is the type of ex.
exprChildRules (LabUnaryPre (op, _) ex, n) dIDs | elem op ["++", "--", "-", "+"] = [Rule (childToMeta $ ex) NumType] ++
        [Rule (Meta n) (childToMeta $ ex)] ++ (exprChildRules ex dIDs)
-- The type of a not expression is bool.
exprChildRules (LabUnaryPre ("!", _) ex, n) dIDs = [Rule (Meta n) BoolType] ++ (exprChildRules ex dIDs)
-- The type of the condition in a ternary expression is bool. They type of the
-- whole expression is the type of the two optional expressions. If they don't
-- have the same type then type inference on the expression fails.
exprChildRules (LabTernary ex1 ex2 ex3, n) dIDs = [boolRule $ ex1] ++ [Rule (Meta n) (childToMeta $ ex2)] ++
        [Rule (Meta n) (childToMeta $ ex3)] ++ (exprChildRules ex1 dIDs) ++
        (exprChildRules ex2 dIDs) ++ (exprChildRules ex3 dIDs)
-- They type of an assignment expression is the type of the value that is being
-- assigned to the variable. The type of the variable is the type if its
-- assigned value.
exprChildRules (LabAssignment ("=", _) ex1 ex2, n) dIDs = [Rule (Meta n) (childToMeta $ ex2)] ++
        [Rule (childToMeta $ ex1) (childToMeta $ ex2)] ++ (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- Similar to the '+' operator. This introduces a horrible cycle but I don't
-- think I can avoid it, because the new type of the variable depends on its old
-- type. The unification algorith will have to handle it.
exprChildRules (LabAssignment ("+=", _) ex1 ex2, n) dIDs =
        [Rule (Meta n) (PlusType (childToMeta $ ex1) (childToMeta $ ex2))] ++
        [Rule (childToMeta $ ex1) (Meta n)] ++ (exprChildRules ex1 dIDs) ++
        (exprChildRules ex2 dIDs)
-- Similar to the '-', '*' and '%' operators. This also introduces a cycle,
-- but I don't think I can avoid it, as the new type of the variable depends on
-- its old type. The unification algorithm will have to handle it.
exprChildRules (LabAssignment (op, _) ex1 ex2, n) dIDs | elem op ["-=", "*=", "%="] =
        [Rule (Meta n) (IntAndInt (childToMeta $ ex1) (childToMeta $ ex2))] ++ [Rule (childToMeta $ ex1) (Meta n)] ++
        [Rule (childToMeta $ ex2) NumType] ++ (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- Similar to the '/' operator.
exprChildRules (LabAssignment (op, _) ex1 ex2, n) dIDs | elem op ["/="] = [Rule (Meta n) FloatType] ++
        [Rule (childToMeta $ ex1) FloatType] ++ [Rule (childToMeta $ ex2) NumType] ++ (exprChildRules ex1 dIDs) ++
        (exprChildRules ex2 dIDs)
-- Bitwise operator assignments. See bitwise operators above.
exprChildRules (LabAssignment op ex1 ex2, n) dIDs = [Rule (Meta n) IntType] ++ [Rule (childToMeta $ ex1) NumType] ++
        [Rule (childToMeta $ ex2) NumType] ++ (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
-- The type of an identifier is the same as the type of the variable it
-- contains and vice versa.
exprChildRules (LabIdentifier var, n) dIDs =
        [Rule (Meta n) (childToMeta $ var)] ++ [Rule (childToMeta $ var) (Meta n)] ++ (varChildRules var dIDs)
-- The type of a reference statement equals reference type and the type of that
-- reference type equals the type of the statement.
exprChildRules (LabReference ex1 ex2, n) dIDs =
        [Rule (Meta n) (ReferenceType (childToMeta $ ex1) (getPropName $ ex2))] ++
        [Rule (ReferenceType (childToMeta $ ex1) (getPropName $ ex2)) (Meta n)] ++
        -- The object must contain the property being referenced.
        [Rule (childToMeta $ ex1) (AtLeastObjectType [(getPropName $ ex2)])] ++
        (exprChildRules ex1 dIDs)
        where
        getPropName (LabIdentifier (prop, q), r) = VariableProperty prop
-- An index represents a reference to a property of an object or an element of
-- an array using square bracket notation. There is no way to differentiate
-- between the two using local static analysis.
exprChildRules (LabIndex ex1 ex2, n) dIDs = (exprChildRules ex1 dIDs) ++
        -- I don't really want to require the index to be an int literal or a string literal.
        -- In the case of arrays, we don't care about the types of particular elements, we just
        -- care that all elements have the same type. Of course this is not the case for objects,
        -- since we need to know which property of the object we are modifying so we can check its
        -- type (i.e. the *value* of the index matters).
        --
        -- Having said that, accesses to previously non-existent elements of arrays after their
        -- creation simply "creates" the element (returns undefined on reads and adds the element
        -- the the array on writes, I'm pretty sure). If we define the type of an array to be
        -- array-type plus its size plus the type of its elements then adding elements to arrays
        -- after creation is not type safe. If we define the type of an array to be array-type
        -- plus the type of its elements then adding elements to an array after creation is type
        -- safe (provided sub-operations are type safe; e.g. the types of the elements remains
        -- homogeneous). Both are legitimate definitions. If we use the first definition then
        -- we require the user to define the array at its creation, which translates easily to
        -- C (as C doesn't automatically cope with dynamic arrays); but it means that we can't
        -- allow indexing of the array via anything but an integer literal. If we use the
        -- second definition then our C needs to implement dynamically expanding arrays; growing
        -- the array to accomodate any index that comes up during execution and filling any un-
        -- assigned elements to undefined. But it also means that we can support loops over
        -- arrays rather than failing type inference on the array whenever the user tries to
        -- loop over it. A big win! This is why this project needs a precise
        -- definition of type safety.
        --
        -- Shane says that we should support dynamic arrays with elements that are all of the same
        -- type or undefined. For now I will just assume that all arrays have static size. I will 
        -- also ignore the Array constructor for now and only deal with literally defined arrays.
        --
        -- To make this work I will need to differentiate between references into arrays
        -- and references into objects. TODO.
        --
        -- Also need to do something with the type of the index. Basically if the thing
        -- we're indexing into is an array then the index must be of type int. TODO
        -- For the moment we will ignore (as in, pretend it can't happen)
        -- references to properties of array objects, other than elements of the
        -- array, using square brackets.
        --
        --
        -- If the index is an integer literal then the statement has type
        -- ReferenceType and the reference has the same type as the statement.
        (if (isIntLiteral $ ex2) then
        [Rule (Meta n) (ReferenceType (childToMeta $ ex1) (IndexProperty . getIntLiteral $ ex2))] ++
        [Rule (ReferenceType (childToMeta $ ex1) (IndexProperty . getIntLiteral $ ex2)) (Meta n)] ++
        -- The object or array must contain the member being referenced.
        [Rule (childToMeta $ ex1) (AtLeastObjectType [(IndexProperty . getIntLiteral $ ex2)])]
        -- If the index is a strng literal then the statement has type
        -- ReferenceType and the reference has the same type as the statement.
        else if (isStringLiteral $ ex2) then
        [Rule (Meta n) (ReferenceType (childToMeta $ ex1) (VariableProperty . getStringLiteral $ ex2))] ++
        [Rule (ReferenceType (childToMeta $ ex1) (VariableProperty . getStringLiteral $ ex2)) (Meta n)] ++
        -- The object or array must contain the member being referenced.
        [Rule (childToMeta $ ex1) (AtLeastObjectType [(VariableProperty . getStringLiteral $ ex2)])]
        -- If the index is not an integer or sting literal then we cannot
        -- determine its value.
        else
        -- Record the type of the reference, even though we don't know which
        -- property it is. In the case that the object is an array, all such
        -- rules must relate references to elements of that array to the same
        -- type.
        ([Rule (Meta n) (ReferenceType (childToMeta $ ex1) (UnknownProperty))] ++
        [Rule (ReferenceType (childToMeta $ ex1) (UnknownProperty)) (Meta n)] ++
        -- If the object is not an array then type inference on the object and
        -- all of its members must fail.
        [Rule (childToMeta ex1) (CorruptIfObjectType . childToMeta $ ex1)] ++
        -- If the object is an array then the index must have reference type
        -- (because we are, for now, disregarding the case where the user
        -- references a property of the array other than an element.)
        [Rule (childToMeta $ ex2) (IntIfArrayType . childToMeta $ ex1)] ++ (exprChildRules ex2 dIDs)))
-- The type of a LabValue is the type of the value it contains.
exprChildRules (LabValue val, n) dIDs = [Rule (Meta n) (childToMeta $ val)] ++ (valueChildRules val dIDs)
-- The type of a function call is EvaluationType. <EvaluationType fun>, where
-- fun has type
-- <FunctionType (ArguentsIDType x) (ReturnType y) (InstantiationType z)>,
-- is equal to (i.e. should unify(?) to) y.
exprChildRules (LabCall ex1 ex2, n) dIDs = [Rule (Meta n) (EvaluationType . childToMeta $ ex1)] ++
        -- The arguments passed to the function when calling it should match the
        -- parameters in the function definition.
        [Rule (ArgumentsIDType . childToMeta $ ex1) (ArgumentsType . argsToMeta $ ex2)] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
        where
        argsToMeta (LabArguments args, n) = map childToMeta args
-- Do I need to bind n to something? (is it ever a link in the chain?) TODO
--
-- Not much to say about arguments. Does this even get called? TODO
exprChildRules (LabArguments args, n) dIDs = mapExprChildRules args dIDs
-- The type of a ParenExpression is they type of the expression it contains.
exprChildRules (LabParenExpression ex, n) dIDs = [Rule (Meta n) (childToMeta $ ex)] ++
        [Rule (childToMeta $ ex) (Meta n)] ++ (exprChildRules ex dIDs)
-- Do I need to bind n to something? (is it ever a link in the chain?) TODO
--
-- Not much to say about break statments. They don't really have a type and I
-- don't think that they can be used on the RHS of an assignment, nor can they
-- be returned or return anything (in the normal sense).
exprChildRules (LabBreak var, n) dIDs = maybeVarChildRules var dIDs
-- Do I need to bind n to something? (is it ever a link in the chain?) TODO
--
-- Not much to say about continue statments. They don't really have a type and I
-- don't think that they can be used on the RHS of an assignment, nor can they
-- be returned or return anything (in the normal sense).
exprChildRules (LabContinue var, n) dIDs = maybeVarChildRules var dIDs
-- Do I need to bind n to something? (is it ever a link in the chain?) TODO
--
-- Not much to say about throw statments. They don't really have a type and I
-- don't think that they can be used on the RHS of an assignment, nor can they
-- be returned or return anything (in the normal sense).
exprChildRules (LabThrow ex, n) dIDs = (exprChildRules ex dIDs)
-- A CallExpression is a reference to a property of an evaluation of a function
-- that returns an object.
exprChildRules (LabCallExpression ex1 (".", _) ex2, n) dIDs =
        -- The type of the statement is ReferenceType and the type of that
        -- reference is the type of the statement.
        [Rule (Meta n) (ReferenceType (childToMeta $ ex1) (getPropName $ ex2))] ++
        [Rule (ReferenceType (childToMeta $ ex1) (getPropName $ ex2)) (Meta n)] ++
        -- The object must containt the property being reference.
        [Rule (childToMeta $ ex1) (AtLeastObjectType [(getPropName $ ex2)])] ++
        (exprChildRules ex1 dIDs)
        where
        -- Assuming that property names appear here as identifiers. Not sure if
        -- correct. TODO
        getPropName (LabIdentifier (prop, q), r) = VariableProperty prop
-- A CallExpression is a reference to a property of an evaluation of a function
-- that returns an object.
exprChildRules (LabCallExpression ex1 ("[]", _) ex2, n) dIDs = (exprChildRules ex1 dIDs) ++
        -- If the index is an integer literal then the type of the statment is
        -- ReferenceType and the type of that reference is the type of they statement.
        (if (isIntLiteral $ ex2) then
        [Rule (Meta n) (ReferenceType (childToMeta $ ex1) (IndexProperty . getIntLiteral $ ex2))] ++
        [Rule (ReferenceType (childToMeta $ ex1) (IndexProperty . getIntLiteral $ ex2)) (Meta n)] ++
        -- The object must contain the property being referenced.
        [Rule (childToMeta $ ex1) (AtLeastObjectType [(IndexProperty . getIntLiteral $ ex2)])]
        -- If the index is a string literal then the type of the statment is
        -- ReferenceType and the type of that reference is the type of they statement.
        else if (isStringLiteral $ ex2) then
        [Rule (Meta n) (ReferenceType (childToMeta $ ex1) (VariableProperty . getStringLiteral $ ex2))] ++
        [Rule (ReferenceType (childToMeta $ ex1) (VariableProperty . getStringLiteral $ ex2)) (Meta n)] ++
        -- The object must contain the property being referenced.
        [Rule (childToMeta $ ex1) (AtLeastObjectType [(VariableProperty . getStringLiteral $ ex2)])]
        -- If the type of the index is not an int or string literal then we
        -- cannot know its value and thus type inference on the object fails.
        else ([Rule (Meta n) AmbiguousType] ++ [Rule (childToMeta ex1) AmbiguousType] ++ (exprChildRules ex2 dIDs)))
-- The type of the statment is functionType. A FunctionType includes an
-- ArgumentsIDType, a ReturnType and a ConstructorType.
exprChildRules (LabFunctionExpression mv vls body, n) dIDs =
        [Rule (Meta n) (FunctionType (ArgumentsIDType . Meta $ n) (ReturnType . childToMeta $ body) (ConstructorType . childToMeta $ body))] ++
        -- Make a rule for the types of the arguments.
        [Rule (ArgumentsIDType (Meta n)) (ArgumentsType (map (makeIDType . argMakeLabel) vls))] ++
        -- Make rules for the name of the function expression if it has one.
        (nameRule mv (funExprMakeLabel thisFunEx) n)
        where
        thisFunEx = (LabFunctionExpression mv vls body, n)
        nameRule Nothing _ _= []
        nameRule _ Nothing _ = []
        nameRule (Just (name, x)) (Just d) y = [Rule (Meta x) (makeIDType d)] ++ [Rule (makeIDType d) (Meta y)]
        makeIDType (DeclaredIdentifier id lab) = IdentifierType id lab
-- Variable declarations.
exprChildRules (LabVarDeclaration var mex, n) dIDs =
        -- The type of the statement equals the type of mex if mex is not Nothing.
        (maybeMetaRule n mex) ++
        -- The type of the variable is equal to the type of mex if mex is not Nothing.
        (maybeMetaRule (childGetLabel $ var) mex) ++ (varChildRules var dIDs) ++
        (maybeExprChildRules mex dIDs)
        where
        maybeMetaRule x Nothing = [Rule (Meta x) UndefType]
        maybeMetaRule x (Just exp) = [Rule (Meta x) (childToMeta $ exp)]
-- The type of a 'new' statment is they instantiation type of the function (constructor) that it calls.
exprChildRules (LabNew (LabCall ex1 ex2, p), n) dIDs = [Rule (Meta n) (InstantiationType . childToMeta $ ex1)] ++
        -- The types of the arguments that the constructor is called with must
        -- match the types of the arguments in its definition.
        [Rule (ArgumentsIDType . childToMeta $ ex1) (ArgumentsType . argsToMeta $ ex2)] ++
        (exprChildRules ex1 dIDs) ++ (exprChildRules ex2 dIDs)
        where
        argsToMeta (LabArguments args, n) = map childToMeta args

-- Remove the LabList wrapper on singleton lists and remove parentheses from
-- expressions.
-- This is used badly in some places. Fix. TODO
removeUselessParenAndList :: ExprChild -> ExprChild
removeUselessParenAndList (LabList [ex], _) = removeUselessParenAndList $ ex
removeUselessParenAndList (LabParenExpression ex, _) = removeUselessParenAndList ex
removeUselessParenAndList ex = ex

-- Return true if the expression is an integer literal.
isIntLiteral :: ExprChild -> Bool
-- This use of removeUselssParenAndList is fine.
isIntLiteral ex = isIntLiteral' . removeUselessParenAndList $ ex
    where
    isIntLiteral' (LabValue (LabInt i, _), _) = True
    isIntLiteral' _ = False

-- Extract the actual integer value from an integer literal expression.
getIntLiteral :: ExprChild -> Int
-- This use of removeUselssParenAndList is fine.
getIntLiteral ex = getIntLiteral' . removeUselessParenAndList $ ex
    where
    getIntLiteral' (LabValue (LabInt i, _), _) = i

-- Return true if the expression in a string literal.
isStringLiteral :: ExprChild -> Bool
-- This use of removeUselssParenAndList is fine.
isStringLiteral ex = isStringLiteral' . removeUselessParenAndList $ ex
    where
    isStringLiteral' (LabValue (LabString s, _), _) = True
    isStringLiteral' (LabValue (LabDQString s, _), _) = True
    isStringLiteral' _ = False

-- Extract the actual string value from a string literal expression.
getStringLiteral :: ExprChild -> String
-- This use of removeUselssParenAndList is fine.
getStringLiteral ex = getStringLiteral' . removeUselessParenAndList $ ex
    where
    getStringLiteral' (LabValue (LabString s, _), _) = s
    getStringLiteral' (LabValue (LabDQString s, _), _) = s

-- Returns true if the ASTChild is a Statement containing an <Assignment "=" ex1 ex2>. False otherwise.
--
-- Here I assume that calling removeUselessParenAndList is ok. But I should allow the
-- user to do {this.x = a, this.y = b;} which I think _might_ give us a LabList containing two simple
-- assignments. There are probably similar problems with the use of removeUselessParenAndList elsewhere.
-- Check for and fix this. TODO
isSimpleAssignment :: ASTChild -> Bool
-- If the AST is a statement and is a simple assignment return true, false otherwise.
isSimpleAssignment (LabStatement ex, _) = isSimpleAssignment' . removeUselessParenAndList $ ex
        where
        isSimpleAssignment' (LabAssignment ("=", _) _ _, _) = True
        isSimpleAssignment' _ = False
-- If the AST is not a statement return false.
isSimpleAssignment _ = False


-- Takes a Statement containing an <Assignment "=" ex1 ex2> and returns ex1.
assignmentGetVar :: ASTChild -> ExprChild
assignmentGetVar (LabStatement ex, _) = assignmentGetVar' . removeUselessParenAndList $ ex
        where
        assignmentGetVar' (LabAssignment _ var _, _) = var

-- Return true if the expression is a reference to a property (this.something or this[something]).
-- Return false otherwise.
--
-- Can you wrap 'this' in parens or something? Can you assign a variable to 'this' and then reference
-- 'this' via the variable?
-- e.g: function Foo(b) { var a = this; if (b) { a.x = 5; } }.
-- It is important that I pick up all references to properties of 'this'.
-- 
-- FOR THE MOMENT any reference to 'this' other than a reference to a property
-- on the left-hand side of a simple assignment expression at the top level of
-- scope inside a function body should cause type inference to fail on that
-- function, both as a constructor and as a function. TODO
isPropertyReference :: ExprChild -> Bool
isPropertyReference (LabReference (LabIdentifier ("this", _),_) _, _) = True
isPropertyReference (LabIndex (LabIdentifier ("this", _), _) _, _) = True
isPropertyReference _ = False

-- Return true if the AST represents a simple assignment expression on a
-- property of 'this', false otherwise.
isPropRefAssignment :: ASTChild -> Bool
isPropRefAssignment ast = (isAssig) && (isPropRef)
    where
    isAssig = isSimpleAssignment $ ast
    isPropRef = if (not isAssig) then False else (isPropertyReference . assignmentGetVar $ ast)

-- Takes an expression which is a reference to a property and returns the name
-- of the property paired with the type of its value.
getPropertyNameType :: ExprChild -> [(PropertyName, Type)]
getPropertyNameType (LabReference (LabIdentifier (_, _), _) (LabIdentifier (prop, _), _), n) =
        [(VariableProperty prop, Meta n)]
getPropertyNameType (LabIndex (LabIdentifier (obj, _), _) prop, n) =
        if (isIntLiteral $ prop) then [((IndexProperty . getIntLiteral $ prop), (Meta n))]
        else if (isStringLiteral $ prop) then [((VariableProperty . getStringLiteral $ prop), (Meta n))]
        else []

-- Takes an ASTChild. If the ASTChild is a Statement containing an
-- <Assignment "=" ex1 ex2> and ex1 is a property (i.e. this.something) return
-- [(VariableProperty "something", childToMeta ex1)].
-- Similar for this["something"] and this[3], this[5], etc.
-- If the variable being assigned isn't a property (isn't of the form
-- this.something) return [].
--
-- NOTE: To get useful rules about the types of the properties from the
-- information in the constructor body, the other (Haskell Land) functions need
-- to be able to deal with this.something. So when a JS program says
-- (this.something, n) = (2, m), all this function does is return (a
-- representation of) a property. Other functions need to make rules that say
-- things like <type(this.something) = Meta n>, <Meta n = Meta m>,
-- <Meta m = type(2)>. TODO (possibly already working?)
getAssignedProperty :: ASTChild -> [(PropertyName, Type)]
getAssignedProperty ast = if (isPropRefAssignment $ ast) then (getPropertyNameType . assignmentGetVar $ ast) else []

-- Check AST for references to properties (this.anything) which aren't in the
-- list. If found return true. Else return false. This may be uneccessary. What
-- if I only include properties declared in simple assignments? Then if I
-- reference any other property of an object created with the constructor, type
-- inference will fail *on that object*. But not on the constructor. Does that
-- matter? TODO
-- Note that this problem is similar for the problem of return statments inside
-- child blocks.
-- Currently never called. Put it in somewhere. TODO
hasUnassignedProps :: [ASTChild] -> [(PropertyName, Type)] -> Bool
-- TODO
hasUnassignedProps ast list = False

-- If a statement in the block is a Return, or anything that has a Block field
-- (except for a function declaration or a function expression) then we make a
-- rule matching the type of the whole block to the type of that statment.
blockRules :: [ASTChild] -> JSASTLabel -> [Rule]
blockRules block n = concat. map getBlockRule $ block
        where
        getBlockRule (LabFunctionDeclaration _ _ _, _) = []
        getBlockRule (LabStatement _, _) = []
        getBlockRule (_, l) = [Rule (Meta n) (Meta l)]

-- Finds the return type of a function body if it is type-safe. Makes a rule
-- relating the type of the block to UndefType is the return type is not type
-- safe.
--
-- We can only infer the return type of a function that (at least) has a return
-- statement in the top level of scope of its body. If all of its return
-- statements are inside inner ASTs then they might not get called. Having said
-- that, simple if-else routines, do-while loops, default routines and finally
-- routines always return if they contain a return statement. Do I want to
-- account for these? TODO
funBodyRules :: [ASTChild] -> JSASTLabel -> [Rule]
funBodyRules block n =
        -- If there is no return type at the top level of scope of the block then
        -- relate the type of the block to UndefType and process all child ASTs
        -- in the block. If a child AST other than a function or an expression
        -- (wrapped in a Statment) contains a return statment with type other
        -- than undefined, then type inference on the function should fail.
        if (not (hasReturnType $ block)) then ([Rule (ReturnType (Meta n)) (UndefType)] ++
        (concat . map getBlockRule $ block))
        -- If there is a return type at the top level of scope of the block then
        -- process all child ASTs in the block.
        else (concat . map getBlockRule $ block)
        where
        -- Inner functions can't return from 'this'. Return an empty list.
        getBlockRule (LabFunctionDeclaration _ _ _, _) = []
        -- Returns are ASTs and LabStatements can only contain ASTs in the form
        -- of function expression bodies, which can't return from 'this'. Return
        -- an empty list.
        getBlockRule (LabStatement _, _) = []
        -- For any other AST, the type of that AST (Meta l) equals the type of its body,
        -- although that rule is generated elsewhere. The type of 'this' equals
        -- the types of its child ASTs (excluding Statements and
        -- FunctionDeclarations.)
        getBlockRule (_, l) = [Rule (ReturnType (Meta n)) (Meta l)]
        -- Return true if the block contains a return statement at the top level
        -- of its scope.
        hasReturnType (b:bx) = ((isReturn $ b) || (hasReturnType bx))
        hasReturnType [] = False
        -- Return true is the AST is a return statement.
        isReturn (LabReturn _, _) = True
        isReturn _ = False

-- Make a rule for the constructor type of a function body - the type of the
-- function's result when it is instantiated.
--
-- Make a list of all properties declared in simple assignment statements (after
-- removing useless parens etc.).
-- E.g. Say we find this.a = x, this.b = y; We get [(this.a, n_a), (this.b, n_b)].
--
-- In future, this should check for references to properties (this.anything)
-- that aren't declared in the above. If such a reference is found, make a rule
-- [Rule (Meta n) (ConstructorType AmbiguosType)]. If no such reference is found
-- return a rule e.g.
-- [
-- Rule
--     (Meta n)
--     (ConstructorType
--         (
--         ObjectType 
--             [
--             (VariableProperty "a", Meta n_a),
--             (VariableProperty "b", Meta n_b)
--             ]
--         )
--     )
-- ]
constructorRules :: [ASTChild] -> JSASTLabel -> [Rule]
constructorRules block n = [Rule (ConstructorType (Meta n)) (ObjectType (concat . map getAssignedProperty $ block))]

-- Make a rule for ASTs that have the same type as their body (a block)
bodyRule :: ASTChild -> JSASTLabel -> Rule
bodyRule body n = Rule (Meta n) (childToMeta $ body)

-- Make a rule for expressions that have Boolean type
boolRule :: ExprChild -> Rule
boolRule (LabList ex, n) = Rule (childToMeta . last $ ex) BoolType
boolRule ex = Rule (childToMeta $ ex) BoolType

-- Make a list of rules from a Maybe ExprChild of Boolean type.
maybeBoolRule :: (Maybe ExprChild) -> [Rule]
maybeBoolRule (Just t) = [boolRule $ t]
maybeBoolRule Nothing = []

-- Generate rules from an AST
astChildRules :: ASTChild -> [DeclaredIdentifier] -> [Rule]
-- The type of the block is equal to the type of anything it returns (blockRules).
astChildRules (LabBlock astList, n) dIDs = (mapASTChildRules astList dIDs) ++ (blockRules astList n)
-- The evaluation type of a function body is the same as the type of anything it
-- returns. It also has a construtor type - the type of the object that the
-- function makes when is instantiated.
astChildRules (LabFunctionBody astList, n) dIDs = (mapASTChildRules astList dIDs) ++ (funBodyRules astList n) ++
        (constructorRules astList n)
-- Function declarations.
astChildRules (LabFunctionDeclaration var args body, n) dIDs = 
        -- The type of the function's identifier is they same as they type of the declaration statement.
        [Rule thisFunID (Meta n)] ++
        -- The type of the declaration statement is FunctionType. A FunctionType
        -- is a tuple comprising an ArgumentsIDType, a ReturnType and a
        -- ConstructorType.
        [Rule (Meta n) (FunctionType (ArgumentsIDType thisFunID) (ReturnType . childToMeta $ body)
            (ConstructorType . childToMeta $ body))] ++
        -- The type of the arguments is a list of identifiers.
        [Rule (ArgumentsIDType thisFunID) (ArgumentsType (map (makeIDType . argMakeLabel) args))] ++
        (varChildRules var dIDs)
        where
        -- The type of an argument in a function declaration is IdentifierType.
        makeIDType (DeclaredIdentifier id lab) = IdentifierType id lab
        -- The type of this function's identifier.
        thisFunID = IdentifierType (childGetValue $ var) (idGetLabel (childGetValue $ var) dIDs)
-- The type of a labelled block is the type of its body.
astChildRules (LabLabelled label body, n) dIDs = [bodyRule body n] ++ (varChildRules label dIDs) ++
        (astChildRules body dIDs)
-- They type of a for loop is the type of its body. The test has boolean type.
astChildRules (LabForVar varEx test count body, n) dIDs = [bodyRule body n] ++ (maybeBoolRule $ test) ++
        (mapExprChildRules varEx dIDs) ++ (maybeExprChildRules test dIDs) ++ (maybeExprChildRules count dIDs) ++
        (astChildRules body dIDs)
-- They type of a for loop is the type of its body. The test has boolean type.
astChildRules (LabFor varEx test count body, n) dIDs = [bodyRule body n] ++ (maybeBoolRule $ test) ++
        (maybeExprChildRules varEx dIDs) ++ (maybeExprChildRules test dIDs) ++ (maybeExprChildRules count dIDs) ++
        (astChildRules body dIDs)
-- More to do here. TODO
-- They type of a for loop is the type of its body.
astChildRules (LabForIn varList obj body, n) dIDs = [bodyRule body n] ++
        (mapVarChildRules varList dIDs) ++ (exprChildRules obj dIDs) ++ (astChildRules body dIDs)
-- More to do here. TODO
-- They type of a for loop is the type of its body.
astChildRules (LabForVarIn varEx obj body, n) dIDs = [bodyRule body n] ++
        (exprChildRules varEx dIDs) ++ (exprChildRules obj dIDs) ++ (astChildRules body dIDs)
-- The type of a while loop is the type of its body. The type of the test is
-- boolean.
astChildRules (LabWhile test body, n) dIDs = [bodyRule body n] ++ [boolRule $ test] ++ (exprChildRules test dIDs) ++
        (astChildRules body dIDs)
-- The type of a do-while loop is the type of its body. They type of the test is
-- bool.
astChildRules (LabDoWhile body test, n) dIDs = [bodyRule body n] ++ [boolRule $ test] ++ (exprChildRules test dIDs) ++
        (astChildRules body dIDs)
-- The type of an if construct is they type of its body. The type of the test
-- is boolean.
astChildRules (LabIf test body, n) dIDs = [bodyRule body n] ++ [boolRule $ test] ++ (exprChildRules test dIDs) ++
        (astChildRules body dIDs)
-- The type of an if-else construct is they type of both of its body blocks
-- (they must have the same type for type inference on the construct to succeed).
-- They type of the test is boolean.
astChildRules (LabIfElse test bodyT bodyF, n) dIDs = [bodyRule bodyT n] ++ [bodyRule bodyF n] ++ [boolRule $ test] ++
        (exprChildRules test dIDs) ++ (astChildRules bodyT dIDs) ++ (astChildRules bodyF dIDs)
-- The type of a switch statment is the type of all of its cases (which in turn
-- have the type of their bodies). They type of all the cases must me the same
-- for type inference on the construct to succeed.
astChildRules (LabSwitch id cases, n) dIDs = [bodyRule cases n] ++ (exprChildRules id dIDs) ++
        (astChildRules cases dIDs)
-- The type of a case in a switch statement is the same as the type of its body.
--
-- Note also that ex has the same type as the variable in the outer switch
-- statement. You should make rules for that. TODO
astChildRules (LabCase ex body, n) dIDs = [bodyRule body n] ++ (exprChildRules ex dIDs) ++ (astChildRules body dIDs)
-- The type of a default statment is the same as the type of its body.
astChildRules (LabDefault body, n) dIDs = [bodyRule body n] ++ (astChildRules body dIDs)
-- The type of a try construct is the same as the type of its body and the type
-- of its catch block. They must have the same type for type inference on the
-- construct to succeed.
astChildRules (LabTry body catches, n) dIDs = [bodyRule body n] ++ [bodyRule catches n] ++ (astChildRules body dIDs) ++
        (astChildRules catches dIDs)
-- The type of a catch statment is the same as the type of its body. The type of
-- the test (usually used to conditionally catch an exception, e.g. only catch
-- an exception if it is equal to 3) is boolean.
astChildRules (LabCatch var mTest body, n) dIDs = [bodyRule body n] ++ (maybeBoolRule $ mTest) ++
        (varChildRules var dIDs) ++ (maybeExprChildRules mTest dIDs) ++ (astChildRules body dIDs)
-- The type of a finally statement is the same as the type of its body.
astChildRules (LabFinally body, n) dIDs = [bodyRule body n] ++ (astChildRules body dIDs)
-- They type of a return statement is they same as the type of the expression it
-- returns.
astChildRules (LabReturn ex, n) dIDs = [Rule (Meta n) (childToMeta $ ex)] ++ (exprChildRules ex dIDs)
-- They type of an instance of the Statment data type is the same as the type of
-- the expression it contains.
astChildRules (LabStatement ex, n) dIDs = [Rule (Meta n) (childToMeta $ ex)] ++ (exprChildRules ex dIDs)