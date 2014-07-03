
-- Module prints human-legible output from the various layers of the pipeline
-- TODO: Comment this file.

--module PrettyPrint
module Main
(
main
) where


import DeclarationGraph
import LabelJSAST
import ParseJS
import System.Environment
import TypeRules

type LabFlag = Bool
type LineFlag = Bool

main :: IO ()
main = do
    (infile:[]) <- getArgs
    pr <- readFile infile
    putStrLn ""

    -- Prints declared functions and function expressions, and the identifiers
    -- that are visible to each one.
    -- putStr "Top Level:"
    -- printCleanedElementList ((makeCleanedFunctions pr):[]) (makeIndent "")

    -- Prints the rules, indented base on their scope, plus an optional list
    -- of the identifiers that are visible at that each scope.
    -- putStr "Top Level:"
    -- printCleanedRulesList
    --     ((makeCleanedFunctionRules pr):[]) (makeIndent "") False

    -- TODO: Rule type needs pretty printing
    -- mapM_ print (makeAllRules pr)

    -- Raw output with a lot of details.
    -- putStrLn . show . makeDeclarationGraph $ pr

    -- Prints the cleaned ATS, indented, with optional labels.
    --
    -- TODO: Experiment with keeping JS code snippets in the tree and printing
    -- them here.
    -- mapPrintASTChild (makeLabelledJSAST pr) (makeIndent "") False

    -- Prints the original AST without labels.
    --
    -- TODO: Add pretty printing for this.
    -- mapM_ print (makeJSAST pr)
    putStrLn ""


makeCleanedFunctions :: String -> CleanedFunction
makeCleanedFunctions input =
    cleanFunction . makeCleanedFunctionRules $ input

makeCleanedFunctionRules :: String -> CleanedFunctionRules
makeCleanedFunctionRules input =
    cleanFunctionRules . makeDeclarationGraph $ input

makeAllRules :: String -> [Rule]
makeAllRules input = graphGetAllRules . makeDeclarationGraph $ input

makeDeclarationGraph :: String -> FunctionRules
makeDeclarationGraph input = getDeclarationGraph . makeLabelledJSAST $ input

makeLabelledJSAST :: String -> [ASTChild]
makeLabelledJSAST input = label . makeJSAST $ input

makeJSAST :: String -> [JSAST]
makeJSAST input = toJSAST . parseTree $ input

makeIndent :: String -> String
makeIndent s = s ++ "..."


printStrAndLabel :: String -> JSASTLabel -> LabFlag -> IO()
printStrAndLabel str lab False = putStr str
printStrAndLabel str lab True = putStr (str ++ " <" ++ (show lab) ++ ">")


printLnStrAndLabel :: String -> JSASTLabel -> LabFlag -> IO()
printLnStrAndLabel str lab printLab = do
    printStrAndLabel str lab printLab
    putStrLn ""


-- Prints CleanedRules, indented according to their depth in the tree
printCleanedRulesList :: CleanedRules a => [a] -> String -> Bool -> IO()
printCleanedRulesList (head:fx) padding printIdentifiers = do
    putStrLn (" " ++ (show fid))
    if printIdentifiers == True then do
        putStrLn (padding ++ " IDENTIFIERS:")
        mapM_ (putStrLn . ((padding ++ " ") ++) . show) $ dIDs
    else
        return()
    putStrLn (padding ++ " RULES:")
    mapM_ (putStrLn . ((padding ++ " ") ++) . show) $ rules
    let newPadding = makeIndent padding
    printList fRules newPadding
    printList feRules newPadding
    printList fx padding
    where
        fid = crFunctionIdentifier head
        rules = crRuleList head
        fRules = crCleanedFunctionRuleList head
        feRules = crCleanedFunctionExpressionRuleList head
        dIDs = crDeclaredIdentifierList head
        printHeading _ [] = return()
        printHeading p (f:fx) = do
            putStrLn ""
            putStr (p ++ " " ++ (crName $ f) ++ ":")
        printList l p = do
            printHeading p l
            printCleanedRulesList l p printIdentifiers
printCleanedRulesList [] _ _ = return()


-- Prints CleanedElements, indented according to their depth in the tree
printCleanedElementList :: (CleanedElement a, Show a) => [a] -> String -> IO()
printCleanedElementList (head:fx) padding = do
    putStrLn (" " ++ (show fid))
    mapM_ (putStrLn . ((padding ++ " ") ++) . show) $ dIDs
    let newPadding = makeIndent padding
    printList fList newPadding
    printList feList newPadding
    printList fx padding
    where
        fid = ceFunctionIdentifier head
        fList = ceCleanedFunctionList head
        feList = ceCleanedFunctionExpressionList head
        dIDs = ceDeclaredIdentifierList head
        printHeading _ [] = return()
        printHeading p (f:fx) = do
            putStrLn ""
            putStr (p ++ " " ++ (ceName $ f) ++ ":")
        printList l p = do
            printHeading p l
            printCleanedElementList l p
printCleanedElementList [] _ = return()


mapPrintASTChild :: [ASTChild] -> String -> LabFlag -> IO()
mapPrintASTChild children padding printLab =
    mapM_ printChild $ children
    where
        printChild c = printASTChild c padding printLab


printASTChild :: ASTChild -> String -> LabFlag -> IO()
printASTChild ((LabBlock children), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabBlock") lab printLab
    mapPrintASTChild children (makeIndent padding) printLab
printASTChild ((LabFunctionBody children), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabFunctionBody") lab printLab
    mapPrintASTChild children (makeIndent padding) printLab
printASTChild ((LabFunctionDeclaration vChild args child), lab)
    padding printLab = do
        printStrAndLabel (padding ++ " LabFunctionDeclaration") lab printLab
        printVarChild vChild "" printLab False
        putStr " ["
        mapPrintVarChild args "" printLab False
        putStrLn " ]"
        printASTChild child (makeIndent padding) printLab
printASTChild ((LabStatement expr), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabStatement") lab printLab
    printExprChild expr (makeIndent padding) printLab
printASTChild ((LabReturn expr), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabReturn") lab printLab
    printExprChild expr (makeIndent padding) printLab
printASTChild ((LabForVar decs cond expr child), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabForVar") lab printLab
    let p = makeIndent padding
    mapPrintExprChild decs p printLab
    maybePrintExprChild cond p printLab
    maybePrintExprChild expr p printLab
    printASTChild child p printLab
printASTChild ((LabForVarIn var obj child), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabForVarIn") lab printLab
    let p = makeIndent padding
    printExprChild var p printLab
    printExprChild obj p printLab
    printASTChild child p printLab
printASTChild (n, lab) padding printLab = do
    printLnStrAndLabel (padding ++ " OTHER ASTCHILD") lab printLab
    putStrLn ((makeIndent padding) ++ " " ++ (show $ n))


mapPrintExprChild :: [ExprChild] -> String -> LabFlag -> IO()
mapPrintExprChild children padding printLab =
    mapM_ printChild $ children
    where
        printChild c = printExprChild c padding printLab


printExprChild :: ExprChild -> String -> LabFlag -> IO()
printExprChild ((LabList exprs), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabList") lab printLab
    mapPrintExprChild exprs (makeIndent padding) printLab
printExprChild ((LabAssignment op expr1 expr2), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabAssignment") lab printLab
    let p = makeIndent padding
    printOpChild op p printLab True
    printExprChild expr1 p printLab
    printExprChild expr2 p printLab
printExprChild ((LabBinary op expr1 expr2), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabBinary") lab printLab
    let p = makeIndent padding
    printOpChild op p printLab True
    printExprChild expr1 p printLab
    printExprChild expr2 p printLab
printExprChild ((LabUnaryPost op expr), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabUnaryPost") lab printLab
    let p = makeIndent padding
    printOpChild op p printLab True
    printExprChild expr p printLab
printExprChild ((LabIdentifier var), lab) padding printLab = do
    printStrAndLabel (padding ++ " LabIdentifier") lab printLab
    printVarChild var "" printLab True
printExprChild ((LabReference obj prop), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabReference") lab printLab
    let p = makeIndent padding
    printExprChild obj p printLab
    printExprChild prop p printLab
printExprChild ((LabIndex obj prop), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabIndex") lab printLab
    let p = makeIndent padding
    printExprChild obj p printLab
    printExprChild prop p printLab
printExprChild ((LabValue val), lab) padding printLab = do
    printStrAndLabel (padding ++ " LabValue") lab printLab
    printValueChild val (makeIndent padding) printLab True
printExprChild ((LabVarDeclaration var expr), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabVarDeclaration") lab printLab
    let p = makeIndent padding
    printVarChild var p printLab True
    maybePrintExprChild expr p printLab
printExprChild ((LabPropNameValue prop expr), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabPropNameValue") lab printLab
    let p = makeIndent padding
    printPropertyNameChild prop p printLab
    printExprChild expr p printLab
printExprChild ((LabFunctionExpression vChild args child), lab)
    padding printLab = do
        printStrAndLabel (padding ++ " LabFunctionExpression") lab printLab
        maybePrintVarChild vChild "" printLab False
        putStr " ["
        mapPrintVarChild args "" printLab False
        putStrLn " ]"
        printASTChild child (makeIndent padding) printLab
printExprChild ((LabCall fid args), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabCall") lab printLab
    let p = makeIndent padding
    printExprChild fid p printLab
    printExprChild args p printLab
printExprChild ((LabArguments exprs), lab) padding printLab = do
    printLnStrAndLabel (padding ++ " LabArguments") lab printLab
    mapPrintExprChild exprs (makeIndent padding) printLab
printExprChild (n, lab) padding printLab = do
    printLnStrAndLabel (padding ++ " OTHER EXPRCHILD") lab printLab
    putStrLn ((makeIndent padding) ++ " " ++ (show $ n))


-- FIXME: Print the "Just". See maybePrintVarChild
maybePrintExprChild :: Maybe ExprChild -> String -> LabFlag -> IO()
maybePrintExprChild (Just expr) padding printLab =
        printExprChild expr padding printLab
-- FIXME: Nothings have no label. Is that a problem?
maybePrintExprChild Nothing padding _ = putStrLn (padding ++ " Nothing")


mapPrintVarChild :: [VarChild] -> String -> LabFlag -> LineFlag -> IO()
mapPrintVarChild children padding printLab printLine =
    mapM_ printChild $ children
    where
        printChild c = printVarChild c padding printLab printLine


printVarChild :: VarChild -> String -> LabFlag -> LineFlag -> IO()
printVarChild (var, lab) padding printLab True =
    printLnStrAndLabel (padding ++ " \"" ++ var ++ "\"") lab printLab
printVarChild (var, lab) padding printLab False =
    printStrAndLabel (padding ++ " \"" ++ var ++ "\"") lab printLab


maybePrintVarChild :: Maybe VarChild -> String -> LabFlag -> LineFlag -> IO()
maybePrintVarChild (Just var) padding printLab False = do
    putStr (padding ++ " Just")
    printVarChild var "" printLab False
maybePrintVarChild (Just var) padding printLab True = do
    putStrLn (padding ++ " Just")
    printVarChild var (makeIndent padding) printLab True
-- FIXME: "Nothing" doesn't have a label
maybePrintVarChild Nothing padding printLab False =
    putStr (padding ++ " \"Nothing\"")
maybePrintVarChild Nothing padding printLab True =
    putStrLn (padding ++ " \"Nothing\"")


printIndexChild :: IndexChild -> String -> LabFlag -> LineFlag -> IO()
printIndexChild (index, lab) padding printLab True =
    printLnStrAndLabel (padding ++ " \"" ++ (show index) ++ "\"") lab printLab
printIndexChild (index, lab) padding printLab False =
    printStrAndLabel (padding ++ " \"" ++ (show index) ++ "\"") lab printLab


printValueChild :: ValueChild -> String -> LabFlag -> LineFlag -> IO()
printValueChild (val, lab) padding printLab printLine =
    printLabelledValue val padding printLab printLine


printOpChild :: OpChild -> String -> LabFlag -> LineFlag -> IO()
printOpChild (op, lab) padding printLab True =
    printLnStrAndLabel (padding ++ " Operator \"" ++ op ++ "\"") lab printLab
printOpChild (op, lab) padding printLab False =
    printStrAndLabel (padding ++ " Operator \"" ++ op ++ "\"") lab printLab


printPropertyNameChild :: PropertyNameChild -> String -> LabFlag -> IO()
printPropertyNameChild ((LabVariableProperty var), lab)
    padding printLab = do
        printStrAndLabel (padding ++ " Property") lab printLab
        printVarChild var "" printLab True
printPropertyNameChild ((LabIndexProperty index), lab)
    padding printLab = do
        printStrAndLabel (padding ++ " Property") lab printLab
        printIndexChild index "" printLab True


printLabelledValue :: LabelledValue -> String -> LabFlag -> LineFlag -> IO()
printLabelledValue (LabObject exprs) padding printLab _ = do
    putStrLn ""
    putStrLn (padding ++ " LabObject")
    mapPrintExprChild exprs (makeIndent padding) printLab
printLabelledValue (LabInt val) padding _ False =
    putStr (" LabInt " ++ (show val))
printLabelledValue (LabFloat val) padding _ False =
    putStr (" LabFloat " ++ (show val))
printLabelledValue (LabDQString val) padding _ False =
    putStr (" LabDQString " ++ (show val))
printLabelledValue (LabArray elems) padding printLab False = do
    putStrLn ""
    putStrLn (padding ++ " LabArray")
    let p = makeIndent padding
    putStrLn (p ++ " [")
    mapPrintExprChild elems p printLab
    putStr (p ++ " ]")
printLabelledValue labVal padding printLab True = do
    printLabelledValue labVal padding printLab False
    putStrLn ""
printLabelledValue n padding _ _ = do
    putStrLn ""
    putStrLn (padding ++ " OTHER LABELLED VALUE")
    putStrLn ((makeIndent padding) ++ " " ++ (show $ n))

