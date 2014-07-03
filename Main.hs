---------------------------------------------------------------------------------
-- Module prints human-legible output from the various layers of the pipeline

--module PrettyPrint
module Main
( main
) where

---------------------------------------------------------------------------------

import           DeclarationGraph
import           LabelJSAST
import           ParseJS
import           System.Environment
import           TypeRules

type PrintLabelFlag = Bool
type PrintLineFlag = Bool

main :: IO ()
main = do
    (infile:[]) <- getArgs
    pr <- readFile infile
--    mapM_ print . toJSAST . parseTree $ pr
--    putStrLn . show . getDeclarationGraph . label . toJSAST . parseTree $ pr
    putStrLn ""
    mapM_ ((\p c -> printASTChild c p False) (makeIndent "")) $ (label . toJSAST . parseTree $ pr)
--    putStrLn ""
--    printCleanedRulesList ((cleanFunctionRules . getDeclarationGraph . label . toJSAST . parseTree $ pr):[]) $ (makeIndent "")
--    putStrLn ""
--    printCleanedElementList ((cleanFunction . cleanFunctionRules . getDeclarationGraph . label . toJSAST . parseTree $ pr):[]) $ (makeIndent "")
--    putStrLn ""
--    mapM_ print . graphGetAllRules . getDeclarationGraph . label . toJSAST . parseTree $ pr
    putStrLn ""


makeIndent :: String -> String
makeIndent s = s ++ "..."


-- Prints CleanedRules, indented according to their depth in the tree
printCleanedRulesList :: CleanedRules a => [a] -> String -> IO()
printCleanedRulesList (head:fx) padding = do
    putStrLn (padding ++ " " ++ (show fid))
    putStrLn (padding ++ " IDENTIFIERS:")
    mapM_ (putStrLn . ((padding ++ " ") ++) . show) $ dIDs
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
                putStrLn (p ++ " " ++ (crName $ f))
            printList l p = do
                printHeading p l
                printCleanedRulesList l p
printCleanedRulesList [] _ = do
    return()


-- Prints CleanedElements, indented according to their depth in the tree
printCleanedElementList :: (CleanedElement a, Show a) => [a] -> String -> IO()
printCleanedElementList (head:fx) padding = do
    putStrLn . ((padding ++ " ") ++) . show $ fid
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
                putStrLn (p ++ " " ++ (ceName $ f))
            printList l p = do
                printHeading p l
                printCleanedElementList l p
printCleanedElementList [] _ = do
    return()


printStringAndLabel :: String -> JSASTLabel -> PrintLabelFlag -> PrintLineFlag -> IO()
printStringAndLabel str label False False =
      putStr str
printStringAndLabel str label True False =
      putStr (str ++ " <" ++ (show label) ++ ">")
printStringAndLabel str label printLabel True = do
      printStringAndLabel str label printLabel False
      putStrLn ""

printASTChild :: ASTChild -> String -> PrintLabelFlag -> IO()
printASTChild ((LabBlock children), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabBlock") label printLabel True
      mapM_ ((\p c -> printASTChild c p printLabel) (makeIndent padding)) $ children
printASTChild ((LabFunctionBody children), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabFunctionBody") label printLabel True
      mapM_ ((\p c -> printASTChild c p printLabel) (makeIndent padding)) $ children
printASTChild ((LabFunctionDeclaration vChild args child), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabFunctionDeclaration") label printLabel False
      printVarChild vChild "" printLabel False
      putStr " ["
      mapM_ (\var -> printVarChild var "" printLabel False) $ args
      putStrLn " ]"
      printASTChild child (makeIndent padding) printLabel
printASTChild ((LabStatement expr), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabStatement") label printLabel True
      printExprChild expr (makeIndent padding) printLabel
printASTChild ((LabReturn expr), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabReturn") label printLabel True
      printExprChild expr (makeIndent padding) printLabel
printASTChild ((LabForVar decs cond expr child), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabForVar") label printLabel True
      mapM_ ((\p e -> printExprChild e p printLabel) (makeIndent padding)) $ decs
      maybePrintExprChild cond (makeIndent padding) printLabel
      maybePrintExprChild expr (makeIndent padding) printLabel
      printASTChild child (makeIndent padding) printLabel
printASTChild ((LabForVarIn var obj child), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabForVarIn") label printLabel True
      printExprChild var (makeIndent padding) printLabel
      printExprChild obj (makeIndent padding) printLabel
      printASTChild child (makeIndent padding) printLabel
printASTChild (n, label) padding printLabel = do
      printStringAndLabel (padding ++ " OTHER ASTCHILD") label printLabel True
      putStrLn ((makeIndent padding) ++ " " ++ (show $ n))


printExprChild :: ExprChild -> String -> PrintLabelFlag -> IO()
printExprChild ((LabList exprs), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabList") label printLabel True
      mapM_ ((\p e -> printExprChild e p printLabel) (makeIndent padding)) $ exprs
printExprChild ((LabAssignment op expr1 expr2), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabAssignment") label printLabel True
      printOpChild op (makeIndent padding) printLabel True
      printExprChild expr1 (makeIndent padding) printLabel
      printExprChild expr2 (makeIndent padding) printLabel
printExprChild ((LabBinary op expr1 expr2), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabBinary") label printLabel True
      printOpChild op (makeIndent padding) printLabel True
      printExprChild expr1 (makeIndent padding) printLabel
      printExprChild expr2 (makeIndent padding) printLabel
printExprChild ((LabUnaryPost op expr), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabUnaryPost") label printLabel True
      printOpChild op (makeIndent padding) printLabel True
      printExprChild expr (makeIndent padding) printLabel
printExprChild ((LabIdentifier var), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabIdentifier") label printLabel False
      printVarChild var "" printLabel True
printExprChild ((LabReference obj prop), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabReference") label printLabel True
      printExprChild obj (makeIndent padding) printLabel
      printExprChild prop (makeIndent padding) printLabel
printExprChild ((LabIndex obj prop), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabIndex") label printLabel True
      printExprChild obj (makeIndent padding) printLabel
      printExprChild prop (makeIndent padding) printLabel
printExprChild ((LabValue val), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabValue") label printLabel False
      printValueChild val (makeIndent padding) printLabel True
printExprChild ((LabVarDeclaration var expr), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabVarDeclaration") label printLabel True
      printVarChild var (makeIndent padding) printLabel True
      maybePrintExprChild expr (makeIndent padding) printLabel
printExprChild ((LabPropNameValue prop expr), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabPropNameValue") label printLabel True
      printPropertyNameChild prop (makeIndent padding) printLabel
      printExprChild expr (makeIndent padding) printLabel
printExprChild ((LabFunctionExpression vChild args child), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabFunctionExpression") label printLabel False
      maybePrintVarChild vChild "" printLabel False
      putStr " ["
      mapM_ (\var -> printVarChild var "" printLabel False) $ args
      putStrLn " ]"
      printASTChild child (makeIndent padding) printLabel
printExprChild ((LabCall fid args), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabCall") label printLabel True
      printExprChild fid (makeIndent padding) printLabel
      printExprChild args (makeIndent padding) printLabel
printExprChild ((LabArguments exprs), label) padding printLabel = do
      printStringAndLabel (padding ++ " LabArguments") label printLabel True
      mapM_ ((\p e -> printExprChild e p printLabel) (makeIndent padding)) $ exprs
printExprChild (n, label) padding printLabel = do
      printStringAndLabel (padding ++ " OTHER EXPRCHILD") label printLabel True
      putStrLn ((makeIndent padding) ++ " " ++ (show $ n))


-- FIXME: Print the "Just". See maybePrintVarChild
maybePrintExprChild :: Maybe ExprChild -> String -> PrintLabelFlag -> IO()
maybePrintExprChild (Just expr) padding printLabel = printExprChild expr padding printLabel
maybePrintExprChild Nothing padding _ = putStrLn (padding ++ " Nothing") -- FIXME: Does this have a label??


printVarChild :: VarChild -> String -> PrintLabelFlag -> PrintLineFlag -> IO()
printVarChild (var, label) padding printLabel printLine = printStringAndLabel (padding ++ " \"" ++ var ++ "\"") label printLabel printLine


maybePrintVarChild :: Maybe VarChild -> String -> PrintLabelFlag -> PrintLineFlag -> IO()
maybePrintVarChild (Just var) padding printLabel False = do
      putStr (padding ++ " Just")
      printVarChild var "" printLabel False
maybePrintVarChild (Just var) padding printLabel True = do
      putStrLn (padding ++ " Just")
      printVarChild var (makeIndent padding) printLabel True
-- FIXME: Does "Nothing" have a label
maybePrintVarChild Nothing padding printLabel False = putStr (padding ++ " \"Nothing\"")
maybePrintVarChild Nothing padding printLabel True = putStrLn (padding ++ " \"Nothing\"")


printIndexChild :: IndexChild -> String -> PrintLabelFlag -> PrintLineFlag -> IO()
printIndexChild (index, label) padding printLabel printLine =
      printStringAndLabel (padding ++ " \"" ++ (show index) ++ "\"") label printLabel printLine


printValueChild :: ValueChild -> String -> PrintLabelFlag -> PrintLineFlag -> IO()
printValueChild (val, label) padding printLabel printLine = printLabelledValue val padding printLabel printLine


printOpChild :: OpChild -> String -> PrintLabelFlag -> PrintLineFlag -> IO()
printOpChild (op, label) padding printLabel printLine =
      printStringAndLabel (padding ++ " Operator \"" ++ op ++ "\"") label printLabel printLine


printPropertyNameChild :: PropertyNameChild -> String -> PrintLabelFlag -> IO()
printPropertyNameChild ((LabVariableProperty var), label) padding printLabel = do
      printStringAndLabel (padding ++ " Property") label printLabel False
      printVarChild var "" printLabel True
printPropertyNameChild ((LabIndexProperty index), label) padding printLabel = do
      printStringAndLabel (padding ++ " Property") label printLabel False
      printIndexChild index "" printLabel True


printLabelledValue :: LabelledValue -> String -> PrintLabelFlag -> PrintLineFlag -> IO()
printLabelledValue (LabObject exprs) padding printLabel _ = do
      putStrLn ""
      putStrLn (padding ++ " LabObject")
      mapM_ ((\p e -> printExprChild e p printLabel) (makeIndent padding)) $ exprs
printLabelledValue (LabInt val) padding _ False = do
      putStr (" LabInt " ++ (show val))
printLabelledValue (LabFloat val) padding _ False = do
      putStr (" LabFloat " ++ (show val))
printLabelledValue (LabDQString val) padding _ False = do
      putStr (" LabDQString " ++ (show val))
printLabelledValue (LabArray elems) padding printLabel False = do
      putStrLn ""
      putStrLn (padding ++ " LabArray")
      putStrLn ((makeIndent padding) ++ " [")
      mapM_ ((\p e -> printExprChild e p printLabel) (makeIndent padding)) $ elems
      putStr ((makeIndent padding) ++ " ]")
printLabelledValue labVal padding printLabel True = do
      printLabelledValue labVal padding printLabel False
      putStrLn ""
printLabelledValue n padding _ _ = do
      putStrLn ""
      putStrLn (padding ++ " OTHER LABELLEDVALUE")
      putStrLn ((makeIndent padding) ++ " " ++ (show $ n))

