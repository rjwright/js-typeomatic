---------------------------------------------------------------------------------
-- Module prints human-legible output from the various layers of the pipeline

--module PrettyPrint
module Main
( main
) where

---------------------------------------------------------------------------------

import ParseJS
import LabelJSAST
import TypeRules
import DeclarationGraph
import System.Environment

type PrintLabelFlag = Bool

main :: IO ()
main = do
    (infile:[]) <- getArgs
    pr <- readFile infile
--    mapM_ print . toJSAST . parseTree $ pr
--    putStrLn . show . getDeclarationGraph . label . toJSAST . parseTree $ pr
    putStrLn ""
    mapM_ ((\p c -> printASTChild c p) (makeIndent "")) $ (label . toJSAST . parseTree $ pr)
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


printASTChild :: ASTChild -> String -> IO()
printASTChild ((LabBlock children), _) padding = do
      putStrLn (padding ++ " LabBlock")
      mapM_ ((\p c -> printASTChild c p) (makeIndent padding)) $ children 
printASTChild ((LabFunctionBody children), _) padding = do
      putStrLn (padding ++ " LabFunctionBody")
      mapM_ ((\p c -> printASTChild c p) (makeIndent padding)) $ children 
printASTChild ((LabFunctionDeclaration vChild args child), _) padding = do
      putStr (padding ++ " LabFunctionDeclaration")
      printVarChild vChild "" False
      putStr " ["
      mapM_ (\var -> printVarChild var "" False) $ args
      putStrLn " ]"
      printASTChild child (makeIndent padding)
printASTChild ((LabStatement expr), _) padding = do
      putStrLn (padding ++ " LabStatement")
      printExprChild expr (makeIndent padding)
printASTChild ((LabReturn expr), _) padding = do
      putStrLn (padding ++ " LabReturn")
      printExprChild expr (makeIndent padding)
printASTChild ((LabForVar decs cond expr child), _) padding = do
      putStrLn (padding ++ " LabForVar")
      mapM_ ((\p e -> printExprChild e p) (makeIndent padding)) $ decs
      maybePrintExprChild cond (makeIndent padding)
      maybePrintExprChild expr (makeIndent padding)
      printASTChild child (makeIndent padding)
printASTChild ((LabForVarIn var obj child), _) padding = do
      putStrLn (padding ++ " LabForVarIn")
      printExprChild var (makeIndent padding)
      printExprChild obj (makeIndent padding)
      printASTChild child (makeIndent padding)
printASTChild (n, _) padding = do
      putStrLn (padding ++ " OTHER ASTCHILD")
      putStrLn ((makeIndent padding) ++ " " ++ (show $ n))


printExprChild :: ExprChild -> String -> IO()
printExprChild ((LabList exprs), _) padding = do
      putStrLn (padding ++ " LabList")
      mapM_ ((\p e -> printExprChild e p  ) (makeIndent padding)) $ exprs
printExprChild ((LabAssignment op expr1 expr2), _) padding = do
      putStrLn (padding ++ " LabAssignment")
      printOpChild op (makeIndent padding) True
      printExprChild expr1 (makeIndent padding)
      printExprChild expr2 (makeIndent padding)
printExprChild ((LabBinary op expr1 expr2), _) padding = do
      putStrLn (padding ++ " LabBinary")
      printOpChild op (makeIndent padding) True
      printExprChild expr1 (makeIndent padding)
      printExprChild expr2 (makeIndent padding)
printExprChild ((LabUnaryPost op expr), _) padding = do
      putStrLn (padding ++ " LabUnaryPost")
      printOpChild op (makeIndent padding) True
      printExprChild expr (makeIndent padding)
printExprChild ((LabIdentifier var), _) padding = do
      putStr (padding ++ " LabIdentifier")
      printVarChild var "" True
printExprChild ((LabReference obj prop), _) padding = do
      putStrLn (padding ++ " LabReference")
      printExprChild obj (makeIndent padding)
      printExprChild prop (makeIndent padding)
printExprChild ((LabIndex obj prop), _) padding = do
      putStrLn (padding ++ " LabIndex")
      printExprChild obj (makeIndent padding)
      printExprChild prop (makeIndent padding)
printExprChild ((LabValue val), _) padding = do
      putStr (padding ++ " LabValue")
      printValueChild val (makeIndent padding) True
printExprChild ((LabVarDeclaration var expr), _) padding = do
      putStrLn (padding ++ " LabVarDeclaration")
      printVarChild var (makeIndent padding) True
      maybePrintExprChild expr (makeIndent padding)
printExprChild ((LabPropNameValue prop expr), _) padding = do
      putStrLn (padding ++ " LabPropNameValue")
      printPropertyNameChild prop (makeIndent padding)
      printExprChild expr (makeIndent padding)
printExprChild ((LabFunctionExpression vChild args child), _) padding = do
      putStr (padding ++ " LabFunctionExpression")
      maybePrintVarChild vChild "" False
      putStr " ["
      mapM_ (\var -> printVarChild var "" False) $ args
      putStrLn " ]"
      printASTChild child (makeIndent padding)
printExprChild ((LabCall fid args), _) padding = do
      putStrLn (padding ++ " LabCall")
      printExprChild fid (makeIndent padding)
      printExprChild args (makeIndent padding)
printExprChild ((LabArguments exprs), _) padding = do
      putStrLn (padding ++ " LabArguments")
      mapM_ ((\p e -> printExprChild e p  ) (makeIndent padding)) $ exprs
printExprChild (n, _) padding = do
      putStrLn (padding ++ " OTHER EXPRCHILD")
      putStrLn ((makeIndent padding) ++ " " ++ (show $ n))


-- FIXME: Print the "Just". See maybePrintVarChild
maybePrintExprChild :: Maybe ExprChild -> String -> IO()
maybePrintExprChild (Just expr) padding = printExprChild expr padding
maybePrintExprChild Nothing padding = putStrLn (padding ++ " Nothing")


printVarChild :: VarChild -> String -> Bool -> IO()
printVarChild (var, label) padding False = putStr (padding ++ " \"" ++ var ++ "\"")
printVarChild (var, label) padding True = putStrLn (padding ++ " \"" ++ var ++ "\"")


maybePrintVarChild :: Maybe VarChild -> String -> Bool -> IO()
maybePrintVarChild (Just child) padding False = do
      putStr (padding ++ " Just")
      printVarChild child "" False
maybePrintVarChild (Just child) padding True = do
      putStrLn (padding ++ " Just")
      printVarChild child (makeIndent padding) True
maybePrintVarChild Nothing padding False = putStr (padding ++ " \"Nothing\"")
maybePrintVarChild Nothing padding True = putStrLn (padding ++ " \"Nothing\"")


printIndexChild :: IndexChild -> String -> Bool -> IO()
printIndexChild (index, label) padding False = putStr (padding ++ " \"" ++ (show index) ++ "\"")
printIndexChild (index, label) padding True = putStrLn (padding ++ " \"" ++ (show index) ++ "\"")


printValueChild :: ValueChild -> String -> Bool -> IO()
printValueChild (val, label) padding newLine = printLabelledValue val padding newLine


printOpChild :: OpChild -> String -> Bool -> IO()
printOpChild (op, label) padding False = putStr (padding ++ " Operator \"" ++ op ++ "\"")
printOpChild (op, label) padding True = putStrLn (padding ++ " Operator \"" ++ op ++ "\"")


printPropertyNameChild :: PropertyNameChild -> String -> IO()
printPropertyNameChild (propName, label) padding = printLabelledPropertyName propName padding


printLabelledPropertyName :: LabelledPropertyName -> String -> IO()
printLabelledPropertyName (LabVariableProperty var) padding = do
      putStr (padding ++ " Property")
      printVarChild var "" True
printLabelledPropertyName (LabIndexProperty index) padding = do
      putStr (padding ++ " Property")
      printIndexChild index "" True


printLabelledValue :: LabelledValue -> String -> Bool -> IO()
printLabelledValue (LabObject exprs) padding _ = do
      putStrLn ""
      putStrLn (padding ++ " LabObject")
      mapM_ ((\p e -> printExprChild e p  ) (makeIndent padding)) $ exprs
printLabelledValue (LabInt val) padding False = do
      putStr (" LabInt " ++ (show val))
printLabelledValue (LabFloat val) padding False = do
      putStr (" LabFloat " ++ (show val))
printLabelledValue (LabDQString val) padding False = do
      putStr (" LabDQString " ++ (show val))
printLabelledValue (LabArray elems) padding False = do
      putStrLn (" LabArray")
      putStrLn ((makeIndent padding) ++ " [")
      mapM_ ((\p e -> printExprChild e p  ) (makeIndent padding)) $ elems
      putStr ((makeIndent padding) ++ " ]")
printLabelledValue labVal padding True = do
      printLabelledValue labVal padding False
      putStrLn ""
printLabelledValue n padding _ = do
      putStrLn ""
      putStrLn (padding ++ " OTHER LABELLEDVALUE")
      putStrLn ((makeIndent padding) ++ " " ++ (show $ n))
