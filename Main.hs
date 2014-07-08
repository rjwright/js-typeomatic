
-- Module prints human-legible output from the various layers of the pipeline
--
-- TODO: Comment this file.

-- module PrettyPrint
module Main
(
main
) where


import DeclarationGraph
import LabelJSAST
import Language.JavaScript.Parser
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
	-- 	((makeCleanedFunctionRules pr):[]) (makeIndent "") True

	-- TODO: Rule type needs pretty printing
	-- mapM_ print (makeAllRules pr)

	-- Raw output with a lot of details.
	-- putStrLn $ show $ makeDeclarationGraph pr

	-- Prints the cleaned ATS, indented, with optional labels.
	--
	-- TODO: Experiment with keeping JS code snippets in the tree and printing
	-- them here.
	-- mapPrintASTChild (makeLabelledJSAST pr) (makeIndent "") False
	mapPrintASTChild (makeLabelledJSAST pr infile) (makeIndent "") True

	-- Prints the original AST without labels.
	--
	-- TODO: Add pretty printing for this.
	mapM_ print (makeJSAST pr infile)

	putStrLn ""
	putStrLn infile
	putStrLn $ show $ parse pr infile
	putStrLn ""
	mapM_ (putStrLn . show) (getSourceFragments (topNodeGetSpan $ parseTree pr infile) infile [])
	mapM_ printSourceFragment (getSourceFragments (topNodeGetSpan $ parseTree pr infile) infile [])
	putStrLn ""
	-- printParseTreeStripped $ parseTree pr
	putStrLn ""
	-- putStrLn $ show $ parseTree pr


makeCleanedFunctions :: String -> String -> CleanedFunction
makeCleanedFunctions input fileName = cleanFunction $ makeCleanedFunctionRules input fileName

makeCleanedFunctionRules :: String -> String -> CleanedFunctionRules
makeCleanedFunctionRules input fileName = cleanFunctionRules $ makeDeclarationGraph input fileName

makeAllRules :: String -> String -> [Rule]
makeAllRules input fileName = graphGetAllRules $ makeDeclarationGraph input fileName

makeDeclarationGraph :: String -> String -> FunctionRules
makeDeclarationGraph input fileName = getDeclarationGraph $ makeLabelledJSAST input fileName

makeLabelledJSAST :: String -> String -> [ASTChild]
makeLabelledJSAST input fileName = label $ makeJSAST input fileName

makeJSAST :: String -> SourceFileName -> [JSAST]
makeJSAST input fileName = toJSAST (parseTree input fileName) fileName

makeIndent :: String -> String
makeIndent s = s ++ "..."


printStrAndLabel :: String -> JSASTLabel -> LabFlag -> IO()
printStrAndLabel str lab False = putStr str
printStrAndLabel str lab True = putStr (str ++ " <" ++ (show lab) ++ ">")


printLnStrAndLabel :: String -> JSASTLabel -> LabFlag -> IO()
printLnStrAndLabel str lab printLab = do
	printStrAndLabel str lab printLab
	putStrLn ""


printParseTreeStripped :: Node -> IO()
printParseTreeStripped (JSSourceElementsTop elements) =
	mapM_ (putStrLn . show . showStripped) elements


-- TODO: This needs to open fileName, read from (startRow, startCol) to (endRow, endCol) and print
-- the result.
printSourceFragment :: SourceFragment -> IO()
printSourceFragment (fileName, startRow, startCol, endRow, endCol) = do
	contents <- readFile fileName
	let singleLines = lines contents
	let fragment = getRange singleLines startRow startCol endRow endCol
	-- putStrLn $ subList 3 7 "123RENEE"
	mapM_ putStrLn fragment
	where
		getRange strings sr sc er ec =
			if (sr == er) then
				if (sc == ec) then
					subList (sr - 1) (length strings) strings
				else
					[subList (sc - 1) (ec - 1) (strings!!(sr - 1))]
			else if (ec == 1) then
				(subList (sr - 1) (er - 1) strings)
			else
				(subList (sr - 1) (er - 2) strings) ++ [subList 0 (ec - 1) (strings!!(er-1))]


subList :: Int -> Int -> [a] -> [a]
subList start end ls = drop start $ take end ls


-- Prints CleanedRules, indented according to their depth in the tree
printCleanedRulesList :: CleanedRules a => [a] -> String -> Bool -> IO()
printCleanedRulesList (head:fx) padding printIdentifiers = do
	putStrLn (" " ++ (show fid))
	if printIdentifiers == True then do
		putStrLn (padding ++ " IDENTIFIERS:")
		mapM_ (putStrLn . ((padding ++ " ") ++) . show) dIDs
	else
		return()
	putStrLn (padding ++ " RULES:")
	mapM_ (putStrLn . ((padding ++ " ") ++) . show) rules
	-- mapM_ (\r -> printRule r padding) rules
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
			putStr (p ++ " " ++ (crName f) ++ ":")
		printList l p = do
			printHeading p l
			printCleanedRulesList l p printIdentifiers
printCleanedRulesList [] _ _ = return()


-- Prints CleanedElements, indented according to their depth in the tree
printCleanedElementList :: (CleanedElement a, Show a) => [a] -> String -> IO()
printCleanedElementList (head:fx) padding = do
	putStrLn (" " ++ (show fid))
	mapM_ (putStrLn . ((padding ++ " ") ++) . show) dIDs
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
			putStr (p ++ " " ++ (ceName f) ++ ":")
		printList l p = do
			printHeading p l
			printCleanedElementList l p
printCleanedElementList [] _ = return()


mapPrintASTChild :: [ASTChild] -> String -> LabFlag -> IO()
mapPrintASTChild children padding printLab =
	mapM_ printChild children
	where
		printChild c = printASTChild c padding printLab


-- TODO: Still need to do:
-- 		LabDefault ASTChild
-- 		LabDoWhile ASTChild ExprChild
-- 		LabFinally ASTChild
-- 		LabFor (Maybe ExprChild) (Maybe ExprChild) (Maybe ExprChild) ASTChild
-- 		LabForIn [VarChild] ExprChild ASTChild
-- 		LabLabelled VarChild ASTChild
printASTChild :: ASTChild -> String -> LabFlag -> IO()
printASTChild ((LabBlock children), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabBlock") lab printLab
	mapPrintASTChild children (makeIndent padding) printLab
printASTChild ((LabCase val child), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabCase") lab printLab
	let p = makeIndent padding
	printExprChild val p printLab
	printASTChild child p printLab
printASTChild ((LabCatch var expr child), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabCatch") lab printLab
	let p = makeIndent padding
	printVarChild var p printLab True
	maybePrintExprChild expr p printLab
	printASTChild child p printLab
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
printASTChild ((LabFunctionBody children), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabFunctionBody") lab printLab
	mapPrintASTChild children (makeIndent padding) printLab
printASTChild ((LabFunctionDeclaration vChild args child), lab) padding printLab = do
	printStrAndLabel (padding ++ " LabFunctionDeclaration") lab printLab
	printVarChild vChild "" printLab False
	putStr " ["
	mapPrintVarChild args "" printLab False
	putStrLn " ]"
	printASTChild child (makeIndent padding) printLab
printASTChild ((LabIf cond child), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabIf") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printASTChild child p printLab
printASTChild ((LabIfElse cond childTrue childFalse), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabIfElse") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printASTChild childTrue p printLab
	printASTChild childFalse p printLab
printASTChild ((LabReturn expr), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabReturn") lab printLab
	printExprChild expr (makeIndent padding) printLab
printASTChild ((LabStatement expr), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabStatement") lab printLab
	printExprChild expr (makeIndent padding) printLab
printASTChild ((LabSwitch var child), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabSwitch") lab printLab
	let p = makeIndent padding
	printExprChild var p printLab
	printASTChild child p printLab
printASTChild ((LabTry tryChild catchChild), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabTry") lab printLab
	let p = makeIndent padding
	printASTChild tryChild p printLab
	printASTChild catchChild p printLab
printASTChild ((LabWhile cond child), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabWhile") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printASTChild child p printLab
printASTChild (n, lab) padding printLab = do
	printLnStrAndLabel (padding ++ " OTHER ASTCHILD") lab printLab
	putStrLn ((makeIndent padding) ++ " " ++ (show n))


mapPrintExprChild :: [ExprChild] -> String -> LabFlag -> IO()
mapPrintExprChild children padding printLab =
	mapM_ printChild children
	where
		printChild c = printExprChild c padding printLab


-- TODO: Still need to do
-- 		LabBreak (Maybe VarChild)
-- 		LabContinue (Maybe VarChild)
printExprChild :: ExprChild -> String -> LabFlag -> IO()
printExprChild ((LabArguments exprs), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabArguments") lab printLab
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
printExprChild ((LabCall fid args), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabCall") lab printLab
	let p = makeIndent padding
	printExprChild fid p printLab
	printExprChild args p printLab
printExprChild ((LabCallExpression call op expr), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabCallExpression") lab printLab
	let p = makeIndent padding
	printExprChild call p printLab
	printOpChild op p printLab True
	printExprChild expr p printLab
printExprChild ((LabFunctionExpression vChild args child), lab) padding printLab = do
	printStrAndLabel (padding ++ " LabFunctionExpression") lab printLab
	maybePrintVarChild vChild "" printLab False
	putStr " ["
	mapPrintVarChild args "" printLab False
	putStrLn " ]"
	printASTChild child (makeIndent padding) printLab
printExprChild ((LabIdentifier var), lab) padding printLab = do
	printStrAndLabel (padding ++ " LabIdentifier") lab printLab
	printVarChild var "" printLab True
printExprChild ((LabIndex obj prop), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabIndex") lab printLab
	let p = makeIndent padding
	printExprChild obj p printLab
	printExprChild prop p printLab
printExprChild ((LabList exprs), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabList") lab printLab
	mapPrintExprChild exprs (makeIndent padding) printLab
printExprChild ((LabNew cons), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabNew") lab printLab
	let p = makeIndent padding
	printExprChild cons p printLab
printExprChild ((LabParenExpression child), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabParenExpression") lab printLab
	printExprChild child (makeIndent padding) printLab
printExprChild ((LabPropNameValue prop expr), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabPropNameValue") lab printLab
	let p = makeIndent padding
	printPropertyNameChild prop p printLab
	printExprChild expr p printLab
printExprChild ((LabReference obj prop), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabReference") lab printLab
	let p = makeIndent padding
	printExprChild obj p printLab
	printExprChild prop p printLab
printExprChild ((LabTernary cond exprTrue exprFalse), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabTernary") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printExprChild exprTrue p printLab
	printExprChild exprFalse p printLab
printExprChild ((LabThrow child), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabThrow") lab printLab
	let p = makeIndent padding
	printExprChild child p printLab
printExprChild ((LabUnaryPost op expr), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabUnaryPost") lab printLab
	let p = makeIndent padding
	printOpChild op p printLab True
	printExprChild expr p printLab
printExprChild ((LabUnaryPre op expr), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabUnaryPre") lab printLab
	let p = makeIndent padding
	printOpChild op p printLab True
	printExprChild expr p printLab
printExprChild ((LabValue val), lab) padding printLab = do
	printStrAndLabel (padding ++ " LabValue") lab printLab
	printValueChild val (makeIndent padding) printLab True
printExprChild ((LabVarDeclaration var expr), lab) padding printLab = do
	printLnStrAndLabel (padding ++ " LabVarDeclaration") lab printLab
	let p = makeIndent padding
	printVarChild var p printLab True
	maybePrintExprChild expr p printLab
printExprChild (n, lab) padding printLab = do
	printLnStrAndLabel (padding ++ " OTHER EXPRCHILD") lab printLab
	putStrLn ((makeIndent padding) ++ " " ++ (show n))


-- FIXME: Print the "Just". See maybePrintVarChild
maybePrintExprChild :: Maybe ExprChild -> String -> LabFlag -> IO()
maybePrintExprChild (Just expr) padding printLab =
	printExprChild expr padding printLab
-- FIXME: Nothings have no label. Is that a problem?
maybePrintExprChild Nothing padding _ = putStrLn (padding ++ " Nothing")


mapPrintVarChild :: [VarChild] -> String -> LabFlag -> LineFlag -> IO()
mapPrintVarChild children padding printLab printLine =
	mapM_ printChild children
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
printPropertyNameChild ((LabIndexProperty index), lab) padding printLab = do
	printStrAndLabel (padding ++ " Property") lab printLab
	printIndexChild index "" printLab True
printPropertyNameChild ((LabVariableProperty var), lab) padding printLab = do
	printStrAndLabel (padding ++ " Property") lab printLab
	printVarChild var "" printLab True


printLabelledValue :: LabelledValue -> String -> LabFlag -> LineFlag -> IO()
printLabelledValue (LabArray elems) padding printLab False = do
	putStrLn ""
	putStrLn (padding ++ " LabArray")
	let p = makeIndent padding
	putStrLn (p ++ " [")
	mapPrintExprChild elems p printLab
	putStr (p ++ " ]")
printLabelledValue (LabBool val) padding _ False =
	putStr (" LabBool " ++ (show val))
printLabelledValue (LabDQString val) padding _ False =
	putStr (" LabDQString " ++ (show val))
printLabelledValue (LabFloat val) padding _ False =
	putStr (" LabFloat " ++ (show val))
printLabelledValue (LabInt val) padding _ False =
	putStr (" LabInt " ++ (show val))
printLabelledValue (LabNull) padding _ False =
	putStr (" LabNull")
printLabelledValue (LabObject exprs) padding printLab _ = do
	putStrLn ""
	putStrLn (padding ++ " LabObject")
	mapPrintExprChild exprs (makeIndent padding) printLab
printLabelledValue (LabString val) padding _ False =
	putStr (" LabString " ++ (show val))
printLabelledValue (LabUndefined) padding _ False =
	putStr (" LabUndefined")
printLabelledValue labVal padding printLab True = do
	printLabelledValue labVal padding printLab False
	putStrLn ""

-- TODO: Implement pretty printing for Rules
-- printRule :: Rule -> String -> IO()
-- printRule (Rule type1 type2) padding = do
--     -- putStrLn (padding ++ "Rule:")
--     putStr (padding ++ " (" ++ (show type1) ++ ") ")
--     putStrLn ("(" ++ (show type2) ++ ")")
--     putStrLn ""

-- TODO: Implement pretty printing for Types
-- printType :: Type -> String -> IO()
-- printType (IdentifierType var lab) padding =
--     putStrLn (padding ++ "IdentifierType " ++ (show var) ++ (show lab))
