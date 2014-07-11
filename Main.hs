
-- Module prints human-legible output from the various layers of the pipeline
--
-- TODO: Comment this file.

-- module PrettyPrint
module Main
(
main
) where

import Data.Char
import Data.List
import DeclarationGraph
import LabelJSAST
import Language.JavaScript.Parser
import ParseJS
import ResolveJSASTSourceFragments
import System.Environment
import TypeRules

type LabFlag = Bool
type LineFlag = Bool
type SourceFlag = Bool

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
	-- mapPrintASTChild (makeLabelledJSAST pr) (makeIndent "") False
	-- putStrLn "The cleaned AST with labels"
	-- mapPrintASTChild (makeLabelledJSAST pr infile) (makeIndent "") True

	-- Prints the original AST without labels.
	--
	-- TODO: Add pretty printing for this.
	-- putStrLn ""
	-- putStrLn "The the original JSAST"
	-- mapM_ print (makeJSAST pr infile)

	putStrLn ""
	-- putStrLn "The input file"
	putStrLn ""
	putStrLn infile
	putStrLn "The raw parse tree"
	putStrLn $ show $ parse pr infile
	-- putStrLn ""
	-- putStrLn "The source fragments"
	-- mapM_ (putStrLn . show) (getSourceFragments (topNodeGetSpan $ parseTree pr infile) infile [])
	-- putStrLn ""
	-- putStrLn "The resolved source fragments"
	-- mapM_
	-- 	(\sf -> printSourceCode sf "")
	-- 		(getSourceFragments (topNodeGetSpan $ parseTree pr infile) infile [])
	-- putStrLn ""
	-- putStrLn "Make JSAST with source fragments"
	-- mapM_ (putStrLn . show) (makeJSASTWithSourceFragments pr infile)
	putStrLn ""
	putStrLn ""
	putStrLn "Pretty print JSAST with source fragments"
	mapPrintASTWS (makeJSASTWithSourceFragments pr infile) (makeIndent "") True
	-- printParseTreeStripped $ parseTree pr
	-- putStrLn ""
	-- putStrLn $ show $ parseTree pr


makeCleanedFunctions :: String -> SourceFileName -> CleanedFunction
makeCleanedFunctions input fileName = cleanFunction $ makeCleanedFunctionRules input fileName

makeCleanedFunctionRules :: String -> SourceFileName -> CleanedFunctionRules
makeCleanedFunctionRules input fileName = cleanFunctionRules $ makeDeclarationGraph input fileName

makeAllRules :: String -> SourceFileName -> [Rule]
makeAllRules input fileName = graphGetAllRules $ makeDeclarationGraph input fileName

makeDeclarationGraph :: String -> SourceFileName -> FunctionRules
makeDeclarationGraph input fileName = getDeclarationGraph $ makeLabelledJSAST input fileName

makeLabelledJSAST :: String -> SourceFileName -> [ASTChild]
makeLabelledJSAST input fileName = label $ makeJSASTWithSourceFragments input fileName

-- FIXME: Passing the file name here might mean that we don't need to thread it through the whole
-- AST.
makeJSASTWithSourceFragments :: String -> SourceFileName -> [JSASTWithSourceFragment]
makeJSASTWithSourceFragments input fileName =
	jsastListMakeSourceFragments (makeJSAST input fileName) (SpanPoint fileName ((length $ lines input) + 1) 1)

makeJSAST :: String -> SourceFileName -> [JSASTWithSourceSpan]
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

stripEnd :: String -> String
stripEnd string  = reverse . dropWhile isSpace $ reverse string

stripStart :: String -> String
stripStart string  = dropWhile isSpace string

subList :: Int -> Int -> [a] -> [a]
subList start end ls = drop start $ take end ls


printSourceCode :: SourceFragment -> String -> IO()
printSourceCode (fileName, startRow, startCol, endRow, endCol) padding = do
	contents <- readFile fileName
	let singleLines = lines contents
	let fragment = getRange singleLines startRow startCol endRow endCol []
	let heading = " * SOURCE "
	let sf = prettifySourceFragment (fileName, startRow, startCol, endRow, endCol)
	let cleanedSF = (filter (\f -> not (isPrefixOf "//" (stripStart f))) (filter (\f -> not (f == "")) fragment))
	let maxLen = max
		(length (heading ++ sf))
		((maximum $ map length cleanedSF) + 3)
	let spacer = take maxLen (cycle ['*'])
	putStrLn (padding ++ " " ++ spacer)
	putStr (padding ++ heading)
	putStrLn sf
	mapM_
		(putStrLn . ((padding ++ " * ") ++) . stripEnd)
		(cleanedSF)
	putStrLn (padding ++ " " ++ spacer)
	where
		getRange strings sr sc er ec result =
			if (sr == er) then
				if (sc == ec) then
					result
				else
					-- result ++ [(show sr) ++ "   " ++ (subList (sc - 1) (ec - 1) (strings!!(sr - 1)))]
					result ++ [(subList (sc - 1) (ec - 1) (strings!!(sr - 1)))]
			else if (ec == 1) then
				-- getRange strings (sr + 1) 1 er ec (result ++ [(show sr) ++ "   " ++ drop (sc - 1) (strings!!(sr - 1))])
				getRange strings (sr + 1) 1 er ec (result ++ [drop (sc - 1) (strings!!(sr - 1))])
			else
				result ++ (subList (sr - 1) (er - 1) strings) ++ [subList 0 (ec - 1) (strings!!(er-1))]


prettifySourceFragment :: SourceFragment -> String
prettifySourceFragment (fileName, sr, sc, er, ec) =
	(reverse $ takeWhile (\c -> not (c == '/')) (reverse fileName))
	++ (" (" ++ (show sr) ++ ", " ++ (show sc) ++ ", " ++ (show er) ++ ", " ++ (show ec) ++ ")")

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
printASTChild ((LabBlock children), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabBlock") lab printLab
	mapPrintASTChild children (makeIndent padding) printLab
printASTChild ((LabCase val child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabCase") lab printLab
	let p = makeIndent padding
	printExprChild val p printLab
	printASTChild child p printLab
printASTChild ((LabCatch var expr child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabCatch") lab printLab
	let p = makeIndent padding
	printVarChild var p printLab True
	maybePrintExprChild expr p printLab
	printASTChild child p printLab
printASTChild ((LabForVar decs cond expr child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabForVar") lab printLab
	let p = makeIndent padding
	mapPrintExprChild decs p printLab
	maybePrintExprChild cond p printLab
	maybePrintExprChild expr p printLab
	printASTChild child p printLab
printASTChild ((LabForVarIn var obj child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabForVarIn") lab printLab
	let p = makeIndent padding
	printExprChild var p printLab
	printExprChild obj p printLab
	printASTChild child p printLab
printASTChild ((LabFunctionBody children), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabFunctionBody") lab printLab
	mapPrintASTChild children (makeIndent padding) printLab
printASTChild ((LabFunctionDeclaration vChild args child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printStrAndLabel (padding ++ " LabFunctionDeclaration") lab printLab
	printVarChild vChild "" printLab False
	putStr " ["
	mapPrintVarChild args "" printLab False
	putStrLn " ]"
	printASTChild child (makeIndent padding) printLab
printASTChild ((LabIf cond child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabIf") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printASTChild child p printLab
printASTChild ((LabIfElse cond childTrue childFalse), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabIfElse") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printASTChild childTrue p printLab
	printASTChild childFalse p printLab
printASTChild ((LabReturn expr), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabReturn") lab printLab
	printExprChild expr (makeIndent padding) printLab
printASTChild ((LabStatement expr), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabStatement") lab printLab
	printExprChild expr (makeIndent padding) printLab
printASTChild ((LabSwitch var child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabSwitch") lab printLab
	let p = makeIndent padding
	printExprChild var p printLab
	printASTChild child p printLab
printASTChild ((LabTry tryChild catchChild), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabTry") lab printLab
	let p = makeIndent padding
	printASTChild tryChild p printLab
	printASTChild catchChild p printLab
printASTChild ((LabWhile cond child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabWhile") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printASTChild child p printLab
printASTChild (n, lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
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
printExprChild ((LabArguments exprs), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabArguments") lab printLab
	mapPrintExprChild exprs (makeIndent padding) printLab
printExprChild ((LabAssignment op expr1 expr2), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabAssignment") lab printLab
	let p = makeIndent padding
	printOpChild op p printLab True
	printExprChild expr1 p printLab
	printExprChild expr2 p printLab
printExprChild ((LabBinary op expr1 expr2), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabBinary") lab printLab
	let p = makeIndent padding
	printOpChild op p printLab True
	printExprChild expr1 p printLab
	printExprChild expr2 p printLab
printExprChild ((LabCall fid args), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabCall") lab printLab
	let p = makeIndent padding
	printExprChild fid p printLab
	printExprChild args p printLab
printExprChild ((LabCallExpression call op expr), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabCallExpression") lab printLab
	let p = makeIndent padding
	printExprChild call p printLab
	printOpChild op p printLab True
	printExprChild expr p printLab
printExprChild ((LabFunctionExpression vChild args child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printStrAndLabel (padding ++ " LabFunctionExpression") lab printLab
	maybePrintVarChild vChild "" printLab False
	putStr " ["
	mapPrintVarChild args "" printLab False
	putStrLn " ]"
	printASTChild child (makeIndent padding) printLab
printExprChild ((LabIdentifier var), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printStrAndLabel (padding ++ " LabIdentifier") lab printLab
	printVarChild var "" printLab True
printExprChild ((LabIndex obj prop), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabIndex") lab printLab
	let p = makeIndent padding
	printExprChild obj p printLab
	printExprChild prop p printLab
printExprChild ((LabList exprs), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabList") lab printLab
	mapPrintExprChild exprs (makeIndent padding) printLab
printExprChild ((LabNew cons), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabNew") lab printLab
	let p = makeIndent padding
	printExprChild cons p printLab
printExprChild ((LabParenExpression child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabParenExpression") lab printLab
	printExprChild child (makeIndent padding) printLab
printExprChild ((LabPropNameValue prop expr), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabPropNameValue") lab printLab
	let p = makeIndent padding
	printPropertyNameChild prop p printLab
	printExprChild expr p printLab
printExprChild ((LabReference obj prop), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabReference") lab printLab
	let p = makeIndent padding
	printExprChild obj p printLab
	printExprChild prop p printLab
printExprChild ((LabTernary cond exprTrue exprFalse), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabTernary") lab printLab
	let p = makeIndent padding
	printExprChild cond p printLab
	printExprChild exprTrue p printLab
	printExprChild exprFalse p printLab
printExprChild ((LabThrow child), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabThrow") lab printLab
	let p = makeIndent padding
	printExprChild child p printLab
printExprChild ((LabUnaryPost op expr), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabUnaryPost") lab printLab
	let p = makeIndent padding
	printOpChild op p printLab True
	printExprChild expr p printLab
printExprChild ((LabUnaryPre op expr), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabUnaryPre") lab printLab
	let p = makeIndent padding
	printOpChild op p printLab True
	printExprChild expr p printLab
printExprChild ((LabValue val), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printStrAndLabel (padding ++ " LabValue") lab printLab
	printValueChild val (makeIndent padding) printLab True
printExprChild ((LabVarDeclaration var expr), lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
	printLnStrAndLabel (padding ++ " LabVarDeclaration") lab printLab
	let p = makeIndent padding
	printVarChild var p printLab True
	maybePrintExprChild expr p printLab
printExprChild (n, lab, sourceFragment) padding printLab = do
	printSourceCode sourceFragment padding
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


-- TODO: THis does nothing. Remove and call printLabelledValue directly.
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


printSource :: SourceFragment -> String -> SourceFlag -> IO()
printSource sourceFragment padding printSrc =
	if printSrc then
		-- FIXME: Move indenting into caller!
		printSourceCode sourceFragment padding
	else
		return()

mapPrintASTWS :: [JSASTWithSourceFragment] -> String -> SourceFlag -> IO()
mapPrintASTWS nodes padding printSrc =
	mapM_ printAST nodes
	where
		printAST n = printASTWS n padding printSrc


-- TODO: Still need to do:
--      WSDefault JSASTWithSourceFragment
--      WSDoWhile JSASTWithSourceFragment ExprWithSourceFragment
--      WSFinally JSASTWithSourceFragment
--      WSFor (Maybe ExprWithSourceFragment) (Maybe ExprWithSourceFragment) (Maybe ExprWithSourceFragment) JSASTWithSourceFragment
--      WSForIn [Variable] ExprWithSourceFragment JSASTWithSourceFragment
--      WSLabelled Variable JSASTWithSourceFragment
printASTWS :: JSASTWithSourceFragment -> String -> SourceFlag -> IO()
printASTWS (AWSF (WSBlock list) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Block")
	printSource sourceFragment padding printSrc
	mapPrintASTWS list (makeIndent padding) printSrc
printASTWS (AWSF (WSCase val body) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Case")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS val p printSrc
	printASTWS body p printSrc
printASTWS (AWSF (WSCatch var expr body) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Catch")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printVariable var p True
	maybePrintExprWS expr p printSrc
	printASTWS body p printSrc
printASTWS (AWSF (WSForVar decs cond expr body) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " ForVar")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	mapPrintExprWS decs p printSrc
	maybePrintExprWS cond p printSrc
	maybePrintExprWS expr p printSrc
	printASTWS body p printSrc
printASTWS (AWSF (WSForVarIn var obj body) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " ForVarIn")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS var p printSrc
	printExprWS obj p printSrc
	printASTWS body p printSrc
printASTWS (AWSF (WSFunctionBody list) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " FunctionBody")
	printSource sourceFragment padding printSrc
	mapPrintASTWS list (makeIndent padding) printSrc
printASTWS (AWSF (WSFunctionDeclaration var args body) sourceFragment) padding printSrc = do
	putStr (padding ++ " FunctionDeclaration")
	printVariable var "" False
	putStr " ["
	mapPrintVariable args "" False
	putStrLn " ]"
	printSource sourceFragment padding printSrc
	printASTWS body (makeIndent padding) printSrc
printASTWS (AWSF (WSIf cond body) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " If")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS cond p printSrc
	printASTWS body p printSrc
printASTWS (AWSF (WSIfElse cond bodyTrue bodyFalse) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " IfElse")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS cond p printSrc
	printASTWS bodyTrue p printSrc
	printASTWS bodyFalse p printSrc
printASTWS (AWSF (WSReturn expr) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Return")
	printSource sourceFragment padding printSrc
	printExprWS expr (makeIndent padding) printSrc
printASTWS (AWSF (WSStatement expr) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Statement")
	printSource sourceFragment padding printSrc
	printExprWS expr (makeIndent padding) printSrc
printASTWS (AWSF (WSSwitch var body) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Switch")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS var p printSrc
	printASTWS body p printSrc
printASTWS (AWSF (WSTry tryBody catchBody) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Try")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printASTWS tryBody p printSrc
	printASTWS catchBody p printSrc
printASTWS (AWSF (WSWhile cond body) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " While")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS cond p printSrc
	printASTWS body p printSrc
printASTWS (AWSF node sourceFragment) padding printSrc = do
	putStrLn (padding ++ " OTHER JSAST")
	printSource sourceFragment padding printSrc
	putStrLn ((makeIndent padding) ++ " " ++ (show node))


mapPrintExprWS :: [ExprWithSourceFragment] -> String -> SourceFlag -> IO()
mapPrintExprWS exprs padding printSrc =
	mapM_ printExpr exprs
	where
		printExpr e = printExprWS e padding printSrc


-- TODO: Remove all use of printLnStrAndLabel/printStrAndLabel.
-- TODO: Still need to do
-- 		WSBreak (Maybe Variable)
-- 		WSContinue (Maybe Variable)
printExprWS :: ExprWithSourceFragment -> String -> SourceFlag -> IO()
printExprWS (EWSF (WSArguments exprs) sourceFragment) padding printSrc = do
	printLnStrAndLabel (padding ++ " Arguments") 0 False
	printSource sourceFragment padding printSrc
	mapPrintExprWS exprs (makeIndent padding) printSrc
printExprWS (EWSF (WSAssignment op expr1 expr2) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Assignment")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printOperator op p True
	printExprWS expr1 p printSrc
	printExprWS expr2 p printSrc
printExprWS (EWSF (WSBinary op expr1 expr2) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Binary")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printOperator op p True
	printExprWS expr1 p printSrc
	printExprWS expr2 p printSrc
printExprWS (EWSF (WSCall fid args) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Call")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS fid p printSrc
	printExprWS args p printSrc
printExprWS (EWSF (WSCallExpression call op expr) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " CallExpression")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS call p printSrc
	printOperator op p True
	printExprWS expr p printSrc
printExprWS (EWSF (WSFunctionExpression var args body) sourceFragment) padding printSrc = do
	putStr (padding ++ " FunctionExpression")
	maybePrintVariable var "" False
	putStr " ["
	mapPrintVariable args "" False
	putStrLn " ]"
	printSource sourceFragment padding printSrc
	printASTWS body (makeIndent padding) printSrc
printExprWS (EWSF (WSIdentifier var) sourceFragment) padding printSrc = do
	putStr (padding ++ " Identifier")
	printVariable var "" True
	printSource sourceFragment padding printSrc
printExprWS (EWSF (WSIndex obj prop) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Index")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS obj p printSrc
	printExprWS prop p printSrc
printExprWS (EWSF (WSList exprs) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " List")
	printSource sourceFragment padding printSrc
	mapPrintExprWS exprs (makeIndent padding) printSrc
printExprWS (EWSF (WSNew cons) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " New")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS cons p printSrc
printExprWS (EWSF (WSParenExpression child) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " ParenExpression")
	printSource sourceFragment padding printSrc
	printExprWS child (makeIndent padding) printSrc
printExprWS (EWSF (WSPropNameValue prop expr) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " PropNameValue")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printPropertyName prop p
	printExprWS expr p printSrc
printExprWS (EWSF (WSReference obj prop) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Reference")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS obj p printSrc
	printExprWS prop p printSrc
printExprWS (EWSF (WSTernary cond exprTrue exprFalse) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Ternary")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS cond p printSrc
	printExprWS exprTrue p printSrc
	printExprWS exprFalse p printSrc
printExprWS (EWSF (WSThrow child) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " Throw")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printExprWS child p printSrc
printExprWS (EWSF (WSUnaryPost op expr) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " UnaryPost")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printOperator op p True
	printExprWS expr p printSrc
printExprWS (EWSF (WSUnaryPre op expr) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " UnaryPre")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printOperator op p True
	printExprWS expr p printSrc
printExprWS (EWSF (WSValue val) sourceFragment) padding printSrc = do
	putStr (padding ++ " Value")
	printValueWS val (makeIndent padding) printSrc True
	printSource sourceFragment padding printSrc
printExprWS (EWSF (WSVarDeclaration var expr) sourceFragment) padding printSrc = do
	putStrLn (padding ++ " VarDeclaration")
	printSource sourceFragment padding printSrc
	let p = makeIndent padding
	printVariable var p True
	maybePrintExprWS expr p printSrc
printExprWS (EWSF n sourceFragment) padding printSrc = do
	printSourceCode sourceFragment padding
	putStrLn (padding ++ " OTHER EXPRESSION")
	putStrLn ((makeIndent padding) ++ " " ++ (show n))


-- FIXME: Print the "Just". See maybePrintVarChild
maybePrintExprWS :: Maybe ExprWithSourceFragment -> String -> SourceFlag -> IO()
maybePrintExprWS (Just expr) padding printSrc =
	printExprWS expr padding printSrc
maybePrintExprWS Nothing padding _ = putStrLn (padding ++ " Nothing")


-- TODO: probably remove this. It doesn't do very much.
mapPrintVariable :: [Variable] -> String -> LineFlag -> IO()
mapPrintVariable vars padding printLine =
	mapM_ printVar vars
	where
		printVar v = printVariable v padding printLine


-- TODO: Probably remove this. It doesn't do very much.
printVariable :: Variable -> String -> LineFlag -> IO()
printVariable var padding True =
	putStrLn (padding ++ " \"" ++ var ++ "\"")
printVariable var padding False =
	putStr (padding ++ " \"" ++ var ++ "\"")


maybePrintVariable :: Maybe Variable -> String -> LineFlag -> IO()
maybePrintVariable (Just var) padding False = do
	putStr (padding ++ " Just")
	printVariable var "" False
maybePrintVariable (Just var) padding True = do
	putStrLn (padding ++ " Just")
	printVariable var (makeIndent padding) True
maybePrintVariable Nothing padding False =
	putStr (padding ++ " \"Nothing\"")
maybePrintVariable Nothing padding True =
	putStrLn (padding ++ " \"Nothing\"")


-- TODO: Remove this. It doesn't do anything.
printIndex :: Index -> String -> LineFlag -> IO()
printIndex index padding True =
	putStrLn (padding ++ " \"" ++ (show index) ++ "\"")
printIndex index padding False =
	putStr (padding ++ " \"" ++ (show index) ++ "\"")


-- TODO: Remove this. It does nothing.
printOperator :: Operator -> String -> LineFlag -> IO()
printOperator op padding True =
	putStrLn (padding ++ " Operator \"" ++ op ++ "\"")
printOperator op padding False =
 	putStr (padding ++ " Operator \"" ++ op ++ "\"")


printPropertyName :: PropertyName -> String -> IO()
printPropertyName (IndexProperty index) padding = do
	putStr (padding ++ " Property")
	printIndex index "" True
printPropertyName (VariableProperty var) padding = do
	putStr (padding ++ " Property")
	printVariable var "" True



printValueWS :: ValueWithSourceFragment -> String -> SourceFlag -> LineFlag -> IO()
printValueWS (WSArray elems) padding printSrc False = do
	putStrLn ""
	putStrLn (padding ++ " Array")
	let p = makeIndent padding
	putStrLn (p ++ " [")
	mapPrintExprWS elems p printSrc
	putStr (p ++ " ]")
printValueWS (WSBool val) padding _ False =
	putStr (" Bool " ++ (show val))
printValueWS (WSDQString val) padding _ False =
	putStr (" DQString " ++ (show val))
printValueWS (WSFloat val) padding _ False =
	putStr (" Float " ++ (show val))
printValueWS (WSInt val) padding _ False =
	putStr (" Int " ++ (show val))
printValueWS (WSNull) padding _ False =
	putStr (" Null")
printValueWS (WSObject exprs) padding printSrc _ = do
	putStrLn ""
	putStrLn (padding ++ " Object")
	mapPrintExprWS exprs (makeIndent padding) printSrc
printValueWS (WSString val) padding _ False =
	putStr (" String " ++ (show val))
printValueWS (WSUndefined) padding _ False =
	putStr (" Undefined")
printValueWS val padding printSrc True = do
	printValueWS val padding printSrc False
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
