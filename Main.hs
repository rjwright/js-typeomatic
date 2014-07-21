
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


-- The methods in the pipeline are:
-- 		ParseJS.parseTree - NEEDS PRETTY PRINT
-- 		ParseJS.getASTWithSource - NEEDS PRETTY PRINT
-- 		ResolveASTSourceFragments.astListWSMakeSourceFragments
-- 		LabelAST.label
-- 		DeclarationGraph.getDeclarationGraph - NEEDS PRETTY PRINT
-- 		DeclarationGraph.graphGetAllRules (optional) - TYPES NEED PRETTY PRINT
-- 		DeclarationGraph.cleanFunctionRules (optional)
-- 		DeclarationGraph.cleanFunction (optional)


module Main
(
main
) where


import DeclarationGraph
import LabelAST
import Language.JavaScript.Parser
import ParseJS
import PrettyPrint
import ResolveSourceFragments
import System.Environment
import TypeRules


-- TODO: Reverse these so that the earlier layers come first.
-- TODO: Put these sections into functions.
main :: IO ()
main = do
	(infile:[]) <- getArgs
	pr <- readFile infile
	putStrLn ""
	-- *****************
	putStrLn ""
	putStrLn "Print the stripped parse tree"
	printParseTreeStripped $ jsnGetNode $ parseTree pr infile
	putStrLn ""
	putStrLn "Print the raw parse tree"
	putStrLn $ show $ parse pr infile
	-- *****************
	-- PRETTY PRINTED
	-- Prints declared functions and function expressions, and the identifiers
	-- that are visible to each one.
	-- putStrLn ""
	-- putStrLn "Pretty print cleaned functions and function expressions, with identifiers"
	-- putStr "Top Level:"
	-- printCleanedElementList ((makeCleanedFunctions pr infile):[]) (makeIndent "")

	-- **PRETTY PRINTED**
	-- Prints the rules, indented base on their scope, with optional source code, and an optional
	-- list of the identifiers that are visible at that each scope.
	putStrLn ""
	putStrLn "Pretty print cleaned function rules with identifiers"
	putStr "Top Level:"
	printCleanedRulesList ((makeCleanedFunctionRules pr infile):[]) (makeIndent "") False True

	-- PRETTY PRINTED (could be improved with pretty printing for types)
	-- Print all the rules, optionally with source code.
	-- putStrLn ""
	-- putStrLn "Pretty print all the rules with code fragments"
	-- mapM_ (\r -> printRule r "" True) (makeAllRules pr infile)

	-- Print the raw declaration graph with rules.
	-- WARNING: Large and more-or-less illegible.
	-- putStrLn ""
	-- putStrLn "Print the declaration graph"
	-- putStrLn $ show $ makeDeclarationGraph pr infile

	-- PRETTY PRINTED
	-- Print the cleaned ATS.
	-- putStrLn ""
	-- putStrLn "Pretty print labelled AST without labels or source fragments"
	-- mapPrintASTChild (makeLabelledAST pr infile) (makeIndent "") False False
	-- PRETTY PRINTED
	-- Print the cleaned ATS with labels.
	-- putStrLn ""
	-- putStrLn "Pretty print labelled AST with labels"
	-- mapPrintASTChild (makeLabelledAST pr infile) (makeIndent "") False True
	-- PRETTY PRINTED
	-- Print the cleaned ATS with source.
	-- putStrLn ""
	-- putStrLn "Pretty print labelled AST with source fragments"
	-- mapPrintASTChild (makeLabelledAST pr infile) (makeIndent "") True False
	-- **PRETTY PRINTED**
	-- Print the cleaned ATS with labels and source.
	putStrLn ""
	putStrLn "Pretty print labelled AST with labels and source fragments"
	mapPrintASTChild (makeLabelledAST pr infile) (makeIndent "") True True

	-- **PRETTY PRINTED**
	-- Pretty print the ASTWithSourceFragment with source fragments
	putStrLn ""
	putStrLn "Pretty print ASTWithSourceFragment with source fragments"
	mapPrintASTWS (makeASTWithSourceFragments pr infile) (makeIndent "") True
	-- **PRETTY PRINTED**
	-- Pretty print the ASTWithSourceFragment without source fragments
	putStrLn ""
	putStrLn "Pretty print ASTWithSourceFragment without source fragments"
	mapPrintASTWS (makeASTWithSourceFragments pr infile) (makeIndent "") False

	-- Prints the first AST (pre-labels).
	-- putStrLn ""
	-- putStrLn "Print the the original AST"
	-- mapM_ print (makeAST pr infile)

	-- Rudimentary. Prints the parse tree using Language.JavaScript's showStripped function. Prints
	-- one top-level parse tree node per line.
	-- putStrLn ""
	-- putStrLn "Print the stripped parse tree"
	-- printParseTreeStripped $ jsnGetNode $ parseTree pr infile

	-- Print the raw parse tree.
	-- putStrLn ""
	-- putStrLn "Print the raw parse tree"
	-- putStrLn $ show $ parse pr infile


-- TODO: Reverse these so that the earlier layers come first.
makeCleanedFunctions :: String -> SourceFileName -> CleanedFunction
makeCleanedFunctions input fileName = cleanFunction $ makeCleanedFunctionRules input fileName


makeCleanedFunctionRules :: String -> SourceFileName -> CleanedFunctionRules
makeCleanedFunctionRules input fileName = cleanFunctionRules $ makeDeclarationGraph input fileName


makeAllRules :: String -> SourceFileName -> [Rule]
makeAllRules input fileName = graphGetAllRules $ makeDeclarationGraph input fileName


makeDeclarationGraph :: String -> SourceFileName -> FunctionRules
makeDeclarationGraph input fileName =
	getDeclarationGraph
		(makeLabelledAST input fileName)
		(fileName, 1, 1, ((length $ lines input) + 1), 1)


makeLabelledAST :: String -> SourceFileName -> [ASTChild]
makeLabelledAST input fileName = label $ makeASTWithSourceFragments input fileName


-- FIXME: Passing the file name here might mean that we don't need to thread it through the whole
-- AST.
makeASTWithSourceFragments :: String -> SourceFileName -> [ASTWithSourceFragment]
makeASTWithSourceFragments input fileName =
	astListWSMakeSourceFragments (makeAST input fileName) (SpanPoint fileName ((length $ lines input) + 1) 1)


makeAST :: String -> SourceFileName -> ([ASTWithSourceSpan], SourceFileName)
makeAST input fileName = getASTWithSource (parseTree input fileName) fileName
