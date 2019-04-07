module Hw2 where

data ASTResult = ASTError String | ASTJust (String, String, Int) deriving (Show, Read)
data ASTDatum = ASTSimpleDatum String | ASTLetDatum String deriving (Show, Read)
data AST = EmptyAST | ASTNode ASTDatum AST AST deriving (Show, Read)

isNumber :: String -> Bool
eagerEvaluation :: AST -> ASTResult
normalEvaluation :: AST -> ASTResult
-- DO NOT MODIFY OR DELETE THE LINES ABOVE -- 
-- IMPLEMENT isNumber, eagerEvaluation and normalEvaluation FUNCTIONS ACCORDING TO GIVEN SIGNATURES -- 

find :: Char -> String -> Bool


isNumber ('-':rest) = numberCheck rest
isNumber x = numberCheck x
numberCheck [] = True
numberCheck x = find (x!!0) "0123456789" && numberCheck (tail x)

find _ [] = False
find  x (y:rest) | x == y = True
                 | otherwise = find x rest

eagerEvaluation x = ASTError "To be Implemented"

normalEvaluation (ASTNode (ASTSimpleDatum x) left right) | x == "num" = checkNum(left)
                                                         | x == "str" = nEvalStr(left)
                                                         | x == "plus" && checkTypeNum(left, right) = ASTError "To be Implemented"
                                                         | (x == "plus" || x == "times" || x == "negate") && not(checkTypeNum(left, right)) = createMessage2(x, left, right)
                                                         | x == "times" && checkTypeNum(left, right) = ASTError "To be Implemented"
                                                         | x == "negate" && checkTypeNum(left, right) = ASTError "To be Implemented"
                                                         | x == "cat" && not(checkTypeStr2(left, right)) = createMessage2(x, left, right)
                                                         | x == "len" && not(checkTypeStr1(left)) = createMessage1(x, left)
                                                         | x == "cat" = ASTError "To be Implemented"
                                                         | x == "len" = ASTError "To be Implemented"
                                                         | otherwise = ASTError "To be Implemented"

nEvalPlus ((ASTNode (ASTSimpleDatum x) left1 right1), (ASTNode (ASTSimpleDatum y) left2 right2)) | 



nEvalStr (ASTNode (ASTSimpleDatum x) EmptyAST EmptyAST) = ASTJust (x, "str", 0)

checkNum (ASTNode (ASTSimpleDatum x) EmptyAST EmptyAST) | isNumber(x) = ASTJust (x, "num", 0)
                                                        | otherwise = ASTError ("the value '" ++ x ++ "' is not a number!")

checkTypeNum((ASTNode (ASTSimpleDatum x) left1 EmptyAST), (ASTNode (ASTSimpleDatum y) left2 EmptyAST)) | x == "num" && y == "num" = True
                                                                                                       | otherwise = False
   
checkTypeStr1(ASTNode (ASTSimpleDatum x) left1 EmptyAST) | x == "str" = True
                                                                                                        | otherwise = False                                                                                                    
checkTypeStr2((ASTNode (ASTSimpleDatum x) left1 EmptyAST), (ASTNode (ASTSimpleDatum y) left2 EmptyAST)) | x == "str" && y == "str" = True
                                                                                                        | otherwise = False
                               
createMessage1(op, (ASTNode (ASTSimpleDatum x) left1 EmptyAST)) = ASTError (op ++ " operation is not defined on " ++ x ++ "!")                                                                                                             
createMessage2(op, (ASTNode (ASTSimpleDatum x) left1 EmptyAST), (ASTNode (ASTSimpleDatum y) left2 EmptyAST)) 
                = ASTError (op ++ " operation not defined between " ++ x ++ " and " ++ y ++ "!")