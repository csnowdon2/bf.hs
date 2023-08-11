import Control.Monad.State
import Tape

------------------
-- Tokenization --
------------------

data Token = L_T | R_T | Inc_T | Dec_T | In_T | Out_T | LoopStart_T | LoopEnd_T

tokenize :: Char -> Token
tokenize '<' = L_T
tokenize '>' = R_T
tokenize '+' = Inc_T
tokenize '-' = Dec_T
tokenize '.' = Out_T
tokenize ',' = In_T
tokenize '[' = LoopStart_T
tokenize ']' = LoopEnd_T
tokenize  _  = error "Invalid token"


-------------
-- Parsing --
-------------

data Operation = Op TapeOp | Loop [Operation]

type Program = [Operation]

parse :: [Token] -> Program
parse = evalState parseState

parseState :: State [Token] [Operation]
parseState = do opM <- extractOp 
                case opM of
                  Nothing -> return []
                  Just op -> parseState >>= return . (:) op

extractOp :: State [Token] (Maybe Operation)
extractOp = do tokens <- get
               case tokens of
                 []   -> return Nothing
                 t:ts -> do put ts
                            case t of
                              LoopEnd_T   -> return Nothing
                              LoopStart_T -> parseState >>= (return . Just . Loop) 
                              L_T         -> return $ Just $ Op L
                              R_T         -> return $ Just $ Op R
                              Inc_T       -> return $ Just $ Op Inc
                              Dec_T       -> return $ Just $ Op Dec
                              In_T        -> return $ Just $ Op In
                              Out_T       -> return $ Just $ Op Out


---------------
-- Execution --
---------------

terminated :: Tape -> Bool
terminated t = getvalue t == 0

runTape :: Program   -> Tape -> IO Tape
apply   :: Operation -> Tape -> IO Tape

runTape = flip $ foldM $ flip apply

apply (Op op)     = operate op 
apply (Loop body) = iterateUntil terminated (runTape body)

iterateUntil :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
iterateUntil p f v 
    | p v       = return v
    | otherwise = f v >>= iterateUntil p f


---------------------
-- Compile and Run --
---------------------

compile :: String -> Program
compile = parse . map tokenize

run :: Program -> IO ()
run prog = runTape prog newtape >> return ()

execute :: String -> IO ()
execute = run . compile


---------------------
-- Sample Programs --
---------------------

helloWorld :: String
helloWorld = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
             \---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

helloWorldInf :: String
helloWorldInf = concat $ repeat "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>\
                                \---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.>"

main :: IO ()
main = execute helloWorld
