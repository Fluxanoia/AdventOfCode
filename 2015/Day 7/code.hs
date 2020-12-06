import Data.List.Split
import Data.Bits
import Data.Int
import Flux.Core

data Token = Arrow
            | Var String
            | Num Int32
            | Assign
            | Or
            | And 
            | Not
            | LShift
            | RShift
            deriving (Show, Eq, Ord)
type Command = (Token, Token, Token, Token)
type Env = [(Token, Int32)]

bound :: Int32 -> Int32
bound x
    | x > 65535 = bound (x - 65536)
    | x < 0     = bound (x + 65536)
    | otherwise = x

op :: Token -> (Int32 -> Int32 -> Int32)
op Or     = (\i j -> bound $ (.|.) i j)
op And    = (\i j -> bound $ (.&.) i j)
op Not    = (\i j -> bound $ complement i)
op LShift = (\i j -> bound $ shift i (fromIntegral j))
op RShift = (\i j -> bound $ shift i (-(fromIntegral j)))
op Assign = (\i j -> bound i)
op _      = error "not an op"

is_var :: Token -> Bool
is_var (Var _) = True
is_var _       = False

vars :: Command -> [Token]
vars (o, o1, o2, r) = filter is_var [o, o1, o2, r]

lvars :: Command -> [Token]
lvars (o, o1, o2, r) = filter is_var [o, o1, o2]

rvar :: Command -> Token
rvar (o, o1, o2, r) = head $ filter is_var [r]

tokenize :: String -> [Token]
tokenize s = tk (splitOn " " s)
    where
        tk :: [String] -> [Token]
        tk []            = []
        tk ("->":xs)     = Arrow:(tk xs)
        tk ("OR":xs)     = Or:(tk xs)
        tk ("AND":xs)    = And:(tk xs)
        tk ("NOT":xs)    = Not:(tk xs)
        tk ("LSHIFT":xs) = LShift:(tk xs)
        tk ("RSHIFT":xs) = RShift:(tk xs)
        tk (x:xs)
            | is_num x  = (Num (read x)):(tk xs)
            | otherwise = (Var x):(tk xs)

parse :: [Token] -> Command
parse (x:Arrow:y:[])       = (Assign, x, Num 0, y)
parse (o1:o:o2:Arrow:r:[]) = (o, o1, o2, r)
parse (o:o1:Arrow:r:[])    = (o, o1, Num 0, r)
parse xs                   = error ("parse failed on " ++ (show xs))

find_cmd :: [Command] -> Token -> Command
find_cmd [] _ = error "can't find command"
find_cmd (c:cs) t
    | t == (rvar c) = c
    | otherwise     = find_cmd cs t

eval :: [Command] -> Token -> Env -> Env
eval _ (Num n) env = (Num n, n):env
eval cmds t env  
    | (length env_results) > 0 = env
    | otherwise                = (t, (op o) ev_1 ev_2):env_2
    where 
        env_results = filter (\p -> fst p == t) env

        ev_1 = snd $ head $ filter (\p -> fst p == v1) env_2
        ev_2 = snd $ head $ filter (\p -> fst p == v2) env_2
        env_1 = eval cmds v1 env
        env_2 = eval cmds v2 env_1
        (o, v1, v2, _) = find_cmd cmds t

task_one :: String -> [String] -> Int32
task_one v strs = snd $ head $ filter (\p -> fst p == (Var v)) env
    where env = eval (map (parse . tokenize) strs) (Var v) []

main :: IO ()
main = flux_main id (task_one "a") (task_one "a")
