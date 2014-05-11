{-
 This is a simple implementation of the BrainF*ck programming Language.
 To evaluate a program run , in GHCI, eval "program".
 In this implementation the "Tape" can be extended infinitely, but it does not wrap around.
-}

--Main Function
interpret store [] =  store
interpret store (p:program)
          | p == '>' = interpret (goForward store) program 
          | p == '<' = interpret (goBack store)    program
          | p == '-' = interpret (decrement store) program
          | p == '+' = interpret (increment store) program
          | p == '[' = loop store program
          | p == ']' = store
          | otherwise = error ("Invalid Character: " ++ (show p))

--Helper Functions For Loops
loop store program
          | isZero store = interpret store (leaveLoop program)
          | otherwise    = loop (interpret store program) program 

leaveLoop (p:program) 
                  |  p == ']' =  program 
                  |  p == '[' =  leaveLoop (leaveLoop program)
                  | otherwise =  leaveLoop program

-- Helper Functions
goBack (p:past, x, future) = (past, p , x:future)
goBack ([], x, future)   = ([] ,  x, future)

goForward (past, x, []) = (x:past, 0, [])
goForward (past, x, f:future) = (x:past, f, future)

increment (past, x, future) = (past, x+1, future)
decrement (past, x, future)   
                          | x >0 = (past,x-1, future)
                          | otherwise = (past,x, future)

isZero (past, x, future) = if x == 0 then True else False

