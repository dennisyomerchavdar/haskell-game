# haskell-game
Haskell hunter-prey simulation
Homework for 2018 Programming Language Concepts course

## Definition
Check PDF file for problem definition
## Usage

ghci hunter_prey.hs
Then inside the interpreter, run
```simulate $scene_description$```  
Example run:  
```simulate [[P ( Prey 1 70 [E,W,E,W,E,W]),O,X,P (Prey 2 100 [N,N,W,S,E,E])],[T,O,H (Hunter 1 Expert 90 2 [E,S,N,E,S,W]),O],[H (Hunter 2 Newbie 70 0 [E,N,S,W,E,N]),O,X,O]]```

