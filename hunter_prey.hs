module Hw2 where

import Data.List -- YOU MAY USE THIS MODULE FOR SORTING THE AGENTS

data Level = Newbie | Intermediate | Expert deriving (Enum, Eq, Ord, Show, Read)
data Hunter = Hunter {hID::Int, hlevel::Level, hEnergy::Int, hNumberOfCatches::Int, hActions::[Direction]} deriving (Eq, Show, Read)
data Prey = Prey {pID::Int, pEnergy::Int, pActions::[Direction]} deriving (Eq, Show, Read)
data Cell = O | X | H Hunter | P Prey | T deriving (Eq, Show, Read)
data Direction = N | S | E | W deriving (Eq, Show, Read)
type Coordinate = (Int, Int)
 -- DO NOT CHANGE THE DEFINITIONS ABOVE. --


 -- INSTANCES OF Ord FOR SORTING, UNCOMMENT AND COMPLETE THE IMPLEMENTATIONS --
 -- instance Ord Hunter where
 --    compare

 -- instance Ord Prey where
 --    compare



 -- WRITE THE REST OF YOUR CODE HERE --

travel :: [[Cell]] -> Int -> Int -> [(Cell, Coordinate)] -> [(Cell, Coordinate)]
travel z y x t                | x < length(z!!0)-1 && z!!y!!x/= O && z!!y!!x/= X && z!!y!!x/= T  = travel z y (x+1) ((z!!y!!x,(y,x)):t)
                              | x < length(z!!0)-1 && z!!y!!x== O = travel z y (x+1) t
                              | x < length(z!!0)-1 && z!!y!!x== X = travel z y (x+1) t
                              | x < length(z!!0)-1 && z!!y!!x== T = travel z y (x+1) t
                              | x < length(z!!0) && y < length(z)-1 && z!!y!!x/= O && z!!y!!x/= X && z!!y!!x/= T  = travel z (y+1) 0 ((z!!y!!x,(y,x)):t)
                              | x < length(z!!0) && y < length(z)-1 && z!!y!!x== O = travel z (y+1) 0 t
                              | x < length(z!!0) && y < length(z)-1 && z!!y!!x== X = travel z (y+1) 0 t
                              | x < length(z!!0) && y < length(z)-1 && z!!y!!x== T = travel z (y+1) 0 t
                              | x < length(z!!0) && y < length(z) && z!!y!!x/= O && z!!y!!x/= X && z!!y!!x/= T  = ((z!!y!!x,(y,x)):t)
                              | x < length(z!!0) && y < length(z) && z!!y!!x== O = t
                              | x < length(z!!0) && y < length(z) && z!!y!!x== X = t
                              | x < length(z!!0) && y < length(z) && z!!y!!x== T = t


seprat :: [(Cell, Coordinate)] -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
seprat a = ([x | x<-a , ishunternotprey (fst(x))],[x | x<-a , not (ishunternotprey (fst(x)))])


ishunternotprey :: Cell -> Bool
ishunternotprey (H x) = True
ishunternotprey (P y) = False


travelseprat :: [[Cell]] -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
travelseprat z = seprat (travel z 0 0 [])


move :: [[Cell]] -> (Cell, Coordinate) -> (Cell, Coordinate)
move z ((H (Hunter num level energy catches (acts@(t:ts)))),(x,y))  |  energy < 10                          = (H (Hunter num level (energy+1) catches ts),(x,y))

                                                                    |  t == N  && x == 0                    = (H (Hunter num level (energy-1) catches ts), (x,y))
                                                                    |  t == S  && x == length(z)-1          = (H (Hunter num level (energy-1) catches ts), (x,y))
                                                                    |  t == E  && y == length(z!!0)-1       = (H (Hunter num level (energy-1) catches ts), (x,y))
                                                                    |  t == W  && y == 0                    = (H (Hunter num level (energy-1) catches ts), (x,y))

                                                                    |  t == N  && z!!(x-1)!!y == T          = (H (Hunter num level (energy-1) catches ts), (x-1,y))
                                                                    |  t == S  && z!!(x+1)!!y == T          = (H (Hunter num level (energy-1) catches ts), (x+1,y))
                                                                    |  t == E  && z!!x!!(y+1) == T          = (H (Hunter num level (energy-1) catches ts), (x,y+1))
                                                                    |  t == W  && z!!x!!(y-1) == T          = (H (Hunter num level (energy-1) catches ts), (x,y-1))

                                                                    |  t == N  && z!!(x-1)!!y == X          = (H (Hunter num level (energy-1) catches ts), (x,y))
                                                                    |  t == S  && z!!(x+1)!!y == X          = (H (Hunter num level (energy-1) catches ts), (x,y))
                                                                    |  t == E  && z!!x!!(y+1) == X          = (H (Hunter num level (energy-1) catches ts), (x,y))
                                                                    |  t == W  && z!!x!!(y-1) == X          = (H (Hunter num level (energy-1) catches ts), (x,y))

                                                                    |  t == N                               = (H (Hunter num level (energy-1) catches ts), (x-1,y))
                                                                    |  t == S                               = (H (Hunter num level (energy-1) catches ts), (x+1,y))
                                                                    |  t == E                               = (H (Hunter num level (energy-1) catches ts), (x,y+1))
                                                                    |  t == W                               = (H (Hunter num level (energy-1) catches ts), (x,y-1))

move z ((P (Prey num energy (acts@(t:ts)))), (x,y))                 | energy < 10 && z!!x!!y == T                                  = (P (Prey num     0       ts), (x,y))
                                                                    | energy < 10                                                  = (P (Prey num (energy+1)  ts), (x,y))

                                                                    |  t == N  && x == 0 && z!!x!!y == T                           = (P (Prey num (energy-11) ts), (x,y))
                                                                    |  t == S  && x == length(z)-1 && z!!x!!y == T                 = (P (Prey num (energy-11) ts), (x,y))
                                                                    |  t == E  && y == length(z!!0)-1 && z!!x!!y == T              = (P (Prey num (energy-11) ts), (x,y))
                                                                    |  t == W  && y == 0 && z!!x!!y == T                           = (P (Prey num (energy-11) ts), (x,y))

                                                                    |  t == N  && x == 0                                           = (P (Prey num (energy-1)  ts), (x,y))
                                                                    |  t == S  && x == length(z)-1                                 = (P (Prey num (energy-1)  ts), (x,y))
                                                                    |  t == E  && y == length(z!!0)-1                              = (P (Prey num (energy-1)  ts), (x,y))
                                                                    |  t == W  && y == 0                                           = (P (Prey num (energy-1)  ts), (x,y))

                                                                    |  t == N  && z!!(x-1)!!y == T                                 = (P (Prey num (energy-11) ts), (x-1,y))
                                                                    |  t == S  && z!!(x+1)!!y == T                                 = (P (Prey num (energy-11) ts), (x+1,y))
                                                                    |  t == E  && z!!x!!(y+1) == T                                 = (P (Prey num (energy-11) ts), (x,y+1))
                                                                    |  t == W  && z!!x!!(y-1) == T                                 = (P (Prey num (energy-11) ts), (x,y-1))

                                                                    |  t == N  && z!!(x-1)!!y == X && z!!x!!y == T                 = (P (Prey num (energy-11)  ts), (x,y))
                                                                    |  t == S  && z!!(x+1)!!y == X && z!!x!!y == T                 = (P (Prey num (energy-11)  ts), (x,y))
                                                                    |  t == E  && z!!x!!(y+1) == X && z!!x!!y == T                 = (P (Prey num (energy-11)  ts), (x,y))
                                                                    |  t == W  && z!!x!!(y-1) == X && z!!x!!y == T                 = (P (Prey num (energy-11)  ts), (x,y))

                                                                    |  t == N  && z!!(x-1)!!y == X                                 = (P (Prey num (energy-1)  ts), (x,y))
                                                                    |  t == S  && z!!(x+1)!!y == X                                 = (P (Prey num (energy-1)  ts), (x,y))
                                                                    |  t == E  && z!!x!!(y+1) == X                                 = (P (Prey num (energy-1)  ts), (x,y))
                                                                    |  t == W  && z!!x!!(y-1) == X                                 = (P (Prey num (energy-1)  ts), (x,y))

                                                                    |  t == N                                                      = (P (Prey num (energy-1)  ts), (x-1,y))
                                                                    |  t == S                                                      = (P (Prey num (energy-1)  ts), (x+1,y))
                                                                    |  t == E                                                      = (P (Prey num (energy-1)  ts), (x,y+1))
                                                                    |  t == W                                                      = (P (Prey num (energy-1)  ts), (x,y-1))


moveall :: [[Cell]] -> ([(Cell, Coordinate)],[(Cell, Coordinate)]) -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
moveall z x = ([move z t | t<-fst(x)], [move z t | t<-snd(x)])


instance Ord Hunter where
                       compare (Hunter num level energy catches acts)  (Hunter num2 level2 energy2 catches2 acts2)                        | level /= level2       = compare level level2
                                                                                                                                          | energy /= energy2     = compare energy energy2
                                                                                                                                          | catches /= catches2   = compare catches catches2
                                                                                                                                          | otherwise             = compare num2 num

instance Ord Prey where
                      compare (Prey num energy acts) (Prey num2 energy2 acts2)                                                            | energy /= energy2     = compare energy energy2
                                                                                                                                          | otherwise             = compare num2 num






instance Ord Cell where
                    compare (H x) (H y) = compare x y
                    compare (P x) (P y) = compare x y


getcoordinate :: (Cell, Coordinate) -> Coordinate
getcoordinate (H(Hunter num level energy catches acts),(x,y)) = (x,y)
getcoordinate (P (Prey num energy acts), (x,y))               = (x,y)

coordomain :: [[Cell]] -> ([(Cell, Coordinate)],[(Cell, Coordinate)]) -> [(Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)])]
coordomain z (hunters, preys)  = [ (coor, reverse (sort [hunt | hunt <- hunters, getcoordinate hunt == coor]), sort [pre | pre <- preys, getcoordinate pre == coor]) | coor <- [(x, y) | x <- [0,1..(length(z)-1)], y <- [0,1..(length(z!!0)-1)] ]]

reversecoordomain2 :: [(Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)])] -> [(Cell, Coordinate)] -> [(Cell, Coordinate)] -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
reversecoordomain2 [] h j = (sort h,sort j)
reversecoordomain2 ((x,y,z):ts) h j = reversecoordomain2 ts (h++y) (j++z)


reversecoordomain :: [(Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)])] -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
reversecoordomain x = reversecoordomain2 x [] []


congrats :: (Cell, Coordinate) -> (Cell, Coordinate)
congrats ((H(Hunter num level energy catches acts)), h)  | energy >80      = ( H(Hunter num level 100 (catches+1) acts) , h)
                                                    | otherwise       =  (H(Hunter num level (energy+20) (catches+1) acts) , h)





eliminate2 :: (Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)]) -> [(Cell, Coordinate)] -> (Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)])
eliminate2 (x,yut@(y1:yr),zut@(z1:(z2:zr))) fullist               | length(yut) == 0 || length(zut) ==0   = (x,(yut++fullist),zut)
                                                                  | length(zut) == 1                      = (x,(((congrats y1):yr)++fullist),[])
                                                                  | otherwise                             = eliminate2 (x, yr, zr) ((congrats (congrats y1)):fullist)
eliminate2 (x, [], zut ) fullist  = (x,fullist,zut)
eliminate2 (x, yut, [] ) fullist  = (x,(yut++fullist),[])
eliminate2 (x, yut@(y1:yr), [zut]) fullist = (x,(((congrats y1):yr)++fullist),[])



eliminate :: (Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)]) -> (Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)])
eliminate x = eliminate2 x []

--The hunter with the highest priority will begin to catch the prey that is the lowest in the ordering, if multiple hunters and preys reside in the same cell.
elimination :: [(Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)])] -> [(Coordinate,[(Cell, Coordinate)],[(Cell, Coordinate)])]
elimination x = [eliminate a | a<-x ]


callthem :: [[Cell]] -> ([(Cell, Coordinate)],[(Cell, Coordinate)]) -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
callthem z a = reversecoordomain (elimination (coordomain z (moveall z a)))


alwayscall :: [[Cell]] -> ([(Cell, Coordinate)],[(Cell, Coordinate)]) -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
alwayscall z ((((H(Hunter num level energy catches acts)),b):xs),k)  | length(acts) == 0       = ((((H(Hunter num level energy catches acts)),b):xs),k)
                                                                     | length(k) == 0          = ((((H(Hunter num level energy catches acts)),b):xs),k)
                                                                     | otherwise               = alwayscall z (callthem z ((((H(Hunter num level energy catches acts)),b):xs),k))


simulate2 :: [[Cell]] -> ([(Cell, Coordinate)],[(Cell, Coordinate)])
simulate2 z = (alwayscall z (travelseprat z))

translate :: ([(Cell, Coordinate)],[(Cell, Coordinate)]) -> ([(Hunter, Coordinate)],[(Prey, Coordinate)])
translate (x,y) = ([celltohunt p | p<- x],[celltoprey p | p<- y])

translate2 :: ([(Hunter, Coordinate)],[(Prey, Coordinate)]) -> ([(Hunter, Coordinate)],[(Prey, Coordinate)])
translate2 (x,y) = ( [(a,(c,b)) | (a,(b,c)) <- x],[ (a,(c,b)) | (a,(b,c)) <- y])

celltohunt :: (Cell,Coordinate) -> (Hunter, Coordinate)
celltohunt (H x,k) = (x,k)

celltoprey :: (Cell,Coordinate) -> (Prey, Coordinate)
celltoprey (P x,k) = (x,k)


sorti :: ([(Hunter, Coordinate)],[(Prey, Coordinate)]) -> ([(Hunter, Coordinate)],[(Prey, Coordinate)])
sorti (a,b) = (reverse (sort a), sort b)

simulate :: [[Cell]] -> ([(Hunter, Coordinate)],[(Prey, Coordinate)])
simulate z = sorti (translate2 (translate (simulate2 z)))
