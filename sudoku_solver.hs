-- Boris Ivanov, ComputerScience 2-1

import Data.List
import Data.Char
import System.Directory

{-
 * В цялата реализация, едно судоку се представя като списък от списъци от цели числа - [[Int]]. 
 * За удобство е дефиниран типът Sudoku = [[Int]].
-}
type Sudoku = [[Int]]

-- Въпросното судоку "от сорс кода"
sudoku :: Sudoku
sudoku = [
    [5,3,0,    0,7,0,    0,0,0],
    [6,0,0,    1,9,5,    0,0,0],
    [0,9,8,    0,0,0,    0,6,0],

    [8,0,0,    0,6,0,    0,0,3],
    [4,0,0,    8,0,3,    0,0,1],
    [7,0,0,    0,2,0,    0,0,6],

    [0,6,0,    0,0,0,    2,8,0],
    [0,0,0,    4,1,9,    0,0,5],
    [0,0,0,    0,8,0,    0,7,9]]


{-
 * Намира елемент в grid по дадена позиция
 *
 * @param grid судоку, от което взимаме елемента
 * @param i номер на ред в grid
 * @param j номер на стълб в grid
 * @return елементът на позиция (i,j) в grid
 -}
at :: Sudoku-> Int -> Int -> Int
at grid i j = 
    if i < 0 || j < 0 || i >= length grid || j >= length (head grid) 
        then -1
    else
        (grid !! i) !! j

{-
 * Проверява дали можем да запишем дадено число в конкретния ред на судокуто, според правилата на играта 
 *
 * @param grid конкретното судоку
 * @param i номер на ред в grid
 * @param val стойността, която проверяваме дали може да се запише в i-тия ред
 * @return true ако можем да запишем val на този ред, false в противен случай
 -}
checkRow :: Sudoku -> Int -> Int -> Bool
checkRow grid i val = not (elem val (grid !! i))

{-
 * Проверява дали можем да запишем дадено число в конкретния стълб на судокуто, според правилата на играта 
 *
 * @param grid конкретното судоку
 * @param j номер на стълб в grid
 * @param val стойността, която проверяваме дали може да се запише в j-тия стълб
 * @return true ако можем да запишем val в този стълб, false в противен случай
 -}
checkCol :: Sudoku -> Int -> Int -> Bool
checkCol grid j val = not (elem val (transpose grid !! j))

{-
 * Проверява дали може да се добави val в конкретното квадратче 3х3, което се определя от позицията (i,j), според правилата на играта
 * Квадратчето е това, в което се намира елементът на позиция (i,j)
 *
 * @param grid конкретното судоку
 * @param i номер на ред в grid
 * @param j номер на стълб в grid
 * @param val стойността, която проверяваме дали може да се запише в 3х3 квадратчето, определено от позицията (i,j)
 * @return true ако можем да запишем val в това квадратче, false в противен случай
 -}
checkSquare :: Sudoku -> Int -> Int -> Int -> Bool
checkSquare grid i j val = not (elem val [at grid m n| m <- [0 .. 8], n <- [0 .. 8], div m 3 == div i 3, div n 3 == div j 3 ])

{-
 * Връща ново судоку, но елементът на позиция (i,j) става равен на newVal
 *
 * @param i номер на ред в (x:xs)
 * @param j номер на стълб в (x:xs)
 * @param newVal новата стойност на елемента на позиция (i,j)
 * @param (x:xs) входното судоку
 * @return (x:xs), но елементът на позиция (i,j) става равен на newVal
 -}
update :: Int -> Int -> Int -> Sudoku -> Sudoku
update i j newVal (x:xs)
    | (x:xs) == [] = []
    | i == 0 = updateNth j newVal x : xs
    | otherwise = x : update (i-1) j newVal xs
    where
    -- Приема като аргумент позиция n, стойност newVal и списък от цели числа и връща нов списък от цели числа, но елементът на позиция n е равен на newVal
    updateNth :: Int -> Int -> [Int] -> [Int]
    updateNth n newVal (x:xs)
        | (x:xs) == [] = []
        | n == 0 = newVal:xs
        | otherwise = x : updateNth (n-1) newVal xs

{-
 * Намира празните клетки на конкретното судоку
 *
 * @param grid конкретното судоку
 * @return списък от наредени двойки (i,j), където (i,j) е позиция на празна клетка в grid
 -}
emptyCells :: Sudoku -> [(Int,Int)]
emptyCells grid = [(i,j) | i <- [0 .. 8], j <- [0 .. 8], at grid i j == 0]

{-
 * От тук започва процесът по решаване на судокуто
 * Ако няма повече празни клетки, то grid е валидно решено судоку и връщаме списък с единствен елемент grid.
 * В противен случай се извиква функцията possibleSolutions с аргументи grid и първата позиция на празна клетка в grid.
 * По този начин, чрез possibleSolutions се генерират всички възможни запълвания и съответно, ако стигнем до невалидно судоку, не го добавяме към решението.
 *
 * @param grid входното судоку
 * @return списък от всички решения на grid. В частност, ако няма решения - празен списък.
 -}
solve :: Sudoku -> [Sudoku]
solve grid = 
    if emptyCells grid == [] then [grid]
    else possibleSolutions grid (head (emptyCells grid))

{-
 * Запълва празната клетката (i,j) с всички възможни стойности, получени от possibleValues
 * и продължава нататък със запълването като извиква процедурата solve за всяка възможна стойност.
 * На дъното на рекурсията, ако има валидно судоку - това е едно решение.
 * Ако за полученото на някоя стъпка судоку, possibleValues връща празен списък, то това е невалидно судоку,
 * тъй като все още имаме празни позиции
 *
 * @param grid входното судоку
 * @param (i,j) позиция на конкретната празна клетка в grid, която ще запълним
 * @return списък от всички решения на grid, а ако няма решение на конкретното входно судоку - празен списък
 -}
possibleSolutions :: Sudoku -> (Int, Int) -> [Sudoku]
possibleSolutions grid (i,j) = [solution | val <- possibleValues grid (i,j), solution <- solve (update i j val grid)]

{-
 * Намира възможните стойности, които могат да се запишат на позиция (i,j)
 *
 * @param grid конкретното судоку
 * @param (i,j) позиция на празна клетка в grid
 * @return списък от числата, които могат да се запишат в тази клетка според правилата на играта
 -}
possibleValues :: Sudoku -> (Int, Int) -> [Int]
possibleValues grid (i,j) = [val | val <- [1 .. 9], checkRow grid i val && checkCol grid j val && checkSquare grid i j val]

{-
 * Превръща цифра в String
 *
 * @param x цифра
 * @return String
 *
 * toStr 3 -> "3"
 -}
toStr :: Int -> String
toStr x 
    | x >= 0 && x <= 9 = [chr (ord '0' + x)] ++ " "
    | otherwise = "z"

{-
 * Превръща дадено судоку в приятен за принтиране в конзолата вариант
 *
 * @param grid входното судоку
 * @param i ред в grid за рекурсивно итериране на grid. При първо повикване е = на 0
 * @param j стълб в grid за рекурсивно итериране на grid. При първо повикване е = на 0
 * @param result държи полученият до конкретната итерация резултат. При първо повикване е ""
 * @return String в приятен формат, готов за директно отпечатване по-късно в конзолата
 -}
sudokuToString :: Sudoku -> Int -> Int -> String -> String
sudokuToString grid i j result
    | i == 9 = result
    | j == 9 && (i == 2 || i == 5) = sudokuToString grid (i+1) 0 (result ++ "\n-------------------\n")
    | j == 9 = sudokuToString grid (i+1) 0 (result ++ "\n")
    | j == 2 || j == 5 = sudokuToString grid i (j+1) (result ++ (toStr (at grid i j)) ++ "|")
    | otherwise = sudokuToString grid i (j+1) (result ++ (toStr (at grid i j)))

{-
 * Взима всички решения на дадено судоку и принтира първото от тях
 *
 * @param solutions списък от решения на едно судоку
 * @return Ако има решения, отпечатва първото, ако няма - известява за това потребителя
 -}
printFirstSolution :: [Sudoku] -> IO ()
printFirstSolution solutions = 
    if solutions == [] then putStrLn("No solutions")
    else putStr (sudokuToString (head solutions) 0 0 "")

{-
 * Следват помощни функции за прочитане на судоку от файл
 * В "sudoku.txt" се допускат всякакви форматиращи символи
 * Единственото условие е да има точно 81 цифри, празните клетки да се отбелязват с 0
-}

{-
 * Превръща даден символ в Int
 *
 * @param ch символ, който ще превърнем в Int
 * @return цяло число, съответстващо на ch, или -1, ако ch не е цифра
 -}
toInt :: Char -> Int
toInt ch 
    | ch >= '0' && ch <= '9' = ord ch - ord '0'
    | otherwise = -1

{-
 * Маха всички елементи, които не са цифрите [0,9] и превръща низа в списък от цели числа
 *
 * @param str низ, прочетен от файла "sudoku.txt"
 * @return списък от всички числа в судокуто
 -}
convertToValidList :: String -> [Int]
convertToValidList str = filter (\ x -> x /= -1) (map toInt str)

{-
 * Превръща низ във валидно Sudoku
 *
 * @param str низ, прочетен от файла "sudoku.txt"
 * @return валидно судоку
 -}
stringToSudoku :: String -> Sudoku
stringToSudoku str = toGrid (convertToValidList str) where
    toGrid lst
        | lst == [] = []
        | otherwise = [(take 9 lst)] ++ toGrid (drop 9 lst)

main = do
    fileExists <- doesFileExist "sudoku.txt"
    if fileExists then do
        fileSudoku <- readFile "sudoku.txt"
        putStrLn "To solve the sudoku from the source code type: 0 \nTo solve the sudoku from the file 'sudoku.txt' type: 1"
        y <- getLine
        if y == "0" then printFirstSolution (solve sudoku)
        else printFirstSolution (solve (stringToSudoku fileSudoku))
    else printFirstSolution (solve sudoku)
