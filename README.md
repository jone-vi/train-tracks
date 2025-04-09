## Explaination of Tracks Puzzle
- You have a grid of squares with a starting and an ending point. Connect the start and end with a track that does not cross itself.
- Clues tell you how many track segments are in each line and column.
- Some track segments are also given as clues.
- Each puzzle has exactly one solution.

## How it is solved?
Firstly the puzzle is parsed from the text-file. It is transformed to a grid, a list of the rows.
Afterward it is created different usefull structures: List of columns, list of rows where each element also contains pointers to the adjecent elements and a list of all possible 2x2 blocks in the grid.

Then it will run a brute force algorithm towards the grid. Placing on track at a time, starting from the upper left field. It will then place either a track or leave the field empty. If the track placed breaks a rule with the element over it or to the left it will try another track. If it validates it will move on to the next element.
For example if a track enters/exits in the "noth" but the track over it does not enter/exit in the "south" it breaks a rule.

Each time a full row has been filled it will validate the amount of elements according to the clue for that row. The same with columns.

Each time it breaks a rule Prolog will backtrack and try a new combination of elements. This means that the code can solve almost any puzzle, but it will spend a lot of time doing so. The work with this program was to optimize it to be able to detect impossible solutions as early as possible.

## Run the program
In the unsolved.txt file you should write some unsolved solvable puzzle. 

```
puzzles 2
size 4x4
3 4 2 3
_ _ _ _ 3
_ _ _ _ 3
╝ _ _ _ 4
╔ _ _ _ 2
size 5x5
2 3 3 5 4
_ _ _ _ _ 2
_ _ _ _ _ 4
╝ _ _ _ _ 5
_ _ _ _ _ 4
_ _ ╔ _ _ 2
```
The first line states the total number of puzzles in the file. The first description for each puzzle is its size. 
Then the puzzle is described line by line, where _ indicates an unspecified square and the track segments are (UTF8 box drawing symbols): ═, ║, ╔, ╗, ╚, ╝. The clues for the number of track symbols are in the first line of each puzzle and at the end of each line.

To run the program you should use [SWI-Prolog](https://www.swi-prolog.org/)

```
swipl PuzzleSolver.pro
```
