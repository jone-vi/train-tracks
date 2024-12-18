## Explaination of Tracks Puzzle
- You have a grid of squares with a starting and an ending point. Connect the start and end with a track that does not cross itself.
- Clues tell you how many track segments are in each line and column.
- Some track segments are also given as clues.
- Each puzzle has exactly one solution.

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
