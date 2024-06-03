# Synteny Blocks

## The Breakpoint Sort
Implement the algorithm for the breakpoint sort that consists of three functions. First function `FindSorted()` will
find an index where the unsorted part of the permutation starts e.i. it will mark the position of the first breakpoint. 
The second function `IndicateAscending()` will create a vector of the same length as the permutation and mark ascending 
parts by ones and descending parts by zeros. Finally, the last function `BreakPointSort()` will perform sorting by 
reversals.

### Task 1
 * In R, create a function `FindSorted()` to find an index, at which the unsorted part starts.

 * Input:
   * a permutation of integers e.g. `0 1 2 3 6 7 4 5 8`

 * Output:
   * a position where the unsorted part starts at e.g. `5`

> **Hint**: 
> Compare successively values of the permutation with an increasing number starting at zero (`0`, `1`, `2` ...) 
> and ending at length of the permutation - 1. The comparison ends when the value in permutation is not the same as 
> the tested value or when the tested value is equal to the length of the permutation - 1.

### Task 2
 * In R, create a function `IndicateAscending()` to mark ascending and descending parts of the permutation.

 * Input:
   * a permutation of integers e.g. `0 4 5 3 2 1 6 7 8`

 * Output:
   * a vector of zeros and ones, where ascending parts are marked by `1` and descending by `0` e.g. `1 1 1 0 0 0 1 1 1`

> **Hint:**
> Create an indication vector of the same length as the permutation containing only `0` values, and then set the first
> and last values to `1`. The ascending parts of the permutation vector will be marked with `1` values in the indication
> vector. Create a loop that iterates through the permutation and if two values next to each other are ascending, 
> i.e. the second is the first + 1, then the indication vector is set to `1` at the given indexes.


### Task 3
 * In R, create a function `BreakPointSort()` to sort a permutation using breakpoints.

 * Input:
   * permutation of integers e.g. `5 1 4 3 7 8 9 2 6`

 * Output:
   * sorted permutation of integers e.g. `1 2 3 4 5 6 7 8 9`

> **Hint:** Add marginal values to the permutation and the following steps are repeated in a loop:
>  * find the start of the unsorted region,
>  * mark ascending/descending parts,
>  * find the smallest value that is marked as descending part,
>  * reversal between the start of the unsorted region and the smallest value marked as descending part.
>
> The loop ends when the permutation is sorted. Watch out for collision situations i.e. no parts marked as descending 
> or there is a single value marked as descending in front of the sorted part of the permutation.


<details>
<summary>Download files from GitHub</summary>
<details>
<summary>Basic Git settings</summary>

> * Configure the Git editor
> ```bash
> git config --global core.editor notepad
> ```
> * Configure your name and email address
> ```bash
> git config --global user.name "Zuzana Nova"
> git config --global user.email z.nova@vut.cz
> ```
> * Check current settings
> ```bash
> git config --global --list
> ```
>
</details>

* Create a fork on your GitHub account. 
  On the GitHub page of this repository find a <kbd>Fork</kbd> button in the upper right corner.
  
* Clone forked repository from your GitHub page to your computer:
```bash
git clone <fork repository address>
```
* In a local repository, set new remote for a project repository:
```bash
git remote add upstream https://github.com/mpa-prg/exercise_08.git
```

#### Send files to GitHub
Create a new commit and send new changes to your remote repository.
* Add file to a new commit.
```bash
git add <file_name>
```
* Create a new commit, enter commit message, save the file and close it.
```bash
git commit
```
* Send a new commit to your GitHub repository.
```bash
git push origin main
```

</details>
