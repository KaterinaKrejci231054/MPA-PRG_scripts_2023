## Time complexity
### Task

The following algorithm, called SelectionSort, is a naive but simple iterative 
method to solve the Sorting problem. First, SelectionSort finds the
smallest element in *a*, and moves it to the first position by swapping it with
whatever happens to be in the first position (i.e., *a1*). Next, SelectionSort
finds the second smallest element in *a*, and moves it to the second position,
again by swapping with *a2*. At the *i*th iteration, SelectionSort finds the *i*th
smallest element in *a*, and moves it to the *i*th position.

Implement functions `IndexOfMin`, `SelectionSort` and `RecursiveSelectionSort`.

`IndexOfMin(array, first, last)` works with array and returns the index of
the smallest element between positions `first` and `last` by examining each
element from `array(first)` to `array(last)`.
```
IndexOfMin(array, first, last)
  index ← first
  for k ← first + 1 to last
    if array(k) < array(index)
      index ← k
  return index
```

Iterative version of SelectionSort
```
SelectionSort(a, n)
  for i ← 1 to n − 1
    j ← IndexOfMin(a, i, n)
    Swap elements a(i) and a(j)
  return a
```

Recursive version of SelectionSort
```
RecursiveSelectionSort(a, first, last)
  if first < last
    index ← IndexOfMin(a, first, last)
    Swap a(first) with a(index)
    a ← RecursiveSelectionSort(a, first + 1, last)
  return a
```

For each implemented function estimate the number of operations e.g. time complexity as a function of the size of 
the input. For `SelectionSort` and `RecursiveSelectionSort` estimate the upper bounds and decide which algorithm is
more efficient.


#### Download files from GitHub
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
git remote add upstream https://github.com/mpa-prg/exercise_03.git
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
