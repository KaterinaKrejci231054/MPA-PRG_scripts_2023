## Introduction to (R and) R/Bioconductor and regular expressions

### Task 1
Load the DNA sequence `fishes.fna.gz` using functions from the `seqinr` package and the `Biostrings` package. 
Note the differences between the created variables.

### Task 2
Next, focus on the `Biostrings` package. Practice working with loaded data:
1. Check the number of loaded sequences:
    ```R
    length(seq)
    ```
2. Determine the lengths of each sequence:
    ```R
    width(seq[1])
    ```
3. View the sequence names (FASTA headers):
    ```R
    names(seq)
    ````
4. Assign the first sequence including the name to the variable `seq1`:
    ```R
    seq1 <- seq[1]
    ```
5. Assign the first sequence without the name to the variable `seq1_sequence`:
    ```R
    seq1_sequence <- seq[[1]]
    ```
6. Assign the first sequence as a vector of characters to the variable `seq1_string`:
    ```R
    seq1_string <- toString(seq[1])
    ```
7. Learn more about the `XStringSet` class and the `Biostrings` package:
    ```R
    help(XStringSet)
    ```

### Task 3
Globally align the two selected sequences using the BLOSUM62 matrix, a gap opening cost of -1 and a gap extension cost of 1.


### Task 4
Practice working with regular expressions:
1. Create a list of names, e.g.:
    ```R
    names_list <-  c("anna", "jana", "kamil", "norbert", "pavel", "petr", "stanislav", "zuzana")
    ```
2. Search for name `jana`:
    ```R
    grep("jana", names_list, perl = TRUE)
    ```
3. Search for all names containing letter `n` at least once:
    ```R
    grep("n+", names_list, perl = TRUE)
    ```
4. Search for all names containing letters `nn`:
    ```R
    grep("n{2}", names_list, perl = TRUE)
    ```
5. Search for all names starting with `n`:
    ```R
    grep("^n", names_list, perl = TRUE)
    ```
6. Search for names `Anna` or `Jana`:
    ```R
    grep("Anna|Jana", names_list, perl = TRUE)
    ```
7. Search for names starting with `z` and ending with `a`:
    ```R
    grep("^z.*a$", names_list, perl=TRUE)
    ```

### Task 5
* Load an amplicon sequencing run from 454 Junior machine `fishes.fna.gz`.
* Get a sequence of a sample (avoid if conditions), that is tagged by forward and reverse MID `ACGAGTGCGT`. 
* How many sequences are there in the sample?

### Task 6
Create a function `demultiplexer()` for demultiplexing of sequencing data.

Function has four inputs:
* path to fasta file,
* a list of forward MIDs,
* a list of reverse MIDs,
* a list of samples labels.

The outputs of the function are:
* fasta files that are named after the samples and contain sequences of the sample without MIDs (perform MID trimming),
* table named `report.txt` containing samples‘ names and  the number of sequences each sample has.

Check the functionality again on the `fishes.fna.gz` file, the list of samples and MIDs can be found in the corresponding table `fishes_MIDs.csv`.


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
git remote add upstream https://github.com/mpa-prg/exercise_02.git
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
