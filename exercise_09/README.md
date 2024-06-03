# *De Novo* Genome Assembly

## The Shortest Common Superstring
### Task
 * In R, implement a function `GreedySuperstring()` according to the pseudocode.

 * Input:
   * a `DNAStringSet` of reads

* Output:
   * a `DNAStringSet` of the final contig

> **Hint:** 
> Create also functions:
>  * `Overlap()` to calculate overlap between two sequences
>  * `OverlapMatrix()` to create a matrix of overlaps among all sequences in `S`

```
GreedySuperstring(S)
1   while length of S > 1
2     overlapMat <- OverlapMatrix(S)
3     seq1, seq2 <- Two sequences from S with the longest overlap
4     Merge seq1 and seq2 and add the new sequence to S
5     Remove seq1 and seq2 from S
6   return S
```


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
git remote add upstream https://github.com/mpa-prg/exercise_09.git
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
