## Git and GitHub - Attendance list
### Task 1
Add a new empty text file named after you `name_surname.txt` to the source repository via pull request.


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

#### Create a pull request
On the GitHub page of your fork, you should be able to click on the <kbd>Contribute</kbd> button 
and then on the <kbd>Open pull request</kbd> button. Here check or edit the description of changes
and confirm it by clicking on the <kbd>Create pull request</kbd> button.

#### Get the latest version of source repository
```bash
git pull upstream main
```

### Task 2
Add your mood or write a short message to the text file named after you `name_surname.txt` in the source repository.

* In your local repository `attendance`, open a file named after you and add your mood today or write a short message.
* Create a new commit.
* Send the commit to your fork on GitHub.
* Open pull request.
* Wait for pull request merge and then download the latest version of project repository.
* Check commit history.
