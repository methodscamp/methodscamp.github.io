# [methodscamp.github.io](https://methodscamp.github.io)

This repository contains the source materials for the website above (Methods Camp at UT's Department of Government).


## 🚀 Deployment instructions (for instructors)

- The website is a [Quarto book](https://quarto.org/docs/books/) published to GitHub pages [using GitHub Actions](https://quarto.org/docs/publishing/github-pages.html#github-action).
- This means that *any* changes to the repository will be automatically knitted/rendered by GitHub. For example, you can use your browser to edit any plain-text .qmd file on GitHub and, after a couple of minutes, the book will reflect the changes. Check out the "Actions" tab of this repository to track the progress of automated rendering.
- For more substantive changes, you should [clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) the repository (using e.g., CLI, GitHub Desktop, or [GitKraken](https://help.gitkraken.com/gitkraken-client/open-clone-init/)). After making local changes to the folder (for example, using RStudio), run `quarto preview` in your terminal to see how the book will look once rendered. Next, simply push any commits to this GitHub repository and GitHub Actions will take care of the online rendering.
- The [`script_setup.R`](script_setup.R) file has auxiliary code (e.g., pre-processes datasets, creates the zipped materials, etc.).

## 📝 [Instructor notes for future iterations](instructor_notes.md)
