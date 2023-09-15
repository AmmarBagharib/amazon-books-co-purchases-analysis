# amazon_graph_analysis

Then, to pull the repository to your local computer, open your terminal/ windows prompt, and in your home folder, create a folder called 'git', navigate to that folder with:
```
cd git
```
and run:
```
git clone https://github.com/AmmarBagharib/amazon_graph_analysis
```

in Mac, in your Terminal you can run `pwd` and it should show:

`(your home folder)/git/amazon_graph_analysis`

navigate to the project folder, and click on `amazon_graph_analysis.Rproj`

The above step should start RStudio and automatically move you into the project folder.

Once in RStudio, within your `console`, run:
```
install.packages("usethis")
```
AND
```
install.packages("renv")
```

Once done, run:
```
usethis::use_git()
```

and then run:
```
renv:init() #install all the packages in this RProject environment.
```

## Working with Git

1. Every time you want to start working on the project, navigate to `amazon_graph_analysis` file path on your computer and click on `amazon_graph_analysis.Rproj`

2. Ensure you **didn't make any changes you haven't already committed** to the repository. In Rstudio, you should be able to see the `Terminal` beside the `Console`. Click on `Terminal`, Run:
```
git status
```

3. Ensure your files are updated with the latest work on the repository. Run:
```
git pull
```

5. Upon working on your analysis, you should from time to time **check the changes** with running the following in your terminal:
```
git status
```

6. Every time you make small changes to a notebook file, testing a code/ model, commit your changes to github by doing these 2 steps:

#example: you would like to commit changes on a **single file** e.g., `eda.rmd`.Run:
```
git add eda.rmd
```

#example: you would like to commit changes on **multiple files** you have worked on. Run:
```
git add eda.rmd analysis.rmd modelling.rmd
```

#example: you would like to commit changes on **all the files** you have worked on. Run:
```
git add .
```

5. You have to add in your message as to what changes were made to notify the group members. Thus, you do so with:
```
git commit -m "input message here"
```

6. Earlier in step 2, we pulled the files from the repository. Now, we **push** our updated files onto the repository with:
```
git push
```

7. Close RStudio when you are finished with your project:
```
conda deactivate
```

# Note: NEVER do work on github on a browser, while working on your analysis on your IDE at the SAME TIME. Should you commit your work on both ends, there will be a merge conflict, because the files on your computer is different from the files on the github repo.

## License

The software provided in this project is offered under the MIT open
source license. Refer to the
[license](https://github.com/AmmarBagharib/amazon_graph_analysis/blob/master/LICENSE.md)
file for more information.







 
