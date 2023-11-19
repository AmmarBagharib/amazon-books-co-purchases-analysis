<img src="https://github.com/AmmarBagharib/amazon_graph_analysis/blob/master/photos/amazon-logo-1.png" width="400" height="130" />

# A deep dive into the Network of Amazon Book Co-Purchases

### [Read the Project Report HERE](https://github.com/AmmarBagharib/amazon_graph_analysis/blob/master/analysis/report.pdf)


# Authors:
- Ammar Bagharib
- Chen Yang
- Randy Ng

## Replicate Analysis

### 1. Getting Github Repo onto local device
Run:
```
git clone https://github.com/AmmarBagharib/amazon_graph_analysis
```

### 2. Navigate to local project folder
then run:
```
cd amazon_graph_analysis
```

in Mac, in your Terminal you can run `pwd` and it should show:

`(your home folder)/git/amazon_graph_analysis`

### 3. Opening Project environment in RStudio

Now, in your Terminal/ Windows Prompt run:
```
open amazon_graph_analysis.Rproj
```
**Alternatively**, you can open the project folder using Finder/ and manually click on `amazon_graph_analysis.Rproj` for the same results.

The above step should start RStudio and automatically move you into the project folder.

Once in RStudio, to install all the packages in this RProject environment, within Rstudio's `console`, run:

```
renv:init() 
```

You should see:
```
This project already has a lockfile. What would you like to do? 

1: Restore the project from the lockfile.
2: Discard the lockfile and re-initialize the project.
3: Activate the project without snapshotting or installing any packages.
4: Abort project initialization.
```

Hit the number associated with "Restore the project from the lockfile." For this example as seen above, we'll be typing `1`. Then hit 'Enter'. You should see RStudio now installing the packages within this RProj environment.

then run:
```
install.packages("usethis")
```
Once done, run:
```
usethis::use_git()
```

### 4. Replicate Analysis

You can run the scripts to generate the outputs needed by the report!

## License

The software provided in this project is offered under the MIT open
source license. Refer to the
[license](https://github.com/AmmarBagharib/amazon_graph_analysis/blob/master/LICENSE.md)
file for more information.







 
