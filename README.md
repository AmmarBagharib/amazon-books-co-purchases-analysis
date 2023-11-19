<img src="https://github.com/AmmarBagharib/amazon_graph_analysis/blob/master/photos/amazon-logo-1.png" width="400" height="130" />

## A Comprehensive Analysis of Singapore Hotel Reviews Pre and Post-COVID Era

### Read the Project Report <[HERE](https://github.com/AmmarBagharib/sg-hotels-sentiment-analysis/blob/main/analysis/report.pdf)>

Project done for the module, Network Analytics with Business Applications

National University of Singapore, Semester 1 AY2023/24
- Ammar Bagharib
- Chen Yang
- Randy Ng
  
Supervised by: Dr. Tan Tianhui 

# Overview

Established in 1995 as a pioneering online bookstore, Amazon has been a cherished destination for book enthusiasts for several decades. While the company has evolved and expanded into a diverse range of industries, it has consistently maintained its status as a go-to hub for book shoppers.

In the scope of this project, our objective is to delve into the evolving co-purchase patterns of books over time, understand it’s characteristics and underlying patterns. With this information, we’re looking to identify key products which drive purchasing behaviour among consumers, devise marketing strategies, as well as to assess the efficacy of incorporating network analytics into the development of product recommendation engines.

In our project, we used the amazon co-purchase dataset from Stanford University’s SNAP library. The dataset was compiled through an extensive crawl of the Amazon website in the summer of 2006, capturing a vast array of product metadata and review details for 548,552 diverse items, spanning categories such as Books, music CDs, DVDs, and VHS video tapes. It encompasses comprehensive information for each product, including titles, sales ranks, and genre/category information. In our analysis, we restricted the product scope to books only.

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

`(your home folder)/amazon_graph_analysis`

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







 
