# CARS

This repository contains the source code for the processing, analysis and presentation of the Coding in Analysis and Research Survey (CARS) 2023. You can see the current state of the analysis on the [live site](https://best-practice-and-impact.github.io/CARS/).


# Setup

You will not be able to build the site without access to the survey data. However, key functionality can be found in the CARS package and re-used elsewhere.

This project can be installed locally by running this code with the project open in RStudio, or with the project root as the working directory (you will need to have [devtools](https://devtools.r-lib.org/) installed):

```
devtools::document()

devtools::install()
```

Alternatively, you can install the package directly from the github repository (you will need to have [remotes](https://cran.r-project.org/web/packages/remotes/index.html) installed):

```
install.packages("remotes")

remotes::install_github("best-practice-and-impact/CARS")
```

If you are working on the project and have access to the API, set up CARS_TOKEN and CARS_SECRET as environment variables. Do not do this within the repository. 

# Building the site

To generate the site, open the RProject file for the project run the script `main.R`. 

# Adding or modifying content

## Workflow

Key functionality should only be added or amended within the package code. Changes should be pushed to a new branch, be reviewed and pass all automated checks before being merged into the main branch.

## Quarto and HTML

The site content can be found in the quarto/ directory. To add new pages to the main site, add .qmd files to the quarto/main/ directory. Template pages should be added to the quarto/templates/ directory.

Remember to update the navigation bar when adding new pages, by editting the _site.yml file. To make these changes, add new settings under "left", for example: 

```
navbar:
  title: My Website
  left:
    - text: Home
      href: index.qmd
    - text: Page 1
      href: page1.qmd
```
where text refers to the name of the page and href refers to the relative path to the page's address. 

Styles for the main pages and the template filtered pages can be found in the `styles.css` file in quarto/main/.

