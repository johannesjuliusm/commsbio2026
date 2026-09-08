## R Project Setup
Author: Johannes Julius Mohn
Contact: johannes.j.mohn@maxplanckschools.de

In the Terminal, clone the project from GitHub

```bash
git clone git@github.com:USERNAME/PROJECTNAME.git
```

Open the R project file `projectname.Rproj`

In the RStudio console, manually install the R environment manager package `renv`.
This is the only package you install manually!

```r
install.packages("renv")
```

In the RStudio console, restore the project environment from root.
This automatically installs all required packages in the correct version from `renv.lock`

```r
renv::restore()
```
