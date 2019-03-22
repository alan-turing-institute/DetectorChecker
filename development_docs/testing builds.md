# Testing builds on multiple operating systems

We want to test the package on Mac OS, Linux and Windows. The following instructions details how to run tests on all these machines, assuming you are testing on a Mac OS machine without access to Windows or Linux.


## Testing on Mac

The simplest way to test the package is to use RStudio on that machine, open the package and then call

```devtools::check()``` 

in the console. This will run all the unit tests as well as a number of other checks on the package. 

Ideally we should have no errors, warnings or notes. Any of these will be a barrier to submitting on CRAN. 

If you have access to a Windows or Linux machine you can also do the above.




## Testing Windows and Linux without access to machine

### Linux

We are using Travis CI (https://travis-ci.com/alan-turing-institute/DetectorChecker). In the repository there is a file called `.travis.yml`. When a push is made to github travis will automatically test the package on a linux machine (running the Xenial version of the operating system). You can navigate to the link above to see whether the tests passed. If so, we are running successfully running on linux. 

### Windows

To test on windows we can use the devtools package. This works by bundling the package and uploading it to http://win-builder.r-project.org/

 Simply open the package in RStudio on the branch you wish to test and call one of the following in the console.

 1. To test on the latest development version of R:
 ```
 devtools::check_win_devel(pkg = ".", args = NULL, manual = TRUE, quiet = FALSE, ...)
 ```
2. To test on the current release version of R:
```
devtools::check_win_release(pkg = ".", args = NULL, manual = TRUE, quiet = FALSE, ...)
````
3. To test on the previous major release of R:
```
check_win_oldrelease(pkg = ".", args = NULL, manual = TRUE, quiet = FALSE, ...)
```


This will send an email to the package maintainer once the results are ready (takes ~20 min). The package maintainer is specified in the `DESCRIPTION` file in the main directory of the repository. In that file there is a section that looks like this:

```
Authors@R: c(
        person("Oscar", "Giles", email = "ogiles@turing.ac.uk", role = c("aut", "cre")),
        person("Tomas", "Lazauskas", email = "tlazauskas@turing.ac.uk", role = c("aut")),
        person("Wilfrid", "Kendall", email = "W.S.Kendall@warwick.ac.uk", role = c("aut")),
        person("Julia", "Brettschneider", email = "jabrettschneider@me.com", role = c("aut")))

```

To set yourself as the package maintainer, add `"cre"` to your role. 

For more information type ```??check_win_release``` in the RStudio console. 

