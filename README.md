
<!-- README.md is generated from README.Rmd. Please edit that file -->

# greta.template

<!-- badges: start -->

<!-- once you've signed into travis and set it to wath your new repository, you can edit the following badges to point to your repo -->

[![Build
Status](https://travis-ci.org/greta-dev/greta.template.svg?branch=master)](https://travis-ci.org/goldingn/greta.template)
[![Codecov test
coverage](https://codecov.io/gh/greta-dev/greta.template/branch/master/graph/badge.svg)](https://codecov.io/gh/greta-dev/greta.template?branch=master)
<!-- badges: end -->

This is an empty repository to help you build an extension to greta.
It’s a [GitHub template
repository](https://github.blog/2019-06-06-generate-new-repositories-with-repository-templates/),
so you can make a copy to build your extension from by following these
steps:

1.  Click *Use this template* to the left of *Clone or download*.
    ![template](https://i.imgur.com/TcLpIvM.png)
2.  Give this repo a new name (preferably `greta.<something>`) and
    create a new repo in your account.
3.  Clone the repository and start filling out all the fields to make
    the package your own. See Jenny Bryan’s [Happy Git with
    R](https://happygitwithr.com/push-pull-github.html) if you’d like
    some help with this.

**Please don’t hit the fork button**, that would make a permanent link
between your repository and this one\!

-----

#### to do list

Here’s a list of things to do once you’ve made a copy of the template,
to make sure the package is ship-shape. Maybe take a look at the
corresponding parts of [greta](https://github.com/greta-dev/greta) or
[tensorflow](https://github.com/rstudio/tensorflow) if you need
inspiration or text to copy and adapt\!

  - [ ] Pick a package name (preferably with ‘greta.’ at the beginning).
    Update the package name in:
      - [ ] the ‘Package’ field in `DESCRIPTION`
      - [ ] the `library()` and `test_check()` calls in
        `tests/testthat.R`
      - [ ] in the `@name` documentation field in `R/package.R`
      - [ ] at the top of `README.Rmd`
      - [ ] the repo name (if it changed since you made it)
      - [ ] the R project file; rename it from `greta.template.Rproj` to
        `greta.<something>.Rproj`
  - [ ] Come up with a helpful package title. Add it to:
      - [ ] the ‘Title’ field in `DESCRIPTION`
      - [ ] the `@title` documentation field in `R/package.R`
      - [ ] at the top of your GitHub repo
  - [ ] Fill in the the author details in `DESCRIPTION`
  - [ ] Update the ‘URL’ and ‘BugReports’ fields in `DESCRIPTION` to
    point to your repo
  - [ ] Decide what sort of license you want to use for your package
    (you are completely free to change the CC0 license in the template).
    See [https://choosealicense.com]() or
    [r-pkgs.org](https://r-pkgs.org/description.html#license) for help
    choosing.
      - [ ] create the license file and edit the ‘License’ field in
        `DESCRIPTION` (e.g. with [the `usethis`
        package](https://usethis.r-lib.org/reference/licenses.html))
  - [ ] Write short paragraph describing the package. Copy it to:
      - [ ] the ‘Description’ field of `DESCRIPTION`
      - [ ] the `@description` documentation field of `R/package.R`
  - [ ] Log in to [travis](https://travis-ci.org) (e.g. with your GitHub
    credentials) and turn on tracking of your new repo
  - [ ] edit the [travis](https://travis-ci.org) and
    [codecov](https://codecov.io) badges in `README.Rmd` to point to
    your package
  - [ ] Write a simple example introducing the package and add it to
    `R/package.R`
  - [ ] Update `README.Rmd` to sell to people with a sales pitch and
    maybe an example that creates a figure, to get people excited about
    your package.
  - [ ] Start adding functions, documentation and examples to new R
    files in `R` folder
  - [ ] Write some unit tests for these functions in the
    `tests/testthat` folder
  - [ ] Delete the example function and test files `R/square.R` and
    `tests/testthat/test-square.R`

-----

If you see something missing in this template or the to do list, please
let us know via the issues, or send a pull request. We’d really
appreciate it. This template comes with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree
to abide by its terms. We also hope people using the template will keep
the Code of Conduct with their package code.

p.s. thanks @karthik for the awesome template-template I shamelessly
copied from
[karthik/binder-test](https://github.com/karthik/binder-test)
