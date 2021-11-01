## mhealthannotator

<!-- badges: start -->
[![R-CMD-check](https://github.com/Sage-Bionetworks/mhealthannotator/workflows/R-CMD-check/badge.svg)](https://github.com/Sage-Bionetworks/mhealthannotator/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->


Mhealthannotator is an RShiny Package used for creating dashboard as a service that can easily visualize Files in Synapse Tables. The goal of this app is to let users be able to easily deploy apps for annotating/labelling data to collaborators more seamlessly. It's used in the mPower and Psorcast project to help create labelled data for analysis and validation to our unlabelled digital health datasets.

### Installation
```r
devtools::install_github("Sage-Bionetworks/mhealthannotator")
```
Notes on Installation:
All functionalities in mhealthannotator use reticulate and the [Synapse Python
client](https://pypi.org/project/synapseclient/). You can set up your environment by having a anaconda environment or a virtual environment set up with Synapseclient. Because mhealthannotator uses reticulate, it is not compatible with the [synapser](https://r-docs.synapse.org/) package..

### Labelling Files in Synapse Tables
Mhealthannotator provides functionality to help label Files in Synapse Tables:

- Able to iteratively parse files (any kind) in batches
- Able to run customized visualization function applied to each Files in Synapse Tables
- Able to be deployred for crowdsourced data labelling for reliability analysis

### References:

- [How to Use](https://sage-bionetworks.github.io/mhealthannotator/articles/how_to_mhealthannotator.html)
- [Schema Configuration](https://sage-bionetworks.github.io/mhealthannotator/articles/build_config_schema.html)
- [Deploying to Shiny Server](https://sage-bionetworks.github.io/mhealthannotator/articles/deploying_mhealthannotator.html)

### Requesting New Features or Bug Fixes
For new feature requests or bug fixes, please create an issue to let the maintainers know. If you’ve found a bug, create an associated issue and illustrate the bug with a minimal reprex. If there is agreement that the problem exists or that a new feature is desired, then the issue will be triaged for future development based on priority.

### Contributing
If you would like to contribute to mhealthannotator, please file an issue so that we can establish a statement of need, avoid redundant work, and track progress on your contribution. Once an issue has been filed and we've identified how to best orient your contribution with package development as a whole, [fork](https://docs.github.com/en/get-started/quickstart/fork-a-repo) the [main repo](https://github.com/Sage-Bionetworks/mhealthannotator), branch off a [feature branch](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow) from `develop`, [commit](http://git-scm.com/docs/git-commit) and [push](http://git-scm.com/docs/git-push) your changes to your fork and submit a [pull request](https://docs.github.com/en/github/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/creating-a-pull-request) for `Sage-Bionetworks/mhealthannotator:develop`.
