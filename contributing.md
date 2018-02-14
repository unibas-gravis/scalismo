## Contributor guide

This guide is for people who would like to contribute to [Scalismo](https://github.com/unibas-gravis/scalismo) or Scalismo related projects such as [Scalismo-faces](https://github.com/unibas-gravis/scalismo-faces). In the following we refer to the projects simply as Scalismo. In case you have reached this site from another project, all the guidelines will also apply to this particular project. 

## How to contribute

Scalismo uses the [fork and pull](https://help.github.com/articles/about-pull-requests/) method for contribution. This means, in order to contribute, you should fork the repository, implement your feature, and finally do a pull request back to the main repository. Scalismo follows the [git flow](http://nvie.com/posts/a-successful-git-branching-model/) branching model. This means, new developments should always originate from the current develop branch, while bug fixes go directly into the master or respective release branch. 

## What to contribute 

### Help with an existing task

You will find all the open tasks on the [project board](https://github.com/unibas-gravis/scalismo/projects/1). (If you are coming to this guide from another project, check the project board of the respective project). Let us know that you are working on an issue by assigning it to you. If you would like to contribute to an issue somebody else is already working on, get in contact by commenting on the corresponding issue. 

### Propose a new feature

If you would like to contribute a new feature to Scalismo, the best way is to open an issue. The issue should clearly describe the problem that the new feature is solving. *Before you work on the feature, you should seek consensus with other developers, to make sure that the proposed feature really addresses a problem, which should be addressed in the scope of the Scalismo project.*  

## Submitting the Pull Request

Before submitting the pull request, run ```sbt test ``` to make sure that all tests still pass. This command will also automatically reformat the code according to the project's standard. Please also make sure that the code is appropriately documented and tested (including automated unit tests).

Add at least one of the maintainers of Scalismo as a reviewer to your pull request. 



