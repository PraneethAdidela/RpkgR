# RpkgR

## R Package Management for Regulated Environments

RpkgR is an R package designed to simplify package repository management in regulated environments. The package provides tools for dependency resolution, repository creation, repository validation, package installation, bundle management, deployment reporting, and repository health assessment.

## Features

* Create and manage local package repositories
* Resolve package dependencies
* Generate dependency manifests
* Validate repository completeness
* Assess repository health
* Build and install package bundles
* Generate deployment and repository reports
* Support reproducible package management workflows

## Installation

### Install from Source

```r
install.packages(
  "RpkgR_1.0.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

### Load Package

```r
library(RpkgR)
```

## Example Workflow

### Create a Repository

```r
repo_path <- tempdir()

pkgr_create_repo(
  repo_path = repo_path
)
```

### Review Dependencies

```r
pkgr_dependency_summary(
  package = "Tplyr",
  repo_path = repo_path
)
```

### Validate Repository

```r
pkgr_validate_repository(
  repo_path = repo_path
)
```

### Assess Repository Health

```r
pkgr_repository_health(
  repo_path = repo_path
)
```

## Main Functional Areas

### Repository Management

* pkgr_create_repo()
* pkgr_build_repo()
* pkgr_validate_repository()
* pkgr_repository_health()
* pkgr_repository_cleanup()

### Dependency Management

* pkgr_dependency_summary()
* pkgr_dependency_tree()
* pkgr_dependency_check()
* pkgr_dependency_gap()
* pkgr_required_files()

### Package Management

* pkgr_add_package()
* pkgr_remove_package()
* pkgr_update_package()
* pkgr_install_package()

### Reporting

* pkgr_repository_report()
* pkgr_deployment_report()
* pkgr_repository_snapshot()

## License

MIT License

## Author

Praneeth Adidela
