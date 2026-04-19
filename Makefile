PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`

all: check

build: install_deps
	R CMD build .

check: build
	R CMD check --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

install_deps:
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'

install: build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck .local-lib README.html

update_deps:
	Rscript -e 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest")); usethis::use_tidy_description()'
	Rscript -e 'options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest")); usethis::use_latest_dependencies(source = "CRAN", overwrite = TRUE)'
	Rscript -e 'roxygen2::roxygenise()'
	Rscript -e 'codemetar::write_codemeta()'

document: build
	mkdir -p .local-lib
	R CMD INSTALL -l .local-lib $(PKGNAME)_$(PKGVERS).tar.gz
	Rscript -e '.libPaths(c(normalizePath(".local-lib"), .libPaths())); rmarkdown::render("README.Rmd")'

format:
	Rscript -e 'styler::style_pkg()'

lint:
	Rscript -e 'lintr::lint_package()'

hooks:
	prek run --all-files

hooks_install:
	prek install
