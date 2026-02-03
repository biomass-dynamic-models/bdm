# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME = `sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION`
PKGVERS = `sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION`


all: clean check

build: dependencies
	R CMD build .

check: build
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

dependencies: documentation
	Rscript \
	-e 'if (!requireNamespace("remotes")) install.packages("remotes")' \
	-e 'remotes::install_deps(dependencies = TRUE)'
    
documentation:
	Rscript \
	-e 'if (!requireNamespace("roxygen2")) install.packages("roxygen2")' \
	-e 'roxygen2::roxygenize()'

install: build
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

clean:
	@rm -rf $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck
	
site: install
	rm -r docs
	Rscript \
	-e 'if (!requireNamespace("pkgdown")) install.packages("pkgdown")' \
	-e 'pkgdown::build_site()' \
	-e 'setwd("docs"); x <- readLines("index.html"); x[111] <- "<h2 data-toc-skip>Homepage</h2><ul class=\"list-unstyled\"><li><a href=\"https://biomass-dynamic-models.github.io\" class=\"external-link\">BDM-fish</a></li></ul>"; writeLines(x, con = "index.html")'
	git add docs
	git commit -m "update pkgdown"
	git push origin main
