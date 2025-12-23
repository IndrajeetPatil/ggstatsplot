# ggstatsplot: ggplot2-Based Plots with Statistical Details

ggstatsplot is an R package that extends ggplot2 to create graphics with statistical test details included in the plots themselves. It provides functions for violin plots, scatterplots, histograms, pie charts, bar charts, and correlation matrices with embedded statistical information.

**Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.**

## Working Effectively

### Environment Setup
Install R and essential development tools:
```bash
sudo apt update && sudo apt install -y r-base r-base-dev
```

Install key R packages via system package manager (faster and more reliable):
```bash
sudo apt install -y r-cran-testthat r-cran-ggplot2 r-cran-dplyr r-cran-devtools
```

Set up user R library directory for additional packages:
```bash
mkdir -p ~/.R/library
echo 'R_LIBS_USER="~/.R/library"' >> ~/.Renviron
```

### Building the Package
**CRITICAL - Package builds are extremely fast:**
- `R CMD build . --no-build-vignettes` -- takes <1 second. NEVER CANCEL.
- `R CMD build .` -- takes <3 seconds with vignettes. NEVER CANCEL.

Build without vignettes (fastest, recommended for development):
```bash
R CMD build . --no-build-vignettes
```

### Testing
**Testing requires many dependencies that may not be available in all environments.**

The package has 13 test files covering all major functions:
```bash
R --slave -e 'library(testthat); test_dir("tests/testthat")'
```

**Alternative testing without dependencies (package structure validation only):**
```bash
# Check that test files exist and are properly structured
ls tests/testthat/test-*.R | wc -l  # Should show 13 test files
```

**Note:** Full testing requires packages like: correlation, datawizard, ggcorrplot, ggrepel, ggside, glue, insight, paletteer, parameters, patchwork, performance, purrr, rlang, statsExpressions. If dependencies are missing, tests will fail with clear error messages about missing packages.

### Package Check
Basic package check (without full dependencies):
```bash
R CMD check package_name.tar.gz --no-manual --no-tests --no-vignettes
```

Full package check (requires all dependencies):
```bash
R CMD check package_name.tar.gz
```

**Timing expectations (measured on Linux system):**
- Basic check: <1 second
- Full check with dependencies: 5-30 minutes depending on dependencies and system
- Package build: <1 second
- Clean operation: instantaneous

### Installing Dependencies
**WARNING: Dependency installation may fail due to network issues or missing packages.**

Using Makefile (preferred when it works):
```bash
make install_deps
```

Manual installation via R (fallback):
```bash
R --slave -e 'install.packages(c("remotes")); remotes::install_deps(dependencies = TRUE)'
```

**Note:** If CRAN mirrors are inaccessible, dependency installation will fail. The package can still be built and basic structure validated without all dependencies.

## Validation

### Code Style and Linting
**ALWAYS run before committing changes:**
```bash
# Check code style (if styler package available)
R --slave -e 'styler::style_pkg()'

# Run linter (if lintr package available)  
R --slave -e 'lintr::lint_package()'
```

Configuration files:
- `.lintr` - lintr configuration with custom rules
- `.pre-commit-config.yaml` - pre-commit hooks for automated checks

### Manual Testing Scenarios
**After making changes to core plotting functions, always test:**

1. **Basic plotting functionality:**
   ```r
   library(ggstatsplot)
   ggbetweenstats(mtcars, am, mpg)  # Test violin plots
   ggscatterstats(mtcars, wt, mpg)  # Test scatterplots  
   gghistostats(ggplot2::mpg, cty)  # Test histograms
   ```

2. **Statistical functions:**
   ```r
   p <- ggbetweenstats(mtcars, am, mpg)
   extract_stats(p)     # Should return statistical details
   extract_subtitle(p)  # Should return formatted subtitle
   ```

3. **Build and load package:**
   ```bash
   R CMD build . --no-build-vignettes
   R CMD INSTALL ggstatsplot_*.tar.gz
   ```

### GitHub Actions CI/CD
The repository has extensive automated testing via GitHub Actions:
- `R-CMD-check.yaml` - Multi-platform R CMD CHECK
- `test-coverage.yaml` - Test coverage analysis
- `lint.yaml` - Code linting
- `styler.yaml` - Code formatting checks

## Common Tasks

### Key File Locations
- **Main functions:** `R/ggbetweenstats.R`, `R/ggscatterstats.R`, `R/gghistostats.R`, etc.
- **Tests:** `tests/testthat/test-*.R`
- **Documentation:** `man/*.Rd` (auto-generated from roxygen comments)
- **Vignettes:** `vignettes/` and `vignettes/web_only/`
- **Data:** `data/` (includes sample datasets like `movies_long`, `iris_long`)

### Package Structure
```
ggstatsplot/
├── R/                    # Core function implementations
├── tests/testthat/       # Test suite using testthat framework
├── man/                  # Documentation (auto-generated)
├── vignettes/           # Package tutorials and examples
├── data/               # Sample datasets
├── .github/workflows/  # CI/CD pipeline definitions
├── DESCRIPTION        # Package metadata and dependencies
└── Makefile          # Build automation
```

### Common Commands Reference
```bash
# Quick package build
R CMD build . --no-build-vignettes

# Install package locally  
R CMD INSTALL package_name.tar.gz

# Check package
make check

# Clean build artifacts
rm -rf *.tar.gz *.Rcheck

# View package dependencies
grep -A 20 "Imports:" DESCRIPTION
```

### Development Workflow
1. Make code changes in `R/` directory
2. Update tests in `tests/testthat/` if needed
3. Build package: `R CMD build . --no-build-vignettes`
4. Install and test: `R CMD INSTALL package_name.tar.gz`
5. Run manual validation scenarios
6. Check code style and lint before committing

### Troubleshooting
- **Network issues:** If CRAN mirrors are inaccessible, you'll see "cannot open URL" warnings. Focus on code structure validation rather than full dependency testing
- **Missing dependencies:** Many advanced features require specific packages. Package structure and basic commands can be validated without all dependencies
- **Build failures:** Most common issues are missing system packages. Use `sudo apt install r-cran-packagename` to install R packages via system package manager
- **Permission errors:** If you see "lib is not writable" errors, ensure R user library is set up: `mkdir -p ~/.R/library && echo 'R_LIBS_USER="~/.R/library"' >> ~/.Renviron`

**Expected behavior in limited environments:**
- Package builds will succeed (takes <1 second)
- Basic R CMD check will show dependency errors but validate package structure  
- Tests will fail due to missing packages, but test files can be validated structurally
- Documentation generation will work for basic functions

**Remember:** This is a mature R package with extensive CI/CD testing. Focus on maintaining code quality and following existing patterns in the codebase.