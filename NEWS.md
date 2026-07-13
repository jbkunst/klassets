# klassets 0.1.0.9000

## Development version

# klassets 0.1.0

- Prepared a versioned release for Shinylive usage.
- Moved optional heavy dependencies (`earth`, `metR`, and `ranger`) from
  `Imports` to `Suggests`.
- Functions that need optional modeling packages now use
  `rlang::check_installed()`, which offers installation in interactive sessions
  and fails clearly otherwise.
- Logistic-regression plots now fall back to raster layers when `metR` is not
  installed.
- Removed stale GitHub `Remotes`; `parttree` is available from CRAN.

# klassets 0.0.0.9000

## Development version

- Added a `NEWS.md` file.
- Modernized pkgdown styling and reference organization.
- Updated GitHub Actions workflows for R CMD check and pkgdown.
- Improved package metadata wording.
