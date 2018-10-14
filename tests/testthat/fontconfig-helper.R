
# Appveyor does not require any configuration since FreeType 2.6.0 is
# automatically installed on this platform along with gdtools. However,
# Fontconfig builds a cache of all system fonts the first time it is run, which
# can take a while. It is a good idea to add the following in a
# fontconfig-helper.R testthat file in order to speed up the cache building on
# Appveyor and on CRAN’s Windows servers:

on_appveyor <- function() {
  identical(Sys.getenv("APPVEYOR"), "True")
}
on_cran <- function() {
  !identical(Sys.getenv("NOT_CRAN"), "true")
}

# Use minimal fonts.conf to speed up fc-cache
if (on_appveyor() || on_cran()) {
  gdtools::set_dummy_conf()
}
