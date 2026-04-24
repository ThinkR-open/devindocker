testthat::context("build_volume_arg (#9)")

test_that("build_volume_arg renders a plain -v pair", {
  testthat::expect_equal(
    build_volume_arg("/host/path", "/container/path"),
    " -v '/host/path:/container/path'"
  )
})

test_that("build_volume_arg appends :delegated when requested (#9)", {
  testthat::expect_equal(
    build_volume_arg("/host", "/container", mode = "delegated"),
    " -v '/host:/container:delegated'"
  )
})

test_that("build_volume_arg accepts :cached and :consistent", {
  testthat::expect_equal(
    build_volume_arg("/a", "/b", mode = "cached"),
    " -v '/a:/b:cached'"
  )
  testthat::expect_equal(
    build_volume_arg("/a", "/b", mode = "consistent"),
    " -v '/a:/b:consistent'"
  )
})

test_that("build_volume_arg rejects unknown modes", {
  testthat::expect_error(
    build_volume_arg("/a", "/b", mode = "bogus"),
    "mode"
  )
})

test_that("NULL / NA mode is equivalent to omitting the mode", {
  plain <- build_volume_arg("/a", "/b")
  testthat::expect_equal(build_volume_arg("/a", "/b", mode = NULL), plain)
  testthat::expect_equal(build_volume_arg("/a", "/b", mode = NA), plain)
})
