context("crypto")

test_that("new master key is at least 32 bytes", {
  res <- new_master_key()
  expect_true(length(res) >= 32L)
})

test_that("encrypt/decrypt and store file roundtrip works", {
  path <- tempfile()
  key <- new_master_key()
  encrypt_and_store_object(
    path = path,
    key = key,
    object = list(a = 1)
  )
  obj <- read_and_decrypt(path = path, key = key)
  expect_equal(yaml::yaml.load(obj), list(a = 1))
})
