context("api")

test_that("using an environment variable for the master key works", {
  key <- new_master_key()
  withr::with_envvar(c("R_ENCRYPTED_CRED_MASTER_KEY" = sodium::bin2hex(key)), {
    write_encrypted_credentials(list(a = 1))
    res <- read_encrypted_credentials()
    expect_equal(res, list(a = 1))
  })
})

test_that("using the master.key file works", {
  withr::with_dir(tempdir(), {
    use_encrypted_credentials()
    write_encrypted_credentials(list(a = 1))
    res <- read_encrypted_credentials()
    expect_equal(res, list(a = 1))
  })
})
