create_master_key_file <- function() {
  master_key <- new_master_key()
  if (file.exists(MASTER_KEY_FILENAME)) {
    warning("master.key file already present in the current working directory.",
      " If you want to generate a new master key, delete the file and ",
      "call this function again.",
      call. = FALSE
    )
    return(invisible(FALSE))
  }
  file.create(MASTER_KEY_FILENAME)
  writeLines(
    text = sodium::bin2hex(master_key),
    con = MASTER_KEY_FILENAME,
    sep = "\n"
  )
  message("Created the master.key file. Never share this file or commit it to git.")
  if (has_package("usethis") && has_package("git2r") && is_git_repo()) {
    usethis::use_git_ignore(MASTER_KEY_FILENAME)
  }
  invisible(TRUE)
}

is_git_repo <- function() {
  git2r::in_repository()
}

has_package <- function(pkg_name) {
  requireNamespace(pkg_name, quietly = TRUE)
}

create_empty_encrypted_credentials_file <- function() {
  if (!file.exists(CREDENTIALS_FILENAME)) {
    message("Created the ", CREDENTIALS_FILENAME, " file. This is where your secrets are stored encryptedly. ")
    encrypt_and_store_object(
      path = CREDENTIALS_FILENAME,
      key = get_master_key(),
      object = list()
    )
  }
}
