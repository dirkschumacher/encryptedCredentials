MASTER_KEY_FILENAME <- "master.key"
CREDENTIALS_FILENAME <- "credentials.yml.enc"

#' Setup encrypted credentials
#'
#' This creates an unencrypted master.key file and an empty encrypted
#' credentials file.
#'
#' @export
#' @include io.R
use_encrypted_credentials <- function() {
  create_master_key_file()
  create_empty_encrypted_credentials_file()
  invisible()
}

#' Read the encrypted credentials file
#'
#' @param credentials_filename the optional filename of the encrypted credentials file.
#'
#' @export
#' @include crypt.R
read_encrypted_credentials <- function(credentials_filename = "credentials.yml.enc") {
  content_decrypted <- read_and_decrypt(
    path = credentials_filename,
    key = get_master_key()
  )
  yaml::yaml.load(content_decrypted)
}

#' Update the encrypted credentials file
#'
#' @param object an object that is stored encrypted on disk. The object will be
#' converted to yaml using the yaml package. Please make sure the content can be
#' properly represented as yaml.
#' @param credentials_filename the optional filename of the encrypted credentials file.
#'
#' @export
#' @include crypt.R
write_encrypted_credentials <- function(object, credentials_filename = "credentials.yml.enc") {
  encrypt_and_store_object(
    path = credentials_filename,
    key = get_master_key(),
    object
  )
  message("It is recommended to restart your R session to remove any traces of data you just wrote to disk.")
  invisible()
}
