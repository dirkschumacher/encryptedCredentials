new_master_key <- function() {
  sodium::random(32L)
}

encrypt_and_store_object <- function(path, key, object) {
  content <- charToRaw(yaml::as.yaml(object))
  nonce <- sodium::random(24L)
  ciphertxt <- c(
    sodium::bin2hex(sodium::data_encrypt(msg = content, key = key, nonce = nonce)),
    sodium::bin2hex(nonce)
  )
  file.create(path)
  writeLines(text = ciphertxt, con = path, sep = "\n")
  invisible()
}

read_and_decrypt <- function(path, key) {
  stopifnot(file.exists(path))
  content <- readLines(path)
  stopifnot(is.character(content), length(content) == 2L)
  nonce <- sodium::hex2bin(content[[2L]])
  ciphertxt <- sodium::hex2bin(content[[1L]])
  msg <- sodium::data_decrypt(ciphertxt, key, nonce)
  rawToChar(msg)
}

get_master_key <- function() {
  env_value <- Sys.getenv("R_ENCRYPTED_CRED_MASTER_KEY")
  key_str <- if (is.character(env_value) && !is.na(env_value) && nchar(env_value) >= 32L) {
    env_value
  } else if (file.exists(MASTER_KEY_FILENAME)) {
    readLines(MASTER_KEY_FILENAME)
  }
  stopifnot(is.character(key_str), !is.na(key_str), nchar(key_str) >= 32L)
  sodium::hex2bin(key_str)
}
