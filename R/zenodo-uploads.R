stop("To prevent accidental sourcing of this script.")

# pak::pak("steno-aarhus/ukbAid")
library(ukbAid)

# Protocol ----------------------------------------------------------------

path <- here::here("doc/protocol/protocol.qmd")
metadata <- zen_create_protocol_metadata(path)
client <- zen_create_file_record(
  path = here::here("doc/protocol/protocol.pdf"),
  metadata = metadata,
  token = zen_get_token(),
  # Test it in the sandbox first
  sandbox = FALSE
)
