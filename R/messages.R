# One-time-per-session informational messages. faSTM is a reimplementation, not
# a wrapper, so a couple of behaviours differ from stm in ways a user should know
# about even if they never read the README. These fire once per session and are
# silenced with options(faSTM.quiet = TRUE) (and by message = FALSE in vignettes).
.faSTM_state <- new.env(parent = emptyenv())

.message_once <- function(id, ...) {
  if (isTRUE(getOption("faSTM.quiet"))) return(invisible())
  if (isTRUE(.faSTM_state[[id]])) return(invisible())
  .faSTM_state[[id]] <- TRUE
  message(...)
}
