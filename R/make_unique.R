#' Make the distractors of a numeric multiple choice question unique
#' @export
#' @param questions the numeric answer possibilities
#' @param solutions a logical vector specifying which one is correct
#' @param tolerance the tolerance to use, defaults to 0.05
#' @return a numeric vector of the same length of questions, with
#' `questions[solutions]` unchanged and `questions[-solutions]` altered
#' as needed so that any pair is at least `tolerance` apart.
make_unique <- function(questions, solutions, tolerance = 0.05) {
  ord <- order(questions)
  alter_me <- questions[ord]
  if (any(diff(alter_me) < tolerance)) {
    dtol <- tolerance*(-length(questions):length(questions))
    dontchange <- which(ord == which(solutions))
    alter_me <- alter_me + dtol[6-dontchange + seq_along(alter_me)]
  }
  # spit it out
  questions[ord] <- alter_me
  questions
}
