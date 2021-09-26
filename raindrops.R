complement <- function(fn){
  function(args){
    result <- fn(args)

    !result
  }
}

modof <- function(a, b){
  a %% b
}

number_to_dropstring <- function(rs, idx){
  # print(idx, rs)
  string_idxs <- c("Pling", "Plang", "Plong")

  if (rs == 0)
    string_idxs[idx]
}

raindrops <- function(number) {
  cons_e <- c(3, 5, 7)

  result_of_mods <- mapply(function(b) modof(number, b), cons_e)

  result_applications <- mapply(number_to_dropstring, result_of_mods, c(1,2,3))

  result_filtered <- Filter(complement(is.null), result_applications)

  result_reduced <- Reduce(function(a, b) paste(a,b, sep = ""), result_filtered, accumulate = TRUE)

  result <- tail(result_reduced, n = 1)

  if (is.null(result))
    as.character(number)
  else
    result
}
