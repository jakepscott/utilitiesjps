#' Convert to Colorful Title
#'
#' This function allows you to add the HTML needed for ggtext to highlight your title.
#' @param title Plain text title
#' @param keywords Word(s) in title you want to highlight
#' @param colors What color(s) you want to highlight the keyword(s) with
#' @keywords
#' @export
#' @examples
#' color_title(title = "This is a really cool package",
#'  keywords = c("really", "cool"),
#'  colors = c("red","blue"))


color_title <- function(title, keywords, colors){

  #Check to make sure everything is character
  if (!is.character(title)) {
    stop("Title must be character object")
  } else if (!is.character(keywords)){
    stop("Keyword(s) must be character object(s)")
  } else if (!is.character(colors)){
    stop("Color(s) must be character object(s)")
  }


  # Split title into individual words
  split <- strsplit(x = title, split = " ")[[1]]

  # For each keyword, convert it to the html text (along with the specified color)
  # Find the key word in the title, and replace the keyword with the HTLM version
  for (i in 1:length(keywords)) {
    # Convert from keyword to HTML with color
    htlm_keyword <- paste0("<span style = 'color: ", colors[i], ";'>",keywords[i],"</span>")

    #Find which entry of the split title is the given keyword and replace it with HTML
    split[which(split==keywords[i])] <- htlm_keyword
  }

  # Make the title a single string
  result <- paste(split, collapse = " ")

  # Return
  return(result)
}


