library(rvest)
library(stringr)
library(xml2)
library(purrr)

root = "~/Web/jeknew2"
assets = "/assets/img"

html_file <- "~/Web/jeknew2/_posts/2012-04-25-renormalisation-group.html"

download_image <- function(img, root, assets) {
  
  path <- rvest::html_attr(img, "src")
  path_split <- unlist(strsplit(path, split = "/"))
  path2 <- tail(path_split,2)
  local <- file.path(assets, paste(path2, collapse = "/"))
  pth <- file.path(root, assets, path2[1])
  dir.create(pth, recursive = TRUE, showWarnings = FALSE)
  download.file(path, file.path(root, local))
  local
}

is_local_img <- function(img, pattern = "blogspot\\.com") {
  src <- rvest::html_attr(img, "src")
  grepl(pattern, src)
}

bpost <- read_html(html_file)

imgs <- bpost %>%
  html_nodes("img")

img_src <- imgs %>%
  html_attr("src")


img <- imgs[[1]]
if(is_local_img(img, "blogspot.com")) {
  # Download the image and return local path
  local <- download_image(img, root, assets)
  # Update the source
  xml2::xml_attr(img, "src") <- local
}
  