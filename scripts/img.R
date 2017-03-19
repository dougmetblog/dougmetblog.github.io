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

# Detect right type of image
is_local_img <- function(img, pattern = "blogspot\\.com") {
  src <- rvest::html_attr(img, "src")
  grepl(pattern, src)
}

# Gets all the standalone images
local_img <- function(html, pattern = "blogspot\\.com") {
  
  imgs <- html_nodes(html, "img")
  imgs[vapply(imgs, is_local_img, logical(1), pattern = pattern)]
}


# Detect a tag with right type of image inside
is_local_img_a <- function(a, pattern = "blogspot\\.com") {
  
  imgs <- html_nodes(a, "img")
  imgs <- imgs[vapply(imgs, is_local_img, logical(1), pattern = pattern)]
  if(length(imgs)>1) {
    stop("An a tag had more than one image!")
  }
  isTRUE(length(imgs)==1)
}

# Gets all the a tags with images inside
local_img_a <- function(html, pattern = "blogspot\\.com") {
  
  a <- html_nodes(html, "a")
  a[vapply(a, is_local_img_a, logical(1), pattern = pattern)]
}



# Start of real script ----------------------------------------------------

# Read in html
bpost <- read_html(html_file)

# First process a tags with images inside
a <- bpost %>% local_img_a()

# Then process images that are on their own (left overs)
imgs <- bpost %>% local_img()


# Something like this
img <- imgs[[1]]
local <- download_image(img, root, assets)
xml2::xml_attr(img, "src") <- local
