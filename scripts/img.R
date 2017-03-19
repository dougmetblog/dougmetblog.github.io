library(xml2)

# Parameters ---------------------------------------------------------------

root = "~/Web/jeknew2"
assets = "/assets/img"

# Functions ---------------------------------------------------------------

download_image <- function(nd, root, assets, attrib = "src") {
  # Grab the URL
  url <- xml2::xml_attr(nd, attrib)
  # The last two paths are what we want
  url_split <- unlist(strsplit(url, split = "/"))
  url2 <- tail(url_split,2)
  # Relative to assets location
  localurl <- file.path(assets, paste(url2, collapse = "/"))
  # Create the directory to put it in
  pth <- file.path(root, assets, url2[1])
  dir.create(pth, recursive = TRUE, showWarnings = FALSE)
  # Download and link the new url
  download.file(url, file.path(root, localurl))
  xml2::xml_attr(nd, attrib) <- localurl
  # Return the URL incase they want it
  localurl
}

# Strip yaml header
split_yaml_html <- function(html_file) {
  
  hlines <- readLines(html_file, warn = FALSE)
  dashes <- cumsum(grepl("^---", hlines))
  yheader <- 1:which(dashes == 2)[1]
  tmpfile <- tempfile(fileext = ".html")
  on.exit(unlink(tmpfile))
  
  writeLines(hlines[-yheader], tmpfile)
  
  list(yaml = hlines[yheader],
       html = xml2::read_html(tmpfile))
}

# Get html as lines
get_html_lines <- function(hnode) {
  body <- xml2::xml_find_first(hnode, "//body")
  tmpfile <- tempfile(fileext = ".html")
  on.exit(unlink(tmpfile))
  write_xml(body, tmpfile)
  html_lines <- readLines(tmpfile, warn = FALSE)
  # Strip off the body tags
  html_lines[!grepl("body>$", html_lines)]
}


# The main script ---------------------------------------------------------

# Read in
html_file <- "~/Web/jeknew2/_posts/2012-04-25-renormalisation-group.html"

bpost <- split_yaml_html(html_file)

# Parse
imgs <- xml_find_all(bpost$html, "//a/img[contains(@src, 'blogspot')]")
aimgs <- xml_find_all(bpost$html, "//a/img[contains(@src, 'blogspot')]/ancestor::a")

# Something like this
locals <- sapply(imgs, download_image, root = root, assets = assets, attrib = "src")
localsa <- sapply(aimgs, download_image, root = root, assets = assets, attrib = "href")

bpost$html_parsed <- get_html_lines(bpost$html)

# Write out
writeLines(c(bpost$yaml, "", bpost$html_parsed), "test.html")
