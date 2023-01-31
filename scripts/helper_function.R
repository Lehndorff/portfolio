#Clean up script
unlink("docs",recursive = T)
file.rename("_site","docs")
