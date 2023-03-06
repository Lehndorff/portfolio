library(rmarkdown)
library(tidyverse)
library(purrr)

load('/Users/lehndorff/Desktop/MSDS/GSM_6027/portfolio/data/NEEA HEMS/analysis_data.rdata')

# sites<-read_csv("data/NEEA HEMS/sites.csv")

sites<-unique(analysis_data$ee_site_id)

site_analysis <- function(x) {
  render("~/Desktop/MSDS/GSM_6027/Other files/eulr_site_analysis.qmd", 
         # output_options=html_document(self_contained = TRUE,warnings = FALSE, messages = FALSE, echo = FALSE),
         params = list(this_site = x), 
         output_file = paste0("~/desktop/MSDS/GSM_6027/Other files/output/",x,".docx"))
}
sites %>% sample(5) %>% map(., function(x) { site_analysis(x)})

dir.create("docs/site_analysis")
file.copy(dir("~/desktop/MSDS/GSM_6027/Other files/output/",full.names = T),"docs/site_analysis",recursive = T)
