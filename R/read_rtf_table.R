library(tidyverse)
library(docxtractr)

# Usage Directions
# 1. download template file from my.ufl.edu -> "My Self Service -> Faculty Promotion & Tenure -> UF Faculty Promotion & Tenure -> Promotion & Tenure Packet -> Packet Template with Activity"
# 2. open the `.rtf` file in MS Word and save it as a `.docx` file (potentially automated via some other package in the future)

doc <- read_docx("UF_PT_PACKET.docx")
num_tables <- docx_tbl_count(doc)
stopifnot(num_tables == 25)

docx_describe_tbls(doc)

#### extract effort table ----
# effort_perc <- docx_extract_tbl(doc, 2)
#
# # TODO: add checks on effort table - (specific headers in row 1)
#
# saveRDS(effort_perc, "data/effort_perc.RDS")

#### extract teaching summary table ----
summary_header <- head(names(docx_extract_tbl(doc, 5)), -3)
teaching_summary <- docx_extract_tbl(doc, 5, header = FALSE)

# TODO: add checks on teaching summary table - (specific headers in row 1)
num_courses <- sum(!(teaching_summary[[1]] %in% c("", "Course")))

saveRDS(teaching_summary, "data/teaching_summary.RDS")

#### extract teaching evals ----
teaching_evals <- docx_extract_tbl(doc, 6, header = FALSE)

# loop for number of courses, as counted from teaching_summary
# TODO: add checks on teaching evals table - (specific headers in row 1)

saveRDS(teaching_evals, "data/teaching_evals.RDS")

#### extract grad committees ----
grad_committees <- docx_extract_tbl(doc, 8)
# TODO: add checks on grad committees table - (specific headers in row 1)
saveRDS(grad_committees, "data/grad_committees.RDS")

# #### extract internal funding ----
# funding_internal <- docx_extract_tbl(doc, 22)
# saveRDS(funding_internal, "data/funding_internal.RDS")
#
# funding_external <- docx_extract_tbl(doc, 23)
# saveRDS(funding_external, "data/funding_external.RDS")


