library(tidyverse)
library(docxtractr)

doc <- read_docx("UF_PT_PACKET.docx")
docx_tbl_count(doc)
docx_describe_tbls(doc)

effort_perc <- docx_extract_tbl(doc, 2)
saveRDS(effort_perc, "data/effort_perc.RDS")

summary_header <- head(names(docx_extract_tbl(doc, 5)), -3)
teaching_summary <- docx_extract_tbl(doc, 5, header = FALSE)
saveRDS(teaching_summary, "data/teaching_summary.RDS")

teaching_evals <- docx_extract_tbl(doc, 6, header = FALSE)
saveRDS(teaching_evals, "data/teaching_evals.RDS")

grad_committees <- docx_extract_tbl(doc, 8)
saveRDS(grad_committees, "data/grad_committees.RDS")

funding_internal <- docx_extract_tbl(doc, 22)
saveRDS(funding_internal, "data/funding_internal.RDS")

funding_external <- docx_extract_tbl(doc, 23)
saveRDS(funding_external, "data/funding_external.RDS")


