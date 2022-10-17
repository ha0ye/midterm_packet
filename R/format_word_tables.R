format_effort_perc <- function(df)
{
    dat_effort <- df
    m <- NROW(dat_effort)
    n <- NCOL(dat_effort)
    colnames(dat_effort) <- seq(n)

    dat_effort %>%
        flextable() %>%
        set_table_properties(layout = "autofit") %>%
        delete_part(part = "header") %>%
        add_header_row(values = dept_name, colwidths = n) %>%
        theme_box() %>%
        bg(i = 1, bg = "#D9D9D9", part = "header") %>%
        bg(j = c(1, n), bg = "#F2F2F2") %>%
        italic(i = 2:m, j = 1) %>%
        bold(i = 2:m, j = 1) %>%
        bold(i = 1, j = n) %>%
        align(align = "right", i = 2:m) %>%
        align(align = "center", part = "header") %>%
        fontsize(size = 9) %>%
        fontsize(size = 10, part = "header")
}

#'
#' Assumes df is extracted directly from the downloaded template. The first row
#' should have the headers, but padded with extra NA values, since the
#' originating table has header cells that span multiple columns.
#'
format_teaching_summary <- function(df)
{
    dat_teaching_summary <- df[-1,]
    m <- NROW(dat_teaching_summary)
    n <- NCOL(dat_teaching_summary)
    colnames(dat_teaching_summary) <- seq(n)

    header_vals <- gsub("Reqyes/no", "Req\nyes/no", df[1,])
    header_vals <- header_vals[!is.na(header_vals)]

    dat_teaching_summary %>%
        flextable() %>%
        set_table_properties(layout = "autofit") %>%
        delete_part(part = "header") %>%
        add_header_row(values = header_vals,
                       colwidths = c(rep.int(1, length(header_vals) - 3),
                                     rep.int(2, 3))
        ) %>%
        theme_box() %>%
        bold(i = 1) %>%
        align(align = "right", i = 2:m, j = c(3, 5:10)) %>%
        align(align = "center", i = 1) %>%
        align(align = "center", part = "header") %>%
        fontsize(size = 9) %>%
        fontsize(size = 10, part = "header")
}

format_teaching_eval <- function(df)
{
    dat_teaching_evals <- df[c(-1, -2, -3),]
    m <- NROW(dat_teaching_evals)
    n <- NCOL(dat_teaching_evals)
    colnames(dat_teaching_evals) <- df[3,]

    dat_teaching_evals %>%
        flextable() %>%
        set_table_properties(layout = "autofit") %>%
        add_header_row(values = df[2,1], colwidths = n) %>%
        add_header_row(values = df[1,1], colwidths = n) %>%
        theme_box() %>%
        align(align = "right", i = 1:m, j = 2:n) %>%
        align(align = "center", i = 3, part = "header") %>%
        color(color = "#FFFFFF", part = "header") %>%
        bg(bg = "#9D9D9D", part = "header") %>%
        italic(i = 2, part = "header") %>%
        fontsize(size = 9) %>%
        fontsize(size = 9, i = 3, part = "header") %>%
        fontsize(size = 10, i = c(1, 2), part = "header")
}

format_grad_committee <- function(df)
{
    if (NROW(df) == 0)
    {
        return(knitr::asis_output("None"))
    }

    stop("HAO, implement this!")
}

print_grants_table <- function(df)
{
    df %>%
        flextable() %>%
        set_table_properties(layout = "autofit") %>%
        theme_box() %>%
        align(align = "center", j = c(1, 2, 3, 5, 6), part = "all")
}

remove_empty_rows <- function(df)
{
    df %>%
        filter(!if_all(everything(), function(x) {x == ""}))
}
