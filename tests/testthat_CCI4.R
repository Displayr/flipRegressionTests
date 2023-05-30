if (identical(Sys.getenv("CIRCLECI"), "true"))
{
    test.files <- list.files("tests/testthat", pattern = "\\.R$")
    test.files <- gsub("test-|\\.R$", "", test.files)
    test.filter <- grep("cases[0-9]$|anova", test.files,
                        invert = TRUE, value = TRUE)
    test.filter <- paste0("^", test.filter, "$")
    if (!dir.exists("reports"))
        dir.create("reports")
    out.file <- paste0("reports/test_results", Sys.getenv("CIRCLE_NODE_INDEX"), ".xml")
    btime <- Sys.time()
    print(btime)
    exit.code <- flipDevTools::RunTestsOnCircleCI(filter = paste0(test.filter, collapse = "|"),
                                                  load_package = "none", output_file = out.file)
    print(Sys.time() - btime)
    q(status = exit.code, save = "no")
}
