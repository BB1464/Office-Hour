library(reticulate)

py_run_string("from pdf2docx import parse")

# path of pdf file
py_run_string("pdf_file = '~/../Desktop/IARSAF CALL FOR APPLICATION.pdf'")

# will create .docx in same path
py_run_string("docx_file = '~/../Desktop/IARSAF CALL FOR APPLICATION.docx'")

# Here is where we convert pdf to docx
py_run_string("parse(pdf_file, docx_file, start=0, end=None)")




library(RDCOMClient)

wordApp <- COMCreate("Word.Application")
wordApp[["Visible"]] <- TRUE
wordApp[["DisplayAlerts"]] <- FALSE
path_To_PDF_File <- "~/../Desktop/IARSAF CALL FOR APPLICATION.pdf"
path_To_Word_File <- "~/../Desktop/IARSAF CALL FOR APPLICATION.docx"

doc <- wordApp[["Documents"]]$Open(normalizePath(path_To_PDF_File),
                                   ConfirmConversions = FALSE)
doc$SaveAs2(path_To_Word_File)

