
setwd(dir = '~/Application WUR/Declare Fellowship')

library(qpdf)

?qpdf


pdf_combine(input = c('Oluwafemi Motivation Letter.docx.pdf','Oluwafemi CV.pdf','Summary.pdf','OYEDELE  OLUWA  FEMI.pdf','Credentials.pdf','Prof Oluwasemire Recommendation.pdf','Reccommendation_Dr_Hauser.docx.pdf'),output = 'Oluwafemi Documents.pdf')
