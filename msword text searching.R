
library(readtext)
doc.text <- readtext("C:/Users/amaher2/OneDrive - KPMG/Documents/IHPA/addenda/TN1354 Review of the musculoskeletal system Part 1 - ICD-10-AM_Version 4_Draft 2.docx")$text

# Split text into parts using new line character:
doc.parts <- strsplit(doc.text, "\n")[[1]]

# First line in the document- the name of the doc
document <- doc.parts[1]
document
# [1] "International Journal of Science and Research (IJSR)"

# Similarly we can extract some other parts from a header
docunumber <-  doc.parts[2]
docunumber

# Search for the B95:
b95loc <- grep("T87.60", doc.parts)
doc.parts[b95loc]


# Search for the Keyword
Keywords.loc <- grep("Keywords:", doc.parts)[1]

# The text in between these 2 keywords will be abstract text:
abstract.text <- paste(doc.parts[abstract.loc:(Keywords.loc-1)], collapse=" ")

# Same way we can get Keywords text:
Background.loc <- Keywords.loc + grep("1\\.", doc.parts[-(1:Keywords.loc)])[1]
Keywords.text <- paste(doc.parts[Keywords.loc:(Background.loc-1)], collapse=" ")
Keywords.text
# [1] "Keywords: Nephronophtisis, NPHP1 deletion, NPHP4 mutations, Tunisian patients"

# Assuming that Methods is part 2
Methods.loc <- Background.loc + grep("2\\.", doc.parts[-(1:Background.loc)])[1]
Background.text <- paste(doc.parts[Background.loc:(Methods.loc-1)], collapse=" ")


# Assuming that Results is Part 3
Results.loc <- Methods.loc- + grep("3\\.", doc.parts[-(1:Methods.loc)])[1]
Methods.text <- paste(doc.parts[Methods.loc:(Results.loc-1)], collapse=" ")

# Similarly with other parts. For example for Acknowledgements section:
Ack.loc <- grep("Acknowledgements", doc.parts)[1]
Ref.loc <- grep("References", doc.parts)[1]
Ack.text <- paste(doc.parts[Ack.loc:(Ref.loc-1)], collapse=" ")
Ack.text
# [1] "6. Acknowledgements We are especially grateful to the study participants. 
# This study was supported by a grant from the Tunisian Ministry of Health and 
# Ministry of Higher Education ...