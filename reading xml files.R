require(XML)
library("methods")
data <- xmlParse("C:/Users/amaher2/OneDrive - KPMG/Documents/IHPA/CLaML/ACHI XML Index Export - 12th Edition 17-10-2021.xml")
xml_data <- xmlToList(data)

df<-xmlToDataFrame("C:/Users/amaher2/OneDrive - KPMG/Documents/IHPA/CLaML/ACHI XML Index Export - 12th Edition 17-10-2021.xml")

# df<-as.data.frame(unlist(xml_data[4], recursive = FALSE))

# Extract the root node.
rootnode <- xmlRoot(data)
# number of nodes in the root.
nodes <- xmlSize(rootnode)

# get entire contents of a record
second_node <- rootnode[2]

# get 3rd attribute of 4th record
attri <- rootnode[[3]][[4]]

cat('number of nodes: ', nodes)
print ('details of 2 record: ')
print (second_node)

# prints the marks of the fourth record
print ('3rd attribute of 4th record: ', attr)
print(data)


















install.packages("xml2")

library(xml2)
x <- read_xml("C:/Users/amaher2/OneDrive - KPMG/Documents/IHPA/CLaML/ACHI XML Index Export - 12th Edition 17-10-2021.xml")


xml_name(x)
childs<-xml_children(x)
c4<-as.list(childs[[4]])

childs[[4]][21]

