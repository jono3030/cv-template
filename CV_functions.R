suppressMessages(library(jsonlite))
suppressMessages(library(tidyverse))
suppressMessages(library(purrr))

#setwd(dirname(file.choose())) # Uncomment to set the pwd when working on the functions
json.table <- 'CV_data.json'
df <- data.frame(fromJSON(txt=json.table))

chosenDfName <- "taro"
titleSpacingValue <- -1.4
hspaceHeader <- 1

vspace_title <- function(spaceVar) {
  sprintf("\\vspace{%sem}", spaceVar)
}

sectionsFunc <- function(titleVar, spaceVar=titleSpacingValue) {
  paste(c(
    paste("##", titleVar, sep=" "),
    vspace_title(spaceVar),
    "\\hrulefill"
  ), collapse = "\n\n")
}

getAllNames <- function(dfName=chosenDfName) {
  tmpList <- c()
  for (i in 1:length(df[[dfName]]$names)) {
    tmpList[[i]] <- df[[dfName]]$names[[i]]
  }
  return(capture.output(cat(tmpList)))
}

name <- function(whichName="all", dfName=chosenDfName) {
  if (whichName == "all") {
    return(sprintf("{\\huge \\textbf{%s}}", getAllNames()))
  } else if (whichName == "first") {
    whichName <- "firstName"
  } else if (whichName == "middle") {
    whichName <- "middleName"
  } else if (whichName == "last") {
    whichName <- "lastName"
  } else if (whichName == "firstLast") {
    first <- df[[dfName]]$names$firstName
    last <- df[[dfName]]$names$lastName
    return(sprintf("{\\huge \\textbf{%s %s}}", first, last))
  }
  name <- df[[dfName]]$names[[whichName]]
  return(name)
}

lastUpdated <- function(dateVar="current") {
  if (dateVar == "current") {
    returnVar <- format(Sys.Date(), '%B %d, %Y')
  } else {
    returnVar <- dateVar
  }
  return(sprintf("{\\small \\textcolor{gray}{\\textit{Last updated:} %s}}", returnVar))
  #{\small \textcolor{gray}{\textit{Last updated:} `r format(Sys.Date(), '%B %d, %Y')`}}
}

getAllProfessions <- function(dfName=chosenDfName) {
  tmpList <- c()
  for (i in 1:length(df[[dfName]]$profession)) {
    tmpList[[i]] <- df[[dfName]]$profession[[i]]
  }
  returnString <- capture.output(cat(paste(tmpList, collapse=" | ")))
  return(returnString)
}

profession <- function(whichProfession="all", dfName=chosenDfName) {
  nrProfessions <- length(df$taro$profession)
  if (whichProfession != "all") {
    profession <- df[[dfName]]$profession[[whichProfession]]
  } else {
    profession <- getAllProfessions()
  }
  return(sprintf("\\textbf{%s}", profession))
}

address <- function(dfName=chosenDfName) {
  street <- df[[dfName]]$address$street
  addition <- df[[dfName]]$address$addition
  city <- df[[dfName]]$address$city
  postalCode <- df[[dfName]]$address$postalCode
  state <- df[[dfName]]$address$state
  country <- df[[dfName]]$address$country
  sprintf("%s, %s, %s, %s, %s, %s",
          street,
          addition,
          city,
          postalCode,
          state,
          country)
}

convertAddressIcon <- function(hspaceVar=hspaceHeader, endSpace=F, dfName=chosenDfName) {
  city <- df[[dfName]]$address$city
  icon <- df[[dfName]]$address$icon
  if (endSpace == TRUE) {
    sprintf("%s \\hspace{-.%sex} %s \\hspace{0ex}", icon, hspaceVar, city)
  } else {
    sprintf("%s \\hspace{-.%sex} %s", icon, hspaceVar, city)
  }
}

convertEmail <- function(type=1, emailAddress=1, whichIcon=2, endSpace=F, dfName=chosenDfName) {
  email <- df[[dfName]]$email[[type]][[emailAddress]]
  icon <- df[[dfName]]$email[[type]][[whichIcon]]
  if (endSpace == TRUE) {
    sprintf("%s \\hspace{-.1ex} \\href{mailto:%s}{%s} \\hspace{0ex}", icon, email, email) 
  } else {
    sprintf("%s \\hspace{-.1ex} \\href{mailto:%s}{%s}", icon, email, email) 
  }
}

convertIconLinks <- function(name, whichLinkName=1, whichLink=2, whichIcon=3, endSpace=F, dfName=chosenDfName) {
  link <- df[[dfName]]$links[[name]][[whichLink]]
  icon <- df[[dfName]]$links[[name]][[whichIcon]]
  linkName <- df[[dfName]]$links[[name]][[whichLinkName]]
  if (endSpace == TRUE) {
  sprintf("%s \\hspace{-.1ex} \\href{%s}{%s} \\hspace{0ex}", icon, link, linkName) 
  } else {
  sprintf("%s \\hspace{-.1ex} \\href{%s}{%s}", icon, link, linkName) 
  }
}

convertPhoneCanada <- function(type=1, phoneNumber=1, whichIcon=2, endSpace=F, dfName=chosenDfName) {
  number <- df[[dfName]]$phone[[type]][[phoneNumber]]
  icon <- df[[dfName]]$phone[[type]][[whichIcon]]
  nr <- unlist(strsplit(as.character(number),""))
  first <- paste(nr[1:3],collapse="")
  second <- paste(nr[4:6],collapse="")
  third <- paste(nr[7:length(nr)],collapse="")
  if (endSpace == TRUE) {
    sprintf("%s \\hspace{-.1ex} (%s) %s-%s \\hspace{0ex}", icon, first, second, third)
  } else {
    sprintf("%s \\hspace{-.1ex} (%s) %s-%s", icon, first, second, third)
  }
}

contactInfo <- function() {
  paste(c(convertEmail(),
        convertPhoneCanada(),
        convertAddressIcon(),
        convertIconLinks("github"),
        convertIconLinks("linkedin")),
        collapse = " \\hspace{0ex} ")
}

headerFunc <- function() {
  paste(c(
    "\\begin{center}",
    name(),
    lastUpdated(),
    vspace_header(),
    profession(),
    contactInfo(),
    vspace_header(),
    "\\end{center}"
  ), collapse = "\n\n")
}

convertSummary <- function(whichSummary=1, dfName=chosenDfName) {
  paste(c(
    "\\onehalfspacing",
    df[[dfName]]$summary[[whichSummary]],
    "\\singlespacing"
  ), collapse="\n")
}

skills <- function(whichType, whichSkill, key=FALSE, dfName=chosenDfName) {
  if (key == TRUE) {
    sprintf("\\textbf{%s}", df[[dfName]]$skills[[whichType]][[whichSkill]])
  } else {
    sprintf("- %s", df[[dfName]]$skills[[whichType]][[whichSkill]])
  }
}

degree <- function(degree, dfName=chosenDfName) {
  degreePrint <- df[[dfName]]$degrees[[degree]][1]
  fieldPrint <- df[[dfName]]$degrees[[degree]][2]
  datesPrint <- df[[dfName]]$degrees[[degree]][3]
  returnObject <- sprintf("**%s** in %s \\hfill %s", degreePrint, fieldPrint, datesPrint)
  return(returnObject)
}

institution <- function(degree, dfName=chosenDfName) {
  name <- df[[dfName]]$degrees[[degree]][4]
  city <- df[[dfName]]$degrees[[degree]][5]
  country <- df[[dfName]]$degrees[[degree]][6]
  sprintf("|     %s \\hfill \\textit{%s, %s}", name, city, country)
}

degreeFunc <- function(degreeVar) {
  paste(c(
    degree(degreeVar),
    "\n",
    vspace_under(),
    institution(degreeVar)
  ), collapse="\n")
}

workExperience <- function(work, dfName=chosenDfName) {
  name <- df[[dfName]]$workExp[[work]][1]
  dates <- df[[dfName]]$workExp[[work]][2]
  returnObject <- sprintf("**%s** \\hfill %s", name, dates)
  return(returnObject)
}

employer <- function(work, cityChoice=TRUE, dfName=chosenDfName) {
  employer <- df[[dfName]]$workExp[[work]][3]
  city <- df[[dfName]]$workExp[[work]][4]
  country <- df[[dfName]]$workExp[[work]][5]
  if (cityChoice == TRUE) {
    returnObject <- sprintf("*%s* \\hfill \\textit{%s, %s}", employer, city, country)
  } else {
    returnObject <- sprintf("*%s* \\hfill \\textit{%s}", employer, country)
  }
  return(returnObject)
}
  
tasks <- function(work, task, dfName=chosenDfName) {
  tasks <- df[[dfName]]$workExp[[work]][[task]]
  sprintf("- %s", tasks)
}

tasksSelect <- function(work, whichTask, dfName=chosenDfName) {
  task <- df[[dfName]]$workExp[[work]]$taskList[[whichTask]]
  sprintf("- %s", task)
}

tasksList <- function(work, dfName=chosenDfName) {
  for (i in df[[dfName]]$workExp[[work]]$taskList) {
    cat("-", i, "\n")
  }
}

tasksListFunc <- function(whichWork, dfName=chosenDfName){
  tasksLength <- length(df[[dfName]]$workExp[[whichWork]]$taskList)
  itemList <- c()    
  for (i in 1:tasksLength) {
        itemList[[i]] <- paste0("- ", df[[dfName]]$workExp[[whichWork]]$taskList[[i]])
  }
  return(itemList)
}

workExpFunc <- function(whichWorkVar) {
  paste(c(
    workExperience(whichWorkVar),
    "\n",
    vspace_under(),
    "\n",
    employer(whichWorkVar),
    "\n",
    vspace_under(),
    "\n",
    tasksListFunc(whichWorkVar)
  ), collapse="\n")
}

awards <- function(name, dfName=chosenDfName) {
  award <- df[[dfName]]$awards[[name]][1]
  year <- df[[dfName]]$awards[[name]][2]
  returnObject <- sprintf("**%s** \\hfill %s", award, year)
  return(returnObject)
}

awardsDescription <- function(name, choiceValue=TRUE, dfName=chosenDfName) {
  description <- df[[dfName]]$awards[[name]][3]
  value <- df[[dfName]]$awards[[name]][4]
  if (choiceValue == TRUE) {
    returnObject <- sprintf("- %s (*%s*)", description, value)
  } else {
    returnObject <- sprintf("- %s", description)
  }
  return(returnObject)
}

awardsValue <- function(name, dfName=chosenDfName) {
  value <- df[[dfName]]$awards[[name]][4]
  returnObject <- sprintf("*Total value: %s*", value)
  return(returnObject)
}

awardsFunc <- function(whichAward) {
  paste(c(
    vspace_over(),
    awards("taro"),
    vspace_under(),
    awardsDescription("taro")
  ), collapse="\n\n")
}

languages <- function(whichLanguage, dfName=chosenDfName) {
  language <- df[[dfName]]$languages[[whichLanguage]]$language
  level <- df[[dfName]]$languages[[whichLanguage]]$level
  returnObject <- sprintf("%s %s", language, level)
  return(returnObject)
}

firstNameFunc <- function(outerLoopVar, reference, dfName=chosenDfName) {
  lengthList <- lengths(strsplit(df[[dfName]]$publications[[reference]]$authors[[1]][outerLoopVar,1], " "))
  tmpList <- c()
  for (j in 1:lengthList) {
    tmpList[[j]] <- paste0(substr(paste0(map(strsplit(df[[dfName]]$publications[[reference]]$authors[[1]][outerLoopVar,1], " "), j)),1,1),
                    ".,")
  }
  return(capture.output(cat(tmpList)))
  }

authorNames <- function(reference, dfName=chosenDfName) {
  len <- dim(df[[dfName]]$publications[[reference]]$authors[[1]])[1] - 1
  lastItem <- dim(df[[dfName]]$publications[[reference]]$authors[[1]])[1]
  initialsList <- c()
    
  for (i in 1:len) {
    initialsList[[i]] <- paste0(df[[dfName]]$publications[[reference]]$authors[[1]][i,2],
                                ", ",
                                firstNameFunc(i, reference))
  }
  
  initialsList[lastItem] <- paste0("& ",
                                   df[[dfName]]$publications[[reference]]$authors[[1]][lastItem,2],
                                   ", ",
                                   substr(firstNameFunc(lastItem, reference),1,2))
  
  authorNames <- capture.output(cat(initialsList))
  return(authorNames)
}

#publications <- function(whichPublication, full=FALSE, abbr=FALSE, reviewChoice=FALSE, dfName=chosenDfName) {
#  highlightedAuthorName <- paste(paste0(name("last"),","), paste0(substr(name("first"),1,1),"."), sep = " ")
#  tmpdf <- df[[dfName]]$publications[[whichPublication]]
#  title <- tmpdf$title
#  author <- gsub(highlightedAuthorName, paste0("**", highlightedAuthorName, "**"), authorNames(whichPublication))
#  year <- tmpdf$year
#  journal <- tmpdf$journal
#  volume <- tmpdf$volume
#  number <- tmpdf$number
#  pages <- tmpdf$pages
#  doi <- tmpdf$doi
#  review <- "Currently under review"
#  if (full == TRUE) {
#    returnObject <- sprintf("%s (%s). %s, *%s*, *%s*(%s), %s.",
#                            author,
#                            year,
#                            title,
#                            journal,
#                            volume,
#                            number,
#                            pages
#                            )
#  } else if (abbr == TRUE) {
#    returnObject <- sprintf("%s (%s). %s, *%s*.",
#                            author,
#                            year,
#                            title,
#                            journal
#                            )
#  } else if (reviewChoice == TRUE) {
#    returnObject <- sprintf("%s (%s). %s. *%s*.",
#                            author,
#                            year,
#                            title,
#                            review
#                            )
#  }
#  return(returnObject)
#}

publications <- function(whichPublication, dfName=chosenDfName) {
  highlightedAuthorName <- paste(paste0(name("last"),","), paste0(substr(name("first"),1,1),"."), sep = " ")
  tmpdf <- df[[dfName]]$publications[[whichPublication]]
  title <- tmpdf$title
  author <- gsub(highlightedAuthorName, paste0("**", highlightedAuthorName, "**"), authorNames(whichPublication))
  year <- tmpdf$year
  journal <- tmpdf$journal
  volume <- tmpdf$volume
  number <- tmpdf$number
  pages <- tmpdf$pages
  doi <- tmpdf$doi
  review <- "Currently under review"
  formattingTypeVar <- tmpdf$formattingType
  if (formattingTypeVar == "full") {
    returnObject <- sprintf("%s (%s). %s, *%s*, *%s*(%s), %s.",
                            author,
                            year,
                            title,
                            journal,
                            volume,
                            number,
                            pages
                            )
  } else if (formattingTypeVar == "abbr") {
    returnObject <- sprintf("%s (%s). %s, *%s*.",
                            author,
                            year,
                            title,
                            journal
                            )
  } else if (formattingTypeVar == "reviewChoice") {
    returnObject <- sprintf("%s (%s). %s. *%s*.",
                            author,
                            year,
                            title,
                            review
                            )
  }
  return(returnObject)
}

publicationsFunc <- function(dfName=chosenDfName) {
  publicationsListLength <- length(df[[dfName]]$publications)
  
  itemList <- c()    
  for (i in 1:publicationsListLength) {
        itemList[[i]] <- paste0(i, ". ", publications(i))
  }
  
  paste(c(
    "\\onehalfspacing",
    "\n",
    itemList,
    "\n",
    "\\singlespacing"
  ), collapse="\n")
}

vspace_under <- function() {
  size <- "-.5"
  sprintf("\\vspace{%sem}", size)
}

vspace_over <- function() {
  size <- ".5"
  sprintf("\\vspace{%sem}", size)
}

vspace_header <- function(sizeVar=-.5) {
  size <- sizeVar
  paste(c(
    sprintf("\\vspace{%sem}", sizeVar),
    "\\hrulefill"
  ), collapse = "\n")
}
