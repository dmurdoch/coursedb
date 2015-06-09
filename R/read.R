listTables <- function(conn = DBconn()) {
      dbListTables(conn)
}

showTable <- function(table, conn = DBconn()) {
      dbGetQuery(conn, paste("select * from", table))
}

readStudents <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from students")
}

readAssignments <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from assignments")
}

readMCAnswers <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from mcAnswers")
}

readLongformGrades <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from longformGrades")
}

readClassParticipation <- function(conn = DBconn()) {
      dbGetQuery(conn, "select * from classParticipation")
}

readAllInfo <- function(conn = DBconn()) {
      a <- dbGetQuery(conn, "select * from students")
      b <- dbGetQuery(conn, "select * from assignments")
      c <- dbGetQuery(conn, "select * from mcAnswers")
      d <- dbGetQuery(conn, "select * from longformGrades")
      e <- dbGetQuery(conn, "select * from classParticipation")
      return(c(a,b,c,d,e))
}

## Name variants!!! LIKE in SQL?
# phonetic matching somehow?
# Used agrep() from R; now finds misspelled names too

NameToID <- function(givenNames = "deedoublenane", lastName = "cee") {
      IDs <- dbGetQuery(conn = DBconn(), 
                        "SELECT ID, lastName, givenNames 
                               FROM students")
      givenMatch <- agrep(givenNames, IDs$givenNames)
      lastMatch <- agrep(lastName, IDs$lastName)
      matchingIDs <- IDs[intersect(givenMatch, lastMatch),]
      if (nrow(matchingIDs) == 0) {warning("No matching IDs.")}
      if (nrow(matchingIDs) > 1) {warning("More than one ID.")}
      return(matchingIDs)
}

IDAndExamNumberToGrade <- function(ID = 111111111, examNumber = "2") {
      # 1         match ID to answers (mcAnswers) and grades (longformGrades)
      df <- as.data.frame(cbind(ID, examNumber))
      a <- dbGetPreparedQuery(conn = DBconn(),
                              "SELECT answer, questionNumber, questionValue, examCode 
                                    FROM mcAnswers AS m 
                                    WHERE m.ID = :ID 
                                    AND m.examNumber = :examNumber",
                              bind.data = df)
      if (length(unique(a$examCode)) > 1) {warning("Exam code not unique.")}      
      g <- dbGetPreparedQuery(conn = DBconn(),
                              "SELECT grade, questionNumber, examCode 
                                    FROM longformGrades AS l 
                                    WHERE l.ID = :ID 
                                    AND l.examNumber = :examNumber",
                              bind.data = df)
      if (length(unique(g$examCode)) > 1) {warning("Exam code not unique.")}
      # 2     compare answers to correct answers, grades to total grades
      df <- as.data.frame(cbind(examNumber, a$examCode[1])); colnames(df) <- c("examNumber", "examCode")
      ca <- dbGetPreparedQuery(conn = DBconn(),
                               "SELECT answer, questionNumber, questionValue 
                                    FROM mcAnswers AS m 
                                    WHERE m.ID = 999999999 
                                    AND m.examNumber = :examNumber 
                                    AND m.examCode = :examCode",
                               bind.data = df)
      tg <- dbGetPreparedQuery(conn = DBconn(),
                               "SELECT grade, questionNumber 
                                    FROM longformGrades AS l 
                                    WHERE l.ID = 999999999 
                                    AND l.examNumber = :examNumber 
                                    AND l.examCode = :examCode",
                               bind.data = df)
      # 3     tally total mark on test
      total <- (sum(as.integer(a$questionValue[a$questionNumber == ca$questionNumber 
                                              && a$answer == ca$answer])) 
               + sum(as.integer(g$grade)))
      outOf <- sum(as.integer(ca$questionValue)) + sum(as.integer(tg$grade))
      return(c(total, outOf))
}
# Do more in SQL to make more compact;
# Not seperate answers student/ correct; inner/ outer joins.


# Shortened function by doubling u on SQL query
IDToAssignmentMark <- function(ID = 111111111, assignmentNumber = "1") {
      # 1   Match ID and assignmentNumber to score (from assignments table)
      #     and get full score for assignment
      df <- as.data.frame(cbind(ID, assignmentNumber))
      a <- dbGetPreparedQuery(conn = DBconn(),
                              "SELECT grade, ID FROM assignments AS a 
                                    WHERE a.ID = :ID 
                                    AND a.assignmentNumber = :assignmentNumber
                                    OR a.ID = 999999999
                                    AND a.assignmentNumber = :assignmentNumber",
                              bind.data = df)
      a
      if (length(unique(a$assignmentNumber)) > 2) {stop("Assignment number not unique.")}
      # 2   Return vector: mark, fraction
      return(c(mark = a[1,1], outOf = a[2,1], fraction = a[1,1]/a[2,1]))
}


IDToAttendance <- function(ID = 111111111, date = Sys.Date(), attendanceMethod = "toDate") {
      # Attendance weighting default: % of dates so far attended; otherwise x/y(specified), capped at 1.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, attended 
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      attendedSoFar <- sum(unique(c$attended[c$date <= date])) # Number of dates attended to date
      if (is.numeric(attendanceMethod)) {
            potentialDates <- attendanceMethod
      } else {
            if (attendanceMethod != "toDate") {
                  warning("Invald attendanceMethod value; defaulting to 'toDate'.")
            } 
            potentialDates <- length(unique(c$date[c$date <= date]))
      }
      attendanceGrade <- min(attendedSoFar/potentialDates, 1)
}

IDToQuestionsAsked <- function(ID = 111111111, date = Sys.Date(), evalMethod = "percent") {
      #     Default evaluation method:
      #     Find the maximum number of questions asked.  
      #     Find the percentage of that maximum that the given ID asked. 
      #     Alternate: 100% if >= evalMethod (a number) questions were asked.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, questionAsked
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      qs <- sum(c$questionAsked[c$date <= date] != "")
      if (is.numeric(evalMethod)) {
            qGrade <- c(questionsAsked = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "percent") {warning("Invalid evalMethod; defaulting to 'percent'.")}
            allQs <- dbGetQuery(conn = DBconn(),
                                "SELECT ID, questionAsked 
                                FROM classParticipation as c
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAsked[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questionsAsked = qs, percentage = qs/potentialQs)
      }
      return(qGrade)
}


IDToQuestionsAnswered <- function(ID = 111111111, date = Sys.Date(), evalMethod = "percent") {
      #     Default evaluation method:
      #     Find the maximum number of questions answered.  
      #     Find the percentage of that maximum that the given ID answered.
      #     Alternate: 100% if >= evalMethod (a number) questions were answered.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, questionAnswered
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      qs <- sum(c$questionAnswered[c$date <= date] != "")
      if (is.numeric(evalMethod)) {
            qGrade <- c(questionsAnswered = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "percent") {warning("Invalid evalMethod; defaulting to 'percent'.")}
            allQs <- dbGetQuery(conn = DBconn(),
                                "SELECT ID, questionAnswered 
                                FROM classParticipation as c
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAnswered[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questionsAnswered = qs, percentage = qs/potentialQs)
      }
      return(qGrade)
}

IDToCombinedQuestions <- function(ID = 111111111, date = Sys.Date(), evalMethod = "percent") {
      #     Default evaluation method:
      #     Find the maximum number of questions asked and answered.  
      #     Find the percentage of that maximum that the given ID asked and answered.
      #     Alternate: 100% if >= evalMethod (a number) questions were asked and/ or answered.
      ID <- as.data.frame(ID)
      c <- dbGetPreparedQuery(conn = DBconn(), 
                              "SELECT date, questionAnswered, questionAsked
                              FROM classParticipation AS c
                              WHERE c.ID = :ID",
                              bind.data = ID)
      qs <- sum(c$questionAsked[c$date <= date] != "") + sum(c$questionAnswered[c$date <= date] != "")
      if (is.numeric(evalMethod)) {
            qGrade <- c(questions = qs, mark = 0, threshold = evalMethod)
            if (qs >= evalMethod) {qGrade[2] <- 1} 
      } else {
            if (evalMethod != "percent") {warning("Invalid evalMethod; defaulting to 'percent'.")}
            allQs <- dbGetQuery(conn = DBconn(),
                                "SELECT ID, questionAnswered, questionAsked
                                FROM classParticipation as c
                                ORDER BY c.ID")
            potentialQs <- qs
            for (i in unique(allQs$ID)) {
                  currentID <- sum(allQs$questionAsked[allQs$ID == i] != "") + sum(allQs$questionAnswered[allQs$ID == i] != "")
                  if (currentID > potentialQs) {potentialQs <- currentID}
            }
            qGrade <- c(questions = qs, percentage = qs/potentialQs)
      }
      return(qGrade)
}

IDToClassParticipation <- function(ID = 111111111, date = Sys.Date(), cpWeighting = c(0.5,0.5), attendanceMethod = "toDate", questionMethod = c("answer", "percent")) {
      # Weighting default: .5 attendance, .5 questions
      if (is.numeric(cpWeighting) != TRUE) {
            warning("IDToClassParticipation requires 'cpWeighting' to be a vector of two numbers; defaulting to c(0.5, 0.5).")
            cpWeighting <- c(0.5, 0.5)
      }
      a <- IDToAttendance(ID, date, attendanceMethod)
      if (questionMethod[1] == "ask") {
            q <- IDToQuestionsAsked(ID, date, questionMethod[2])
      } else if (questionMethod[1] == "both") {
            q <- IDToCombinedQuestions(ID, date, questionMethod[2])
      } else {
            if (questionMethod[1] != "answer") {
                  warning("questionMethod[1] is invalid; defaulting to 'answer'.")
            }
            q <- IDToQuestionsAnswered(ID, date, questionMethod[2])
      }
      
      return(a * cpWeighting[1] + q[2] * cpWeighting[2])
}

AssignmentMarks <- function(ID = 111111111, date = Sys.Date()) {
      a <- readAssignments()
      uniqueA <- unique(a$assignmentNumber[a$date <= date])
      aMarks <- matrix(ncol = 2, nrow = length(uniqueA))
      colnames(aMarks) = c("mark", "outOf")
      rownames(aMarks) = uniqueA
      for (i in uniqueA) {
            aMarks[i,] <- IDToAssignmentMark(ID, i)
      }
      return(aMarks)
}

TestMarks <- function(ID = 111111111, date = Sys.Date()) {
      allMC <- readMCAnswers()
      allLF <- readLongformGrades()
      uniqueT <- unique(c(unique(allMC$examNumber[allMC$date <= date])), unique(allLF$examNumber[allLF$date <= date]))
      tMarks <- matrix(ncol = 2, nrow = length(uniqueT))
      colnames(tMarks) = c("mark", "outOf")
      rownames(tMarks) = uniqueT
      uniqueT
      tMarks
      for (i in uniqueT) {
            tMarks[i,] <- IDAndExamNumberToGrade(ID, i)
      }
      for (i in nrow(tMarks):1) {
            if (is.na(tMarks[i,2]) || tMarks[i,2] == 0) {
                  tMarks <- tMarks[-i,]
            }
      }
      return(tMarks)
}

# Do it simpler.
# Different functions for different options.
# If you want to calculate this way, use this.

# Murdoch: After each test, write script that handles grading, entering into masterspreadsheet.

IDToCurrentGrade <- function(ID = 111111111, totalWeighting = c(0.2, 0, 0.3, 0.5), outOf = "all", cpWeighting = c(0.5, 0.5), attendanceMethod = "toDate", questionMethod = c("answer", "percent")) {
      # 1   Determine weighting (eg. assignments = .2, exams = .5)
      #                              Default:   20% Assignments
      #                                          0% Participation
      #                                         30% Tests
      #                                         50% Final
      #           Should default be out of all marks to date      "toDate", 
      #                 ALL possible marks                        "all", 
      #### Not implemented:   only the elements completed               "completed", 
      #                 or other specific elements                "other"?
      
      # 2   Fetch all the grades -- assignments, tests, participation, etc.
      
      # 2.1 Fetch assignments
      
      #toDate, "all", and "other" options
      if (outOf == "all") {
            outOf <- Sys.Date()
      }
      
      allA <- readAssignments()
      aMarks <- matrix(ncol = 2, nrow = length(unique(allA$assignmentNumber[allA$date <= outOf])))
      colnames(aMarks) = c("mark", "outOf")
      rownames(aMarks) = unique(allA$assignmentNumber[allA$date <= outOf])
      for (i in unique(allA$assignmentNumber[allA$date <= outOf])) {
            aMarks[i,] <- IDToAssignmentMark(ID, i)
      }
      aMarks
      
      # 2.2 Fetch tests
      
      allMC <- readMCAnswers()
      allLF <- readLongformGrades()
      tMarks <- matrix(ncol = 2, nrow = length(unique(c(unique(allMC$examNumber[allMC$date <= outOf])), unique(allLF$examNumber[allLF$date <= outOf]))))
      unique(c(unique(allMC$examNumber[allMC$date <= outOf])), unique(allLF$examNumber[allLF$date <= outOf]))
      colnames(tMarks) = c("mark", "outOf")
      rownames(tMarks) = unique(c(unique(allMC$examNumber[allMC$date <= outOf]), unique(allLF$examNumber[allLF$date <= outOf])))
      for (i in unique(c(unique(allMC$examNumber[allMC$date <= outOf]), unique(allLF$examNumber[allLF$date <= outOf])))) {
            tMarks[i,] <- IDAndExamNumberToGrade(ID, i)
      }
      for (i in nrow(tMarks):1) {
            if (!is.na(tMarks[i,2]) && tMarks[i,2] > 0) {
                  final <- i
                  break
            }
      }
      
      # 2.3 Fetch class participation
      
      cpMark <- IDToClassParticipation(ID = ID, date = outOf, cpWeighting = cpWeighting, attendanceMethod = attendanceMethod, questionMethod = questionMethod)
      
      # 3   Weight grades according to weighting function.
      
      aw <- sum(aMarks[,1])/sum(aMarks[,2]) * totalWeighting[1]
      cw <- cpMark * totalWeighting[2]
      tw <- sum(tMarks[-final,1], na.rm = TRUE) / sum(tMarks[-final,2]) * totalWeighting[3]
      ew <- tMarks[final,1]/tMarks[final,2] * totalWeighting[4]
      return(sum(aw, cw, tw, ew))
}

# What are my notes on this student?
IDToNotes <- function(ID = 111111111) {
      df <- as.data.frame(ID)
      
      dbGetPreparedQuery(conn = DBconn(), 
                         "SELECT notes FROM students AS s WHERE s.ID = :ID", 
                         bind.data = df)
}