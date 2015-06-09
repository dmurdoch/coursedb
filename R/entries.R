# Problem: entries saved unnecessarily as decimals


NewStudentEntry <- function(ID, email, lastName, givenNames, program, notes) {
      df <- data.frame(ID = ID, email = email, lastName = lastName, givenNames = givenNames, program = program, notes = notes)
      sql <- "INSERT INTO students VALUES (:ID, :email, :lastName, :givenNames, :program, :notes)" 
      dbGetPreparedQuery(conn, sql, bind.data = df)
}
# NewStudentEntry(ID = 111444777, 
#                 email = "ameining@uwo.ca", 
#                 lastName = "Meining", 
#                 givenNames = "Andrew", 
#                 program = "economics", 
#                 notes = "")
# readStudents()

NewAssignmentEntry <- function(ID = 999999999, assignmentNumber, date = Sys.Date(), grade) {
      df <- data.frame(ID = ID, assignmentNumber = assignmentNumber, date = date, grade = grade)
      sql <- "INSERT INTO assignments VALUES (:ID, :assignmentNumber, :date, :grade)" 
      dbGetPreparedQuery(conn, sql, bind.data = df)      
}
# NewAssignmentEntry(ID = 111444777,
#                    assignmentNumber = 4,
#                    grade = 10)
# readAssignments()

NewMCEntry <- function(ID = 999999999, answer, questionNumber, questionValue = 2, examNumber, examCode, date = Sys.Date()) {
      df <- data.frame(ID = as.integer(ID), answer = as.integer(answer), questionNumber = as.integer(questionNumber), questionValue = questionValue, examNumber = examNumber, date = date, examCode = examCode)
      sql <- "INSERT INTO mcAnswers VALUES (:ID, :answer, :questionNumber, :questionValue, :examNumber, :date, :examCode)"
      dbGetPreparedQuery(conn, sql, bind.data = df)      
}
# NewMCEntry(ID = 111444777,
#            answer = 1,
#            questionNumber = 1,
#            questionValue = 2,
#            examNumber = 4,
#            examCode = 101)
# readMCAnswers()

NewLFEntry <- function(ID = 999999999, grade, questionNumber, examNumber, examCode, date = Sys.Date()) {
      df <- data.frame(ID = as.integer(ID), grade = grade, questionNumber = as.integer(questionNumber), examNumber = examNumber, date = date, examCode = examCode)
      sql <- "INSERT INTO longformGrades VALUES (:ID, :grade, :questionNumber, :examNumber, :date, :examCode)"
      dbGetPreparedQuery(conn, sql, bind.data = df)
}
# NewLFEntry(ID = 111444777,
#            grade = 10,
#            questionNumber = 1,
#            examNumber = 4,
#            examCode = 101)
# readLongformGrades()

NewCPEntry <- function(ID = 999999999, date = Sys.Date(), attended = TRUE, questionAnswered = "", questionAsked = "", participationNotes = "") {
      df <- data.frame(ID = as.integer(ID), date = date, attended = attended, questionAnswered = questionAnswered, questionAsked = questionAsked, participationNotes = participationNotes)
      sql <- "INSERT INTO classParticipation VALUES (:ID, :date, :attended, :questionAnswered, :questionAsked, :participationNotes)"
      dbGetPreparedQuery(conn, sql, bind.data = df)
}
# NewCPEntry(ID = 111444777,
#            attended = FALSE)
# readClassParticipation()