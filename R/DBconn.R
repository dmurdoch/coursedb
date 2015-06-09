DBconn <- local({
  conn <- NULL
  function(dbfile = "course.db") {
    if (!missing(dbfile) && !is.null(conn)) {
      if (dbIsValid(conn))
        dbDisconnect(conn)
	conn <<- NULL
    }
    if (is.null(conn) || !dbIsValid(conn)) {
      sqlite <- SQLite()
      conn <<- dbConnect(sqlite, dbfile)
    }
    conn
  }
})


