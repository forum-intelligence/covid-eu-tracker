con <- pool::dbPool(
  RPostgres::Postgres(),
  dbname = "covid",
  host = "127.0.0.1",
  port = 5432, 
  user = "user",
  password = "password"
)

#* Get EU data
#* @serializer unboxedJSON
#* @get /eu
function(){
  DBI::dbReadTable(con, "europe")
}

#* Get Italy data
#* @serializer unboxedJSON
#* @get /italy
function(){
  DBI::dbReadTable(con, "italy")
}

#* Get Germany data
#* @serializer unboxedJSON
#* @get /germany
function(){
  DBI::dbReadTable(con, "germany")
}

#* Get Switzerland data
#* @serializer unboxedJSON
#* @get /switzerland
function(){
  DBI::dbReadTable(con, "switzerland")
}

#* Get Austria data
#* @serializer unboxedJSON
#* @get /austria
function(){
  DBI::dbReadTable(con, "austria")
}

#* Get France data
#* @serializer unboxedJSON
#* @get /france
function(){
  DBI::dbReadTable(con, "france")
}

#* Get Spain data
#* @serializer unboxedJSON
#* @get /spain
function(){
  DBI::dbReadTable(con, "spain")
}

#* Get England data
#* @serializer unboxedJSON
#* @get /england
function(){
  DBI::dbReadTable(con, "england")
}

#* Get World data
#* @serializer unboxedJSON
#* @get /world
function(){
  DBI::dbReadTable(con, "world")
}

#* Get Europe data
#* @serializer unboxedJSON
#* @get /europe
function(){
  DBI::dbReadTable(con, "europe")
}

#* Get Norway data
#* @serializer unboxedJSON
#* @get /norway
function(){
  DBI::dbReadTable(con, "norway")
}

#* Get Poland data
#* @serializer unboxedJSON
#* @get /poland
function(){
  DBI::dbReadTable(con, "poland")
}

#* Get Scotland data
#* @serializer unboxedJSON
#* @get /scotland
function(){
  DBI::dbReadTable(con, "scotland")
}

#* Get Sweden data
#* @serializer unboxedJSON
#* @get /sweden
function(){
  DBI::dbReadTable(con, "sweden")
}

#* Get Ukraine data
#* @serializer unboxedJSON
#* @get /ukraine
function(){
  DBI::dbReadTable(con, "ukraine")
}

#* Get Belgium data
#* @serializer unboxedJSON
#* @get /belgium
function(){
  DBI::dbReadTable(con, "belgium")
}
