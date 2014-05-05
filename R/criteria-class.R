
# state <- "IN"
# county <- "Marion"
# monitor.id <- c("18-097-0057-1", "04-013-0019-1")
# pollutant = "o3"





setClass("criteria",
         slots = c(monitor = "data.frame",
                   standard = "data.frame",
                   design.value = "data.frame"
         ))

criteria <- function(pollutant, state, county, monitor.id){ #one pollutant, state, county; more than one monitor.id
  
  
  if(missing("county") == F & missing("state") == T){
    stop("the state parameter is missing")
  }
  
  if(missing("state") == T & missing("monitor.id") == T){
    stop("must provide a value for state or monitor.id")
  }
  
  if(missing("state") == F & missing("monitor.id") == F){
    warning("Both state and monitor.id parameters were given. The state parameter was ignored and monitor.id was used to specify the site(s).")
  }
  
  # get file path for inst folder in library ISDr
  package.folder <- normalizePath(system.file(package = "cRiteria"))
  
  # load sites.df
  load(paste0(package.folder, "\\sites\\Site_Info.Rdata"))
  
  # load state and county table
  load(paste0(package.folder, "\\sites\\State_Co_Table.Rdata"))
  
  # load standards
  load(paste0(package.folder, "\\standards\\Standards.Rdata"))
  
  if(!missing("monitor.id")){
    id <- data.frame(monitor.id)
    id <- as.data.frame(matrix(unlist(strsplit(monitor.id, "-")), ncol = 4, byrow = T))
    names(id) <- c("State_Code", "County_Code", "Site_ID", "POC")
    
    merged.df <- merge(state.county.df, id, by.x = c("State_Code", "County_Code"))
    merged.site.df <- merge(merged.df, sites.df)
    remove(sites.df)
    remove(state.county.df)
    remove(merged.df)
  } else {
    state.county.df <- state.county.df[state.county.df$State_Abbr == state, ]
    
    if(!missing("county")){
      state.county.df <- state.county.df[state.county.df$County_Name == county, ] 
    }
    
    merged.site.df <- merge(state.county.df, sites.df)
    remove(sites.df)
    remove(state.county.df)
  } 
  
  # load design values
  load(paste0(package.folder, "\\pollutants\\", pollutant, ".Rdata"))
  
  dv.df <- merged.site.df[, c("State_Code", "County_Code", "Site_ID")]
  names(dv.df) <- c("state", "county.code", "site.id")
  dv.df <- merge(dv.df, design.value.df)
  remove(design.value.df)
  
  stnd <- standards[standards$Abbreviation == pollutant, ]
  row.names(stnd) <- 1:dim(stnd)[1]
  
  new("criteria", monitor = merged.site.df, standard = stnd, design.value = dv.df)
}

setMethod("show",
          signature = "criteria",
          definition = function(object){
            cat("An object of class \"", class(object), "\" containing information\n", sep = "")
            cat("about", dim(object@monitor)[1], object@standard$Pollutant, " monitors")
          }
)

# create an accessors for slots
setGeneric("monitor", function(object, ...) standardGeneric("monitor"))
setMethod("monitor", signature = "criteria", function(object) object@monitor)

setGeneric("standard", function(object, ...) standardGeneric("standard"))
setMethod("standard", signature = "criteria", function(object) object@standard)

setGeneric("designValue", function(object, ...) standardGeneric("designValue"))
setMethod("designValue", signature = "criteria", function(object) object@design.value)

# create subsetting method
setMethod("[", "criteria",
          function(x, i, j = missing, drop = "missing"){
            .monitor <- x@monitor[i, ]
            dv.df <- .monitor[, c("State_Code", "County_Code", "Site_ID")]
            names(dv.df) <- c("state", "county.code", "site.id")
            .design.value <- merge(dv.df, x@design.value)
            new("criteria", monitor = .monitor, standard = x@standard,
                design.value = .design.value[order(.design.value$design.value.year), ])
          })


