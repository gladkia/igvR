.DataAnnotationTrack <- setClass("DataAnnotationTrack",
                                 contains="AnnotationTrack",
                                 slots=c(
                                    tbl="data.frame"
                                    )
                                 )



#----------------------------------------------------------------------------------------------------
DataAnnotationTrack <- function(trackName,
                                tbl,
                                color="black",
                                displayMode="COLLAPSED",
                                searchable=FALSE
                                )
{

   printf("DataAnnotationTrack ctor")
   url <- "http://temporaryFileAt.httpuv.webserver"
   indexURL <- NA_character_

   obj <- .DataAnnotationTrack(AnnotationTrack(trackName=trackName,
                                               displayMode=displayMode,
                                               color=color,
                                               fileFormat="bed",
                                               sourceType="file",
                                               url=url,
                                               indexURL=indexURL,
                                               expandedRowHeight=30,
                                               squishedRowHeight=15,
                                               GFF.GTF.id.columnName="Name",
                                               maxRows=500,
                                               searchable=searchable),
                               tbl=tbl)
   obj


} # DataAnnotationTrack
#----------------------------------------------------------------------------------------------------

