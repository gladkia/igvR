.DataAnnotationTrack <- setClass("DataAnnotationTrack",
                                 contains="AnnotationTrack",
                                 slots=c(
                                    tbl="data.frame"
                                    )
                                 )



#----------------------------------------------------------------------------------------------------
DataAnnotationTrack <- function(trackName,
                                tbl,
                                displayMode="COLLAPSED",
                                searchable=FALSE
                                )
{

   obj <- .DataAnnotationTrack(AnnotationTrack(displayMode=displayMode,
                                               expandedRowHeight=30,
                                               squishedRowHeight=15,
                                               GFF.GTF.id.columnName="Name",
                                               maxRows=500,
                                               searchable=searchable),
                               tbl=tbl)
   obj


} # DataAnnotationTrack
#----------------------------------------------------------------------------------------------------

