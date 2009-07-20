`GenerateEpisodes` <-
function(HRVData,NewBegFrom, NewEndFrom, DispBeg, DispEnd, OldTag="", NewTag="", verbose=FALSE) {	
#-----------------------------------
# Creates new episodes from old ones
#-----------------------------------
#  NewBegFrom, NewEndFrom -> source of new beginning and end of episodes ("Beg" for indicating the beginning or end as the beginning of the old episode, "End" for end)
#  DispBeg, DispEnd -> absolute displacements from the beginning or end for new episodes in seconds
#  OldTag -> specifies tag of old episodes
#  NewTag -> specifies tag for new episodes (if empty, copies OldTag)
#  Verbose -> TRUE for verbose mode

#  Example: arguments for creating episodes displaced one minute before old ones
#    NewBegFrom="Beg", NewEndFrom="End", DispBeg=-60, DispEnd=-60

#  Example: arguments for creating episodes of length of 1 minute just after previous ones
#    NewBegFrom="End", NewEndFrom="End", DispBeg=0, DispEnd=60

   if (verbose) {
		cat("** Creating new episodes from old ones **\n")

      if (OldTag=="") {
         cat("   No tag specified: using all old episodes\n")
      } else {
         cat("   Using episodes with tag:",OldTag,"\n")
      }

      if (NewTag=="") {
         cat("   Duplicating old tags\n")
      } else {
         cat("   Creating episodes with tag:",NewTag,"\n")
      }
   }

   if (OldTag=="") {
      NewEpisodes=HRVData$Episodes
   } else {
      NewEpisodes=subset(HRVData$Episodes,HRVData$Episodes$Type==OldTag)
   }

   if (NewBegFrom=="Beg") {
      BegInstants=NewEpisodes$InitTime+DispBeg
   } else if (NewBegFrom=="End") {
      BegInstants=NewEpisodes$InitTime+NewEpisodes$Duration+DispBeg
   } else {
      stop("  --- Illegal value for NewBegFrom: possible values are \"Beg\" and \"End\"\n    --- Quitting now!! ---\n")
   }

   if (NewEndFrom=="Beg") {
      EndInstants=NewEpisodes$InitTime+DispEnd
   } else if (NewEndFrom=="End") {
      EndInstants=NewEpisodes$InitTime+NewEpisodes$Duration+DispEnd
   } else {
      stop("  --- Illegal value for NewEndFrom: possible values are \"Beg\" and \"End\"\n    --- Quitting now!! ---\n")
   }

   NewEpisodes$InitTime=BegInstants
   NewEpisodes$Duration=EndInstants-BegInstants
   if (NewTag!="") {
      NewEpisodes$Type=NewTag
   }

   if (verbose) {
      cat("   Created",length(NewEpisodes$InitTime),"episodes from previous ones\n")
   }

   HRVData$Episodes=rbind(HRVData$Episodes,NewEpisodes)
   HRVData$Episodes=HRVData$Episodes[order(HRVData$Episodes$InitTime),]  # Sorts episodes by InitTime
   HRVData$Episodes=HRVData$Episodes[!duplicated(HRVData$Episodes),]  # Removes duplicated episodes

   if (verbose) {
      cat("   Number of episodes:",length(HRVData$Episodes$InitTime),"\n")
   }

   return(HRVData)

}

