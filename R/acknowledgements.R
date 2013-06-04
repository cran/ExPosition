acknowledgements <-
function(){
	print("This function serves to keep a list of those who have contributed to ExPosition (and related packages) throughout development.")
	
	num.people <- 10
	
	full.list <- array("", c(num.people, 1), list(1:num.people, c("Person")))
	#rbind(
		full.list[1,] <- c("Michael Meyners: Feedback and suggestions for prettyGraphs")
		full.list[2,] <- c("Anjali Krishnan: Testing and feedback")
		full.list[3,] <- c("Michael Kriegsman: Testing and feedback")
		full.list[4,] <- c("Jenny Wong: Testing and feedback")
		full.list[5,] <- c("Daniel Faso: Testing and feedback")
		full.list[6,] <- c("Shaikat Hossain: Testing and feedback")
		full.list[7,] <- c("Amy Louise Schwarz: Testing and feedback")
		full.list[8,] <- c("Adam Teed: Testing and feedback")
		full.list[9,] <- c("Rachel Williams: Data entry and creation")
 		full.list[10,] <- c("Students of Research Methods 3 at UT Dallas (2010, 2011, and 2012): Feedback, suggestions, interface testing and quality control, suffering")		
	#)
	print(full.list)
}
