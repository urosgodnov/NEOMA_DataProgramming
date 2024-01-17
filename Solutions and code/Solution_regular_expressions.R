text1<-c("auto","bus","oh no no","tehere were 3 guys!","I cannot do it!","Let mi add this3nonsense!","A bird!","an9")


#Solution 1:
# Create a variable called my_pattern and implement the required pattern for finding any digit in the variable text1.
#Use function grepl to verify if there is a digit in the string variable  

grepl("\\d", text1)

#Solution 2
#Use function gregexpr to find all the positions in text1 where there is a digit.
#Place the results in a variable called string_position.  

string_position<-gregexpr("\\d", text1)[[4]]


#Solution 3
#Create a variable called my_pattern and implement the required pattern for finding one digit 
#and one uppercase alphanumeric character, in variable text1.

my_pattern<-grepl("[0-9A-Z]",text1)
my_pattern

#Solution 4
#Use function regexpr to find the position of the first space in text1.
#Place the results in a variable called first_space.

regexec("\\s",text1)


## Solution 5
#Create a pattern that checks in text1 if there is a lowercase character, 
#followed by any character and then by a digit.

grepl("[a-z][a-z][0-9]", text1)
