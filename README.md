<b><u>List based assignment</u></b>
<br/>
<b><i>Assignemnt 1</i></b>
Every Student has some marks associated with it. Student details contains its id and name.
And for Marks, there are subjectId, studentId and number of marks a student scored.
<br/>
Following are the requirements which is required to gain from above scenario (i.e. Student and marks)
<br/>
<ul>
<li>
Input:- (subjectId, percentage, pass/fail)<br/>
Output:- for input pass, evaluate that how much students(id, name) are passed in the inputted subjectId
	for input fail, evaluate that how much students(id, name) are failed in the inputted subjectId
	<br/>
Note:- percentage is the input which defines the minimum passing criteria <br/>
e.g. <br/>
Pass count: 15<br/>
Fail count: 10<br/>
</li>
<li>
Input:- (subjectId, count, top/bottom)<br/>
Output:- based on the last input(top/bottom), output the students details who have scored max/min in that subjectId<br/>
e.g. <br/>
input: 1 5 top<br/>
output: <br/>
Kunal 85<br/>
Himanshu 84<br/>
Geetika 83<br/>
Anmol 82<br/>
Mahesh 81<br/>
</li>
<li>
Input:-<br/>
(top/bottom, count)<br/>
OutPut:-<br/>
Overall top/least scorer based on all the subjects score, fetch students name<br/>
count- input defines that how much students name are to be printed on console<br/>
e.g.<br/>
input: top 2<br/>

output:<br/>
Himanshu 75%<br/>
Geetika 74%<br/>
</li>

<li>
Input:-<br/>
(percentage, good_scholarship, normal_or_no_scholarship)<br/>
Output:- two groups of students with the amount of scholarship<br/>
e.g.<br/>
input: 85% 2000 500<br/>
output: <br/>
Kunal 2000<br/>
Himanshu 500<br/>
Geetika 2000<br/>
Mahesh 500<br/>
</li>
<li>
Input:-<br/>
(pass/fail, percentage)<br/>
count and print the number of students and all names who are passed/fail,<br/>
Pass or fail would be decided by percentage input field.<br/>
e.g.<br/>
input: fail 30<br/>
output: <br/>
Kunal 28%<br/>
Himanshu 29%<br/>
</li>
<li>
Find the student(s) who have scored 95% or above and print its details.<br/>
input: 95%<br/>
output:<br/>
Kunal 95%<br/>
Himanshu 96%<br/>
Geetika 97%<br/>
</li>
<li>
For every student, find its marks in detail (just like detailed Report card of a student.)<br/>
Note:- must use groupBy method of List<br/>
input: reportcard<br/>
output:<br/>
Kunal 75 70 80 75 75%<br/>
Himanshu 74 70 81 75 75%<br/>
Geetika 70 70 85 75 75%<br/>
</li>
</ul>

<i>Developer Notes:</i>

There would be two case classes<br/>
1) Student(id: Long, name: String)<br/>
2) Marks(subjectId: Int,studentId: Long, marksObtained: float)<br/>

In order to fill data in those case classes, either take inputs from a file, or take static inputs. But there must be atleast 5 subjects, and atleast 10 students.
e.g. List(Student(1, "Kunal"), Student(2, "Himanshu"), Student(3, "Geetika") ....)
List(Marks(1, 1, 95), Marks(2, 1, 75), ...)
So basically here Kunal has marks 95 and 75 for the paper 1 and 2 respectively.
<br/>

<b><i>Assignemnt 2</i></b>
- Find the last element of list with its index value(dont use inbuilt methods to extract last element directly)<br/>
- print the table of each element in the List<br/>
- aggregate the contents of two lists of same size into a single list<br/>
	List(1,2) and List("a", "b") results List(List(1, "a"), List(2, "b"))<br/>
- find sum and multiplication of the list (dont use inbuilt methods)<br/>
- apply quicksort and mergesort on the Lists<br/>
- implement Stack and Queue using Lists.<br/>


<b><u>Collection Based Assignment</u></b>
Here comes up with yet another student assignment and it goes something like this, every Student has some marks associated with them.
Student details contains its id and name. And for Marks, there are subjectId, studentId and number of marks a student scored.


<b><i>Assignment 1</i></b> Now, I require a case class named ScoreCard having fields (studentId: Long, marks: Map[Long, Float], percentage: Float).
<br/>
<ul>
<li>
Write a method which takes no parameter and generates a Map with key student name and value as ScoreCard. As there can be more than one student with same name, the logic we have to follow is that, if two or more student has same name the key shold be the name of the student and the values (ScoreCard s) should be in a List, otherwise the key should be the student name and value should be the case class ScoreCard. e.g. Map should be Map[String, AnyRef].
</li>
<li>
Write a method which takes input as student name and print the score cards. If it finds one or more than one score card print all of them other wise print "No data found". The print should be in increasing order of the student id.
</li>
</ul>
<br/>
<b><i>Assignment 2 </i></b> The Student class should contain one more field this time, gender. The values of gender must be set in a Enumeration.
<br/>
<ul>
<li>
Write a method getScoreCardByGender to return a tuple of ScoreCards (e.g. (List[ScoreCard], List[ScoreCard])), where first field in the tuple has male student's score card and the second field has female student's score cards.
</li>
<li>
Write a method which calls the getScoreCardByGender method and gives the result which has more than 50%.
</li>
<li>
Write a method to find out similar percentage between the two groups (male, female). for example Geetika -75, Kunal -75
</li>
<li>
Write a method fo find out the percentage that girls group has scored but no same percentage has got in the boys group. e.g. ( Geetika -75, Neha - 73, charmy - 72) - (Kunal -75, Anmol - 73, Nitin - 71) = Charmy-72
</li>
</ul>