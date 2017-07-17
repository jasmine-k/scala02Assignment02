package com.knoldus.kip.service

import com.knoldus.kip.RamDatabase
import com.knoldus.kip.models.{Student, Marks}

trait ListAssignment {

  //Assignment -1
  def failedStudents(subjectId: Long, percentage: Double, passOrFail: String): Int = {

    val marks: List[Marks] = RamDatabase.marksList.filter(_.subjectId == subjectId)

    passOrFail.toLowerCase match {
      case "pass" => marks.filter(_.marksObtained >= percentage).size
      case "fail" => marks.filter(_.marksObtained < percentage).size

    }

  }
  def topBottomStudents(subjectId: Long, count: Int, topOrBottom: String): List[Student] = {

    val marks: List[Marks] = RamDatabase.marksList.filter(_.subjectId == subjectId).sortBy(_.marksObtained)
    val studentList = marks.flatMap(x => RamDatabase.studentList.filter(_.id == x.studentId))

    topOrBottom.toLowerCase match {

      case "top" => {
        studentList.reverse.take(count)
      }
      case "bottom" => {
        studentList.take(count)
      }

    }

  }

  def topAndLeastScorers(topOrBottom: String, count: Int): List[Student] = {

    val marksList = RamDatabase.marksList
    val studentsList = RamDatabase.studentList
    val totalOfEachStudent: List[Float] = studentsList.map(x => marksList.filter(_.studentId == x.id).map(_.marksObtained).sum) // RamDatabase.marksList.filter(._studentId == x.id).map(_.marksObtained).sum)

    val studentNameWithMarks = studentsList.zip(totalOfEachStudent).sortBy(_._2)

    topOrBottom.toLowerCase match {

      case "top" => studentNameWithMarks.reverse.take(count).map(_._1)
      case "bottom" => studentNameWithMarks.take(count).map(_._1)
    }

  }

  def getScholarshipGroups(percentage: Float, goodScholarship: Int, normalScholarship: Int)
  : (List[(Student, Int)], List[(Student, Int)]) = {

    val studentsList = RamDatabase.studentList
    val totalOfEachStudent: List[Float] = RamDatabase.studentList.map(x => RamDatabase.marksList.filter(_.studentId == x.id).map(_.marksObtained).sum) // RamDatabase.marksList.filter(._studentId == x.id).map(_.marksObtained).sum)
    val maxMarks = RamDatabase.marksList.count(_.studentId == RamDatabase.marksList(0).studentId) * 100
    val percentOfEach = totalOfEachStudent.map(_ / maxMarks * 100)
    val studentNameWithPercentage = RamDatabase.studentList.zip(percentOfEach)

    val goodScholarshipList: List[(Student, Int)] = studentNameWithPercentage.filter(_._2 >= percentage).map(_._1).map(x => (x, goodScholarship))
    val normalScholarshipList: List[(Student, Int)] = studentNameWithPercentage.filter(_._2 < percentage).map(_._1).map(x => (x, normalScholarship))

    (goodScholarshipList, normalScholarshipList)
  }
  def passedOrFailed(passOrFail: String, percentage: Float): List[Student] = {

    val totalOfEachStudent: List[Float] = RamDatabase.studentList.map(x=> RamDatabase.marksList.filter(_.studentId == x.id).map(_.marksObtained).sum)// RamDatabase.marksList.filter(._studentId == x.id).map(_.marksObtained).sum)
    val maxMarks = RamDatabase.marksList.count(_.studentId == RamDatabase.marksList(0).studentId) *100
    val percentOfEach = totalOfEachStudent.map(_/maxMarks*100)
    val studentNameWithPercentage = RamDatabase.studentList.zip(percentOfEach)

    passOrFail.toLowerCase match {
      case "pass" => studentNameWithPercentage.filter(_._2 >= percentage).map(_._1)

      case "fail" => studentNameWithPercentage.filter(_._2 < percentage).map(_._1)

    }
  }

  def studentsWithMoreThan95: List[Student] = {

    val totalOfEachStudent: List[Float] = RamDatabase.studentList.map(x=> RamDatabase.marksList.filter(_.studentId == x.id).map(_.marksObtained).sum)// RamDatabase.marksList.filter(._studentId == x.id).map(_.marksObtained).sum)
    val maxMarks = RamDatabase.marksList.count(_.studentId == RamDatabase.marksList(0).studentId) *100
    val percentOfEach = totalOfEachStudent.map(_/maxMarks*100)
    val studentNameWithPercentage = RamDatabase.studentList.zip(percentOfEach)

    studentNameWithPercentage.filter(_._2 > 95).map(_._1)

  }

  def generateReport: List[(String, List[Int])] = {
    val marks:List[List[Int]] = RamDatabase.studentList.map(value => RamDatabase.marksList.groupBy( _.studentId==value.id)(true).map(_.marksObtained.toInt))
    val studentName = RamDatabase.studentList.map(_.name)
    studentName.zip(marks)

  } //Must use the groupBy() Method of the List

  //Assignment - 2
  def getLastElementWithIndex(list: List[String]): (String, Int) = {

    def lengthOfList[A](list:List[A]):Int = list match {
      case Nil => 0
      case head::tail => 1+ lengthOfList(tail)
    }
    def lastElement(list:List[String], listLength:Int): String ={

      if(listLength == 1) {
        list.head
      }

      else{
        lastElement(list.tail, listLength-1)
      }

    }

    val listLength: Int = lengthOfList(list)
    (lastElement(list,listLength), listLength-1)

  }

  def printTable(list: List[Long]): List[Long] = {
    val index: List[Int] = (for(counter <- 1 to 10)
      yield counter).toList

    list.flatMap(item => index.map(_*item))
  }

  def aggregateLists(list1: List[String], list2: List[Long]): List[List[(String, Long)]] = {
    list1.zip(list2).map(item => List(item))
  }

  def getSumOfList(list: List[Long]): Long = list match {
    case head::tail => head + getSumOfList(tail)
    case Nil => 0

  }

  def getMultiplicationOfList(list: List[Long]) : Long =  list match {
    case head::tail => head * getMultiplicationOfList(tail)
    case Nil => 1

  }

  def quickSortList(list: List[Long]): List[Long] =  list match {
    case head::tail => quickSortList(tail.filter(_<head)) ::: head:: quickSortList(tail.filter(_>=head))
    case Nil => Nil

  }

  def mergeSortList(list: List[Long]): List[Long] = {
    val mid = list.length/2
    if(mid == 0){
      list
    }
    else{
      val (left,right) = list.splitAt(mid)
      merge(mergeSortList(left), mergeSortList(right))
    }
  }
  def merge(left: List[Long], right:List[Long]): List[Long] = (left,right) match {

    case (left,Nil) => left
    case (Nil, right) => right
    case(leftHead::leftTail, rightHead:: rightTail) =>
        if(leftHead< rightHead){
        leftHead::merge(leftTail,right)
      }
      else{
          rightHead::merge(left, rightTail)
        }
  }
}
