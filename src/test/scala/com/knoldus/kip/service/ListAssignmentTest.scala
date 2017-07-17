package com.knoldus.kip.service

import com.knoldus.kip.models.Gender._
import com.knoldus.kip.models.Student
import org.scalatest.FunSuite

class ListAssignmentTest extends FunSuite with ListAssignment {

  test("testing failed students") {
    assert(failedStudents(1, 60.0, "fail") == 2)
  }

  test("testing top bottom students") {

    assert(topBottomStudents(1, 3, "top") == List(Student(2, "Archana", FEMALE), Student(1, "Anmol", MALE), Student(9, "Prerna", FEMALE)))
    assert(topBottomStudents(1, 3, "bottom") == List(Student(6, "Jassi", FEMALE), Student(10, "Shubham", MALE),Student(3, "Saniya", FEMALE)))
  }

  test("testing topAndLeastScorers") {
    assert(topAndLeastScorers("bottom", 3) == List( Student(6, "Jassi", FEMALE),Student(10, "Shubham", MALE),Student(8, "Anuja", FEMALE)))
  }

  test("testing passedOrFailed") {
    assert(passedOrFailed("pass", 84) == List(Student(1, "Anmol", MALE), Student(2, "Archana", FEMALE)))
  }

  test("testing generateReport") {
    assert(generateReport == List(("Anmol", List(100, 90, 85, 60, 90)), ("Archana", List(100, 100, 80, 60, 85)), ("Saniya", List(67, 90, 80, 60, 80)),
      ("Babbar", List(70, 95, 80, 60, 90)), ("Dolly", List(80, 60, 80, 60, 95)), ("Jassi", List(40, 80, 80, 60, 50)),
      ("Shubham", List(76, 66, 80, 70, 90)), ("Anuja", List(70, 80, 80, 60, 70)), ("Prerna", List(96, 76, 80, 60, 74)),
      ("Shubham", List(56, 86, 80, 60, 60))))
  }

  test("testing getLastElementWithIndex"){
    assert(getLastElementWithIndex(List("A","b","c"))==("c",2))
  }

  test("testing printTable"){
    assert(printTable(List(1,2)) == List(1,2,3,4,5,6,7,8,9,10,2,4,6,8,10,12,14,16,18,20))
  }

  test("testing aggregateLists"){
    assert(aggregateLists(List("a","b"),List(1L,2L))==List(List(("a",1)), List(("b",2))))
  }

  test("testing getSumOfList"){
    assert(getSumOfList(List(1,2,6)) == 9)
  }

  test("testing getMultiplicationOfList"){
    assert(getMultiplicationOfList(List(1,2,6)) == 12)
  }

  test("testing quickSortList"){
    assert(quickSortList(List(9,45,1,7,342,222)) ==  List(1, 7, 9, 45, 222, 342))
  }

  test("testing mergeSortList"){
    assert(mergeSortList(List(33,44,22,-10,99))==List(-10, 22, 33, 44, 99))
  }
}