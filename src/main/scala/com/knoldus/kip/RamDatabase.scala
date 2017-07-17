package com.knoldus.kip

import com.knoldus.kip.models.Gender._
import com.knoldus.kip.models.{Marks, Student}

object RamDatabase {

  val studentList: List[Student] = List(
    Student(1, "Anmol", MALE),
    Student(2, "Archana", FEMALE),
    Student(3, "Saniya", FEMALE),
    Student(4, "Babbar", MALE),
    Student(5, "Dolly", FEMALE),
    Student(6, "Jassi", FEMALE),
    Student(7, "Shubham", MALE),
    Student(8, "Anuja", FEMALE),
    Student(9, "Prerna", FEMALE),
    Student(10, "Shubham", MALE)
  )

  val marksList: List[Marks] = List(
    Marks(1, 1, 100), Marks(1, 2, 100), Marks(1, 3, 67), Marks(1, 4, 70), Marks(1, 5, 80),
    Marks(1, 6, 40), Marks(1, 7, 76), Marks(1, 8, 70), Marks(1, 9, 96), Marks(1, 10, 56),
    Marks(2, 1, 90), Marks(2, 2, 100), Marks(2, 3, 90), Marks(2, 4, 95), Marks(2, 5, 60),
    Marks(2, 6, 80), Marks(2, 7, 66), Marks(2, 8, 80), Marks(2, 9, 76), Marks(2, 10, 86),
    Marks(3, 1, 85), Marks(3, 2, 80), Marks(3, 3, 80), Marks(3, 4, 80), Marks(3, 5, 80),
    Marks(3, 6, 80), Marks(3, 7, 80), Marks(3, 8, 80), Marks(3, 9, 80), Marks(3, 10, 80),
    Marks(4, 1, 60), Marks(4, 2, 60), Marks(4, 3, 60), Marks(4, 4, 60), Marks(4, 5, 60),
    Marks(4, 6, 60), Marks(4, 7, 70), Marks(4, 8, 60), Marks(4, 9, 60), Marks(4, 10, 60),
    Marks(5, 1, 90), Marks(5, 2, 85), Marks(5, 3, 80), Marks(5, 4, 90), Marks(5, 5, 95),
    Marks(5, 6, 50), Marks(5, 7, 90), Marks(5, 8, 70), Marks(5, 9, 74), Marks(5, 10, 60)
  )

  /*
  val scoreCards : ListBuffer[ScoreCard] = new ListBuffer[ScoreCard]

  def add(scoreCard: ScoreCard) : ListBuffer[ScoreCard] = {
    scoreCards += scoreCard
  }

  def getScoreCard(studentId: Long): Option[ScoreCard] = {
    scoreCards.find(_.studentId == studentId)
  }

  def updateScoreCard(scoreCard: ScoreCard): ListBuffer[ScoreCard] = {
    getScoreCard(scoreCard.studentId) match {
      case Some(card: ScoreCard) => scoreCards -= card
        scoreCards += scoreCard
      case None => add(scoreCard)
    }
  }
*/
}
