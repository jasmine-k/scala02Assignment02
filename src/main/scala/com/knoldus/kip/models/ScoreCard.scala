package com.knoldus.kip.models

case class ScoreCard(studentId: Long, marks: Map[Long, Float], percentage: Float)
