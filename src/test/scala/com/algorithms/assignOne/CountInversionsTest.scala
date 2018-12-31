package com.algorithms.assignOne

import org.scalatest.FunSuite

class CountInversionsTest extends FunSuite {
  val countInversions = new CountInversions(
    "/Users/last.feremenga/workarea/algorithms/stanford_engineering/scala/data/IntegerArray.txt",
    "/Users/last.feremenga/workarea/algorithms/stanford_engineering/scala/data/output.txt"
  )

  test("CountInversions.splitList") {
    val list = List(1, 2, 3, 4, 5)
    val split = countInversions.splitList(list)
    assert(split._1 === List(1, 2))
    assert(split._2 === List(3, 4, 5))
  }

  test("CountInversions.countMerge") {
    val fhalf = (List(3, 4, 6), 1)
    val shalf = (List(2, 5, 7), 2)
    val cmerge = countInversions.countMerge(fhalf, shalf, false)
    println(cmerge)
    assert(cmerge._1 === List(2, 3, 4, 5, 6, 7))
    assert(cmerge._2 === 7)
  }
}
