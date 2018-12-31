package com.algorithms.assignOne

import java.io.{BufferedWriter, FileWriter}

import scala.io.Source

class CountInversions(filepath: String, output_filepath: String) {

  // Write output to file
  def writeSortedList(output: (List[Int], Int) ) = {

    val numbers = output._1
    val invs = output._2
    val writer = new BufferedWriter(new FileWriter(output_filepath))

    writer.write(
      "It took " + invs +
        "inversions to sort this file \n")

    writer.write("----------------------\n")
    writer.write("\n")

    numbers.foreach(writer.write)
    writer.close()
  }

  // Split list into two halves
  def splitList(list: List[Int]): (List[Int], List[Int]) = {
    val half: Int = list.length / 2
    list.splitAt(half)
  }

  // Merge and count inversions
  def countMerge(fhalf: Tuple2[List[Int], Int],
                 shalf: Tuple2[List[Int], Int]) = {

    val inv = fhalf._2 + shalf._2

    def merge(flist: List[Int],
              slist: List[Int],
              invs: Int): Tuple2[List[Int], Int] = (flist, slist) match {
      case (Nil, s :: srest) => (s :: srest, invs)
      case (f :: frest, Nil) => (f :: frest, invs)
      case (f :: frest, s :: srest) => {
        if (f <= s) (f :: merge(frest, s :: srest, invs)._1, invs)
        else (s :: merge(f :: frest, srest, invs)._1, invs + srest.length)
      }
    }

    merge(fhalf._1, shalf._1, inv)
  }

  // Do the actual sorting and counting
  def mySort(list: List[Int]): (List[Int], Int) = {
    if (list.length == 1) (list, 0)
    else {
      val halves = splitList(list)
      countMerge(mySort(halves._1), mySort(halves._2))
    }
  }
}

object Invert {
  def main(args: Array[String]) = {
    val ipath = args(0)
    val opath = args(1)

    // Package input file as list
    def unsortedList: List[Int] =
      Source
        .fromFile(ipath)
        .getLines()
        .toList
        .map(_.toInt)

    val countInversions = new CountInversions(ipath, opath)
    countInversions.writeSortedList(countInversions.mySort(unsortedList))
  }
}
