package com.algorithms.assignOne

import java.io.{BufferedWriter, FileWriter}

import scala.io.Source

class CountInversions(filepath: String, output_filepath: String) {

  // Write output to file
  def writeSortedList(output: (List[Int], Long)) = {

    val numbers = output._1
    val invs = output._2
    val writer = new BufferedWriter(new FileWriter(output_filepath))

    writer.write(
      "It took " + invs +
        " inversions to sort this file \n")

    writer.write("----------------------\n")
    writer.write("\n")

    numbers.foreach(ln => writer.write(ln + "\n"))
    writer.close()
  }

  // Split list into two halves
  def splitList(list: List[Int]): (List[Int], List[Int]) = {
    val half: Int = list.length / 2
    list.splitAt(half)
  }

  // Merge and count inversions, using tail recursion
  def countMerge(fhalf: Tuple2[List[Int], Long],
                 shalf: Tuple2[List[Int], Long],
                 reversed: Boolean): (List[Int], Long) = {

    val inv = fhalf._2 + shalf._2

    def merge(flist: List[Int],
              slist: List[Int],
              acc_invs: Long,
              acc_merge: List[Int]): Tuple2[List[Int], Long] =
      (flist, slist) match {
        case (Nil, s :: srest) => (slist:::acc_merge, acc_invs)
        case (f :: frest, Nil) => (flist:::acc_merge, acc_invs)
        case (f :: frest, s :: srest) => {
          if (f < s) merge(frest, slist, acc_invs, List(f):::acc_merge)
          else merge(flist, srest, acc_invs + flist.length, List(s):::acc_merge)
        }
      }

    val cmerge_reverse = merge(fhalf._1, shalf._1, inv, Nil)
    val cmerge = (cmerge_reverse._1.reverse, cmerge_reverse._2)
    if(reversed) cmerge_reverse
    else cmerge
  }

  // Do the actual sorting and counting
  def mySort(list: List[Int]): (List[Int], Long) = {
    if (list.length == 1) (list, 0)
    else {
      val halves = splitList(list)
      countMerge(mySort(halves._1), mySort(halves._2), false)
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
