package com.ir

import java.io.File
import java.util

import scala.collection.mutable
import scala.io.Source

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 4
  * Description:  Document Topic Classification by SVM
  */


object TopicClassificator {

  //  val inverted = mutable.HashMap[String, mutable.SortedSet[(Int, Int)]]()

  val inverted = mutable.HashMap[String, mutable.HashMap[Int, Int]]()

  val topicMap = mutable.HashMap[String, Int]()
  var counter = 0

  /**
    * Main method
    */
  def main(args: Array[String]): Unit = {
    println("Harambe!")
    def pwd = System.getProperty("user.dir") //current directory
    val current_dir = pwd
    println(current_dir)
    //    val folders = new java.io.File(current_dir + "/20news-18828").listFiles
    //    for (folder <- folders) {

    //test on only one label/topic
    val folder = new java.io.File(current_dir + "/20news-18828/alt.atheism")

    topicMap.put(folder.getName, counter)
    counter += 1

    //do equivalent for indexMap



    val files = new java.io.File(folder.toString).listFiles
    for (file <- files) {
      // we need the doc_ID to create an invertex Index
      val docID = file.getName().replace(".conll", "").toInt

      createIndices(extractLines(file),docID)
    }


    //    }

    //   topicMap.foreach(topic => println(topic))

    inverted.foreach(line => println(line))
  }

  /**
    * Reads input file and separates at tabs
    * @param file: set of German Wikis in tab-separated CONLL-X dependency format
    * @return lines: Iterator over an array of strings
    */
  def extractLines(file: File) = {
    val lines = Source.fromFile(file)
      .getLines()
      .map(line => line.split("\t"))
    lines
  }
  /**
    * Fills a HashMap which maps type of word to document identifier.
    * @param lines: Iterator over an array of strings
    */
  def createIndices(lines: Iterator[Array[String]], doc_id: Int) = {
    for (line <- lines) {
      if (line.length > 1) {
        val lemma = line(2)
        //        val doc_id = line(5).toInt
        val docidtf_pair = new mutable.HashMap[Int, Int]()

        //create non existing lemma entry
        if (!inverted.contains(lemma)) {

          docidtf_pair.put(doc_id,1)

          inverted += lemma -> docidtf_pair
        }
        else{
          // add it to existing lemma, but non-existing docid
          if (!inverted(lemma).contains(doc_id)){
            inverted(lemma).put(doc_id, 1)
          }else{
            // increase tf of existing lemma for existing docid
            inverted(lemma)(doc_id) = inverted(lemma)(doc_id) + 1
          }

        }

      }
    }
  }



}
