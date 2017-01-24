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


  // map from class label (int) to sortedMap from feature(int) to tfidf (double)
  //  var trainingMatrix = mutable.HashMap[Int, scala.collection.immutable.SortedMap[Int, Double]]()

  var trainingMatrix = mutable.HashMap[Int, List[List[(Int, Double)]]]()

  //  var inverted = mutable.HashMap[String, mutable.HashMap[Int, Int]]()
  var inverted = mutable.HashMap[Int, mutable.HashMap[String, Int]]()

  var docFreqMap = mutable.HashMap[String, Int]()

  // a map from label class to a Set of docId's
  val label2Docs = mutable.HashMap[String, Set[Int]]()

  // map from topic/label to corresponding label number required for liblinear
  var newsgroupMap = scala.collection.immutable.SortedMap[String, Int]()
  var labelNum = 1
  var inversedIndexMap = scala.collection.immutable.SortedMap[Int, String]()

  // map from word/term to corresponding index number required for liblinear
  var indexMap = scala.collection.immutable.SortedMap[String, Int]()
  var indexNum = 0
  var inversedNewsgroupMap = scala.collection.immutable.SortedMap[Int, String]()
  /**
    * Main method
    */
  def main(args: Array[String]): Unit = {
    println("Harambe!")
    def pwd = System.getProperty("user.dir") //current directory
    val current_dir = pwd
    println(current_dir)
    val folders = new java.io.File(current_dir + "/20news-18828").listFiles
    for (folder <- folders) {

      //test on only one label/topic
      //    val folder = new java.io.File(current_dir + "/20news-18828/alt.atheism")

      val label = folder.getName
      newsgroupMap = newsgroupMap + (label -> labelNum)
      labelNum += 1


      val files = new java.io.File(folder.toString).listFiles
      for (file <- files) {

        // we need the doc_ID to create an invertex Index
        val docID = file.getName().replace(".conll", "").toInt


        //remember which documents correspond to which label
        if(!label2Docs.contains(label)){
          label2Docs.put(label, Set(docID))
        }else{
          label2Docs(label) += docID
        }


        createIndices(extractLines(file), docID, label)


      }

      println(label + " : " + newsgroupMap(label) )
      println(label2Docs(label).size)
    }

    inversedIndexMap = inverseIndexMap
    inversedNewsgroupMap = inverseNewsgroupMap

    println(newsgroupMap.size)
    //    println(newsgroupMap)
    println(inversedNewsgroupMap)
    println(indexMap.size)
    //    println(indexMap)
    println(inversedIndexMap)


    println(inverted.size)


    //    for(term <- indexMap.keySet){
    //      println(term + " : " + get_idf(term) )
    //    }



    //    println("0 was -Infinity for document: " + label2Docs(inversedNewsgroupMap(1))(0) +  " and " +  inversedIndexMap(0))



    fill_dfMap
    println(docFreqMap)

//    println("subject " + get_idf("subject"))
//    println("when " + get_idf("when"))
//    println("nude " + get_idf("nude"))
//    println("of " + get_idf("of"))

    generateTable

        println(trainingMatrix(1)(0))
//    trainingMatrix.foreach(entry => println(entry._2))



  }

  def writeTable(output: String) = {

    // toDo

  }

  def fill_dfMap = {
    //        // keep track of the occurences of a term in the total document collection
    //        // and update the state
    indexMap.foreach(entry => docFreqMap.put(entry._1, 0))

    for((docid, vectors) <- inverted){
      for(term <- vectors.keySet){
        docFreqMap(term) = docFreqMap(term) +1
      }
    }

  }


  def generateTable = {

    for((label, docSet) <- label2Docs){
      val labelNum = newsgroupMap(label)
      // for each label class we initialize
      //      trainingMatrix.put(labelNum, scala.collection.immutable.SortedMap[Int, Double]())


      val tmp = List[List[(Int, Double)]]()
      //initialize
      trainingMatrix.put(labelNum, tmp)

      // going through the relevent docids for that class
      for(docid <- docSet){
        //      for(docid <- label2Docs(inversedNewsgroupMap(1))){

        var featureList = List[(Int, Double)]()

        for((term, tf) <- inverted(docid)){

          val indexNum = indexMap(term)
          val value = tf * get_idf(term)
          featureList = featureList :+ (indexNum, value)
        }
        trainingMatrix(labelNum) = trainingMatrix(labelNum) :+ featureList.sortBy(_._1)

      }
    }

  }


  def inverseIndexMap = {
    indexMap.map({case(k, v) => v -> k})
  }

  def inverseNewsgroupMap = {
    newsgroupMap.map({case(k, v) => v -> k})
  }

  /**
    * Returns IDF score of a given term
    * @param term
    * @return IDF score
    */
  def get_idf(term: String): Double = {
    val N = inverted.size
    val n_i = docFreqMap(term)
    math.log(N/n_i)
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
  def createIndices(lines: Iterator[Array[String]],
                    doc_id: Int, label: String) = {
    for (line <- lines) {
      if (line.length > 1) {
        val term = line(2)

        // if indexMap is missing that term, add it
        if(!indexMap.contains(term)){
          indexMap = indexMap + (term -> indexNum)
          indexNum += 1
        } // else do nothing, since it is already contained and has an indexNum


        val term_tfpairs = mutable.HashMap[String, Int]()

        //create non existing term entry
        if (!inverted.contains(doc_id)) {
          term_tfpairs.put(term ,1)
          inverted += doc_id -> term_tfpairs
        }
        else{
          // add it to existing term, but non-existing docid
          if (!inverted(doc_id).contains(term)){
            inverted(doc_id).put(term, 1)
          }else{
            // increase tf of existing term for existing docid
            inverted(doc_id)(term) = inverted(doc_id)(term) + 1
          }
        }

      }
    }
  }



}
