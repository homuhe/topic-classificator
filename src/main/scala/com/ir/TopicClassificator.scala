package com.ir

import java.io.{File, PrintWriter}
import scala.collection.mutable
import scala.io.Source

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 4
  * Description:  Document Topic Classification by SVM
  */


object TopicClassificator {

    //Resulting matrix of feature vectors
    //trainingMatrix(v)(x)(y)._z: v label number, x doc number, y term-value tuple, z=1 term, z=2 tf-idf
  var trainingMatrix = mutable.HashMap[Int, List[List[(Int, Double)]]]()

  var newsgroupMap = scala.collection.immutable.SortedMap[String, Int]()  // topic/label to Integer
  var inverted = mutable.HashMap[Int, mutable.HashMap[String, Int]]()     // docID       to (term, tf)
  var indexMap = scala.collection.immutable.SortedMap[String, Int]()      // term        to Index
  val label2Docs = mutable.HashMap[String, Set[Int]]()                    // label       to Set of DocIDs
  var docFreqMap = mutable.HashMap[String, Int]()
  var labelNum = 1
  var indexNum = 1


  /**
    * Main Method
    * @param args Input directory & Output file
    */
  def main(args: Array[String]): Unit = {

    println("Reading in:")

    var folders = Array[File]()
    var output = ""

    if (args.length != 2) help()
    else {
      folders = read_dir(args(0))
      output = args(1)
    }

    for (folder <- folders) {

      println(folder)

      val label = label2num(folder)
      val files = new java.io.File(folder.toString).listFiles

      for (file <- files) {

        val docID = label2docs(file, label)
        createIndices(extractLines(file), docID, label)
      }
    }

    fill_dfMap()
    generateTable()
    writeTable(output)
  }



  /**
    * Method which reads in files of given directory recursively.
    * @param input_directory
    * @return Array of all files contained in directory.
    */
  def read_dir(input_directory: String): Array[File] = {
    val folders = new java.io.File(input_directory).listFiles
    folders
  }

  /**
    * Creates a folder to Integer mapping
    * @param folder of input directory
    */
  def label2num(folder: File): String = {
    val label = folder.getName
    newsgroupMap = newsgroupMap + (label -> labelNum)
    labelNum += 1
    label
  }

  def label2docs(f: File, label: String): Int = {

    val docID = f.getName.replace(".conll", "").toInt

    //remembering which documents correspond to which label
    if(!label2Docs.contains(label))
      label2Docs.put(label, Set(docID))
    else label2Docs(label) += docID
    docID
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
    * Calculating df score
    */
  def fill_dfMap(): Unit = {
    //keeping track of term occurrences all documents
    indexMap.foreach(entry => docFreqMap.put(entry._1, 0))

    for((docid, vectors) <- inverted) {
      for(term <- vectors.keySet)
        docFreqMap(term) = docFreqMap(term) +1
    }
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
    * Fills a HashMap which maps type of word to document identifier.
    * @param lines: Iterator over an array of strings
    */
  def createIndices(lines: Iterator[Array[String]], doc_id: Int, label: String): Unit = {

    for (line <- lines if line.length > 1) {
      val term = line(2)

      // if indexMap is missing that term, add it
      if(!indexMap.contains(term)) {
        indexMap = indexMap + (term -> indexNum)
        indexNum += 1
      } // else do nothing, since it is already contained and has an indexNum

      val term_tfpairs = mutable.HashMap[String, Int]()

      //create non existing term entry
      if (!inverted.contains(doc_id)) {
        term_tfpairs.put(term , 1)
        inverted += doc_id -> term_tfpairs
      }
      else {
        // add it to existing term, but non-existing docid
        if (!inverted(doc_id).contains(term)) {
          inverted(doc_id).put(term, 1)
        }
        else {
          // increase tf of existing term for existing docid
          inverted(doc_id)(term) = inverted(doc_id)(term) + 1
        }
      }
    }
  }

  /**
    * Generates matrix with feature vectors
    */
  def generateTable(): Unit = {

    for((label, docSet) <- label2Docs){
      val labelNum = newsgroupMap(label)
      // for each label class we initialize
      //      trainingMatrix.put(labelNum, scala.collection.immutable.SortedMap[Int, Double]())
      val tmp = List[List[(Int, Double)]]()
      //initialize
      trainingMatrix.put(labelNum, tmp)

      // going through the relevent docids for that class
      for(docid <- docSet){

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

  /**
    * Writes trainingMatrix in output file
    * @param output file
    */
  def writeTable(output: String): Unit = {

    println("Writing training file...")

    val pw = new PrintWriter(new File(output))

    for ((classLabel, instances) <- trainingMatrix) {

      for (instance <- instances) {
        pw.write(classLabel + " ")
        var dummy = ""
        for ((index, value) <- instance) {
          dummy += index + ":" + value + " "
        }
        pw.write(dummy.trim() + "\n")
      }
    }
    pw.close()

    println("Done!")
  }

  /**
    * Help function for correct usage
    */
  def help() = {
    println("Usage: ./wildcard arg1 arg2")
    println("\t\targ1: INPUT  - directory with files to train")
    println("\t\targ2: OUTPUT - file in liblinear format for SVM training")
    sys.exit()
  }
}
