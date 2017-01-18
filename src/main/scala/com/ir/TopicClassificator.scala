package com.ir

import java.io.File

/** Author:       Alexander Hartmann,
  *               Holger Muth-Hellebrandt
  *
  * Task:         Assignment 4
  * Description:  Document Topic Classification by SVM
  */


object TopicClassificator {

  /**
    * Main method
    */
  def main(args: Array[String]): Unit = {
    println("Harambe!")
    def pwd = System.getProperty("user.dir") //current directory
    val current_dir = pwd
    val folders = new java.io.File(current_dir + "/20news-18828").listFiles
    for (folder <- folders) {
      val files = new java.io.File(folder.toString).listFiles
      for (file <- files) println(file)

      //TODO Fill map
      //.../label/... .conll
      /**
        * /home/holger/Downloads/20news-18828/rec.motorcycles/104837.conll
        * /home/holger/Downloads/20news-18828/rec.motorcycles/103173.conll
        * /home/holger/Downloads/20news-18828/rec.motorcycles/105220.conll
        **/
    }
  }

}
