package genome_comparison

import java.io.{File, PrintWriter}

import utilities.FileHandling.{openFileWithIterator, verifyDirectory, verifyFile, timeStamp}

/**
  * Author: Alex N. Salazar
  * Created on 27-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object PopulationMetrics {

  case class Config(
                     popFiles: File = null,
                     labels: File = null,
                     minSim: Double = 0.3,
                     outputDir: File = null)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("population-metrics") {
      opt[File]('p', "population-summary") required() action { (x, c) =>
        c.copy(popFiles = x)
      } text ("Population summary file")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory")
      note("\n\nOPTIONAL\n\n")
      opt[Double]("min-similarity") action { (x, c) =>
        c.copy(minSim = x)
      } text ("Minimum similarity threshold. Samples with score below this will be ommited (default is 0.3).")
      opt[File]('l', "labels") action { (x, c) =>
        c.copy(labels = x)
      } text ("Tab-delimited files containing: <sample_id>,<label_id>")
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyFile(config.popFiles)
      verifyDirectory(config.outputDir)
      populationMetrics(config)
    }
  }

  def populationMetrics(config: Config): Unit = {
    //load labels file as a map if provided
    val labels = {
      if (config.labels == null) Map[String, String]()
      else {
        openFileWithIterator(config.labels).foldLeft(Map[String, String]())((lmap, line) => {
          val split = line.split("\t")
          assume(lmap.get(split.head) == None, split.head)
          lmap + (split.head -> split(1))
        })
      }
    }
    //create output files
    val pw = new PrintWriter(config.outputDir + "/population_metrics.top_samples.txt")
    val pw2 = new PrintWriter(config.outputDir + "/population_metrics.sample_scores.txt")
    pw2.println("Sample\tJaccardIndex\tLabel")
    println(timeStamp + "Creating metrics...")
    //open summarized population file and output metrics
    val subregion2samples = loadSummarizedPopFile(config.popFiles).mapValues(_.filter(_._2 >= config.minSim)).map(_._2)
    val total_subregions = subregion2samples.size
    println(timeStamp + "Total number of subregions: " + total_subregions)
    pw.println("Sample\tInstances\tLabel")
    //merge all top scoring samples into a list, and get all instances of that sample and it's scores
    subregion2samples.flatten.groupBy(_._1).mapValues(_.map(_._2)).foreach(x => {
      val label = {
        val tmp = labels.get(x._1)
        if (tmp == None) "None" else tmp.get
      }
      x._2.foreach(y => pw2.println(x._1 + "\t" + y + "\t" + label))
    })
    pw2.close;
    //create a list of samples and it's contribution as a top sample, then output to file
    subregion2samples.map(x => x.map(y => (y._1, 1.0 / x.size))).flatten.groupBy(_._1)
      .foreach(x => {
        val label = {
          val tmp = labels.get(x._1)
          if (tmp == None) "None" else tmp.get
        }
        pw.println(x._1 + "\t" + x._2.map(_._2).sum + "\t" + label)
      })
      pw.close
    println(timeStamp + "Successfully completed!")
  }

  /**
    * Function to load population summary file.
    *
    * @return Map[(String, Int), List[(String, Double)]]
    **/
  def loadSummarizedPopFile: File => Map[(String, Int), List[(String, Double)]] = file => {
    openFileWithIterator(file).drop(1)
      //iterate through each line and add to subregion
      .foldLeft(Map[(String, Int), List[(String, Double)]]())((smap, line) => {
      //split line
      val tmp = line.split("\t")
      //get total number of samples in current subregion
      val total_samples = tmp(3).toInt
      //get current sample entries for current subregion
      val current = smap.get((tmp.head, tmp(1).toInt))
      assert(current == None, "Expected no entry record for subregion: " + (tmp.head, tmp(1).toInt))
      val samples = if(tmp.size <= 5) List[String]() else tmp(5).split(";").toList
      assert(total_samples == samples.size, "Total samples reported (" + total_samples + ") and total sample ID's " +
        "found do not match (" + tmp)
      //add sample entry for current subregion
      smap + ((tmp.head, tmp(1).toInt) -> samples.map(x => (x, tmp(4).toDouble)))
    })
  }

}
