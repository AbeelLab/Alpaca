package genome_comparison

import java.io.{File, PrintWriter}

import com.sksamuel.avro4s.{AvroInputStream, AvroSchema}
import utilities.FileHandling.{openFileWithIterator, timeStamp, verifyDirectory, verifyFile}

import scala.annotation.tailrec
import scala.math.BigDecimal

/**
  * Author: Alex N. Salazar
  * Created on 23-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object PopulationSummary {

  case class Config(
                     simFiles: File = null,
                     alpacaDB: File = null,
                     topSamples: Int = 1,
                     maxTopProportion: Double = 0.15,
                     prefix: String = null,
                     minKmers: Int = -1,
                     outputDir: File = null)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("summarize-population") {
      opt[File]('a', "alpaca-db") required() action { (x, c) =>
        c.copy(alpacaDB = x)
      } text ("Alpaca database path.")
      opt[File]('l', "similarity-list") required() action { (x, c) =>
        c.copy(simFiles = x)
      } text ("List of alpaca similarity file paths.")
      opt[String]("prefix") required() action { (x, c) =>
        c.copy(prefix = x)
      } text ("Prefix for output file and sample ID.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory")
      note("\nOPTIONAL\n")
      opt[Double]("max-top-proportion") action { (x,c) =>
        c.copy(maxTopProportion = x)
      } text("Ovewrite jaccard index to 0.0 if there is more than this proportion of top samples for a single " +
        "subregion (default is 0.15).")
      opt[Int]("min-kmers") action { (x, c) =>
        c.copy(minKmers = x)
      } text ("Minimum number of kmers (as an integer value) from target required to compute jaccard index (default " +
        "is 0.2*subregion-length).")
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyFile(config.simFiles)
      verifyDirectory(config.outputDir)
      verifyDirectory(config.alpacaDB)
      summarizeComparison(config)
    }
  }

  //create case class for data serialization
  case class AlpacaEntry(ref_name: String, bam_index: Int, start: Int, end: Int, local_kmers: Set[String])

  //create schema for data serialization
  val schema = AvroSchema[AlpacaEntry]

  def summarizeComparison(config: Config): Unit = {
    //assert that all paths exists
    val population = openFileWithIterator(config.simFiles).toList.map(new File(_))
    population.foreach(verifyFile(_))
    //open alpaca db and get all contig sequences and corresponding subregions
    val top_scored_samples = {
      println(timeStamp + "Extracting subregions:")
      //get all alpaca files inside the db
      val all_regions = config.alpacaDB.listFiles().filter(_.getName.endsWith(".alpaca"))
        //iterate through each alpaca file and gather contig and subregions into a set
        .foldLeft(Set[(String, Int, Int)]())((set, file) => {
        println(timeStamp + "--Processing " + file.getName.replace(".alpaca", ""))
        //open current alpaca file and add subreions to set
        AvroInputStream.data[AlpacaEntry](file).iterator.foldLeft(set)((local_set, subregion) => {
          //add subregion to set
          local_set + ((subregion.ref_name, subregion.start, subregion.end))
        })
      })
      println(timeStamp + "Found " + all_regions.size + " subregions")
      //create map
      all_regions.map(x => x -> Seq[(String, Double)]()).toMap
    }

    /**
      * Function to determine whether given subregion contains minimum number of kmers in the set
      */
    def isMinTkmerSize: SubregionLine => Boolean = subregion => {
      if (config.minKmers != -1) subregion.tksize >= config.minKmers
      else subregion.tksize >= (((subregion.end - subregion.start) + 1) * 0.1).toInt
    }

    def isWithinMaxProportion: Int => Boolean = total_samples =>
      total_samples <= round(population.size * config.maxTopProportion)

    //create output file
    val pw = new PrintWriter(config.outputDir + "/summarized_population.txt")
    pw.println("Chrm\tStart\tEnd\tTotalSamples\tJaccardIndex\tSamples")
    println(timeStamp + "Summarizing population")
    //partial function for getting top scorers
    val partialTopScorer = getTopScorers(config.topSamples) _
    //iterate through population and add to top soring map only if it's the highest scoring sample observed
    population.foldLeft(top_scored_samples)((map, sample) => {
      openFileWithIterator(sample).drop(1).foldLeft(map)((local_map, _subregion) => {
        //parse current subregion
        val subregion = toSubregionLine(_subregion)
        //get current entry in map
        val current_top_samples = local_map((subregion.chrm, subregion.start, subregion.end))
        if (!isMinTkmerSize(subregion)) local_map
        else {
          //add entry to map
          local_map + ((subregion.chrm, subregion.start, subregion.end) ->
            (current_top_samples.:+((subregion.sample, round(subregion.jaccard_index)))))
        }
      })
      //sort each region and get all top scorers
    }).mapValues(all_sample_entries => partialTopScorer(all_sample_entries.sortBy(-_._2), Seq(), Seq()).flatten)
      //iterate through each subregion and print
      .foreach(subregion => {
      val jaccard_index = if(subregion._2.isEmpty || !isWithinMaxProportion(subregion._2.size)) 0.0 else subregion._2.head._2
      //output to file
        pw.println(Seq(subregion._1._1, subregion._1._2, subregion._1._3, subregion._2.size, jaccard_index,
          subregion._2.map(_._1).mkString(";")).mkString("\t"))
      })
    pw.close
    println(timeStamp + "Successfully completed!")
  }

  case class SubregionLine(chrm: String, start: Int, end: Int, sample: String, tksize: Int, jaccard_index: Double)

  def toSubregionLine: String => SubregionLine = line => {
    val tmp = line.split("\t")
    new SubregionLine(tmp.head, tmp(1).toInt, tmp(2).toInt, tmp(4), tmp(5).toInt, tmp(6).toDouble)
  }

  def round: Double => Double = value => {
    if (value.isNaN) Double.NaN else BigDecimal(value).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

  /**
    * Curried method to tail-recursively add top X samples given a descending sequence of samples and their scores.
    * Creates a sequence with X ranks. Each rank will contain multiple samples if different samples have the same
    * score as that rank's top scorer.
    *
    * @param top_samples
    * @param samples
    * @param acc
    * @param top_scorers
    * @return
    */
  @tailrec def getTopScorers(top_samples: Int)(samples: Seq[(String, Double)],
                                               acc: Seq[(String, Double)],
                                               top_scorers: Seq[Seq[(String, Double)]]): Seq[Seq[(String, Double)]] = {
    //no more samples, return top scorers
    if (samples.isEmpty) (top_scorers.:+(acc)).filterNot(_.isEmpty)
    //if already obtained top X samples, return top scorers
    else if (top_scorers.size == top_samples) top_scorers
    //add current sample to acc if the acc is empty or it shares the same score as acc's first rank
    else if (acc.isEmpty || acc.head._2 == samples.head._2) getTopScorers(top_samples)(samples.tail, acc.:+(samples.head), top_scorers)
    //add acc to top scorers
    else getTopScorers(top_samples)(samples.tail, Seq(), top_scorers.:+(acc))
  }


}
