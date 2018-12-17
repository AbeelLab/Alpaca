package genome_kmerizer

import java.io.{File, PrintWriter}

import com.sksamuel.avro4s.{AvroInputStream, AvroSchema}
import utilities.FileHandling.{verifyDirectory, timeStamp}

/**
  * Author: Alex N. Salazar
  * Created on 26-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object DBmetrics {

  case class Config(alpacaDB: File = null, outputDir: File = null, sampleID: String = null, kmerSize: Int = -1)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("db-metrics") {
      opt[File]('a', "alpaca-db") required() action { (x, c) =>
        c.copy(alpacaDB = x)
      } text ("Path to alpaca database.")
      opt[File]('o', "output-dir") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory path.")
      opt[Int]("kmer-size") required() action { (x, c) =>
        c.copy(kmerSize = x)
      } text ("Kmer size used when constructing database.")
      note("\nOPTIONAl\n\n")
      opt[String]("sample-id") action { (x, c) =>
        c.copy(sampleID = x)
      } text ("If provided, will add sample ID to end-most column in the output file.")
    }

      parser.parse(args, Config()).map { config =>
        //check whether output directory exists. If not, create it.
        verifyDirectory(config.alpacaDB)
        verifyDirectory(config.outputDir)
        dbMetrics(config)
      }
  }

  //create case class for data serialization
  case class AlpacaEntry(ref_name: String, bam_index: Int, start: Int, end: Int, local_kmers: Set[String])
  //create schema for data serialization
  val schema = AvroSchema[AlpacaEntry]

  def dbMetrics(config: Config): Unit = {
    //get all alpaca files
    val alpaca_files = config.alpacaDB.listFiles().filter(_.getName.endsWith(".alpaca"))
    //create output file
    val pw = new PrintWriter(config.outputDir + "/db_metrics.total_bins.txt")
    val header = "Chrm\tTotalSubregions\tMean\tSD" + (if(config.sampleID == null) "" else "\tSample")
    val header2 = "Chrm\tStart\tKmerSetSize\tRatio" + (if(config.sampleID == null) "" else "\tSample")
    //create output file
    val pw2 = new PrintWriter(config.outputDir + "/db_metrics.kmer_set_sizes.txt")
    pw.println(header)
    pw2.println(header2)
    //iterate through each extract contig sequence and kmer set size for each subregion
    val metrics_per_contig = alpaca_files.foldLeft(Map[String, List[Int]]())((amap, file) => {
      println(timeStamp + "Processing: " + file.getName.replace(".alpaca", ""))
      //load kmerized subregions
      val kmerized_subregions = AvroInputStream.data[AlpacaEntry](file).iterator
      //extract every single subregion as well as total number of kmers
      kmerized_subregions.foldLeft(amap)((local_amap, subregion) => {
        //compute the theoretical kmer set size
        val theoretical_kmer_set_size = computeTheoreticalSetSize((subregion.end, subregion.start), config.kmerSize)
        //compute the ratio between constructued kmer set size and theoretical one
        val ratio = subregion.local_kmers.size.toDouble / theoretical_kmer_set_size
        //create sequence entry fo output file
        val entries = Seq(subregion.ref_name, subregion.start, subregion.local_kmers.size, ratio)
        //output as if in no sample ID provided
        if(config.sampleID == null) pw2.println(entries.mkString("\t"))
          //add sample ID
        else pw2.println((entries.:+(config.sampleID)).mkString("\t"))
        //update map accordingly
        val current = local_amap.getOrElse(subregion.ref_name, List[Int]())
        local_amap + (subregion.ref_name -> (current.:+(subregion.local_kmers.size)))
      })
    }).mapValues(kmer_set_sizes => getMetrics(kmer_set_sizes))
    //iterate through each contig sequence and output metrics
    metrics_per_contig.foreach(contig => {
      val entries = Seq(contig._1, contig._2._1, contig._2._2, contig._2._3)
      if(config.sampleID == null) pw.println(entries.mkString("\t"))
      else pw.println((entries.:+(config.sampleID)).mkString("\t"))
    })
    pw.close
    pw2.close
    println(timeStamp + "Successfully completed!")
  }

  /**
    * Function to compute list size, mean, and standard deviation of a list of integers
    * @return (Double, Double)
    */
  def getMetrics: List[Int] => (Int, Double, Double) = xs => {
    //compute size
    val size = xs.size
    //compute mean
    val mean = xs.sum.toDouble / size
    //compute variance
    val variance = xs.map(a => {math.pow(a.toDouble - mean, 2)}).sum / size
    //compute standard deviation
    val std = math.sqrt(variance)
    (size, mean, std)
  }

  def computeTheoreticalSetSize: ((Int,Int), Int) => Int = (coords, kmer_size) => ((coords._2 - coords._1) + 1)-kmer_size
}
