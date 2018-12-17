package genome_comparison

import java.io.{File, PrintWriter}

import com.sksamuel.avro4s.{AvroInputStream, AvroSchema}
import htsjdk.samtools.{SAMRecord, SamReaderFactory}
import utilities.AlpacaUtils
import utilities.FileHandling.{timeStamp, verifyDirectory, verifyFile, openFileWithIterator}

/**
  * Author: Alex N. Salazar
  * Created on 21-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object GenomeSimilarity extends AlpacaUtils {

  case class Config(
                     alpacaDB: File = null,
                     bam: File = null,
                     targetFasta: File = null,
                     pafFile: File = null,
                     minMapq: Int = 5,
                     kmerSize: Int = 21,
                     prefix: String = null,
                     genomeSize: Int = -1,
                     verbose: Boolean = false,
                     minCount: Int = 2,
                     exclude: String = null,
                     outputDir: File = null)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("genome-similarity") {
      opt[File]('a', "alpaca-db") required() action { (x, c) =>
        c.copy(alpacaDB = x)
      } text ("Alpaca database path.")
      opt[File]('b', "bam-file") required() action { (x, c) =>
        c.copy(bam = x)
      } text ("Sorted BAM file of sample to reference genome(assumes indexed BAM file is in the same directory).")
      opt[Int]('g', "genome-size") required() action { (x, c) =>
        c.copy(genomeSize = x)
      } text ("Approximate genome size.")
      opt[String]("prefix") required() action { (x, c) =>
        c.copy(prefix = x)
      } text ("Prefix for output file and sample ID.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory")
      note("\nOPTIONAL\n")
      opt[Int]("kmer-size") action { (x, c) =>
        c.copy(kmerSize = x)
      } text ("Size of kmers (default is 21).")
      opt[Int]("min-mapq") action { (x, c) =>
        c.copy(minMapq = x)
      } text ("Minimum mapping quality (default is 5).")
      opt[Int]("min-count") action { (x, c) =>
        c.copy(minCount = x)
      } text ("Minimum number of times a kmer must be observed to add to local kmer set (default is 2). Overrides " +
        "default implementation of automatically calculating min count based on 0.05*(coverage).")
      opt[String]("exclude") action { (x, c) =>
        c.copy(exclude = x)
      } text ("Exclude specific contig sequences from analysis (comma separated)")
      opt[Unit]("verbose") action { (x, c) =>
        c.copy(verbose = true)
      }
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyFile(config.bam)
      verifyDirectory(config.alpacaDB)
      verifyDirectory(config.outputDir)
      if (config.targetFasta == null && config.pafFile == null) kmerizedComparison(config)
      else if (config.targetFasta != null && config.pafFile == null) {
        assert(false, "Target assembly provided. Requires whole genome alignment file in PAF format.")
      } else if (config.targetFasta == null && config.pafFile != null) {
        assert(false, "Whole genome alignmnet file provided. Requires target assembly in FASTA format.")
      } else {
        verifyFile(config.targetFasta)
        verifyFile(config.pafFile)
      }
    }
  }

  //create case class for data serialization
  case class AlpacaEntry(ref_name: String, bam_index: Int, start: Int, end: Int, local_kmers: Set[Long])

  //create schema for data serialization
  val schema = AvroSchema[AlpacaEntry]

  def kmerizedComparison(config: Config): Unit = {
    //list all alpaca files
    val alpaca_files = {
      //get all alpaca files
      val tmp = config.alpacaDB.listFiles().filter(_.getName.endsWith(".alpaca"))
      //if no exclusions were made, return initial list of files
      if (config.exclude == null) tmp
      //remove exclusions
      else {
        val exclude = config.exclude.split(",").toSet
        tmp.filterNot(x => exclude.contains(x.getName.replace(".alpaca", "")))
      }
    }
    //get bam_indeces
    val bam_indeces = getBAMindeces(config.bam)
    //compute min kmer count
    val min_count = {
      if (config.minCount != 2) config.minCount
      else {
        println(timeStamp + "Computing mininimum kmer count based on coverage")
        val tmp = automaticMinCountThreshold(config.bam, config.genomeSize)
        println(timeStamp + "Computed minimum kmer count of " + tmp)
        if (tmp < 2) 2 else tmp
      }
    }
    println(timeStamp + "Using minimum kmer count of " + min_count)
    println(timeStamp + "Found " + alpaca_files.size + " kmerized contig sequences")
    //curried kmerizer method
    val kmerizer = kmerizeSubregion(config.kmerSize) _
    //make output file
    val pw = new PrintWriter(config.outputDir + "/" + config.prefix + "_alpaca_similarity.txt")
    pw.println("Chrm\tStart\tEnd\tReferenceKmerSet\tTargetID\tTargetKmerSet\tJaccardIndex")
    //iterate through each alpaca file in parallel
    alpaca_files.par.foreach(alpaca => {
      //get contig name from file
      val contig_name = alpaca.getName.replace(".alpaca", "")
      println(timeStamp + "Processing " + contig_name)
      //attempt to get index entry
      val bam_index_entry = bam_indeces.get(contig_name)
      //skip if the index is not found
      if (bam_index_entry == None) {
        println(timeStamp + "WARNING: Could not find BAM index for " + contig_name + ". Skipping")
      }
      //continue
      else {
        //load kmerized subregions
        val kmerized_subregions = AvroInputStream.data[AlpacaEntry](alpaca).iterator
        //iterate through each subregion
        kmerized_subregions.foreach(subregion => {
          //get overlapping reads for current
          val overlapping_reads = Option(getReadsOverlapping(config.bam, contig_name, (subregion.start, subregion.end)))
          //get kmers for given set of reads
          val sample_kmerset = kmerizer(overlapping_reads, contig_name, (subregion.start, subregion.end),
            config.minMapq, Array[Byte](), min_count, false, true)
          //compute intersection
          val intersect = sample_kmerset.intersect(subregion.local_kmers).size.toDouble
          //compute union
          val union = sample_kmerset.union(subregion.local_kmers).size.toDouble
          //compute jaccard index
          val jaccard_index = intersect / union
          //output string
          val result = Seq(subregion.ref_name, subregion.start, subregion.end, subregion.local_kmers.size,
            config.prefix, sample_kmerset.size, jaccard_index)
          pw.println(result.mkString("\t"))
        })
        }
    })
    pw.close
    println(timeStamp + "Successfully completed!")
  }
}
