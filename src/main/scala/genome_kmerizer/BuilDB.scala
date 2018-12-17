package genome_kmerizer

/**
  * Author: Alex N. Salazar
  * Created on 21-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */

import java.io._

import atk.FastaIterator
import utilities.FileHandling.{timeStamp, verifyDirectory, verifyFile}
import utilities.AlpacaUtils
import htsjdk.samtools.{SAMRecord, SamReaderFactory}
import com.sksamuel.avro4s.AvroSchema
import com.sksamuel.avro4s.AvroOutputStream
import genome_comparison.GenomeSimilarity.{automaticMinCountThreshold, getReadsOverlapping}

import scala.collection.parallel.immutable.ParVector
object BuilDB extends AlpacaUtils {

  case class Config(
                     genome: File = null,
                     bam: File = null,
                     minMapq: Int = 5,
                     kmerSize: Int = 21,
                     minCount: Int = 2,
                     binSize: Int = 2000,
                     genomeSize: Int = -1,
                     verbose: Boolean = false,
                     bamOnly: Boolean = false,
                     trustAssembly: Boolean = false,
                     includeSoftClipping: Boolean = false,
                     exclude: String = null,
                     outputDir: File = null)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("build-db") {
      opt[File]('r', "reference-assembly") required() action { (x, c) =>
        c.copy(genome = x)
      } text ("Assembly of refere" +
        "nce genome in FASTA format.")
      opt[File]('b', "bam-file") required() action { (x, c) =>
        c.copy(bam = x)
      } text ("Sorted BAM file (assumes indexed BAM file is in the same directory).")
      opt[Int]('g', "genome-size") required() action { (x, c) =>
        c.copy(genomeSize = x)
      } text ("Approximate genome size.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory")
      note("\nOPTIONAL\n")
      opt[Int]("bin-size") action { (x, c) =>
        c.copy(binSize = x)
      } text ("Size of binned subregions (default is 2000).")
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
      opt[Unit]("include-clipping") action { (x, c) =>
        c.copy(includeSoftClipping = true)
      } text ("Include soft-clipped sequence in alignments (default is false).")
      opt[Unit]("trust-assembly") action { (x, c) =>
        c.copy(trustAssembly = true)
      } text ("Trust kmers observed from assembly regardless of observed count (default is false).")
      opt[Unit]("bam-only") action { (x, c) =>
        c.copy(bamOnly = true)
      } text ("Kmerize using only read-alignments and not assembly sequence (default is false).")
      opt[String]("exclude") action { (x, c) =>
        c.copy(exclude = x)
      }
      opt[Unit]("verbose") action { (x, c) =>
        c.copy(verbose = true)
      }
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyFile(config.bam)
      verifyFile(config.genome)
      verifyDirectory(config.outputDir)
      kmerizeReferenceGenome(config)
    }
  }

  //create case class for data serialization
  case class AlpacaEntry(ref_name: String, bam_index: Int, start: Int, end: Int, local_kmers: Set[Long])

  //create schema for data serialization
  val schema = AvroSchema[AlpacaEntry]


  def kmerizeReferenceGenome(config: Config): Unit = {
    //curry kmerized method
    val kmerSubregionMethod = kmerizeSubregion(config.kmerSize) _
    println(timeStamp + "Loading FASTA assembly")
    //load fasta assembly
    //val assembly = new FastaIterator(config.genome)
    val assembly = new FastaIterator(config.genome)
    println(timeStamp + "Loading BAM header indeces")
    //get bam indeces
    val bam_indeces = getBAMindeces(config.bam)
    //create alpaca database directory
    val alpaca_directory = new File(config.outputDir + "/alpaca")
    alpaca_directory.mkdir()
    println(timeStamp + "--Found " + bam_indeces.size + " indexed sequences")
    //get all exclude contigs
    val exclude = if (config.exclude == null) List[(String)]() else config.exclude.split(",").toList
    if (config.exclude != null) println(timeStamp + "Excluding the following sequences: " + exclude.mkString(","))
    val all_contigs = fetchFastaEntries(assembly, ParVector()).filterNot(x => exclude.contains(x._1))
    //compute min kmer count
    val min_count = {
      if(config.minCount != 2) config.minCount
      else {
        println(timeStamp + "Computing mininimum kmer count based on coverage...")
        val tmp = automaticMinCountThreshold(config.bam, config.genomeSize)
        println(timeStamp + "Computed minimum kmer count of " + tmp)
        if(tmp < 2) 2 else tmp
      }
    }
    println(timeStamp + "Using minimum kmer count of " + min_count)
    //iterate through assembly and get kmerized bins
    all_contigs.foreach { case (fasta_entry) => {
      println(timeStamp + "Processing " + fasta_entry._1 +
        (if (config.bamOnly) ("\n" + timeStamp + "--Kmerizing from BAM only") else ""))
      //create output file to store serialized data for current contig
      val os = AvroOutputStream.data[AlpacaEntry](new File(alpaca_directory + "/" + fasta_entry._1 + ".alpaca"))
      //fetch bam header
      val bam_index_entry = {
        val tmp = bam_indeces.get(fasta_entry._1)
        assert(tmp != None, "Could not find BAM header entry for " + fasta_entry._1)
        tmp.get
      }
      //kmerize bins using both assembly sequence and aligned reads
      fasta_entry._2.grouped(config.binSize).foldLeft(1)((start, subregion_seq) => {
        //define subregion coordinates
        val subregion = (start, (start + config.binSize) - 1)
        //get overlapping reads for current
        val overlapping_reads = getReadsOverlapping(config.bam, fasta_entry._1, subregion)
        //kmerize bin using current overlapping reads as well as previous carry overs
        val local_kmerset = kmerSubregionMethod(overlapping_reads, fasta_entry._1, subregion,
          config.minMapq, subregion_seq, min_count, config.trustAssembly, config.includeSoftClipping, config.bamOnly)
        //create alpaca entry for serialization
        val subregion_entry = new AlpacaEntry(fasta_entry._1, bam_index_entry.index, subregion._1, subregion._2, local_kmerset)
        //write data
        os.write(subregion_entry)
        if (config.verbose) println(timeStamp + "--Total kmers found for " + subregion._1 + "-" + subregion._2 + ": " +
          local_kmerset.size)
        (subregion._2 + 1)
      })
      //flush output
      os.flush()
      //close
      os.close
    }
    }
    println(timeStamp + "Successfully completed!")
  }


  def fetchFastaEntries(assembly: FastaIterator, entries: ParVector[(String, Array[Byte])]): ParVector[(String, Array[Byte])] = {
    if (!assembly.hasNext) entries
    else {
      val current_entry = assembly.next()
      fetchFastaEntries(assembly, entries.:+(current_entry.getDescription.substring(1), current_entry.getSequence.toCharArray.map(encode(_))))
    }
  }


}
