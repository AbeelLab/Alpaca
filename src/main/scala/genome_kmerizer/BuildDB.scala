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
import utilities.FileHandling.{timeStamp, verifyDirectory, verifyFile, openFileWithIterator}
import utilities.AlpacaUtils
import com.sksamuel.avro4s.AvroSchema
import com.sksamuel.avro4s.AvroOutputStream

import scala.collection.parallel.immutable.ParVector
object BuildDB extends AlpacaUtils {

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
                     prefix: String = null,
                     exclude: File = null,
                     outputDir: File = null)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("build-db") {
      opt[File]('r', "reference-assembly") required() action { (x, c) =>
        c.copy(genome = x)
      } text ("Assembly of reference genome in FASTA format.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory")
      opt[String]('p', "prefix") required() action { (x, c) =>
        c.copy(prefix = x)
      } text ("Prefix for database name.")
      note("\nOPTIONAL ALPACA PARAMETERS\n")
      opt[Int]("bin-size") action { (x, c) =>
        c.copy(binSize = x)
      } text ("Size of binned subregions (default is 2000).")
      opt[Int]("kmer-size") action { (x, c) =>
        c.copy(kmerSize = x)
      } text ("Size of kmers (default is 21).")
      opt[File]("exclude") action { (x, c) =>
        c.copy(exclude = x)
      } text ("Text file containing names of contig/scaffolds to exclude from database, one per line.")
      opt[Unit]("verbose") action { (x, c) =>
        c.copy(verbose = true)
      }
      note("\nOPTIONAL ALIGNMENT PARAMETERS\n")
      opt[File]('b', "bam-file") action { (x, c) =>
        c.copy(bam = x)
      } text ("Sorted BAM file (assumes indexed BAM file is in the same directory). Will sample kmers the provided " +
        "alignment.")
      opt[Int]('g', "genome-size") action { (x, c) =>
        c.copy(genomeSize = x)
      } text ("Approximate genome size. Used only to automatically calculate minimum kmer count based on coverage.")
      opt[Int]("min-count") action { (x, c) =>
        c.copy(minCount = x)
      } text ("Minimum number of times a kmer must be observed to add to local kmer set of a sub-region (default is " +
        "2). Overrides default implementation of automatically calculating min count based on 0.05*(coverage). Only " +
        "used when a BAM file is provided.")
      opt[Int]("min-mapq") action { (x, c) =>
        c.copy(minMapq = x)
      } text ("Minimum mapping quality (default is 5).")
      opt[Unit]("trust-assembly") action { (x, c) =>
        c.copy(trustAssembly = true)
      } text ("Trust kmers sampled from assembly regardless of observed count.")
      opt[Unit]("bam-only") action { (x, c) =>
        c.copy(bamOnly = true)
      } text ("Kmerize using only read-alignments and not assembly sequence. Overrides '--trust-assembly' parameter.")
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyFile(config.genome)
      verifyDirectory(config.outputDir)
      if(config.bam != null) assert(config.genomeSize != -1, "BAM file provided. Must specify (approximate) genome " +
        "size")
      kmerizeReferenceGenome(config)
    }
  }

  //create case class for data serialization
  case class AlpacaEntry(ref_name: String, bam_index: Int, start: Int, end: Int, local_kmers: Set[Int])

  //create schema for data serialization
  val schema = AvroSchema[AlpacaEntry]


  def kmerizeReferenceGenome(config: Config): Unit = {
    //curry kmerized method
    val kmerSubregionMethod = kmerizeSubregion(config.kmerSize) _
    println(timeStamp + "Loading FASTA assembly")
    //load fasta assembly
    val assembly = new FastaIterator(config.genome)
    //create alpaca database directory
    val alpaca_directory = new File(config.outputDir + "/" + config.prefix)
    assume(!alpaca_directory.exists(), "Alpaca DB destination already exists: " + config.outputDir + "/" + config.prefix)
    alpaca_directory.mkdir()
    //get all exclude contigs, if provided
    val exclude = if (config.exclude == null) Set[(String)]() else openFileWithIterator(config.exclude).toSet
    if (config.exclude != null) println(timeStamp + "Excluding the following sequences: " + exclude.mkString(","))
    //load all contigs/scaffolds as array vectors
    val all_contigs = fetchFastaEntries(assembly, ParVector())
      .map(x => (x._1.split("\\s+").head, x._2)).filterNot(x => exclude.contains(x._1))
    println(timeStamp + "Loading header indeces")
    //get bam indeces, if bam provided. if not, create a pseudo one
    val header_indeces = {
      //bam file provided
      if(config.bam != null) getBAMindeces(config.bam)
      //create pseudo bam index
      else {
        //create 2-tuple of name and size
        all_contigs.seq.map(x => (x._1, x._2.size))
          //get set index and create BAMheader object
          .zipWithIndex.map(x => (x._1._1, new BAMheader(x._1._1, x._1._2, x._2))).toMap
      }
    }
    //stdout log
    println(timeStamp + "--Loaded " + header_indeces.size + " indexed sequences")
    //compute min kmer count
    val min_count = {
      //no bam provided, so take all kmers that appear atleast once
      if(config.bam == null) 1
      //user specified min kmer count
      else if(config.minCount != 2) config.minCount
        //compute min kmer count based on coverage
      else {
        println(timeStamp + "Computing mininimum kmer count based on coverage...")
        //automatically compute min kmer count
        val tmp = automaticMinCountThreshold(config.bam, config.genomeSize)
        println(timeStamp + "Computed minimum kmer count of " + tmp)
        //assure all min counts is at least 2
        if(tmp < 2) 2 else tmp
      }
    }
    println(timeStamp + "Using minimum kmer count of " + min_count)
    //iterate through assembly and get kmerized bins
    all_contigs.foreach { case (fasta_entry) => {
      //stdout log message
      println(timeStamp + "Processing " + fasta_entry._1 +
        (if (config.bamOnly) ("\n" + timeStamp + "--Kmerizing from BAM only") else ""))
      //create output file to store serialized data for current contig
      val os = AvroOutputStream.data[AlpacaEntry](new File(alpaca_directory + "/" + fasta_entry._1 + ".alpaca"))
      //fetch bam header
      val header_index_entry = {
        //attempt to get header index
        val tmp = header_indeces.get(fasta_entry._1)
        assert(tmp != None, "Could not find BAM header entry for " + fasta_entry._1)
        //return index
        tmp.get
      }
      //kmerize bins
      fasta_entry._2.grouped(config.binSize).foldLeft(1)((start, subregion_seq) => {
        //define subregion coordinates
        val subregion = (start, (start + config.binSize) - 1)
        //kmerize bin using current overlapping reads
        val local_kmerset = {
          //no bam provided, kmerize from assembly only
          if(config.bam == null){
            kmerSubregionMethod(None, fasta_entry._1, subregion, config.minMapq, subregion_seq, min_count, true, false)
          }
          //bam provided, kmerize from reads and optionally assembly
          else {
            //get overlapping reads
            val overlapping_reads = Option(getReadsOverlapping(config.bam, fasta_entry._1, subregion))
            //compute local kmerset
            kmerSubregionMethod(overlapping_reads, fasta_entry._1, subregion,
              config.minMapq, subregion_seq, min_count, config.trustAssembly, config.bamOnly)
          }
        }
        //create alpaca entry for serialization
        val subregion_entry = new AlpacaEntry(fasta_entry._1, header_index_entry.index, subregion._1, subregion._2, local_kmerset)
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

  /**
    * Method to iterate through FASTA iterator and construct a parallelized vector of contig/scaffold names to a
    * byte-encoded representation ofit's DNA sequence.
    * @param assembly
    * @param entries
    * @return
    */
  def fetchFastaEntries(assembly: FastaIterator,
                        entries: ParVector[(String, Array[Byte])]): ParVector[(String, Array[Byte])] = {
    //no more fasta entries, return parvector
    if (!assembly.hasNext) entries
    else {
      //get next fasta entry
      val current_entry = assembly.next()
      //add contig/scaffold name and it's byte-encoded sequence to parvector
      fetchFastaEntries(assembly,
        entries.:+(current_entry.getDescription.substring(1), current_entry.getSequence.toCharArray.map(encode(_))))
    }
  }


}
