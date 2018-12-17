package genome_comparison

import java.io.{File, PrintWriter}

import utilities.FileHandling.{makeSLURMheader, openFileWithIterator, timeStamp, verifyDirectory, verifyFile}

/**
  * Author: Alex N. Salazar
  * Created on 22-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object BatchSubmit {

  case class Config(
                     alpacaDB: File = null,
                     alpacaJar: File = null,
                     bamFiles: File = null,
                     minMapq: Int = 5,
                     kmerSize: Int = 21,
                     verbose: Boolean = false,
                     exclude: String = null,
                     outputDir: File = null,
                     cpus: Int = 1,
                     memory: Int = 10000,
                     genomeSize: Int = -1,
                     time: Int = 1,
                     partition: String = "general",
                     qos: String = "short"
                   )

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("batch-submit") {
      opt[File]('a', "alpaca-db") required() action { (x, c) =>
        c.copy(alpacaDB = x)
      } text ("Alpaca database path.")
      opt[File]('b', "alpaca-jar") required() action { (x, c) =>
        c.copy(alpacaJar = x)
      } text ("Alpaca jar path.")
      opt[File]('l', "bam-list") required() action { (x, c) =>
        c.copy(bamFiles = x)
      } text ("List of BAM files.")
      opt[Int]('g', "genome-size") required() action { (x, c) =>
        c.copy(genomeSize = x)
      } text ("Approximate genome size.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory")
      note("\nOPTIONAL ALPACA\n")
      opt[Int]("kmer-size") action { (x, c) =>
        c.copy(kmerSize = x)
      } text ("Size of kmers (default is 21).")
      opt[Int]("min-mapq") action { (x, c) =>
        c.copy(minMapq = x)
      } text ("Minimum mapping quality (default is 5).")
      opt[String]("exclude") action { (x, c) =>
        c.copy(exclude = x)
      } text ("Exclude specific contig sequences from analysis (comma separated)")
      opt[Unit]("verbose") action { (x, c) =>
        c.copy(verbose = true)
      }
      note("\nOPTIONAL SLURM\n")
      opt[Int]("allocated-cpus") action { (x, c) =>
        c.copy(cpus = x)
      } text ("Allocated CPUs for every alignment job (default is 1).")
      opt[Int]("allocated-memory") action { (x, c) =>
        c.copy(memory = x)
      } text ("Allocated memory for every alignment job in Mb (default is 10000).")
      opt[Int]("allocated-runtime") action { (x, c) =>
        c.copy(time = x)
      } text ("Allocated time for job in hours (default is 2).")
      opt[String]("partition") action { (x, c) =>
        c.copy(partition = x)
      } text ("Desired partition for jobs to be submitted to (default is \"general\".")
      opt[String]("qos") action { (x, c) =>
        c.copy(qos = x)
      } text ("Desired qos for jobs to be submitted under (default is \"short\".")
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyFile(config.bamFiles)
      verifyDirectory(config.alpacaDB)
      verifyDirectory(config.outputDir)
      createBatchSubmitScript(config)
    }
  }

  def createBatchSubmitScript(config: Config): Unit = {
    //open list of bam files
    val bam_list = openFileWithIterator(config.bamFiles).toList.map(_.split("\t")).map(x => (x(0), new File(x(1))))
    println(timeStamp + "Found " + bam_list.size + " samples")
    //assure file paths are valid
    bam_list.foreach(sample => verifyFile(sample._2))
    //iterate through bam list
    bam_list.foreach(bam => {
      println(timeStamp + "--Processing " + bam._1)
      //make local directory
      val local_directory = new File(config.outputDir + "/" + bam._1)
      local_directory.mkdir()
      //create submit script
      val submit_script = new PrintWriter(local_directory + "/runAlpaca.sh")
      //make slurm header
      submit_script.println(makeSLURMheader(config.memory, config.time, config.partition, config.qos,
        local_directory.getAbsolutePath + "/" + bam._1 + ".out", config.cpus))
      submit_script.println(Seq("srun","java", "-Xmx" + config.memory + "m", "-jar", config.alpacaJar.getAbsolutePath,
        "genome-similarity", "-a", config.alpacaDB.getAbsolutePath, "-b", bam._2.getAbsolutePath,
        "--prefix", bam._1, "--kmer-size", config.kmerSize, "--min-mapq", config.minMapq, "--genome-size", config.genomeSize,
        "-o", local_directory.getAbsolutePath).mkString(" "))
      submit_script.close
    })
    println(timeStamp + "Successfully completed!")
  }
}
