package genome_comparison

import java.io.{File, PrintWriter}

import utilities.FileHandling.{openFileWithIterator, timeStamp, verifyDirectory, verifyFile}

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
                     genomeSize: Int = -1,
                     mem: Int = -1,
                     clusterConfig: File = null
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
      } text ("Tab-delimited file containing: sample ID, path of BAM file. One per line.")
      opt[Int]('g', "genome-size") required() action { (x, c) =>
        c.copy(genomeSize = x)
      } text ("Approximate genome size.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory")
      opt[Int]("memory") required() action { (x, c) =>
        c.copy(mem = x)
      } text ("Memory allocation (in mb) for each job.")
      note("\nOPTIONAL\n")
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
      opt[File]("cluster-config") required() action { (x, c) =>
        c.copy(clusterConfig = x)
      } text ("Scheduler configuration file. Similarities can be computed in parallel via a cluster scheduler if a " +
        "configuration file is provided with the native scheduler parameters. See README.md for format specification.")
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
    /**
      * Function to create Alpaca similarity command given name of target sample, bam file, and local directory
      * @return String
      */
    def createAlpacaCommand: (String, File, File) => String = (name, bam, local_directory) => {
      Seq("java",
        "-Xmx" + config.mem + "m",
        "-jar", config.alpacaJar.getAbsolutePath,
        "genome-similarity", "-a", config.alpacaDB.getAbsolutePath,
        "-b", bam.getAbsolutePath,
        "--prefix", name,
        "--kmer-size", config.kmerSize,
        "--min-mapq", config.minMapq,
        "--genome-size", config.genomeSize,
        "-o", local_directory.getAbsolutePath)
        .mkString(" ")
    }
    //open list of bam files
    val bam_list = openFileWithIterator(config.bamFiles).toList.map(_.split("\t")).map(x => (x(0), new File(x(1))))
    println(timeStamp + "Found " + bam_list.size + " samples")
    //assure file paths are valid
    bam_list.foreach(sample => verifyFile(sample._2))
    //open configuration file, if provided. If not, return empty list
    val cluster_config = {
      if(config.clusterConfig == null) List("$COMMAND")
      else {
        assert(config.mem != -1, "Cluster configuration file provided. Must specify memory allocation per job.")
        openFileWithIterator(config.clusterConfig).toList
      }
    }
    //iterate through bam list
    bam_list.foreach(bam => {
      println(timeStamp + "--Processing " + bam._1)
      //make local directory
      val local_directory = new File(config.outputDir + "/" + bam._1)
      local_directory.mkdir()
      //create submit script
      val submit_script = new PrintWriter(local_directory + "/runAlpaca.sh")
      //create alpaca command
      val command = createAlpacaCommand(bam._1, bam._2, local_directory)
      //output mash command to local script
      cluster_config.foreach(line => {
        //for each line, perform the following
        submit_script.println(
          //add file path to stdout file
          line.replaceAll("\\$STDOUT", local_directory.getAbsolutePath + "/" + bam._1 + ".out")
            //add mash command
            .replaceAll("\\$COMMAND", command)
        )
      })
      submit_script.close
    })
    println(timeStamp + "Successfully completed!")
  }
}
