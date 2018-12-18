package mash

/**
  * Author: Alex N. Salazar
  * Created on 9-1-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */

import java.io.{File, PrintWriter}

import utilities.FileHandling.{verifyDirectory, verifyFile, openFileWithIterator}

object ConstructMashSketches {

  case class Config(
                     libraries: File = null,
                     bashBinary: File = null,
                     outputDir: File = null,
                     kmerSize: Int = 21,
                     kmerCount: Int = 2,
                     sketchSize: Int = 100000,
                     clusterConfig: File = null,
                     isFasta: Boolean = false)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("mash-sketches") {
      opt[File]('l', "libraries") required() action { (x, c) =>
        c.copy(libraries = x)
      } text ("Tab-delimited file containing sample name, forward reads, and reverse reads (if paired end). One " +
        "entry per line.")
      opt[File]("mash-binary") required() action { (x, c) =>
        c.copy(bashBinary = x)
      } text ("Full path to MASH binary.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory. If it doesn't not exist, the directory will be created.")
      note("\nOPTIONAL\n")
      opt[Unit]("fasta") action { (x,c) =>
        c.copy(isFasta = true)
      } text ("Libraries area all FASTA assembly files.")
      opt[Int]("kmer-size") action { (x, c) =>
        c.copy(kmerSize = x)
      } text ("Kmer size (default is 21).")
      opt[Int]("min-kmer-count") action { (x, c) =>
        c.copy(kmerCount = x)
      } text ("Minimum count for a kmer to be considered (default is 2).")
      opt[Int]("sketch-size") action { (x, c) =>
        c.copy(sketchSize = x)
      } text ("Sketch size (default is 100000).")
      opt[File]("cluster-config") required() action { (x, c) =>
        c.copy(clusterConfig = x)
      } text ("Scheduler configuration file. Sketeches can be computed in parallel via a cluster scheduler if a " +
        "configuration file is provided with the native scheduler parameters. See README.md for format specification.")

    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyDirectory(config.outputDir)
      verifyFile(config.libraries)
      verifyFile(config.bashBinary)
      constructMashSketches(config)
    }
  }

  def constructMashSketches(config: Config): Unit = {
    /**
      * Function to create mash sketch command given a sequence of read files and the sample name
      * assuming given samples are one or more FASTQ files
      * @return Mash sketch command with user-specified parameters
      */
    def makeMashSketchCommand: (Seq[File],String) => String = (files,name) =>
      Seq("zcat", files.map(_.getAbsolutePath).mkString(" "), "|", "mash", "sketch",
        "-k", config.kmerSize, "-s", config.sketchSize, "-m", config.kmerCount,
        "-o", (config.outputDir.getAbsolutePath + "/" + name + "/" + name), "-r", "-").mkString(" ")

    /**
      * Function to create mash sketch command given a sequence of read files and the sample name
      * assuming there is a single FASTA assembly file.
      * @return Mash sketch command with user-specified parameters
      */
    def makeMashSketchCommandFasta: (Seq[File], String) => String = (files, name) =>
      Seq("mash", "sketch", "-k", config.kmerSize, "-s", config.sketchSize, "-m", config.kmerCount,
        "-o", (config.outputDir.getAbsolutePath + "/" + name + "/" + name), files.head.getAbsolutePath).mkString(" ")

    //open target file
    val libraries =
      openFileWithIterator(config.libraries).toList.map(_.split("\t")).map(x => (x.head, x.tail.toSeq.map(new File(_))))
    //open configuration file, if provided. If not, return empty list
    val cluster_config = {
      if(config.clusterConfig == null) List("\\$COMMAND")
      else openFileWithIterator(config.clusterConfig).toList
    }
    println("Found " + libraries.size + " samples/libraries to sketch.")
    //iterate through each library and create mash sketch command
    libraries.foreach(library => {
      println("--Processing: " + library._1)
      //check if the files provided are valid, if not, throw warning and move on
      if(!library._2.forall(x => x.exists() && x.isFile))
        println("----WARNING: one of the libraries for sample " + library._1 + " is not a valid file.")
      else {
        //get local directory path
        val local_dir_path = config.outputDir.getAbsolutePath + "/" + library._1
        //create local directory
        val local_dir = new File(local_dir_path)
        local_dir.mkdir()
        //create local slurm script
        val local_pw = new PrintWriter(local_dir_path + "/runMashSketch.sh")
        //create mash command
        val mash_command = {
          if(config.isFasta) makeMashSketchCommandFasta(library._2, library._1)
          else makeMashSketchCommand(library._2, library._1)
        }
        //output mash command to local script
          cluster_config.foreach(line => {
            //for each line, perform the following
            local_pw.println(
              //add file path to stdout file
              line.replaceAll("\\$STDOUT", local_dir_path + "/" + library._1 + ".out")
                //add mash command
                .replaceAll("\\$COMMAND", mash_command)
            )
          })
        //local local script
        local_pw.close
      }
    })
    println("Successfully completed!")
  }


}
