package mash

import java.io.{File, PrintWriter}

import utilities.FileHandling.{getFileName, openFileWithIterator, timeStamp, verifyDirectory, verifyFile}
import atk.Tool.progress

import scala.annotation.tailrec
import scala.sys.process.Process

/**
  * Author: Alex N. Salazar
  * Created on 18-12-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object ConstructMashDistance {

  case class Config(
                     sketches: File = null,
                     outputDir: File = null,
                     prefix: String = null)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("mash-distance") {
      opt[File]('s', "sketches") required() action { (x, c) =>
        c.copy(sketches = x)
      } text ("List of sketches, one per line.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory. If it doesn't not exist, the directory will be created.")
      opt[String]('p', "prefix") required() action { (x, c) =>
        c.copy(prefix = x)
      } text ("Prefix for output matrix.")

    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyDirectory(config.outputDir)
      verifyFile(config.sketches)
      mashDistance(config)
    }
  }

  def mashDistance(config: Config): Unit = {
    /**
      * Create MASH dist command given two sketch files.
      *
      * @return String of MASH command
      */
    def runMashDistance: (File, File) => Double = (ref, subj) => {
      val line = Process(Seq("mash", "dist", ref.getAbsolutePath, subj.getAbsolutePath)).!!
      line.split("\t")(2).toDouble
    }

    //open list of sketches
    val sketches = openFileWithIterator(config.sketches).toList.map(new File(_))
    println("Verifying sketch files are valid" + timeStamp)
    //assert that all sketch files are valid
    sketches.foreach(verifyFile(_))

    /**
      * Tail-recursive method to compute all pairwise mash distances for a given list of sketches
      * @param remaining_sketches
      * @param distances
      * @return
      */
    @tailrec def pairwiseMashDist(remaining_sketches: List[File],
                         distances: List[(String, String, Double)]): List[(String, String, Double)] = {
      remaining_sketches match {
        //no more sketches to process
        case Nil => distances
        // continue as subj (head) and targets (tail)
        case (subj :: targets) => {
          //get name of subj
          val subj_name = getFileName(subj)
          //update distances
          val updated_distances = targets.foldLeft(distances)((acc_distances,target) => {
            progress(1000)
            //get name of target
            val target_name = getFileName(target)
            //compute mash distance
            val mash_dist = runMashDistance(subj, target)
            //add mash distance to list and move on
            (subj_name, target_name, mash_dist) :: acc_distances
          })
          //move on with targets
          pairwiseMashDist(targets, updated_distances)
        }
      }
    }
    println(timeStamp + "Computing pairwise mash distances:")
    //compute all pairwise mash distances in the given list of sketches and convert to map
    val all_mash_dist = {
      //compute pairwise mash distance
      pairwiseMashDist(sketches, List())
          //iterate through each distance and append two 2-tuples: ((subj,target), distance) along with a swap of the
        // subj and target
        .foldLeft(List[((String, String), Double)]()){case (dist_map, (subj,target,dist)) => {
          ((target,subj),dist) :: (((subj,target), dist) :: dist_map)
        }}.toMap
    }
    println(timeStamp + "Formatting to matrix and writing to disk")
    //set order of sketches
    val sketch_order = sketches.map(getFileName(_))
    //create output file
    val pw = new PrintWriter(config.outputDir + "/" + config.prefix + ".matrix")
    //output columns
    pw.println(sketch_order.mkString("\t"))
    //output matrix
    sketch_order.foreach(subj => {
      //output row name
      pw.print(subj)
      //iterate through order
      sketch_order.foreach(target => {
        //get mash distance
        val mash_dist = if(subj == target) 0.0 else all_mash_dist((subj, target))
        //output mash dist
        pw.print("\t" + mash_dist)
      })
      pw.println
    })
    pw.close()
    println(timeStamp + "Successsfully completed!")
  }

}
