package visualization

import java.io.File

import doodle.backend.StandardInterpreter._
import doodle.core.Image._
import doodle.core._
import doodle.core.font.Font
import doodle.core.font.FontFace.Bold
import doodle.core.font.FontFamily.Monospaced
import doodle.core.font.FontSize.Points
import doodle.jvm.FileFrame.svgSave
import doodle.syntax._
import utilities.FileHandling.{openFileWithIterator, timeStamp, verifyDirectory, verifyFile}


/**
  * Author: Alex N. Salazar
  * Created on 24-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object AlpacaLayout {

  case class Config(
                     summarizedPop: File = null,
                     labels: File = null,
                     outputDir: File = null,
                     sampleOrdering: File = null,
                     chrmMappings: File = null,
                     canvasHeight: Int = 1000,
                     canvasWidth: Int = 1000,
                     chrmSpacing: Int = 20,
                     outputPrefix: String = "alpaca",
                     multipleGenomes: Boolean = false,
                     cFontSize: Int = 40,
                     sFontSize: Int = 20,
                     maxChrmSize: Int = -1,
                     originSpacing: Int = 0)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("alpaca-layout") {
      opt[File]('s', "summarized-population") required() action { (x, c) =>
        c.copy(summarizedPop = x)
      } text ("Summarized population file from alpaca similarity comparison. If comparing " +
        "multiple genomes, pass tab-delimited file containing sample ID and corresponding summarized population file " +
        "and turn on '--multiple-genomes' parameter.")
      opt[File]('l', "labels") required() action { (x, c) =>
        c.copy(labels = x)
      } text ("Tab-delimited file containing sample id, sub-lineage, lineage, hue, saturation, and lightness.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory. If it doesn't not exist, the directory will be created.")
      opt[String]("prefix") required() action { (x, c) =>
        c.copy(outputPrefix = x)
      } text ("Prefix for output file name.")
      note("\nOPTIONAL ALPACA\n")
      opt[Unit]("multiple-genomes") action { (x, c) =>
        c.copy(multipleGenomes = true)
      } text ("Visualizing multiple genomes. See '--summarized-population' parameter.")
      opt[File]("chromosome-mappings") action { (x, c) =>
        c.copy(chrmMappings = x)
      } text ("Tab-delimited file containing chromosome name, alternate name to display. Implies desired order as " +
        "well.")
      opt[File]("sample-ordering") action { (x, c) =>
        c.copy(sampleOrdering = x)
      } text ("File containing sample name, one per line, for ordering purposes.")
      note("\nOPTIONAL AESTHETICS\n")
      opt[Int]("max-size") action { (x, c) =>
        c.copy(maxChrmSize = x)
      } text ("Use a pre-defined maximum chromosome size. Will be use for scaling purposes.")
      opt[Int]("image-height") action { (x, c) =>
        c.copy(canvasHeight = x)
      } text ("Height of final image (default 1000 units).")
      opt[Int]("image-width") action { (x, c) =>
        c.copy(canvasWidth = x)
      } text ("Width of final image (default 1000 units).")
      opt[Int]("chromosome-spacing") action { (x, c) =>
        c.copy(chrmSpacing = x)
      } text ("Spacing between chromosomes drawn vertically (default 20).")
      opt[Int]("origin-spacing") action { (x, c) =>
        c.copy(originSpacing = x)
      } text ("Width of final image (default 1000 units).")
      opt[Int]("cfont-size") action { (x, c) =>
        c.copy(cFontSize = x)
      } text ("Font size for displaying chromosome name (default is 40).")
      opt[Int]("sfont-size") action { (x, c) =>
        c.copy(sFontSize = x)
      } text ("Font size for displaying sample name (default is 20). Only used when visualizing multiple samples.")
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyDirectory(config.outputDir)
      verifyFile(config.summarizedPop)
      drawGenome(config)
    }
  }

  def drawGenome(config: Config): Unit = {
    //font style
    val sfont = Font(Monospaced, Bold, Points(config.sFontSize))
    val cfont = Font(Monospaced, Bold, Points(config.cFontSize))
    //empty rectangle for spacing between chromosomes
    val empty_chrm = rectangle(config.originSpacing, config.chrmSpacing).noLine.noFill
    //empty rectangle for spacing at the origin
    val spacing_object = rectangle(config.originSpacing, config.chrmSpacing).noLine.noFill

    /**
      * Method to compute aesthetic metrics for a given a summary population file
      * @param file Summay population file as computed by Alpaca
      * @return Returns a 3-tuple: (Map(Sequence ID-> Size), max chrm size (canvas), chrm height (canvas))
      */
    def getChrmAesthetics(file: File): (Map[String, Int], Int, Double) = {
      //get contig sizes through the summarized population file
      val _chrm_sizes = openFileWithIterator(file).drop(1)
        .foldLeft(Map[String, Int]())((amap, line) => {
          val split = line.split("\t")
          val current = amap.getOrElse(split.head, 0)
          val max_size = List(current, split(2).toInt).max
          amap + (split.head -> max_size)
        })
      //max chromosome size
      val _max_chrm_size = if (config.maxChrmSize != -1) config.maxChrmSize else _chrm_sizes.map(_._2).max
      //calculate height for all chromosome drawings
      val _chrm_height = (config.canvasHeight).toDouble / (_chrm_sizes.size * 2 - 1)
      (_chrm_sizes, _max_chrm_size, _chrm_height)
    }

    //get sample labels and createa a map of sample id to color
    val labels = openFileWithIterator(config.labels).foldLeft(Map[String, Color]())((lmap, line) => {
      val tmp = line.split("\t")
      assume(lmap.get(tmp(0)) == None, line)
      lmap + (tmp(0) -> (Color.hsl(tmp(1).toInt.degrees, tmp(2).toDouble.normalized, tmp(3).toDouble.normalized)))
    })
    println(timeStamp + "Found " + labels.size + " labelled samples")
    //get alternate chromosome name mappings if provided
    val chrm_mappings = {
      if (config.chrmMappings == null) {
        openFileWithIterator(config.summarizedPop).drop(1).foldLeft(Set[String]())((b,a) => b + (a.split("\t").head))
          .toList.zipWithIndex.map(x => (x._1, (x._1, x._2))).toMap
      }
      else openFileWithIterator(config.chrmMappings).foldLeft((Map[String, (String, Int)](), 0))((mmap, line) => {
        val tmp = line.split("\t")
        (mmap._1 + (tmp.head -> (tmp(1), mmap._2)), mmap._2 + 1)
      })._1
    }
    //Alpaca visualization for a single genome
    if (!config.multipleGenomes) {
      val (chrm_sizes, max_chrm_size, chrm_height) = getChrmAesthetics(config.summarizedPop)
      println("Using individual contig sequence heights of: " + chrm_height)
      println(timeStamp + "Found the following sequences:")
      chrm_sizes.foreach(x => println(timeStamp + "--" + x))
      println(timeStamp + "Visualizing single genome")
      //build map of contig sequence, start to list of samples and their scores
      val sample_scores = loadSummarizedPopFile(config.summarizedPop)
      //iterate through each contig assembly and draw
      chrm_sizes.toList.sortBy(x => chrm_mappings(x._1)._2).foldLeft(Image.empty)((drawing, chrm) => {
        //get display name
        val display_name = {
          val tmp = chrm_mappings.get(chrm._1)
          if (tmp == None) chrm._1 else tmp.get._1
        }
        println(timeStamp + "Processing " + display_name)
        //normalize wdith of contig sequence
        val normalized_width = normalizePosition(1, config.canvasWidth, 1, max_chrm_size, chrm._2)
        //iterate through each position and draw potential mosaid structures
        val all_corresponding_positions = sample_scores.filter(_._1._1 == chrm._1).toList.sortBy(_._1._2)
        //get width for individual position drawings
        val individual_widths = normalized_width / all_corresponding_positions.size
        //create empty box to assure same width
        val empty_box = rectangle(normalizePosition(1, config.canvasWidth, 1, max_chrm_size, max_chrm_size - chrm._2),
          chrm_height).noFill.noLine
        //mosaic drawing
        val mosaic_drawing = all_corresponding_positions.foldLeft(Image.empty)((mdrawing, local_samples) => {
          mdrawing.beside(makeMosaicStructure(local_samples._2, chrm_height, individual_widths, labels))
        })
        //update drawing
        (mosaic_drawing.below(text(display_name).font(cfont).above(empty_chrm))).below(drawing)
      }).save(config.outputDir.getAbsolutePath + "/" + config.outputPrefix + ".svg")
      println("Successfully completed!")
    }
    //Alpaca visualization for multiple genomes
    else {
      println(timeStamp + "Curating population summary for multiple genomes.")
      //create a map where the keys are sample ids and values is the map data structure of the population summary
      val sample_to_sample_scores = openFileWithIterator(config.summarizedPop)
        .foldLeft(Map[String, Map[(String, Int), List[(String, Double)]]]())((ssmap, line) => {
          val tmp = line.split("\t")
          assert(ssmap.get(tmp.head) == None, "Multiple entries for the same sample ID: " + tmp.head)
          ssmap + (tmp.head -> loadSummarizedPopFile(new File(tmp(1))))
        })
      //create sample ordering based on file order
      val sample_ordering = openFileWithIterator(config.summarizedPop).foldLeft(Seq[String]())((map, line) => {
        val tmp = line.split("\t")
        map.+:(tmp.head)
      }).zipWithIndex.toMap
      //create a map of sample ID -> total number of subregion entries
      val sample_entries = sample_to_sample_scores.mapValues(_.size)
      //sanity check that they all have the same number of entries
      assert(sample_entries.forall(x => sample_entries.forall(_._2 == x._2)), "Samples do not have the same number of" +
        " subregion entries:\n" + sample_entries.toList.mkString("\t"))
      //get chrm aesthetics
      val (chrm_sizes, max_chrm_size, chrm_height) =
        getChrmAesthetics(new File(openFileWithIterator(config.summarizedPop).toList.head.split("\t")(1)))
      println(timeStamp + "Using individual contig sequence heights of: " + chrm_height)
      println(timeStamp + "Found the following sequences:")
      chrm_sizes.foreach(x => println(timeStamp + "--" + x))
      println(timeStamp + "Visualizing multiple genomes")
      //iterate through each contig assembly and draw
      chrm_sizes.toList.sortBy(x => chrm_mappings(x._1)._2).foldLeft(Image.empty)((drawing, chrm) => {
        //get chrm_name
        val chrm_name = {
          val tmp = chrm_mappings.get(chrm._1)
          if (tmp == None) chrm._1 else tmp.get._1
        }
        println(timeStamp + "--Processing " + chrm_name)
        //normalize wdith of contig sequence
        val normalized_width = normalizePosition(1, config.canvasWidth, 1, max_chrm_size, chrm._2)
        //fetch samples and the corresponding scores for current chromosome as well ordering if provided
        val grouped_drawing = sample_to_sample_scores.mapValues(_.filter(_._1._1 == chrm._1)).toList
          .sortBy(x => -sample_ordering(x._1)).foldLeft(text(chrm_name).font(cfont))((local_drawing, sample) => {
          //get display name
          val sample_name = sample._1 + "_"
          //iterate through each position and draw potential mosaic structures
          val all_corresponding_positions = sample._2.toList.sortBy(_._1._2)
          //get width for individual position drawings
          val individual_widths = normalized_width / all_corresponding_positions.size
          //create empty box to assure same width
          val empty_box = rectangle(normalizePosition(1, config.canvasWidth, 1, max_chrm_size, max_chrm_size - chrm._2),
            chrm_height).noFill.noLine
          //mosaic drawing
          val mosaic_drawing = all_corresponding_positions.foldLeft(Image.empty)((mdrawing, local_samples) => {
            mdrawing.beside(makeMosaicStructure(local_samples._2, chrm_height, individual_widths, labels))
          }).beside(empty_box)
          //updated local group drawing
          text(sample_name).font(sfont).beside(mosaic_drawing).below(local_drawing)
        }).above(empty_chrm)
        grouped_drawing.below(drawing)
      }).save(config.outputDir.getAbsolutePath + "/" + config.outputPrefix + ".svg")
      println(timeStamp + "Successfully completed!")
    }
  }

  /**
    * Function to draw mosaic classifications for a given bin
    *
    * @return Image
    */
  def makeMosaicStructure(sample_scores: List[(String, Double)],
                          chrm_height: Double,
                          default_width: Double,
                          labels: Map[String, Color]): Image = {
    //compute initial allocated size for each sample
    val default_height = chrm_height / sample_scores.size
    //get normalized size for each sample based on their jaccard index
    val samples_with_normalized_heights = {
      //normalize by jaccard index, group by color, get the total size contribution per color
      val tmp = sample_scores.map(x => (x._1, x._2 * default_height))
        .groupBy(x => labels(x._1)).mapValues(_.map(_._2).sum).toList
      //total contribution from samples
      val total_bar_size = tmp.map(_._2).sum
      //unexplained contribution
      val empty_filler = chrm_height - total_bar_size
      //add white color to display amount of unknown genetic contribution
      tmp.:+((Color.hsl(0.degrees, 0.normalized, 1.toDouble.normalized), empty_filler))
    }
    //draw individual contributions from each color (lineage)
    allAbove(samples_with_normalized_heights.sortBy(-_._2).map(x => rectangle(default_width, x._2).noLine.fillColor(x._1)))
  }

  /**
    * Function to load population summary file.
    *
    * @return Map[(String, Int), List[(String, Double)]
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

  /**
    * Method to normalize position  in respects to some range [a,b] using the normalization variable function:
    *   {(b-a)*(x - min_x) / (max_x - min_x))} + a
    * @param a Left-boundary of range
    * @param b Right-boundary of range
    * @param min_x Minimum value for all values in X
    * @param max_x Maximum value for all values in X
    * @param x Value to be normalized
    * @return Normalized value of x within range [a,b]
    */
  def normalizePosition(a: Double, b: Double, min_x: Double,
                        max_x: Double, x: Double): Double = ((b-a)*(x - min_x)/(max_x - min_x)) + a

}
