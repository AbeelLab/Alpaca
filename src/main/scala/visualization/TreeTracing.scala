package visualization

/**
  * Author: Alex N. Salazar
  * Created on 20-2-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */

import java.io.{BufferedReader, File, FileInputStream, InputStreamReader}

import _root_.Newick.{TreeNode, TreeParser}
import doodle.backend.StandardInterpreter._
import doodle.core.Image._
import doodle.core.PathElement.{lineTo, moveTo}
import doodle.core._
import doodle.core.font.FontFace.Bold
import doodle.core.font.FontFamily.Monospaced
import doodle.core.font.FontSize.Points
import doodle.core.font._
import doodle.jvm.FileFrame.svgSave
import doodle.syntax._
import utilities.FileHandling.{openFileWithIterator, timeStamp, verifyDirectory, verifyFile}

object TreeTracing {

  case class Config(
                     tree: File = null,
                     outputDir: File = null,
                     canvasHeight: Int = 1000,
                     canvasWidth: Int = 1000,
                     branchWidth: Int = 10,
                     fontSize: Int = 25,
                     labels: File = null,
                     proportions: File = null,
                     minProportionsValue: Double = 0.05,
                     proportionValueOffset: Int = 15,
                     maxChar: Int = 10,
                     hue: Double = 0,
                     saturation: Double = 0,
                     mappings: File = null,
                     prefix: String = null,
                     featureFraction: Double = 0.75,
                     lineWidth: Int = -1,
                     features: File = null)

  def main(args: Array[String]) {
    val parser = new scopt.OptionParser[Config]("tree-tracing") {
      opt[File]('t', "tree") required() action { (x, c) =>
        c.copy(tree = x)
      } text ("Binary tree in Newick format.")
      opt[File]('o', "output-directory") required() action { (x, c) =>
        c.copy(outputDir = x)
      } text ("Output directory. If it doesn't not exist, the directory will be created.")
      opt[File]("proportions") required() action { (x, c) =>
        c.copy(proportions = x)
      } text ("Tab-delimited file containing: sample ID, absolute counts. Used to draw thickness of branches to " +
        "emphasize frequency of paths. Uses aesthetic parameter '--branch-width'. Assumes a header line.")
      opt[String]("prefix") required() action { (x, c) =>
        c.copy(prefix = x)
      } text ("Prefix for output file name.")
      note("\nOPTIONAL METADATA\n")
      opt[File]("labels") action { (x, c) =>
        c.copy(labels = x)
      } text ("Tab-delimited file containing labels per sample, on per line: sample,label,hue,saturation,lightness")
      opt[File]("mappings") action { (x, c) =>
        c.copy(mappings = x)
      } text ("Tab-delimited file containing: sample ID, sample display name")
      note("\n\nOPTIONAL AESTHETICS\n")
      opt[Double]("min-proportion") action { (x, c) =>
        c.copy(minProportionsValue = x)
      } text ("Display the proportion at every node only when its at least this value (default is 0.05).")
      opt[Int]("proportion-offset") action { (x, c) =>
        c.copy(proportionValueOffset = x)
      } text ("Display proportion offset from node by this value (default is 15).")
      opt[Int]("image-height") action { (x, c) =>
        c.copy(canvasHeight = x)
      } text ("Height of final image (default 1000 units).")
      opt[Int]("image-width") action { (x, c) =>
        c.copy(canvasWidth = x)
      } text ("Width of final image (default 1000 units).")
      opt[Int]("font-size") action { (x, c) =>
        c.copy(fontSize = x)
      } text ("Font-size of labels (default is 25).")
      opt[Int]("max-char") action { (x, c) =>
        c.copy(maxChar = x)
      } text ("Maximum number of characters to display (default is 10).")
      opt[Int]("branch-width") action { (x, c) =>
        c.copy(branchWidth = x)
      } text ("Line width for feature boxes (default is \"font-size\"/4).")
      opt[Int]("line-width") action { (x, c) =>
        c.copy(lineWidth = x)
      } text ("Line width for feature boxes (default is \"font-size\"/4).")
    }
    parser.parse(args, Config()).map { config =>
      //check whether output directory exists. If not, create it.
      verifyDirectory(config.outputDir)
      verifyFile(config.tree)
      drawTree(config)
    }
  }


  def drawTree(config: Config): Unit = {

    /**
      * Case classes for labels. Might consider moving them to utilities later.
      * @param hue
      * @param saturation
      * @param lightness
      */
    case class HSL(hue: Angle, saturation: Normalized, lightness: Normalized)
    case class Label(name: String, hsl: HSL)


    println(timeStamp + "Opening Newick-formatted tree")
    //Open and parse newick formatted tree
    val fileReader = new BufferedReader(new InputStreamReader(new FileInputStream(config.tree), "UTF-8"))
    //load tree
    val tree = new TreeParser(fileReader).tokenize("Newick Tree")
    //get root
    val root = tree.getRoot
    //compute total number of leafs
    val total_leafs = root.numberLeaves + 2
    //attempt to get features
    val features = {
      if(config.features == null) Map[String,Seq[Double]]()
      else {
        println(timeStamp + "User specified features file")
        val tmp = openFileWithIterator(config.features).toList.map(_.split("\t")).map(x => (x(0), x.tail.map(_.toDouble).toSeq)).toMap
        val max = tmp.map(_._2.max).toList.max
        val min = tmp.map(_._2.min).toList.min
        tmp.mapValues(x => x.map(y => 1 - (y-min)/(max-min)))
      }
    }
    //get total number of features
    val total_features = if (features.isEmpty) 1 else features.map(_._2.size).max
    val size_sanity = features.filter(_._2.size != total_features)
    //sanity check
    if(config.features != null && !size_sanity.isEmpty) {
      println(timeStamp + "Warning: not all samples have the same number of features: " +
        size_sanity.map(x => "--" + x._1 + "\t" + x._2.mkString("\t")).foreach(println)
      )
    }
    //sanity check
    if(config.features != null && total_leafs-2 != features.size)
      println ("Warning: total number samples found in features file (" + features.size + ") " +
        "is not the same as total number of samples in tree (" + (total_leafs-2) + "). Colour intensity may be skewed" +
        " by missing samples." + timeStamp)
    //attempt to get labels
    val labels = {
      if(config.labels == null) Map[String, Label]()
      else {
        openFileWithIterator(config.labels).drop(1).toList.map(_.split("\t")).map(x => {
          val hsl = new HSL(x(1).toDouble.degrees, x(2).toDouble.normalized, x(3).toDouble.normalized)
          (x.head, new Label(x(4), hsl))
        }).toMap
      }
    }
    //open mappings file
    val mappings = {
      if(config.mappings == null) Map[String,String]()
      else {
        println(timeStamp + "User specified mapping file (sample ID to display name)")
        openFileWithIterator(config.mappings).toList.map(_.split("\t")).map(x => (x(0),x(1))).toMap
      }
    }
    //sanity check
    if(config.mappings != null && total_leafs-2 != mappings.size)
      println(timeStamp + "Warning: total number of samples in tree does not match total number of samples in mapping" +
        " file")
    //open proportions file if provided
    val proportions = {
      if(config.proportions == null) Map[String, Double]()
      else {
        println(timeStamp + "Proportions file provided")
        val tmp = openFileWithIterator(config.proportions).toList
        tmp.drop(2).map(_.split("\t")).map(x => x(0) -> x(1).toDouble).toMap
      }
    }
    val proportions_sum = if(proportions.isEmpty) -1.0 else proportions.toList.map(_._2).sum
    if(config.proportions != null) println(timeStamp + "--Total proportions count: " + proportions_sum)
    val true_width = (config.canvasWidth * config.featureFraction) //+ (config.maxChar*config.fontSize)/2

    //Y spacing of leafs
    val ys = config.canvasHeight.toDouble / total_leafs
    //font style
    val font = Font(Monospaced, Bold, Points(config.fontSize))


    /**
      * Method to traverse tree post order to iteratively:
      * -Add all leaf nodes and pre-computed Y-value based on heirchical order from top
      * -Add all parent nodes nd pre-compute y-value based on pre-compute Y-values of children
      * -Retain maximum branch length (distance)
      * -Retain maximum level of tree
      *
      * @param node  Current node
      * @param slots Hashcode of node and it's pre-compute Y-value
      * @param dists All distances
      * @param s     Slot number of node
      * @param n     Maximum level of tree
      * @return
      */
    def getSlotsAndNodesPostOrder(node: TreeNode, slots: Map[Int, Double],
                                  dists: List[Double],
                                  s: Int, n: Int): (Map[Int, Double], Int, Double) = {
      //update maximum level in the tree
      val n_updated = if (node.height > n) node.height else n
      //update y-value for leaf node, update leaf count
      if (node.isLeaf)
        getSlotsAndNodesPostOrder(node.posorderNext,
          slots + (node.hashCode() -> s * ys), node.weight :: dists, s + 1, n_updated)
      else {
        //compute y-value for parent node based on right/left child
        val y_value = (slots(node.getChild(0).hashCode()) + slots(node.getChild(1).hashCode())) / 2
        //if root, return collections
        if (node.isRoot)
          (slots + (node.hashCode() -> y_value), n_updated + 2, dists.max)
        //else add parent node and it's y-value
        else
          getSlotsAndNodesPostOrder(node.posorderNext,
            slots + (node.hashCode() -> y_value),
            node.weight :: dists, s, n_updated)
      }
    }

    /**
      * Obtain map of hierchical order of nodes and set of all nodes (leaf+parents) in the tree
      */
    val (map_node2slot, total_levels, max_dist) = getSlotsAndNodesPostOrder(root.leftmostLeaf, Map(), List(), 1, 1)
    println(timeStamp + "Found " + map_node2slot.size + " nodes with " + total_levels + " total levels and maxmium " +
      "distance of " + max_dist)

    /**
      * Function to normalize weight
      *
      * @return
      */
    def normalize: Double => Double = x => x / (max_dist)

    /**
      * Method to calculate sum of branch length (distance) from a given node to the root
      *
      * @param node
      * @param dist
      * @return
      */
    def getCumulativeDist(node: TreeNode, dist: Double): Double = {
      if (node.isRoot) dist
      else getCumulativeDist(node.parent, dist + normalize(node.weight)) //normalize(node.weight))
    }

    //true line width based on user input
    val true_line_width = if(config.lineWidth == -1) config.fontSize/4 else config.lineWidth

    /**
      * Method to draw feature box given box dimension and hsl lightness
      * @param w
      * @param h
      * @param lightness
      * @return
      */
    def squareCell(w: Double, h: Int, lightness: Double): Image =
        Image.rectangle(w, h).lineWidth(true_line_width)
          .fillColor(Color.hsl(config.hue.degrees, config.saturation.normalized, lightness.normalized))

    //compute size for box based on total number of features
    val feature_size = (config.canvasWidth*(1-config.featureFraction)) / total_features


    /**
      * Function to draw feature boxes given node and coordinates
      * @return
      */
    def drawFeatures: (String, (Double, Double)) => Image = (node, coords) => {
      //get features for current leaf node
      val values = features.get(node)
      //sanity check
      if(values == None) {
        println(timeStamp + "Warning: could not draw features for " + node)
        Empty
      } else {
        //draw all features next to leaf
        allBeside((0 to total_features - 1).toList map { lightness =>
          squareCell(feature_size, config.fontSize, values.get(lightness))}).at(coords._1, coords._2)
      }
    }


    def getLeaves(remaining: List[TreeNode], leaves: List[String]): List[String] = {
      if(remaining.isEmpty) leaves
      else if(remaining.head.isLeaf) getLeaves(remaining.tail, leaves.:+(remaining.head.label))
      else  {
        val total_children = remaining.head.numberChildren()
        val updated_remaining = (0 until remaining.head.numberChildren())
          .foldLeft(remaining.tail)((acc, index) => acc.:+(remaining.head.getChild(index)))
        getLeaves(updated_remaining, leaves)
      }
    }

    /**
      * Function to draw leaf node
      * @return
      */
    def drawNode: (String, (Double, Double)) => Image = (name, coords) => {
      //get colour for leaf
      val label_colour = {
        //try to get colour label
        val label = labels.get(name)
        //default to black is can't get sample's value
        if(label == None) Color.black
          //create clolour
        else Color.hsl(label.get.hsl.hue, label.get.hsl.saturation, label.get.hsl.lightness)
      }
      val display_name = {
        val display = mappings.get(name)
        if(display == None) name else display.get
      }
      //draw feature blocks next to leaf node
      text(display_name.take(config.maxChar)).font(font).fillColor(label_colour)
        .at(coords._1 + (config.fontSize * config.maxChar)*0.3, coords._2)
    }

    /**
      * Function to draw branch given parent coordinates and leaf coordinates
      *
      * @return
      */
    def drawBranch: (TreeNode, (Double, Double), (Double, Double)) => Image = (node, current, previous) => {
      val node_proportions = {
        if(proportions.isEmpty) 0.0
        else {
          val tmp = getLeaves(List(node), List()).map(proportions.get(_)).map(x => if(x == None) 0 else x.get).sum / proportions_sum
          BigDecimal(tmp).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble
        }
      }
      //draw branch
      val branch = {
        val tmp = openPath(List(
          moveTo(previous._1, previous._2),
          lineTo(previous._1, current._2),
          lineTo(current._1, current._2)))
        if(proportions.isEmpty) tmp
        else tmp.lineWidth(config.branchWidth*node_proportions)
      }

      //if leaf, draw node
      if (!node.isLeaf) {
        if(proportions.isEmpty || node_proportions < config.minProportionsValue) branch
        else text(node_proportions.toString).font(font).at(current._1 + config.proportionValueOffset, current._2).on(branch)
      }
      else {
        //draw node
        val with_node = drawNode(node.label, current).on(branch)
        //if not features where provided, return image above
        if(features.isEmpty) with_node
          //features were provided, draw them
        else {
          val scaling_factor = (1 - (config.canvasWidth/1000.toDouble)/10) + 0.1
          drawFeatures(node.label,
            ((config.fontSize * config.maxChar)*0.3 + feature_size*total_features*scaling_factor + current._1,
              current._2)).on(with_node)
        }
      }
    }

    /**
      * Function to compute X,Y coordinates for a given node
      *
      * @return
      */
    def getCoords: (TreeNode) => (Double, Double) = (node) => {
      //get cumulative branch length (distance)
      val dist = getCumulativeDist(node, 0)
      //calculate x position as a fraction of canvas width
      val x = true_width * dist
      //get pre-computed y-value
      val y = map_node2slot(node.hashCode())
      (x, y)
    }

    /**
      * Function to add children of a node to a list
      *
      * @return
      */
    def getChildren: (TreeNode, List[TreeNode]) => List[TreeNode] = (node, list) => {
      if (node.isLeaf) list else (0 to node.numberChildren() - 1).foldLeft(list)((b, a) => node.getChild(a) :: b)
    }

    /**
      * Method to recursively draw tree. Recursively traverses every subtree and draws branches/nodes
      *
      * @param toVisit Nodes to visit
      * @param drawing tree image
      * @return
      */
    def drawSubTree(toVisit: List[TreeNode], drawing: Image): Image = {
      if (toVisit.isEmpty) drawing
      else {
        //current node
        val current_node = toVisit.head
        //compute current nodes coordinates
        val (x, y) = getCoords(current_node)
        //for when it is the first iteration
        if (current_node.isRoot) {
          drawSubTree(getChildren(current_node, toVisit.tail), drawBranch(current_node, (x,y), (x-10,y)))
        }
        else {
          //compute parent node coordinates
          val (x2, y2) = getCoords(current_node.parent)
          //recursively draw
          drawSubTree(getChildren(current_node, toVisit.tail), drawBranch(current_node, (x, y), (x2, y2)).on(drawing))
        }
      }
    }
    //create output file
    val output_file = new File(config.outputDir + "/" + config.prefix + ".svg")
    println(timeStamp + "Drawing:")
    //draw phylogenetic tree
    val phylo_tree = drawSubTree(List(root), Image.Empty)
    //if no labels were provided
    if(labels.isEmpty) phylo_tree.save(output_file.getAbsolutePath)
    else {
      println(timeStamp + "--Adding legend")
      //get all label groups
      val labels_groups = labels.map(_._2).toList.groupBy(_.name).mapValues(_.head).toList
      //draw legend
      val legend = labels_groups.map(group => {
        text(group._1).font(font)
          .fillColor(Color.hsl(group._2.hsl.hue, group._2.hsl.saturation, group._2.hsl.lightness))
          .above(rectangle(5,5).noLine.noFill)
      })
      println(timeStamp + "Writing to disk")
      //add legend to phylogenetic tree
      allAbove(legend).above(phylo_tree).save(output_file.getAbsolutePath)
    }
    println(timeStamp + "Successfully completed!")

  }

}
