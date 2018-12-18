package cli

/**
  * Author: Alex N. Salazar
  * Created on 21-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
object Alpaca {
  def main(args: Array[String]): Unit = {
    val help = (
      "Usage: java -jar alpaca.jar [tool]\n\n" +
        "ALPACA DATABASE\n" +
        "build-db                Kmerize binned-subregions for a subject genome and store as a database.\n" +
        "db-metrics              Obtain metrics for a given database.\n\n" +
        "TARGET COMPARISON\n" +
        "genome-similarity       Compute similarity between a target genome and given alpaca database.\n" +
        "batch-submit            Automated scripts for multiple target genome comparisons.\n\n"+
        "POPULATION COMPARISON\n" +
        "population-summary      Summarize a set of target genome similarities as a population.\n" +
        "population-metrics      Obtain metrics for a summarized population.\n\n" +
        "VISUALIZATION\n" +
        "alpaca-layout           Generate an alpaca layout figure.\n" +
        "tree-tracing            Generate a tree tracing figure.\n\n" +
        "MISC.\n"+
        "mash-sketches           Automated script for constructing mash sketches.\n"+
        "mash-distances          Automated script for computing paiwise mash distance.\n"
      )

    if (args.length == 0) {
      println(help)
    } else {
      args(0) match {
        case "build-db" => genome_kmerizer.BuildDB.main(args.drop(1))
        case "genome-similarity" => genome_comparison.GenomeSimilarity.main(args.drop(1))
        case "db-metrics" => genome_kmerizer.DBmetrics.main(args.drop(1))
        case "population-summary" => genome_comparison.PopulationSummary.main(args.drop(1))
        case "batch-submit" => genome_comparison.BatchSubmit.main(args.drop(1))
        case "population-metrics" => genome_comparison.PopulationMetrics.main(args.drop(1))
        case "mash-sketches" => mash.ConstructMashSketches.main(args.drop(1))
        case "mash-distances" => mash.ConstructMashDistance.main(args.drop(1))
        case "tree-tracing" => visualization.TreeTracing.main(args.drop(1))
        case "alpaca-layout" => visualization.AlpacaLayout.main(args.drop(1))
        case _ => println(help)
      }
    }
  }

}
