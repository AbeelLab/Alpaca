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
        "genome-similarity       Compute similarity between a target genome and given alpaca database.\n\n." +
        "POPULATION COMPARISON\n" +
        "population-summary      Summarize a set of target genome similarities as a population.\n" +
        "population-metrics      Obtain metrics for a summarized population.\n"
      )

    if (args.length == 0) {
      println(help)
    } else {
      args(0) match {
        case "build-db" => genome_kmerizer.BuilDB.main(args.drop(1))
        case "genome-similarity" => genome_comparison.GenomeSimilarity.main(args.drop(1))
        case "db-metrics" => genome_kmerizer.DBmetrics.main(args.drop(1))
        case "population-summary" => genome_comparison.PopulationSummary.main(args.drop(1))
        case "batch-submit" => genome_comparison.BatchSubmit.main(args.drop(1))
        case "population-metrics" => genome_comparison.PopulationMetrics.main(args.drop(1))
        case _ => println(help)
      }
    }
  }

}
