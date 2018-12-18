# Alpaca

Alpaca is a stand-alone method for identifying mosaic events in microbial genomes.

In short, different sub-regions in a microbial genome can possess
different evolutionary histories due to horizontal gene transfer and/or
genome hybridization. The ancestry for the genetic content in these
types of genomes is therefore non-linear.

To investigate non-linear ancestry, Alpaca partitions a given
reference genome into bins and computes the sequence similarity of each bin independently
across a (population) of target genomes using kmer sets. Mosaic events
can thus be identified by placing the similarity scores of each bin in context to
the population structure of the target genomes (e.g. neighbouring bins scoring
highly to different clades).

Alpaca comes with custom-made visualizations to explore similarity scores
in context to the population structure of the target genomes.


More details in our method description here.

## How to run

There are two main steps: (1) Partitioning and kmerizing a reference genome and
(2) computing similarity to a target genome.

### 1. Partitioning and kmerizing a reference genome

This can be done by creating an Alpaca database for a given reference genome:

```bash
java -jar /path/to/binary/alpaca.jar build-db -r ref.fa
-o /path/to/output/directory/ --prefix alpaca_db
```

Note that max memory usage can be specified via '-Xmx' command. For example,
running the above command with 2g of memory:

```bash
java -Xmx2g -jar /path/to/binary/alpaca.jar build-db -r ref.fa
-o /path/to/output/directory/ --prefix alpaca_db
```

#### OPTIONAL
To capture sequence heterozygosity due to multi-ploidy chromosomes, you
can provide native read-alignments:

```bash
java -Xmx2g -jar /path/to/binary/alpaca.jar build-db -r ref.fa
-o /path/to/output/directory/ --prefix alpaca_db -b ref_alignments.sorted.bam
--genome-size <value>
```

Alpaca automatically computes a minimum kmer count to avoid erroneous kmers
based on total number of aligned bases and the provided approximate genome size in nt.
You can provide your own threshold by replacing '--genome-size' with '--min-count'.

Additionally, you can obtain metrics about the database by running the following command:

```bash
java -jar /path/to/binary/alpaca.jar db-metrics -a /path/to/alpaca/db/
-o /path/to/output/directory/ --kmer-size <value>
```

Note that you must provide the size of the kmer used during the database
construction (default is 21).

Additional parameters and descriptions can be found in the command-line
interface by running the binary.

### 2. Computing similarity to a target genome

#### Against a single target genome

You can compare the similarity of all bins in a reference genome
independently against their corresponding bins in a target genome by
providing the alpaca database of the reference and read-alignments from
the target (e.g. Illumina reads aligned to the reference genome):

```bash
java -jar /path/to/binary/alpaca.jar genome-similarity -a /path/to/alpaca/db/
-b target_alignments.sorted.bam -g <value>
--prefix target_similarity -o /path/to/output/directory/
```

#### Against a population

You can automatically create individual job scripts for comparing the similarity
against a population (e.g. large sequencing dataset) by running the following command:

```bash
java -jar /path/to/binary/alpaca.jar batch-submit -a /path/to/alpaca/db/
-b /path/to/binary/alpaca.jar -l bam_list.txt -g <value> -o . --memory <value>
```

The BAM list contains information about population to be compared against.
It is a tab-delimited file containing two values in every line: sample ID and path to BAM file.

To run in a cluster scheduler, you can provide a configuration file via '--cluster-config'.
Alpaca will create individual job scripts for each comparison using parameters from the
configuration file. You simply need to add two strings: "$STDOUT" and "$COMMAND". The
former specifies the path for stdout and stderr. The latter specifies the location to insert
the alpaca command. For example, if using SLURM, it would look something like this:

```bash
#!/bin/sh
#SBATCH --partition=general --qos=short
#SBATCH --mem=4000
#SBATCH --time 1:00:00
#SBATCH --out=$STDOUT

set -x

srun $COMMAND
```

Note that the approximate genome size provided is used for every target. Modify accordingly.

#### Summarizing results

You can summarize similarities against multiple target genomes (e.g. population)
via the following command:

```bash
java -jar /path/to/binary/alpaca.jar population-summary
-a /path/to/alpaca/db/ -l similarity_list.txt -o /path/to/output/directory
--prefix summary
```

The similarity list is a file containing the path to the output of every
comparison, one per line.

You can also obtain metrics about the population summary via:

```bash
 java -jar /path/to/binary/alpaca.jar population-metrics -p summarized_population.txt
 -o /path/to/output/directory/
```

## How to visualize

Alpaca comes with two custom made visualizations: (1) Alpaca layout and
(2) Tree tracing.

### Alpaca layout

The alpaca layout figure draws each contig/scaffold/chromosome as a sequence
of rectangles each representing a bin. Each bin is then colored by the
(proportioned) contribution of a label (e.g. lineage, clade) based on
 the top samples in that bin.

 You can generate this figure via:

 ```bash
java -jar /path/to/binary/alpaca.jar alpaca-layout
-s summarized_population.txt -l labels.txt -o /path/to/output/directory/
 ```

 The 'summarized_population.txt' is the output when running the 'population-summary'
 command (see section above). The 'labels.txt' file is tab-delimited file
 with the following values: sample ID, Hue, Saturation, Lightness, label ID.
 The last three values describe the assigned color for that sample in
 [HSL code](http://colorizer.org/). It's recommended to assign the same HSL
 code for samples within the same lineage/clade.

### Tree tracing

 The tree tracing figure traces the path(s) of the top scoring samples in
 a binary tree:

```bash
java -jar /path/to/binary/alpaca.jar tree-tracing
--proportions population_metrics.top_samples.txt
--labels labels.txt
-t tree.nwk
-o /path/to/output/directory/ --prefix <value>
```

The 'population_metrics.top_samples.txt' is one of the outputs from the
summary metrics command (see section above). The 'labels.txt' file is
the same as the one provided in the alpaca layout figure. This file is
optional but recommended as it provides a more intuitive visualization.

The 'tree.nwk' format is a binary tree explaining the evolutionary relationship/clustering
of every genome in the population in Newick format. A phylogenetic tree
can be provided as long as it is a binary tree. We recommend using
MASH to obtain this tree. We provide automated scripts:

To create individual scripts for constructing sketches:

```bash
java -jar /path/to/binary/alpaca.jar mash-sketches -l libraries.txt
--mash-binary /path/to/mash/binary/mash -o /path/to/output/directory/
 --cluster-config slurm_config.txt
```

The libraries file is a tab-delimited file containing the following values
in each line: sample ID, path to forward reads (if paired), path to reverse reads (optionally, if paired)

To perform pairwise distance and format as a distance matrix:

```bash
java -jar /path/to/binary/alpaca.jar mash-distances -s all_sketches.txt
-o /path/to/output/directory/ --prefix <value>
```

The 'all_sketches.txt' file contains the path to each mash sketch, one
per line.


The resulting matrix file can be converted to Newick format using R:
```R
require(ape)
distances = as.dist(read.table("<value>.matrix", as.is=TRUE))
clusters = hclust(distances)
my_tree = as.phylo(clusters)
write.tree(phy=my_tree, file="tree.nwk")
````
