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

Alpaca comes with custom made visualizations.