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

Alpaca comes with custom-made visualizations the explore similarity scores
in context to the population structure of the target genomes.


More details in our method description here.
