package utilities

import java.io.File
import htsjdk.samtools.{SAMRecord, SAMRecordIterator, SamReaderFactory}
import utilities.FileHandling.timeStamp
import scala.annotation.tailrec
import sys.process._

/**
  * Author: Alex N. Salazar
  * Created on 21-6-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
trait AlpacaUtils extends HashingUtils {

  /**
    * Function to compute
    *
    * @return
    */
  def automaticMinCountThreshold: (File, Int) => Int = (bam_file, genome_size) => {
    //command for samtools stats
    val command = Seq("samtools", "stats", bam_file.getAbsolutePath).mkString(" ")
    //run command and capture output
    val output = Process(command).!!.split("\n").find(_.contains("bases mapped (cigar)"))
    //assert that there is the desired line
    assert(output != None, "Could not find total number of bases mapped from 'samtools stats' command.")
    //get total basees aligned
    val total_bases_aligned = output.get.split("\\s+")(4).toDouble
    println(timeStamp + "--Total bases aligned: " + total_bases_aligned)
    //estimate coverage
    val coverage = total_bases_aligned / genome_size
    println(timeStamp + "--Estimated coverage: " + coverage)
    //estimate min count
    Math.round(coverage * (0.05)).toInt
  }


  /**
    * Case class for BAM header entry
    *
    * @param ref_name
    * @param length
    * @param index
    */
  case class BAMheader(ref_name: String, length: Int, index: Int)

  /**
    * Load bam indeces into map callable by fasta sequence name
    *
    * @param bam_file
    * @return
    */
  def getBAMindeces(bam_file: File): Map[String, BAMheader] = {
    //load bam file and load sam header
    val tmp = SamReaderFactory.makeDefault().open(bam_file).getFileHeader.getSequenceDictionary.getSequences
    //iterate through each index in the sam header
    (0 to tmp.size() - 1).foldLeft(Map[String, BAMheader]())((map, index) => {
      //get value at index
      val entry = tmp.get(index)
      //create new instance of sam header
      val sam_header = new BAMheader(entry.getSequenceName, entry.getSequenceLength, entry.getSequenceIndex)
      //add to map
      map + (entry.getSequenceName -> (sam_header))
    })
  }

  /**
    * Function to obtain reads overlapping a given subregion in a given file using native htsjdk library
    *
    * @return SAMRecordIterator
    */
  def getReadsOverlapping: (File, String, (Int, Int)) => SAMRecordIterator = (bam, name, subregion) => {
    SamReaderFactory.makeDefault().open(bam).queryOverlapping(name, subregion._1, subregion._2)
  }

  /**
    * Function to extract reads from SAMRecord iterator of a given subregion while also keeping track of reads
    * that may carry over to the neighbour subregion
    *
    * @return (Iterator[SAMRecord], Iterator[SAMRecord])
    */
  def getOverlappingReads(all_reads: SAMRecordIterator, subregion: (Int, Int),
                          kmer_size: Int, min_mapq: Int): (Iterator[SAMRecord], Iterator[SAMRecord]) = {
    /**
      * Tail recursive method that extracts reads from given subregion
      *
      * @param collected Reads contained in the given subregion
      * @param carry_ons Reads that may be part of the neighbouring subregion
      * @return (Iterator[SAMRecord], Iterator[SAMRecord])
      */
    @tailrec def _getOverlappingReads(collected: Iterator[SAMRecord],
                                      carry_ons: Iterator[SAMRecord]): (Iterator[SAMRecord], Iterator[SAMRecord]) = {
      //no more reads left, end
      if (!all_reads.hasNext) (collected, carry_ons)
      else {
        //get current read
        val current_read = all_reads.next
        //read alignment is too close to the boundary to extract proper kmer, end
        if (current_read.getAlignmentStart > (subregion._2 - kmer_size)) (collected, carry_ons ++ Iterator(current_read))
        else {
          //read alignment is below minimum mapping quality, move on
          if (current_read.getMappingQuality < min_mapq) _getOverlappingReads(collected, carry_ons)
          //read alignment is not end contained, therefore overlaps with ending boundary, add to both collections
          else if (!isEndContained(current_read.getAlignmentEnd, subregion._2)) {
            _getOverlappingReads(collected ++ Iterator(current_read), carry_ons ++ Iterator(current_read))
          }
          //read alignment is contained, add only the first collection
          else _getOverlappingReads(collected ++ Iterator(current_read), carry_ons)
        }
      }
    }

    _getOverlappingReads(Iterator[SAMRecord](), Iterator[SAMRecord]())
  }

  /**
    * Method to create a kmer set for some given subregion. Will use kmers derived from the assembly and from reads
    * aligned to it.
    *
    * @param reads               SAMRecordIterator
    * @param min_mapq            Minimum required mapping quality for an alignment
    * @param sequence            Sequence from assembly
    * @param trust_assembly      Trust sequence from assembly and therefore do not require minimum kmer count
    * @param includeSoftClipping Include kmers from soft-clipped sequence in alignments
    * @return HashSet[String]
    */
  def kmerizeSubregion(kmer_size: Int)(reads: SAMRecordIterator,
                                       contig: String,
                                       subregion: (Int, Int),
                                       min_mapq: Int,
                                       sequence: Array[Byte],
                                       min_count: Int,
                                       trust_assembly: Boolean,
                                       includeSoftClipping: Boolean,
                                       bam_only: Boolean): Set[Long] = {

    //mutable hashset for final set of kmers
    var kmer_hashmap = scala.collection.mutable.HashMap[Int, Int]()

    /**
      * Function to add kmer to hashset conditional if its observed a specified minimum amount of times.
      *
      * @return Unit
      */
    def addKmer2Set: (Array[Byte], Boolean) => Unit = (kmer, trust_kmer) => {
      //get smallest lexicographic version of the kmer based on the decoded version
      val smallest_kmer = List(kmer, kmer.reverse.map(reverseComplement(_))).sortBy(_.map(decode(_)).toString).head
      //get kmer hash value from smallest kmer
      val kmer_hash = getKmerByteHash(smallest_kmer)
      //if kmer is to be trusted, automatically add to hashset and add counter
      if (trust_kmer) kmer_hashmap.update(kmer_hash, min_count)
      //require kmer to be observe at least a minimum number of times
      else {
        //current value
        val kmer_count = kmer_hashmap.getOrElse(kmer_hash, 0)
        //if kmer does not meet minimum threshold, increment +1
        if (kmer_count < min_count) kmer_hashmap.update(kmer_hash, kmer_count + 1)
        //kmer meets minimum threshold, leave as it is
      }
    }
    //kmerize subregion from assembly
    if (!bam_only) {
      sequence.sliding(kmer_size).foreach(kmer => if (kmer.size == kmer_size) addKmer2Set(kmer, trust_assembly))
    }
    //iterate through all reads aligned in the subregion and kmerize
    while (reads.hasNext) {
      //get current read
      val current_read = reads.next()
      //only proceed if current had has minimum mapping quality
      if (current_read.getMappingQuality >= min_mapq) {
        //sanity assert statement that reads overlaps with current subregion
        assert(isOverlap(subregion, (current_read.getAlignmentStart, current_read.getAlignmentEnd)), timeStamp +
          "Current read " + (current_read.getAlignmentStart, current_read.getAlignmentEnd) + " unexpectedly does " +
          "not overlap with current subregion " + subregion + ".")
        //get cigar of read
        val read_cigar = current_read.getCigar
        //kmerize read only if there is sufficient sequence after considering subregion boundary
        if (current_read.getAlignmentEnd - subregion._1 >= kmer_size &&
          subregion._2 - current_read.getAlignmentStart >= kmer_size) {
          //get potential start offset from reads overlapping boundaries
          val start_index = {
            if (current_read.getAlignmentStart >= subregion._1)
              current_read.getReadPositionAtReferencePosition(current_read.getAlignmentStart, true)
            else current_read.getReadPositionAtReferencePosition(subregion._1, true)
          }
          //get potential ending offset from reads overlapping boundaries
          val end_index = {
            if (current_read.getAlignmentEnd <= subregion._2)
              current_read.getReadPositionAtReferencePosition(current_read.getAlignmentEnd, true)
            else current_read.getReadPositionAtReferencePosition(subregion._2, true)
          }
          //get true aligned sequence accounting for read clipping at the ends
          val aligned_seq = {
            //if not allowing clipped sequence or if it is not clipped, simply return entire read sequence
            if (!includeSoftClipping || !read_cigar.isClipped) {
              /**
              try {
                current_read.getReadString.substring(start_index - 1, end_index)
              }
              catch {
                case e: java.lang.StringIndexOutOfBoundsException =>
                  println(current_read.getReferenceName, current_read.getAlignmentStart, current_read.getAlignmentEnd,
                    current_read.getCigar, current_read.getReadLength, start_index - 1, end_index)
              }
                */
              current_read.getReadString.substring(start_index - 1, end_index)
            }
            else {
              //compute starting offset
              val start_offset = {
                //if first cigar entry is not clipped return 0
                if (!read_cigar.getFirstCigarElement.getOperator.isClipping) 0
                else getOffset(read_cigar.getFirstCigarElement.toString)
              }
              //compute ending offset
              val end_offset = {
                //if last cigar entry is not clipped return size of read sequence
                if (!read_cigar.getLastCigarElement.getOperator.isClipping) 0
                else getOffset(read_cigar.getLastCigarElement.toString)
              }
              /**
              try {
                //return true aligned sequence
                current_read.getReadString.substring(start_index - start_offset - 1, end_index + end_offset)
              } catch {
                case e: java.lang.StringIndexOutOfBoundsException =>
                  println(current_read.getReferenceName, current_read.getAlignmentStart, current_read.getAlignmentEnd,
                    current_read.getCigar, current_read.getReadLength, start_index, end_index, start_offset, end_offset,
                    start_index - start_offset - 1, end_index + end_offset)
              }
                */
              current_read.getReadString.substring(start_index - start_offset - 1, end_index + end_offset)
            }
          }
          //kmerize aligned read sequence and add to hashset only if the kmer is of the specified size
          aligned_seq.toCharArray.map(encode(_)).sliding(kmer_size)
            .foreach(kmer => if (kmer.size == kmer_size) addKmer2Set(kmer, false))
        }
      }
    }
    //return hashset of region
    kmer_hashmap.foldLeft(Set[Long]())((kset, kmer) => if (kmer._2 < min_count) kset else kset + (kmer._1))
  }

  def getOffset: String => Int = cigar_entry => if (!cigar_entry.contains("S")) 0 else cigar_entry.takeWhile(_.isDigit).toInt


  def isOverlap: ((Int, Int), (Int, Int)) => Boolean = (region, alignment) => {
    alignment._1 <= region._2 && alignment._2 >= region._1
  }

  def isEndContained: (Int, Int) => Boolean = (alignment_end, region_end) => alignment_end <= region_end

}


