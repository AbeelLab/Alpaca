package utilities

import scala.util.hashing.MurmurHash3

/**
  * Author: Alex N. Salazar
  * Created on 2-7-2018
  * Contact a.n.salazar@tudelft.nl for questions
  *
  * Description:
  */
trait HashingUtils {

  /**
    * Bytes stored as values
    */
  val zero_byte = 0.toByte
  val one_byte = 1.toByte
  val two_byte = 2.toByte
  val three_byte = 3.toByte
  val min1_byte = (-1).toByte

  /**
    * Method to map nucleotides to bytes.
    *
    * @param c Nucleotide
    * @return Byte
    */
  def encode(c: Char): Byte = c match {
    case 'a' | 'A' => zero_byte
    case 'c' | 'C' => one_byte
    case 'g' | 'G' => two_byte
    case 't' | 'T' => three_byte
    case 'n' | 'N' => min1_byte
    case _ => {
      assert(false, "Cannot encode " + c);
      min1_byte
    }
  }

  /**
    * Method to decode bytes to nucleotides
    * @param c
    * @return
    */
  def decode(c: Byte): Char = c match {
    case `zero_byte` => 'A'
    case `one_byte` => 'C'
    case `two_byte` => 'G'
    case `three_byte` => 'T'
    case `min1_byte` => 'N'
    case _ => {
      assert(false, "Cannot decode " + c)
      '?'
    }
  }

  /**
    * Method to obtain reverse complement of a sequence encoded as byte array
    * @param c Nucleotide encoded as a byte array
    * @return Byte
    */
  def reverseComplement(c: Byte): Byte =  c match {
    case `zero_byte` => three_byte
    case `one_byte` => two_byte
    case `two_byte` => one_byte
    case `three_byte` => zero_byte
    case `min1_byte` => min1_byte
    case _ => {
      assert(false, "Cannot reverse complement " + c);
      min1_byte
    }
  }

  /**
    * Method to hash byte array using murmurhash
    * @return Long
    */
  def getKmerByteHash: Array[Byte] => Int = kmer => MurmurHash3.bytesHash(kmer)


}
