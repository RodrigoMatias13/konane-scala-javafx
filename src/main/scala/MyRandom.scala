// Gerador de numeros aleatorios puro - nao usa side effects.
// Cada chamada a nextInt devolve um novo MyRandom em vez de alterar o estado.
case class MyRandom(seed: Long) {
  def nextInt: (Int, MyRandom) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRandom = MyRandom(newSeed)
    val value = (newSeed >>> 16).toInt
    (value, nextRandom)
  }
}