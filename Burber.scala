package Barbers

import java.io.{File, PrintWriter}
import scala.io.Source
import scala.collection.mutable

class work_with_log(file_name: String) {
  private val file = new PrintWrit\er(new File(file_name)
  def close(): Unit = file.close()
  def write(str: String): Unit =
 {
    file.write(str)
    file.write("\r\n")
  }
}

class Check_Log(clients : Int, barbers : Int, sofa_size : Int) {
  var dict = mutable.Map[Int, Int]()
  var now_at_sofa = 0
  def load_info() : List[String] = {
    Source.fromFile("log.txt").getLines().toList
  }

  def parse_log(str : String) : Unit = {
    val info = str.split(' ')
    val actor = info(0)
    val id = info(1).toInt
    def client_to_do() : Unit = {
      if (info(2) == "at")
        now_at_sofa += 1
    }

    def unknown_log() : Unit = {
      println("Unknown log")
      System.exit(1)
    }

    def barber_to_do() : Unit = {
      if (info(2) == "take") {
        if (dict.contains(id)) {
          if (dict.apply(id) == -1) {
            dict(id) = info(3).toInt
            now_at_sofa -= 1
          }
          else {
            println("Double clients")
            System.exit(1)
          }
        }
        else {
          dict update(id, info(3).toInt)
          now_at_sofa -= 1
        }
      }
      else {
        if (dict.apply(id) != -1) {
          dict(id) = -1
        }
        else {
          println("Free barber done work")
          System.exit(1)
        }
      }
    }
    actor match {
      case "Client" => client_to_do()
      case "Barber"  => barber_to_do()
      case _ => unknown_log()
    }
    if (now_at_sofa > sofa_size) {
      println("Sofa overflow")
      System.exit(1)
    }
    if (now_at_sofa < 0) {
      println("Taking from empty sofa")
      System.exit(1)
    }
  }
  def main(): Unit = {
    println("Errors in log:")
    val g = load_info()
    for (x <- g) parse_log(x)
    println("Log is correct")
  }
}
class Barber(id : Int, sofa : Sofa) extends Thread{
  var client_id : Int = 0

  override def run() = {
    while(sofa.compete <= sofa.num_of_clients) {
      while (!pop_client())
      {}
      Thread.sleep(1000)
      work()
      if (sofa.compete == sofa.num_of_clients) {
        sofa.log.close()
        System.exit(0)
      }
    }
  }
  def work() : Unit = sofa.lock.synchronized {
    sofa.compete += 1
    sofa.log.write("Barber " + id + " done "+ client_id)
  }
  def pop_client() : Boolean = sofa.lock.synchronized {
    if (sofa.now_on_sofa > 0) {
      client_id = sofa.push_client()
      sofa.log.write("Barber " + id + " take " + client_id)
      true
    }
    else false
  }
}
class Client(id : Int, sofa : Sofa) extends Thread{
  override def run() = sofa.lock.synchronized {
    if (sofa.now_on_sofa < sofa.size_of_sofa) {
      sofa.pop_client(id)
      sofa.log.write("Client " + id + " at sofa")
    }
    else {
      sofa.log.write("Client " + id + " go home")
      sofa.compete += 1
    }
  }
}
class Sofa(clients : Int, barbers : Int, size : Int) {
  val r = scala.util.Random
  var lock : AnyRef = new Object()
  val log = new work_with_log("log.txt")
  log.write("number of clients " + clients)
  log.write("number of barbers " + barbers)
  log.write("size of sofa " + size + "\r\n")
  val num_of_clients = clients
  val num_of_barbers = barbers
  val size_of_sofa = size
  var compete = 0
  var arr = Array.fill[Int](size_of_sofa)(-1)
  var head = 0
  var tail = 0
  var now_on_sofa = 0
  def pop_client(x : Int) : Unit = lock.synchronized {
    arr(head) = x
    now_on_sofa += 1
    head = (head + 1) % size_of_sofa
  }
  def push_client() : Int = lock.synchronized {
    val res = arr(tail)
    now_on_sofa -= 1
    tail = (tail + 1) % size_of_sofa
    res
  }
  def main() = {
    for(i <- 0 until barbers)
 new Barber(i, this).start()
    for (i <- 0 until clients) {
      new Client(i, this).start()
      Thread.sleep(r.nextInt(100))
    }
  }
}
object Barber_Shop {
  def main(args: Array[String]): Unit = {
    println("Enter the number of clients")
    val clients = scala.io.StdIn.readInt()
    println("Enter the number of barbers")
    val barbers = scala.io.StdIn.readInt()
    println("Enter the size of sofa")
    val size = scala.io.StdIn.readInt()
    new Sofa(clients, barbers, size).main()
    new Check_Log(clients, barbers, size).main()
  }
}