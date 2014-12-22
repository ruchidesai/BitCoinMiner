import akka.actor._
import common._
import akka.routing.RoundRobinRouter
import java.security.MessageDigest
//import com.typesafe.config.ConfigFactory

object project1
{   
  def main(args: Array[String])
  {
    if(args.length == 1)
	{
	  val hostAddress: String = java.net.InetAddress.getLocalHost.getHostAddress()
      println("My address is " + hostAddress)
	  val nrOfZeroes = args(0).toInt
	  val nrOfStrings = 100
	  val nrOfWorkers = 10
	  val nrOfMessages = 15

          val system = ActorSystem("MasterSystem")

          
      /*val system = ActorSystem("MasterSystem", ConfigFactory.load(ConfigFactory.parseString("""akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
	}
   remote {
     transport = "akka.remote.netty.NettyRemoteTransport"
     netty {
       hostname = "localhost"
       port = 9009
     }
   }
}
""")))*/	
      val master = system.actorOf(Props(new Master(nrOfZeroes, nrOfStrings, nrOfWorkers)), name = "master")
	  master ! Calculate(nrOfMessages)
	}
	else if(args.length == 2)
	{
	  val ip_address = args(1)
	  /*val system = ActorSystem("WorkerSystem", ConfigFactory.load(ConfigFactory.parseString("""akka {
  actor {
    provider = "akka.remote.RemoteActorRefProvider"
	}
   remote {
     transport = "akka.remote.netty.NettyRemoteTransport"
     netty {
       hostname = "localhost"
       port = 0
     }
   }
}
""")))*/
      val system = ActorSystem("WorkerSystem")
      val worker = system.actorOf(Props(new Remote_Worker(ip_address)), name = "worker")
      worker ! "START"
	}
  }
  
  class Master(nrOfZeroes: Int, nrOfStrings: Int, nrOfWorkers: Int) extends Actor
  {
    val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")
	
	def receive =
    {
      case Calculate(nrOfMessages) =>
	    for (i <- 0 until nrOfMessages) workerRouter ! Work(nrOfZeroes, nrOfStrings)
	  
	  case "Register" =>
	    sender ! Work(nrOfZeroes: Int, nrOfStrings: Int)
	  
	  case Result(value) =>
		println(value)
    }
  }
  
  class Worker extends Actor
  {	
	def hex_Digest(s:String):String =
    {      
      val sha = MessageDigest.getInstance("SHA-256")
      
      sha.digest(s.getBytes).foldLeft("")((s:String,b:Byte) =>
        s + Character.forDigit((b & 0xf0)>> 4,16) + Character.forDigit(b & 0x0f,16))
    }
    
    def randomString(len: Int): String =
    {
      val rand = new scala.util.Random(System.nanoTime)
      val sb = new StringBuilder(len)
      val ab = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
      for (i <- 0 until len)
      {
        sb.append(ab(rand.nextInt(ab.length)))
      }
      sb.toString
    }
	
	def mine_bitcoin(nrOfZeroes: Int ,nrOfStrings:Int):String =
    {
      
      val gatorlinkID: String = "neha"
      var random_string: String = ""
      var sha_output: String = ""
      var nrOfResults:Int = 0
      var bitcoin_list:String = " "
     
        
      var zeroes: String = "0"
      for (i <- 0 until (nrOfZeroes - 1))
        zeroes = zeroes.concat("0")
        
      do
      {
        random_string = gatorlinkID.concat(randomString(10))
        //println(random_string)
        sha_output = hex_Digest(random_string)
        if(sha_output.startsWith(zeroes) )
        {
          bitcoin_list = bitcoin_list.concat(random_string+"\t"+sha_output + "\n")
          nrOfResults += 1
        }
      }while(nrOfResults <= nrOfStrings);
        
      bitcoin_list
    
    }
	
	def receive =
    {
      case Work(nrOfZeroes, nrOfStrings) =>      
		sender ! Result(mine_bitcoin(nrOfZeroes,nrOfStrings))
    }
  }
  
  class Remote_Worker(ip_address: String) extends Worker
  {
    val remote_master = context.actorFor("akka.tcp://MasterSystem@" + ip_address + ":5150/user/master")
	
	override def receive =
    {
      case "START" => 
        remote_master ! "Register"
		
	  case Work(nrOfZeroes, nrOfStrings) =>      
		sender ! Result(mine_bitcoin(nrOfZeroes,nrOfStrings))
    }
  }
}




