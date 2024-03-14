import util.Try 
import collection.mutable

extension (sc: StringContext)
  def scr(args: Any*): Seq[String] =
    val scr = sc.s(args*).split(" ").map(_.toString.trim).filter(_.nonEmpty).toSeq
    exec(scr)

type Eval = Seq[String] => Seq[String]

type Env = Map[String, Eval]

def callOSProcess(xs: Seq[String]): Seq[String] = 
  val temp = os.temp()
  val result = os.proc(xs).call(stdout = temp)
  os.read(temp).linesIterator.toSeq

val stack = mutable.Stack.empty[String] 

val env = Map[String, Eval]( 
  "sum"->  (xs => Seq(xs.map(_.toDouble).sum.toString)),
  "sub"->  (xs => Seq((xs.take(1).map(_.toDouble).sum - xs.drop(1).map(_.toDouble).sum).toString)),
  "os" -> callOSProcess,
  "push" -> (xs => {
    val res = exec(xs) 
    stack.pushAll(res); 
    println(s"stack=$stack"); 
    Seq()
  }),
  "pop" -> (xs => stack.pop() +: xs),

)

def exec(xs: Seq[String]): Seq[String] = 
  xs match
    case Seq() => Seq()
    case Seq(f, xs*) if env.isDefinedAt(f) => 
      Try(env(f).apply(exec(xs))).recover{case e => Seq(e.toString)}.get
    case Seq(f, xs*) => f +: exec(xs)

/* TODO 
  develop a simple interpreted dynamic language that can do everything
  should it be a stack machine?
  should it have space as function application f 1 2 3 or f(1,2,3)?
*/