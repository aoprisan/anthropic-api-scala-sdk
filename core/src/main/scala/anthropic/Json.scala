package anthropic

import scala.collection.immutable.VectorMap

/** A small immutable JSON AST. The SDK keeps its own so that it adds no runtime dependencies. Convert
  * from circe/jsoniter/upickle values with [[Json.parse]] on their printed form, and back with
  * [[Json.render]]; build values with [[Json.obj]] and [[Json.arr]].
  *
  * Objects keep their field order. Numbers are `BigDecimal`, so integers and decimals both survive
  * a round trip; two numbers are equal when their values are (`1` equals `1.0`).
  */
sealed trait Json extends Product with Serializable {

  /** Compact JSON text. */
  def render: String = {
    val sb = new java.lang.StringBuilder
    Json.write(this, sb)
    sb.toString
  }

  override def toString: String = render

  /** A field of an object; `None` on anything else. */
  def get(key: String): Option[Json] = this match {
    case Json.Obj(f) => f.get(key)
    case _           => None
  }

  /** An element of an array; `None` on anything else. */
  def at(index: Int): Option[Json] = this match {
    case Json.Arr(xs) if index >= 0 && index < xs.length => Some(xs(index))
    case _                                               => None
  }

  /** `obj("a")("b")`: a field lookup that yields [[Json.Null]] when absent, for reading nested values. */
  def apply(key: String): Json = get(key).getOrElse(Json.Null)

  def isNull: Boolean = this == Json.Null

  def asString: Option[String] = this match {
    case Json.Str(s) => Some(s)
    case _           => None
  }

  def asBoolean: Option[Boolean] = this match {
    case Json.Bool(b) => Some(b)
    case _            => None
  }

  def asNumber: Option[BigDecimal] = this match {
    case Json.Num(n) => Some(n)
    case _           => None
  }

  /** The number, when it is an integer that fits in a `Long`. */
  def asLong: Option[Long] = this match {
    case Json.Num(n) if n.isWhole && n.isValidLong => Some(n.toLongExact)
    case _                                          => None
  }

  def asInt: Option[Int] = asLong.filter(l => l.isValidInt).map(_.toInt)

  def asDouble: Option[Double] = asNumber.map(_.toDouble)

  def asArray: Option[Vector[Json]] = this match {
    case Json.Arr(xs) => Some(xs)
    case _            => None
  }

  def asObject: Option[VectorMap[String, Json]] = this match {
    case Json.Obj(f) => Some(f)
    case _           => None
  }
}

object Json {
  case object Null                                     extends Json
  final case class Bool(value: Boolean)                extends Json
  final case class Num(value: BigDecimal)              extends Json
  final case class Str(value: String)                  extends Json
  final case class Arr(items: Vector[Json])            extends Json
  final case class Obj(fields: VectorMap[String, Json]) extends Json {

    /** This object with `key` set to `value` (replacing it in place, or appended). */
    def +(field: (String, Json)): Obj = Obj(fields + field)

    /** This object with the fields of `other` added; `other` wins on a clash. */
    def ++(other: Iterable[(String, Json)]): Obj = Obj(fields ++ other)
  }

  val True: Json  = Bool(true)
  val False: Json = Bool(false)

  def obj(fields: (String, Json)*): Obj = Obj(VectorMap.from(fields))
  def arr(items: Json*): Arr            = Arr(items.toVector)
  def str(s: String): Json              = Str(s)
  def num(n: Long): Json                = Num(BigDecimal(n))
  def num(n: Double): Json = {
    if (n.isNaN || n.isInfinite) throw new IllegalArgumentException(s"$n is not representable in JSON")
    Num(BigDecimal(n))
  }
  def bool(b: Boolean): Json = Bool(b)

  /** An array of strings. */
  def strings(items: Iterable[String]): Json = Arr(items.iterator.map(Str(_)).toVector)

  /** Parse JSON text. */
  def parse(text: String): Either[String, Json] =
    try {
      val p = new Parser(text)
      val v = p.value()
      p.skipWs()
      if (p.pos != text.length) Left(s"trailing characters at offset ${p.pos}") else Right(v)
    } catch {
      case e: ParseError => Left(e.getMessage)
    }

  /** Parse, throwing `IllegalArgumentException` on malformed input. */
  def unsafeParse(text: String): Json = parse(text) match {
    case Right(v) => v
    case Left(e)  => throw new IllegalArgumentException(e)
  }

  private def write(j: Json, sb: java.lang.StringBuilder): Unit = j match {
    case Null    => sb.append("null")
    case Bool(b) => sb.append(b)
    case Num(n)  =>
      // Integers print without an exponent or a trailing `.0`.
      val bd = n.bigDecimal
      sb.append(if (n.isWhole && bd.scale <= 0 && bd.precision - bd.scale <= 30) bd.toBigInteger.toString else bd.toString)
    case Str(s)  => quote(s, sb)
    case Arr(xs) =>
      sb.append('[')
      var first = true
      xs.foreach { x =>
        if (!first) sb.append(',')
        first = false
        write(x, sb)
      }
      sb.append(']')
    case Obj(fs) =>
      sb.append('{')
      var first = true
      fs.foreach { case (k, v) =>
        if (!first) sb.append(',')
        first = false
        quote(k, sb)
        sb.append(':')
        write(v, sb)
      }
      sb.append('}')
  }

  private def quote(s: String, sb: java.lang.StringBuilder): Unit = {
    sb.append('"')
    var i = 0
    while (i < s.length) {
      val c = s.charAt(i)
      c match {
        case '"'           => sb.append("\\\"")
        case '\\'          => sb.append("\\\\")
        case '\n'          => sb.append("\\n")
        case '\r'          => sb.append("\\r")
        case '\t'          => sb.append("\\t")
        case '\b'          => sb.append("\\b")
        case '\f'          => sb.append("\\f")
        case c if c < 0x20 => sb.append("\\u%04x".format(c.toInt))
        case c             => sb.append(c)
      }
      i += 1
    }
    sb.append('"')
  }

  final private class ParseError(msg: String) extends Exception(msg)

  /** Deepest array/object nesting the parser accepts; deeper input fails instead of overflowing the stack. */
  private val MaxDepth = 512

  final private class Parser(s: String) {
    var pos           = 0
    private var depth = 0

    private def fail(what: String): Nothing = throw new ParseError(s"$what at offset $pos")

    def skipWs(): Unit =
      while (pos < s.length && " \t\r\n".indexOf(s.charAt(pos).toInt) >= 0) pos += 1

    private def expect(lit: String): Unit =
      if (s.startsWith(lit, pos)) pos += lit.length else fail(s"expected '$lit'")

    def value(): Json = {
      skipWs()
      if (pos >= s.length) fail("unexpected end of input")
      s.charAt(pos) match {
        case '{'                          => obj()
        case '['                          => arr()
        case '"'                          => Str(string())
        case 't'                          => expect("true"); True
        case 'f'                          => expect("false"); False
        case 'n'                          => expect("null"); Null
        case c if c == '-' || c.isDigit   => num()
        case c                            => fail(s"unexpected character '$c'")
      }
    }

    private def enter(): Unit = {
      depth += 1
      if (depth > MaxDepth) fail(s"nesting deeper than $MaxDepth levels")
    }

    private def obj(): Json = {
      pos += 1
      enter()
      val b = VectorMap.newBuilder[String, Json]
      skipWs()
      if (pos < s.length && s.charAt(pos) == '}') {
        pos += 1
        depth -= 1
        Obj(VectorMap.empty)
      } else {
        var more = true
        while (more) {
          skipWs()
          if (pos >= s.length || s.charAt(pos) != '"') fail("expected object key")
          val k = string()
          skipWs()
          expect(":")
          b += k -> value()
          skipWs()
          if (pos < s.length && s.charAt(pos) == ',') pos += 1
          else if (pos < s.length && s.charAt(pos) == '}') { pos += 1; more = false }
          else fail("expected ',' or '}'")
        }
        depth -= 1
        Obj(b.result())
      }
    }

    private def arr(): Json = {
      pos += 1
      enter()
      val b = Vector.newBuilder[Json]
      skipWs()
      if (pos < s.length && s.charAt(pos) == ']') {
        pos += 1
        depth -= 1
        Arr(Vector.empty)
      } else {
        var more = true
        while (more) {
          b += value()
          skipWs()
          if (pos < s.length && s.charAt(pos) == ',') pos += 1
          else if (pos < s.length && s.charAt(pos) == ']') { pos += 1; more = false }
          else fail("expected ',' or ']'")
        }
        depth -= 1
        Arr(b.result())
      }
    }

    private def string(): String = {
      pos += 1
      val sb   = new java.lang.StringBuilder
      var done = false
      while (!done) {
        if (pos >= s.length) fail("unterminated string")
        val c = s.charAt(pos)
        pos += 1
        c match {
          case '"'  => done = true
          case '\\' =>
            if (pos >= s.length) fail("unterminated escape")
            val e = s.charAt(pos)
            pos += 1
            e match {
              case '"'  => sb.append('"')
              case '\\' => sb.append('\\')
              case '/'  => sb.append('/')
              case 'b'  => sb.append('\b')
              case 'f'  => sb.append('\f')
              case 'n'  => sb.append('\n')
              case 'r'  => sb.append('\r')
              case 't'  => sb.append('\t')
              case 'u'  =>
                if (pos + 4 > s.length) fail("bad unicode escape")
                val hex = s.substring(pos, pos + 4)
                if (!hex.forall(ch => Character.digit(ch, 16) >= 0)) fail("bad unicode escape")
                sb.append(Integer.parseInt(hex, 16).toChar)
                pos += 4
              case other => fail(s"bad escape '\\$other'")
            }
          case c if c < 0x20 => fail("control character in string")
          case c             => sb.append(c)
        }
      }
      sb.toString
    }

    private def num(): Json = {
      val start = pos
      if (s.charAt(pos) == '-') pos += 1
      def digits(): Unit = {
        val d0 = pos
        while (pos < s.length && s.charAt(pos).isDigit) pos += 1
        if (pos == d0) fail("expected digit")
      }
      digits()
      if (pos < s.length && s.charAt(pos) == '.') { pos += 1; digits() }
      if (pos < s.length && (s.charAt(pos) == 'e' || s.charAt(pos) == 'E')) {
        pos += 1
        if (pos < s.length && (s.charAt(pos) == '+' || s.charAt(pos) == '-')) pos += 1
        digits()
      }
      val text = s.substring(start, pos)
      try Num(BigDecimal(text))
      catch { case _: NumberFormatException => fail(s"number '$text' is out of range") }
    }
  }
}
