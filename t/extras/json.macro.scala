package ms.webmaster.macroserialization

// todo: collide with https://github.com/plokhotnyuk/jsoniter-scala ?

import `org.scala-lang:scala-compiler:2.10.0`
import scala.language.experimental.macros
import scala.reflect.macros.Context // scala 2.11: {BlackboxContext => Context}
import scala.annotation.{switch, tailrec}
import java.io._

object Json {
  private def interleave[T](lst: List[T], x: T) = lst.flatMap(List(x, _)).tail

  class Exception(where: String, s: String) extends RuntimeException(s"$where: $s")

  case class PackOptions(packAsMap: Class[_]=>Boolean/*, optionAsNullable: Boolean */)
  object PackOptions {
    val default = PackOptions(packAsMap = (_=>false))
  }

  /******************************************************************************************************************
   *  Packer                                                                                                        *
   ******************************************************************************************************************/
  trait AuxPacker[T] {
    def packToAppendable(os: Appendable, t: T, packOptions: Json.PackOptions)
  }

  final def packToString[A](a: A): String = macro packToString1Impl[A]
  final def packToString1Impl[A : c.WeakTypeTag](c: Context)(a: c.Expr[A]): c.Expr[String] = {
    import c.universe._
    val JSON = q"_root_.ms.webmaster.macroserialization.Json"
    // todo: avoid StringWriter creation for simple types
    c.Expr[String](q"""
      val sw = new java.io.StringWriter
      $JSON.packToAppendable(sw, $a, $JSON.PackOptions.default)
      sw.toString
    """)
  }

  final def packToString[A](a: A, packOptions: Json.PackOptions /*= Json.PackOptions.default*/): String = macro packToString2Impl[A]

  final def packToString2Impl[A : c.WeakTypeTag](c: Context)(a: c.Expr[A], packOptions: c.Expr[Json.PackOptions]): c.Expr[String] = {
    import c.universe._
    val JSON = q"_root_.ms.webmaster.macroserialization.Json"
    // todo: avoid StringWriter creation for simple types
    c.Expr[String](q"""
      val sw = new java.io.StringWriter
      $JSON.packToAppendable(sw, $a, $packOptions)
      sw.toString
    """)
  }


  final def packCharSequence(out: Appendable, cs: CharSequence) {
    if (cs == null) {
      out append "null"
    } else {
      out append '"'
      for(i <- 0 until cs.length)
        (cs.charAt(i): @switch) match {
          case '"'                                            => out append "\\\""
          case '\\'                                           => out append "\\\\"
          case '\b'                                           => out append "\\b"
          case '\f'                                           => out append "\\f"
          case '\n'                                           => out append "\\n"
          case '\r'                                           => out append "\\r"
          case '\t'                                           => out append "\\t"
          case c if Character.getType(c) == Character.CONTROL => out append "\\u%04X".format(c.toInt)
          case c                                              => out append c
        }
      out append '"'
    }
  }

  // type not known at compile time
  final def packAny(out: Appendable, value: Any, packOptions: Json.PackOptions) {
    value match {
      case null => out append "null"

      case x: collection.GenMapLike[_,_,_] =>
        var needcomma = false
        out append '{'
        x foreach { case (k,v) =>
          if (needcomma) out.append(',') else needcomma=true
          packAny(out, k, packOptions)
          out append ':'
          packAny(out, v, packOptions)
        }
        out append '}'

      case x: Array[_] =>
        var needcomma = false
        out append '['
        for(v <- x) {
          if (needcomma) out.append(',') else needcomma=true
          packAny(out, v, packOptions)
        }
        out append ']'

      case x: TraversableOnce[_] => // also iterator?
        var needcomma = false
        out append '['
        for(v <- x) {
          if (needcomma) out.append(',') else needcomma=true
          packAny(out, v, packOptions)
        }
        out append ']'

      case x: Product       => packAny(out, x.productIterator, packOptions) // tuple, case class

      case x: CharSequence  => packCharSequence(out, x)

      case x: Byte          => out append x.toString
      case x: Short         => out append x.toString
      case x: Char          => out append x.toInt.toString
      case x: Int           => out append x.toString
      case x: Long          => out append x.toString
      case x: Float         => out append x.toString // FIXME: do not encode NaN and Infinities
      case x: Double        => out append x.toString // FIXME: do not encode NaN and Infinities
      case x: Boolean       => out append x.toString

      case _ =>
        val instanceMirror = scala.reflect.runtime.currentMirror.reflect(value)
        if (instanceMirror.symbol.isClass && instanceMirror.symbol.asClass.isDerivedValueClass) {
          val List(getter) = instanceMirror.symbol.asClass.selfType.declarations filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
          packAny(out, instanceMirror.reflectMethod(getter).apply(), packOptions)
        } else {
          ???
        }
    }
  }

  final def packToAppendable[A](output: Appendable, value: A): Unit = macro packToAppendable1Impl[A]

  final def packToAppendable1Impl[A : c.WeakTypeTag](c: Context)(output:      c.Expr[Appendable],
                                                                 value:       c.Expr[A]): c.Expr[Unit] = {
    import c.universe._
    val JSON = q"_root_.ms.webmaster.macroserialization.Json"
    c.Expr[Unit](q"""
      $JSON.packToAppendable($output, $value, $JSON.PackOptions.default)
    """)
  }

  final def packToAppendable[A](output: Appendable, value: A, packOptions: Json.PackOptions = Json.PackOptions.default): Unit = macro packToAppendable2Impl[A]

  final def packToAppendable2Impl[A : c.WeakTypeTag](c: Context)(output:      c.Expr[Appendable],
                                                                 value:       c.Expr[A],
                                                                 packOptions: c.Expr[Json.PackOptions]): c.Expr[Unit] = {
    import c.universe._
    import definitions._

    val JSON = q"_root_.ms.webmaster.macroserialization.Json"

    val out = newTermName(c.fresh("out$"))

    def packcode(tpe: c.Type, w: c.Tree, alreadyGeneratedPackers: List[(Type, TermName)]): c.Tree = {
      if (tpe =:= NullTpe)                                                   { q"""$out.append("null")"""
      } else if(tpe =:= AnyTpe     || tpe =:= AnyValTpe
             || tpe =:= AnyValTpe  || tpe =:= c.typeOf[java.lang.Object]   ) { q"$JSON.packAny($out, $w, $packOptions)"
      } else if(tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character]) { q"$out.append($w.toInt.toString)"
      } else if(tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]
             || tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]
             || tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]
             || tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]
             || tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]  // FIXME: do not encode NaN and Infinities
             || tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]  // FIXME: do not encode NaN and Infinities
             || tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) { q"$out.append($w.toString)"
      } else if (tpe <:< c.typeOf[CharSequence])                             { q"$JSON.packCharSequence($out, $w)"

      } else if (tpe.baseClasses.exists(_.fullName == "scala.collection.GenMapLike")) {
        val MethodType(List(arg), tpeValue) = tpe.member(newTermName("apply")).typeSignatureIn(tpe)
        val tpeKey = arg.typeSignatureIn(tpe)
        q"""$out.append('{')
            var needComma = false
            $w foreach { x =>
              if (needComma) $out.append(',')
              needComma = true
              ${ packcode(tpeKey, q"x._1", alreadyGeneratedPackers) }
              $out.append(':')
              ${ packcode(tpeValue, q"x._2", alreadyGeneratedPackers) }
            }
            $out.append('}')"""

      } else if (tpe.typeSymbol.fullName == "scala.Array") { // Array is final class
        val TypeRef(_, _, List(tpeElement)) = tpe
        val tmp = newTermName(c.fresh("x$"))
        q"""val $tmp = $w /* eval once */
            $out.append('[')
            0 until $tmp.length foreach { i =>
              if (i != 0) $out.append(',')
              ${ packcode(tpeElement, q"$tmp(i)", alreadyGeneratedPackers) }
            }
            $out.append(']')"""

      } else if (tpe.baseClasses.exists(_.fullName == "scala.collection.TraversableOnce")) {
        val NullaryMethodType(tpeElement) = tpe.member(newTermName("head")).typeSignatureIn(tpe)
        q"""var needComma = false
            $out.append('[')
            $w foreach { x =>
              if (needComma) $out.append(',')
              needComma = true
              ${ packcode(tpeElement, q"x", alreadyGeneratedPackers) }
            }
            $out.append(']')"""

      } else if (tpe.typeSymbol.fullName == "scala.None") { // todo: pack as null
        q"""$out.append('[')
            $out.append(']')"""

      } else if (tpe.typeSymbol.fullName == "scala.Some") {
        val TypeRef(_, _, List(tpeElement)) = tpe
        q"""$out.append('[')
            ${ packcode(tpeElement, q"$w.get", alreadyGeneratedPackers) }
            $out.append(']')"""

      } else if (tpe.typeSymbol.fullName == "scala.Option") { // todo: pack None as null
        val TypeRef(_, _, List(tpeElement)) = tpe
        q"""$out.append('[')
            $w foreach { x =>
              ${ packcode(tpeElement, q"x", alreadyGeneratedPackers) }
            }
            $out.append(']')"""

      } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed && tpe.typeSymbol.asClass.isAbstract && // trait | abstract class
                 tpe.typeSymbol.asClass.knownDirectSubclasses.forall(sub => sub.asClass.isCaseClass && sub.asClass.isModuleClass)) { // all subs are case objects, unpack string
        ??? // todo

      } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass) { // case class or tuple
        if (tpe.typeSymbol.asClass.isModuleClass) { // case object, pack as name
          q"$JSON.packCharSequence($out, ${tpe.typeSymbol.name.decodedName.toString})"

        } else {
          alreadyGeneratedPackers.find(_._1 =:= tpe) match {
            case Some((_, funname)) =>
              q"""$funname($w) /* reuse */"""
            case None =>
              val funname = newTermName(c.fresh("packer$"))
              q"""def $funname(zzz: $tpe) {
                    $out.append('[')
                    ..${
                      val getters: List[MethodSymbol] = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
                      val packers = for((sym, i) <- getters.zipWithIndex;
                                        NullaryMethodType(tpeElement) = sym.typeSignatureIn(tpe))
                                    yield packcode(tpeElement, q"zzz.${sym.name.toTermName}", (tpe->funname) :: alreadyGeneratedPackers)
                      interleave(packers, q"$out.append(',')")
                    }
                    $out.append(']')
                  }
                  $funname($w)"""
          }
        }

      } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isDerivedValueClass) { // value class; pack as element type
        val List(getter) = tpe.declarations filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
        val NullaryMethodType(tpeElement) = getter.typeSignatureIn(tpe)
        packcode(tpeElement, q"$w.${getter.name.toTermName}", alreadyGeneratedPackers)

      } else {
        // can refer to local Json
        q"implicitly[Json.AuxPacker[$tpe]].packToAppendable($out, $w, $packOptions)"
      }
    }
    val rc = c.Expr[Unit](q"""val $out = ${output.tree} /* eval once */
                              ${packcode(c.weakTypeOf[A], value.tree, List.empty)}""")
    // println(s"// packer ${c.weakTypeOf[A]}\n" + rc)
    rc
  }


  /******************************************************************************************************************
   *  Unpacker                                                                                                      *
   ******************************************************************************************************************/
  trait AuxUnpacker[T] {
    def unpackFromPushbackReader(in: PushbackReader): T
  }

  @tailrec
  final def getcNoSpace(in: Reader): Int = (in.read(): @switch) match {
    case ' ' | '\t' | '\r' | '\n' => getcNoSpace(in)
    case c => c
  }

  @inline final def expect(where: String, in: Reader, x: Int) {
    val c = getcNoSpace(in)
    if (c != x)
      throw new Json.Exception(where, s"${printAsChar(x)} expected, got ${printAsChar(c)}")
  }

  private[this] def unhex(i: Int): Int = (i: @switch) match {
    case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' => i - '0'
    case 'A' | 'B' | 'C' | 'D' | 'E' | 'F'                         => i - 'A' + 10
    case 'a' | 'b' | 'c' | 'd' | 'e' | 'f'                         => i - 'a' + 10
  }

  final def printAsChar(x: Int) = (x: @switch) match {
    case -1                          => "EOF"
    case c if 0x0021<=c && c<=0x007E => "'%c'" format c
    case c if 0x0000<=c && c<=0xFFFF => "'\\u%04X'" format c
  }

  final def unpackString(in: PushbackReader): String = (getcNoSpace(in): @switch) match {
    case '"' =>
      val sb = new java.lang.StringBuilder
      @tailrec def f(): String = (in.read(): @switch) match {
        case -1 =>
          throw new EOFException
        case '"' =>
          sb.toString
        case '\\' =>
          sb append ((in.read(): @switch) match {
            case -1     => throw new EOFException
            case 'u'    => (unhex(in.read())*4096 + unhex(in.read())*256 + unhex(in.read())*16 + unhex(in.read())).toChar
            case 'b'    => '\b'
            case 'f'    => '\f'
            case 'n'    => '\n'
            case 'r'    => '\r'
            case 't'    => '\t'
            case c      => c.toChar
          })
          f()
        case c   =>
          sb append c.toChar
          f()
      }
      f()
    case 'n' =>
      in.unread('n')
      unpackNull(in)

    case c => throw new Json.Exception("unpackString", "'\"' or 'null' expected, got " + printAsChar(c))
  }

  final def unpackLong(in: PushbackReader): Long = (getcNoSpace(in): @switch) match {
    case '-' =>
      -unpackLong(in)
    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
      @tailrec def f(x: Long): Long = (in.read(): @switch) match {
        case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9') =>
          f(x * 10 + (c - '0'))
        case c =>
          if (c != -1) in.unread(c)
          x
      }
      f((c - '0').toLong)
    case c@'"' => // Longs which may not fit into Double sometimes are presented as strings (for example journald-gatewayd)
      in.unread(c)
      unpackString(in).toLong
    case c => throw new Json.Exception("unpackLong", "digit or '-' expected, got " + printAsChar(c))
  }

  final def unpackDouble(in: PushbackReader): Double = (getcNoSpace(in): @switch) match {
    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '-') =>
      val sb = new java.lang.StringBuilder
      sb append c.toChar
      @tailrec def f(): Double = (in.read(): @switch) match {
        case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'e' | 'E' | '.' | '+' | '-') =>
          sb append c.toChar
          f()
        case c =>
          if (c != -1) in.unread(c)
          java.lang.Double.parseDouble(sb.toString)
      }
      f()

    case c => throw new Json.Exception("unpackDouble", "digit or '-' expected, got " + printAsChar(c))
  }


  final def unpackNull(in: Reader): Null = (getcNoSpace(in): @switch) match {
    case 'n' =>
      (in.read(): @switch) match { case 'u' => case c => throw new Json.Exception("unpackNull", "'u' expected, got " + printAsChar(c)) }
      (in.read(): @switch) match { case 'l' => case c => throw new Json.Exception("unpackNull", "'l' expected, got " + printAsChar(c)) }
      (in.read(): @switch) match { case 'l' => case c => throw new Json.Exception("unpackNull", "'l' expected, got " + printAsChar(c)) }
      null

    case c => throw new Json.Exception("unpackNull", "'n' expected, got " + printAsChar(c))
  }

  final def unpackBoolean(in: Reader): Boolean = (getcNoSpace(in): @switch) match {
    case 't' =>
      (in.read(): @switch) match { case 'r' => case c => throw new Json.Exception("unpackBoolean", "'r' expected, got " + printAsChar(c)) }
      (in.read(): @switch) match { case 'u' => case c => throw new Json.Exception("unpackBoolean", "'u' expected, got " + printAsChar(c)) }
      (in.read(): @switch) match { case 'e' => case c => throw new Json.Exception("unpackBoolean", "'e' expected, got " + printAsChar(c)) }
      true

    case 'f' =>
      (in.read(): @switch) match { case 'a' => case c => throw new Json.Exception("unpackBoolean", "'a' expected, got " + printAsChar(c)) }
      (in.read(): @switch) match { case 'l' => case c => throw new Json.Exception("unpackBoolean", "'l' expected, got " + printAsChar(c)) }
      (in.read(): @switch) match { case 's' => case c => throw new Json.Exception("unpackBoolean", "'s' expected, got " + printAsChar(c)) }
      (in.read(): @switch) match { case 'e' => case c => throw new Json.Exception("unpackBoolean", "'e' expected, got " + printAsChar(c)) }
      false

    case c => throw new Json.Exception("unpackBoolean", "'t' or 'f' expected, got " + printAsChar(c))
  }


  final def unpackAnyVal(in: PushbackReader): AnyVal = (getcNoSpace(in): @switch) match {
    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '-') =>
      in.unread(c)
      unpackDouble(in)

    case c@('t' | 'f') =>
      in.unread(c)
      unpackBoolean(in)

    case c => throw new Json.Exception("unpackAnyVal", "digit or 't' or 'f' expected, got " + printAsChar(c))
  }

  final def unpackAnyRef(in: PushbackReader): AnyRef = {
    unpackAny(in).asInstanceOf[AnyRef]
  }

  final def unpackAny(in: PushbackReader): Any = (getcNoSpace(in): @switch) match {
    case '{' =>
      val b = Map.newBuilder[Any, Any]
      (getcNoSpace(in): @switch) match {
        case -1  => throw new Json.Exception("unpackAny", "map content expected, got EOF")
        case '}' =>
        case c   => in.unread(c)
                    var n: Int = 0
                    do {
                      val k: Any = unpackAny(in)
                      expect("unpackAny", in, ':')
                      val v: Any = unpackAny(in)
                      b += k -> v
                      n = getcNoSpace(in)
                    } while (n == ',')
                    if (n != '}') throw new Json.Exception("unpackAny", "'}' or ',' expected, got " + printAsChar(n))
      }
      b.result

    case '[' => //array
      val b = Seq.newBuilder[Any]
      (getcNoSpace(in): @switch) match {
        case -1  => throw new Json.Exception("unpackAny", "array content expected, got EOF")
        case ']' =>
        case c   => in.unread(c)
                    var n: Int = 0
                    do {
                      b += unpackAny(in)
                      n = getcNoSpace(in)
                    } while (n == ',')
                    if (n != ']') throw new Json.Exception("unpackAny", "']' or ',' expected, got " + printAsChar(n))
      }
      b.result

    case '"' =>
      in.unread('"')
      unpackString(in)

    case 'n' =>
      in.unread('n')
      unpackNull(in)

    case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '-') =>
      in.unread(c)
      unpackDouble(in)

    case c@('t' | 'f') =>
      in.unread(c)
      unpackBoolean(in)

    case c => throw new Json.Exception("unpackAny", "unexpected " + printAsChar(c))
  }

  // TODO: own reader like https://github.com/frugalmechanic/fm-serializer/tree/master/src/main/scala/fm/serializer/json
  final def unpackFromString[A](input: String): A = macro unpackFromStringImpl[A]

  final def unpackFromStringImpl[A : c.WeakTypeTag](c: Context)(input: c.Expr[String]): c.Expr[A] = {
    import c.universe._
    // todo: avoid StringReader creation for simple types
    unpackFromReaderImpl(c)(c.Expr[Reader](q"""
      new java.io.StringReader($input)
    """))
  }

  final def unpackFromReader[A](input: Reader): A = macro unpackFromReaderImpl[A]

  final def unpackFromReaderImpl[A: c.WeakTypeTag](c: Context)(input: c.Expr[Reader]): c.Expr[A] = {
    import c.universe._
    unpackFromPushbackReaderImpl(c)(c.Expr[PushbackReader](q"""
      new java.io.PushbackReader($input, 1)
    """))
  }

  final def unpackFromPushbackReader[A](input: PushbackReader): A = macro unpackFromPushbackReaderImpl[A]

  final def unpackFromPushbackReaderImpl[A: c.WeakTypeTag](c: Context)(input: c.Expr[PushbackReader]): c.Expr[A] = {
    import c.universe._
    import definitions._

    val JSON = q"_root_.ms.webmaster.macroserialization.Json"

    val in = newTermName(c.fresh("in$"))

    def distinctWith[T](lst: List[T], equ: (T, T) => Boolean): List[T] = lst match {
      case Nil                                   => Nil
      case hd::tl if tl exists (x => equ(x, hd)) => distinctWith(tl, equ)
      case hd::tl                                => hd :: distinctWith(tl, equ)
    }

    // "a.b.c" -> Select(Select(Ident("a"), "b"), "c")
    def nameAsTree(m: String): Tree =
      m.split("\\.this\\.") match {
        case Array(t, n) => n.split('.').foldLeft[Tree](This(newTypeName(t))) { Select(_, _) }
        case Array(n)    => n.split('.').foldLeft[Tree](null) {
          case (null, part  ) => Ident(newTermName(part))
          case (tre,  part  ) => Select(tre, newTermName(part))
        }
      }

    def getCompanionSymbol(s: Symbol): Symbol = s.companionSymbol match {
      case NoSymbol if s.owner.isMethod || s.owner.isTerm =>
        val tn = s.name.toTermName
        def findInContext(ctx: scala.tools.nsc.typechecker.Contexts#Context): Symbol =
          ctx.scope.asInstanceOf[Scope] find { sym => sym.isModule && sym.owner == s.owner && sym.name.toTermName == tn } match {
            case Some(sym)                => assert(!sym.isMethod); sym
            case None if ctx.outer != ctx => findInContext(ctx.outer)
            case None                     => NoSymbol
          }
        findInContext(c.asInstanceOf[scala.reflect.macros.runtime.Context].callsiteTyper.context)

      case sym: Symbol => sym
    }


    def unpackOption(readElt: /*c.Tree*/Typed): Tree = {
      q"""$JSON.expect("unpackOption", $in, '[')
          $JSON.getcNoSpace($in) match {
            case -1  => throw new Json.Exception("unpackOption", "array content expected, got EOF")
            case ']' => None
            case c   => $in.unread(c)
                        val e = Some($readElt)
                        $JSON.expect("unpackOption", $in, ']')
                        e
          }"""
    }

    def unpackcode(tpe: c.Type /*desired*/, alreadyGeneratedUnpackers: List[(Type, TermName)]): Typed = {

      val tt: Typed = alreadyGeneratedUnpackers.find(_._1 =:= tpe) match {
        case Some((tpe2, funname)) => // handle recursive types
          q"""$funname: $tpe2"""
        case None =>
                 if (tpe =:= NullTpe                                            ) { q"$JSON.unpackNull($in)           : $NullTpe"
          } else if (tpe =:= AnyTpe                                             ) { q"$JSON.unpackAny($in)            : $AnyTpe"
          } else if (tpe =:= AnyValTpe                                          ) { q"$JSON.unpackAnyVal($in)         : $AnyValTpe"
          } else if (tpe =:= AnyRefTpe  || tpe =:= c.typeOf[java.lang.Object   ]) { q"$JSON.unpackAnyRef($in)         : $AnyRefTpe"
        // todo: null is ok when unpacking java.lang.Byte
          } else if (tpe =:= ByteTpe    || tpe =:= c.typeOf[java.lang.Byte     ]) { q"$JSON.unpackLong($in).toByte    : $ByteTpe"
          } else if (tpe =:= ShortTpe   || tpe =:= c.typeOf[java.lang.Short    ]) { q"$JSON.unpackLong($in).toShort   : $ShortTpe"
          } else if (tpe =:= CharTpe    || tpe =:= c.typeOf[java.lang.Character]) { q"$JSON.unpackLong($in).toChar    : $CharTpe"
          } else if (tpe =:= IntTpe     || tpe =:= c.typeOf[java.lang.Integer  ]) { q"$JSON.unpackLong($in).toInt     : $IntTpe"
          } else if (tpe =:= LongTpe    || tpe =:= c.typeOf[java.lang.Long     ]) { q"$JSON.unpackLong($in)           : $LongTpe"
          } else if (tpe =:= FloatTpe   || tpe =:= c.typeOf[java.lang.Float    ]) { q"$JSON.unpackDouble($in).toFloat : $FloatTpe"
          } else if (tpe =:= DoubleTpe  || tpe =:= c.typeOf[java.lang.Double   ]) { q"$JSON.unpackDouble($in)         : $DoubleTpe"
          } else if (tpe =:= BooleanTpe || tpe =:= c.typeOf[java.lang.Boolean  ]) { q"$JSON.unpackBoolean($in)        : $BooleanTpe"
          } else if (tpe =:= c.typeOf[String]                                   ) { q"$JSON.unpackString($in)         : $tpe"
          } else if (tpe.baseClasses.exists(_.fullName == "scala.collection.GenMapLike")) {
            val MethodType(List(arg), tpeValue) = tpe.member(newTermName("apply")).typeSignatureIn(tpe)
            val tpeKey = arg.typeSignatureIn(tpe)
            val TypeRef(_, _, typeArgs) = tpe
            val keyUnpacker = unpackcode(tpeKey,   alreadyGeneratedUnpackers)
            val valueUnpacker = unpackcode(tpeValue, alreadyGeneratedUnpackers)
            q"""
              {
                $JSON.expect(${s"unpack $tpe as GenMapLike"}, $in, '{')
                val bldr = ${tpe.typeSymbol.companionSymbol}.newBuilder[..${typeArgs map { case t if t =:= tpeKey   => keyUnpacker.tpt.tpe
                                                                                           case t if t =:= tpeValue => valueUnpacker.tpt.tpe
                                                                                           case t                   => t }}] /* TODO: concrete */
                $JSON.getcNoSpace($in) match {
                  case -1  => throw new $JSON.Exception(${s"unpack $tpe as GenMapLike"}, "map content expected, got EOF")
                  case '}' =>
                  case c   => $in.unread(c)
                              var n: Int = 0
                              do {
                                bldr += scala.Tuple2[${keyUnpacker.tpt.tpe}, ${valueUnpacker.tpt.tpe}](
                                          ${keyUnpacker},
                                          { $JSON.expect(${s"unpack $tpe as GenMapLike"}, $in, ':')
                                            ${valueUnpacker}
                                          }
                                        )
                                n = $JSON.getcNoSpace($in)
                              } while (n == ',')
                              if (n != '}')
                                throw new $JSON.Exception(${s"unpack $tpe as GenMapLike"}, "'}' or ',' expected, got " + $JSON.printAsChar(n))
                }
                bldr.result
              } : $tpe
             """

          } else if (tpe.typeSymbol.fullName == "scala.Array") { // Array is final class
            val TypeRef(_, _, List(tpeElement)) = tpe
            val eltUnpacker = unpackcode(tpeElement, alreadyGeneratedUnpackers)
            q"""
              {
                $JSON.expect(${s"unpack Array[$tpeElement]"}, $in, '[')
                val bldr = _root_.scala.Array.newBuilder[${eltUnpacker.tpt.tpe}]
                $JSON.getcNoSpace($in) match {
                  case -1  => throw new $JSON.Exception(${s"unpack Array[$tpeElement]"}, "array content expected, got EOF")
                  case ']' =>
                  case c   => $in.unread(c)
                              var n: Int = 0
                              do {
                                bldr += ${eltUnpacker}
                                n = $JSON.getcNoSpace($in)
                              } while (n == ',')
                              if (n != ']')
                                throw new $JSON.Exception(${s"unpack Array[$tpeElement]"}, "']' or ',' expected, got " + $JSON.printAsChar(n))
                }
                bldr.result
              } : $tpe
             """

          } else if (tpe.baseClasses.exists(_.fullName == "scala.collection.Traversable") &&
                     tpe.typeSymbol.companionSymbol.typeSignature.member(newTermName("newBuilder")).isMethod) {
            val NullaryMethodType(tpeElement) = tpe.member(newTermName("head")).typeSignatureIn(tpe)
            val TypeRef(_, _, typeArgs) = tpe
            val eltUnpacker = unpackcode(tpeElement, alreadyGeneratedUnpackers)
            q"""
              {
                $JSON.expect(${s"unpack $tpe as Traversable"}, $in, '[')
                val bldr = ${tpe.typeSymbol.companionSymbol}.newBuilder[..${typeArgs map { case t if t =:= tpeElement => eltUnpacker.tpt.tpe
                                                                                           case t                     => t }}]
                $JSON.getcNoSpace($in) match {
                  case -1  => throw new $JSON.Exception(${s"unpack $tpe as Traversable"}, "array content expected, got EOF")
                  case ']' =>
                  case c   => $in.unread(c)
                              var n: Int = 0
                              do {
                                bldr += $eltUnpacker
                                n = $JSON.getcNoSpace($in)
                              } while (n == ',')
                              if (n != ']')
                                throw new $JSON.Exception(${s"unpack $tpe as Traversable"}, "']' or ',' expected, got " + $JSON.printAsChar(n))
                }
                bldr.result
              } : $tpe
             """

          } else if (tpe.typeSymbol.fullName == "scala.Option") { // Option is sealed class
            val TypeRef(_, _, List(tpeElement)) = tpe
            val eltUnpacker = unpackcode(tpeElement, alreadyGeneratedUnpackers)
            q"${unpackOption(eltUnpacker)} : $tpe"

          } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isSealed && tpe.typeSymbol.asClass.isAbstract && // trait | abstract class
                     tpe.typeSymbol.asClass.knownDirectSubclasses.forall(sub => sub.asClass.isCaseClass && sub.asClass.isModuleClass)) { // all subs are case objects, unpack string
              q"""
                (
                  $JSON.unpackString($in) match {
                    case ..${ tpe.typeSymbol.asClass.knownDirectSubclasses map (sub => cq"${sub.name.decodedName.toString} => ${sub.asClass.module}") }
                  }
                ) : $tpe
               """

          } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isCaseClass) { // case class or tuple
            val getters: List[MethodSymbol] = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
            if (getters.zipWithIndex.forall{ case(sym, i) => sym.name.decoded == "_"+(i+1) }) { // tuple
              val unpackers: List[Typed]        = getters.zipWithIndex map { case (sym, i) =>
                                                    val NullaryMethodType(tpeElement) = sym.typeSignatureIn(tpe)
                                                    val eltUnpacker = unpackcode(tpeElement, alreadyGeneratedUnpackers)
                                                    (if (i == 0) {
                                                      eltUnpacker
                                                    } else {
                                                      q"""{ $JSON.expect(${s"unpack Tuple${getters.length}"}, $in, ',')
                                                            $eltUnpacker
                                                          } : ${eltUnpacker.tpt.tpe}
                                                       """
                                                    })
                                                  }
              q"""
                {
                  $JSON.expect(${s"unpack Tuple $tpe"}, $in, '[')
                  val tmp = new ${tpe}(..${unpackers})
                  $JSON.expect(${s"unpack Tuple $tpe"}, $in, ']')
                  tmp
                } : $tpe
               """

            } else { // case class
              val symCompanion: Symbol = getCompanionSymbol(tpe.typeSymbol)
              val tpeCompanion: Type   = symCompanion.typeSignature

              case class Param(name: Name, tpe: Type, default: Symbol) {
                val isOption = tpe.typeSymbol.fullName == "scala.Option"
                val tpeElement = if (isOption) {
                                   val TypeRef(_, _, List(tpeElement)) = tpe
                                   tpeElement
                                 } else tpe
                val varname  = newTermName(name.encoded+"$var")
              }

              val params: List[Param] = {
                val members = tpeCompanion.members.toList //++ tpeCompanion.members.toList ++ tpeCompanion.members.toList ++ tpeCompanion.members.toList // !!! tpeCompanion.members is not stable, sometimes it misses some methods
                for((sym, i) <- getters.zipWithIndex) yield {
                  val default: Symbol = members find { member => member.isMethod && member.name.decoded=="<init>$default$" + (i+1) } match { // sometimes there is no "<init>$default$1" only "apply$default$1"
                                          case Some(member) => require(member.asMethod.paramss==Nil && member.asMethod.returnType=:=sym.asMethod.returnType)
                                                               member
                                          case None         => NoSymbol
                                        }
                  val NullaryMethodType(tpeElement) = sym.typeSignatureIn(tpe)
                  Param(sym.name, tpeElement, default)
                }
              }

              val paramUnpackers: List[(/*desired*/Type, TermName)] = distinctWith[Type](params.map(_.tpeElement), _ =:= _).map(t => t -> newTermName(c.fresh(s"unpackparam$$${/*t*/""}$$"))) // scala-2.10 has problems with such names

              val funname = newTermName(c.fresh(s"unpacker$$${/*tpe*/""}$$")) // scala-2.10 has problems with such names
              q"""
              {
                def $funname: $tpe = {
                  ..${ paramUnpackers map { case xx@(t, uname) =>
                         val availableUnpackers = (tpe->funname) :: paramUnpackers.filter(_ ne xx) ::: alreadyGeneratedUnpackers
                         val code = unpackcode(t, availableUnpackers)
                         q"def $uname: $t = $code"
                       } : List[Tree] /* SI-6840 */
                     }

                  val c = $JSON.getcNoSpace($in)
                  if (c=='[') {
                    val tmp = new ${tpe}(..${ params.zipWithIndex map { case (p,i) =>
                                                val readElt = q"""${paramUnpackers.find(_._1 =:= p.tpeElement).get._2}: ${p.tpeElement}"""
                                                val tree = if (p.isOption) unpackOption(readElt) else readElt
                                                if (i==0)
                                                  tree
                                                else
                                                  q"$JSON.expect(${s"unpack case class $tpe as array"}, $in, ','); $tree" } : List[Tree] /* SI-6840 */ })
                    $JSON.expect(${s"unpack case class $tpe as array"}, $in, ']')
                    tmp
                  } else if (c=='{') {
                    ..${ params map { p => q"var ${p.varname}: Option[${p.tpeElement}] = None" } : List[Tree] /* SI-6840 */ }
                    $JSON.getcNoSpace($in) match {
                      case -1  => throw new $JSON.Exception(${"unpack case class $tpe as map"}, "map content expected, got EOF")
                      case '}' =>
                      case c   => $in.unread(c)
                                  var r: Int = 0
                                  do {
                                    val s = $JSON.unpackString($in)
                                    $JSON.expect(${s"unpack case class $tpe as map"}, $in, ':')
                                    ${ params.foldLeft[Tree](q"$JSON.unpackAny($in)" /* skip unknown field */) {
                                       case (tree, p) =>
                                         q"""if (s == ${p.name.decoded}) {
                                               ${p.varname} = Some(${ paramUnpackers.find(_._1 =:= p.tpeElement).get._2 })
                                             } else {
                                               $tree
                                             }"""
                                     }}
                                    r = $JSON.getcNoSpace($in)
                                  } while (r == ',')
                                  if (r != '}')
                                    throw new $JSON.Exception(${s"unpack case class $tpe as map"}, "'}' or ',' expected, got " + $JSON.printAsChar(r))
                    }
                    new ${tpe}(
                      ..${ params map { case p if p.isOption          => q"""${p.varname}"""
                                        case p if p.default==NoSymbol => q"""${p.varname}.getOrElse(throw new $JSON.Exception(${s"unpack case class $tpe as map"}, ${s"missing \042${p.name}\042"}))"""
                                        case p                        => q"""${p.varname}.getOrElse(${nameAsTree(tpe.toString)}.${p.default.asTerm.name.toTermName})""" // SI-XXXX?
                                      } : List[Tree] /* SI-6840 */ }
                    )
                  } else if (c == -1) {
                    throw new $JSON.Exception(${s"unpack case class $tpe"}, "'{' or '[' expected, got EOF")
                  } else {
                    ${ (if (params.length==1) {  // constructor has one and only param (it is known at compile time)
                          q"""$in.unread(c)
                              new ${tpe}( ${ { val readElt = q"""${paramUnpackers.head._2}: ${paramUnpackers.head._1}"""
                                               if (params.head.isOption) unpackOption(readElt) else readElt } : Tree /* SI-6840 */
                                           } )"""
                         } else {
                           q"""throw new $JSON.Exception(${s"unpack case class $tpe"}, "no idea how to do it")"""
                         }) : Tree /* SI-6840 */
                     }
                  }
                }
                $funname
              } : $tpe
              """
            }

          } else if (tpe.typeSymbol.isClass && tpe.typeSymbol.asClass.isDerivedValueClass && // value class; unpack as element type if constructor is public
                     { val List(ctor) = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) filter(_.isConstructor); ctor.isPublic }) {
            val List(getter) = tpe.declarations.sorted filter(_.isMethod) map(_.asMethod) takeWhile(!_.isConstructor) filter(_.paramss==Nil /* nullary */)
            val NullaryMethodType(tpeElement) = getter.typeSignatureIn(tpe)
            val eltUnpacker = unpackcode(tpeElement, alreadyGeneratedUnpackers)
            q"""new $tpe($eltUnpacker) : $tpe"""

          } else {
            tpe match {
              case RefinedType(List(AnyRefTpe), decls) if decls.nonEmpty => // Json.unpack[{def a: String}]; unpack like a case class; crashes scala-2.10
                case class Param(name: Name, tpe: Type) {
                  val isOption = tpe.typeSymbol.fullName == "scala.Option"
                  val tpeElement = if (isOption) {
                                     val TypeRef(_, _, List(tpeElement)) = tpe
                                     tpeElement
                                   } else tpe
                  val varname  = newTermName(name.encoded+"$var")
                  val eltUnpacker = unpackcode(this.tpeElement, alreadyGeneratedUnpackers)
                }
                val params: List[Param] = decls.toList map { case sym if sym.isMethod && sym.asMethod.paramss==Nil => Param(sym.name.toTermName, sym.asMethod.returnType) }

                val ccname = newTypeName(c.fresh("refined$"))

                val block: Tree = c.typecheck(q"""
                                    case class $ccname(..${params map {
                                                             case p if p.isOption => q"${p.name.toTermName}: scala.Option[${p.eltUnpacker.tpt.tpe /*concrete element type*/}]"
                                                             case p               => q"${p.name.toTermName}:              ${p.eltUnpacker.tpt.tpe /*concrete element type*/}"
                                                           }}) {
                                      def tupled = (..${params map (p => q"${p.name.toTermName}")})
                                    }
                                  """)
                val ccTpe: Type = block match { case Block((ClassDef(_,`ccname`,_,template)::module::Nil), _) => template.tpe }

                q"""
                  {
                    $block

                    ..${ params map { p => q"var ${p.varname}: Option[${p.eltUnpacker.tpt.tpe /*concrete element type*/}] = None" } : List[Tree] /* SI-6840 */ }

                    $JSON.expect("unpack refined type", $in, '{')
                    $JSON.getcNoSpace($in) match {
                      case -1  => throw new $JSON.Exception("unpack refined type", "map content expected, got EOF")
                      case '}' =>
                      case c   => $in.unread(c)
                                  var r: Int = 0
                                  do {
                                    val s = $JSON.unpackString($in)
                                    $JSON.expect("unpack refined type", $in, ':')
                                    ${ params.foldLeft[Tree](q"$JSON.unpackAny($in)" /* skip unknown field */) {
                                       case (tree, p) =>
                                         q"""if (s == ${p.name.decoded}) {
                                               ${p.varname} = Some(${ p.eltUnpacker })
                                             } else {
                                               $tree
                                             }"""
                                     }}
                                    r = $JSON.getcNoSpace($in)
                                  } while (r == ',')
                                  if (r != '}')
                                    throw new $JSON.Exception("unpack refined type", "'}' or ',' expected, got " + $JSON.printAsChar(r))
                    }

                    new $ccTpe(..${ params map { case p if p.isOption => q"""${p.varname}"""
                                                 case p               => q"""${p.varname}.getOrElse(throw new $JSON.Exception("unpack refined type", ${s"missing \042${p.name}\042"}))"""
                                               } : List[Tree] /* SI-6840 */ })
                  } : $ccTpe
                """

              case _ =>
                // can refer to local `Json`
                q"implicitly[Json.AuxUnpacker[$tpe]].unpackFromPushbackReader($in) : $tpe"
            }
          }
      }
      require(tt.tpt != null && tt.tpt.tpe != null)
      tt
    }

    val rc = c.Expr[A](q"""val $in = ${input.tree}
                           ${unpackcode(c.weakTypeOf[A], List.empty)}""")
    // println(s"// unpacker ${c.weakTypeOf[A]}\n" + rc)
    rc
  }



  class Keep(val jsonstring: String) {
    def unpack[A]: A = macro Keep_unpackImpl[A]
    override def toString = s"Keep(${jsonstring})"
  }
  def Keep_unpackImpl[A : c.WeakTypeTag](c: Context): c.Expr[A] = {
    import c.universe._
    unpackFromStringImpl[A](c)( c.Expr[String](Select(c.prefix.tree, newTermName("jsonstring"))) )
  }

  implicit object AuxUnpackerKeep extends AuxUnpacker[Keep] {
    def unpackFromPushbackReader(in: PushbackReader): Keep = {
      val sb = new java.lang.StringBuilder
      var unreaded: Int = -2
      def unread(c: Int) {
        assert(unreaded == -2)
        //in.unread(c)
        unreaded = c
      }
      def read(): Int = if (unreaded != -2) {
                          val c = unreaded; unreaded = -2; c
                        } else {
                          (in.read(): @switch) match {
                            case -1 => -1
                            case c  => sb.append(c.toChar); c
                          }
                        }
      @tailrec def getcNoSpace(): Int = (read(): @switch) match {
        case ' ' | '\t' | '\r' | '\n' => getcNoSpace()
        case c                        => c
      }

      def unpackAny(): Unit = (getcNoSpace(): @switch) match {
        case '{' => (getcNoSpace(): @switch) match {
                      case -1  => throw new Json.Exception("unpackKeep", "map content expected, got EOF")
                      case '}' =>
                      case c   => unread(c)
                                  var n: Int = 0
                                  do {
                                    unpackAny()
                                    val c = getcNoSpace()
                                    if (c != ':') throw new Json.Exception("unpackKeep", s"':' expected, got ${printAsChar(c)}")
                                    unpackAny()
                                    n = getcNoSpace()
                                  } while (n == ',')
                                  if (n != '}') throw new Json.Exception("unpackKeep", "'}' or ',' expected, got " + printAsChar(n))
                    }
        case '[' => (getcNoSpace(): @switch) match {
                      case -1  => throw new Json.Exception("unpackKeep", "array content expected, got EOF")
                      case ']' =>
                      case c   => unread(c)
                                  var n: Int = 0
                                  do {
                                    unpackAny()
                                    n = getcNoSpace()
                                  } while (n == ',')
                                  if (n != ']') throw new Json.Exception("unpackKeep", "']' or ',' expected, got " + printAsChar(n))
                    }
        case '"' => @tailrec def f(): Unit = (read(): @switch) match {
                      case -1   => throw new EOFException
                      case '"'  =>
                      case '\\' => (read(): @switch) match {
                                     case -1     => throw new EOFException
                                     case 'u'    => (unhex(read())*4096 + unhex(read())*256 + unhex(read())*16 + unhex(read()))
                                     case c      =>
                                   }
                                   f()
                      case c    => f()
                    }
                    f()
        case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '-') =>
                    @tailrec def f(): Unit = (read(): @switch) match {
                      case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'e' | 'E' | '.' | '+' | '-') => f()
                      case -1                                                                                          =>
                      case c                                                                                           => unread(c)
                    }
                    f()
        case 'n' => (read(): @switch) match { case 'u' => case c => throw new Json.Exception("unpackKeep reading 'null'",  "'u' expected, got " + printAsChar(c)) }
                    (read(): @switch) match { case 'l' => case c => throw new Json.Exception("unpackKeep reading 'null'",  "'l' expected, got " + printAsChar(c)) }
                    (read(): @switch) match { case 'l' => case c => throw new Json.Exception("unpackKeep reading 'null'",  "'l' expected, got " + printAsChar(c)) }
        case 't' => (read(): @switch) match { case 'r' => case c => throw new Json.Exception("unpackKeep reading 'true'",  "'r' expected, got " + printAsChar(c)) }
                    (read(): @switch) match { case 'u' => case c => throw new Json.Exception("unpackKeep reading 'true'",  "'u' expected, got " + printAsChar(c)) }
                    (read(): @switch) match { case 'e' => case c => throw new Json.Exception("unpackKeep reading 'true'",  "'e' expected, got " + printAsChar(c)) }
        case 'f' => (read(): @switch) match { case 'a' => case c => throw new Json.Exception("unpackKeep reading 'false'", "'a' expected, got " + printAsChar(c)) }
                    (read(): @switch) match { case 'l' => case c => throw new Json.Exception("unpackKeep reading 'false'", "'l' expected, got " + printAsChar(c)) }
                    (read(): @switch) match { case 's' => case c => throw new Json.Exception("unpackKeep reading 'false'", "'s' expected, got " + printAsChar(c)) }
                    (read(): @switch) match { case 'e' => case c => throw new Json.Exception("unpackKeep reading 'false'", "'e' expected, got " + printAsChar(c)) }
        case c   => throw new Json.Exception("unpackKeep", "unexpected " + printAsChar(c))
      }
      unpackAny()
      if (unreaded != -2)
        ???
      else
        new Keep(sb.toString)
    }
  }



  // useful to look up a few first tokens when unpacking ADTs
  object Tokenizer {
    sealed trait Token
    case class Bool(value: Boolean) extends Token
    val False = Bool(false)
    val True  = Bool(true)
    case class Num (value: Double ) extends Token
    case class Str (value: String ) extends Token
    case object Null                extends Token
    case object `[`                 extends Token
    case object `,`                 extends Token
    case object `]`                 extends Token
    case object `{`                 extends Token
    case object `:`                 extends Token
    case object `}`                 extends Token

    def tokenizeString(str: String): Iterator[Token] = tokenizeReader(new StringReader(str))

    def tokenizeReader(in: Reader): Iterator[Token] = new Iterator[Token] {
      private var current = -2
      def hasNext: Boolean = current >= 0 || {
        current = getcNoSpace(in)
        current >= 0
      }
      def next: Token = {
        if (!hasNext)
          throw new java.util.NoSuchElementException("next on empty iterator")
        (current: @switch) match {
          case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '.' | '-') =>
                      val sb = new java.lang.StringBuilder
                      sb append c.toChar
                      @tailrec def f(): Double = in.read() match {
                        case c@('0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | 'e' | 'E' | '.' | '+' | '-') =>
                          sb append c.toChar
                          f()
                        case c =>
                          current = c
                          java.lang.Double.parseDouble(sb.toString)
                      }
                      Num(f())
          case '"' => val sb = new java.lang.StringBuilder
                      @tailrec def f(): String = (in.read(): @switch) match {
                        case -1   =>
                          throw new EOFException
                        case '"'  =>
                          sb.toString
                        case '\\' =>
                          sb append ((in.read(): @switch) match {
                            case -1     => throw new EOFException
                            case 'u'    => (unhex(in.read())*4096 + unhex(in.read())*256 + unhex(in.read())*16 + unhex(in.read())).toChar
                            case 'b'    => '\b'
                            case 'f'    => '\f'
                            case 'n'    => '\n'
                            case 'r'    => '\r'
                            case 't'    => '\t'
                            case c      => c.toChar
                          })
                          f()
                        case c    =>
                          sb append c.toChar
                          f()
                      }
                      current = -2; Str(f())
          case '{' => current = -2; `{`
          case '}' => current = -2; `}`
          case '[' => current = -2; `[`
          case ']' => current = -2; `]`
          case ':' => current = -2; `:`
          case ',' => current = -2; `,`
          case 'n' => (in.read(): @switch) match { case 'u' => case c => throw new Json.Exception("Tokenizer reading 'null'",  "'u' expected, got " + printAsChar(c)) }
                      (in.read(): @switch) match { case 'l' => case c => throw new Json.Exception("Tokenizer reading 'null'",  "'l' expected, got " + printAsChar(c)) }
                      (in.read(): @switch) match { case 'l' => case c => throw new Json.Exception("Tokenizer reading 'null'",  "'l' expected, got " + printAsChar(c)) }
                      current = -2
                      Null
          case 't' => (in.read(): @switch) match { case 'r' => case c => throw new Json.Exception("Tokenizer reading 'true'",  "'r' expected, got " + printAsChar(c)) }
                      (in.read(): @switch) match { case 'u' => case c => throw new Json.Exception("Tokenizer reading 'true'",  "'u' expected, got " + printAsChar(c)) }
                      (in.read(): @switch) match { case 'e' => case c => throw new Json.Exception("Tokenizer reading 'true'",  "'e' expected, got " + printAsChar(c)) }
                      current = -2
                      True
          case 'f' => (in.read(): @switch) match { case 'a' => case c => throw new Json.Exception("Tokenizer reading 'false'", "'a' expected, got " + printAsChar(c)) }
                      (in.read(): @switch) match { case 'l' => case c => throw new Json.Exception("Tokenizer reading 'false'", "'l' expected, got " + printAsChar(c)) }
                      (in.read(): @switch) match { case 's' => case c => throw new Json.Exception("Tokenizer reading 'false'", "'s' expected, got " + printAsChar(c)) }
                      (in.read(): @switch) match { case 'e' => case c => throw new Json.Exception("Tokenizer reading 'false'", "'e' expected, got " + printAsChar(c)) }
                      current = -2
                      False
          case c => throw new Json.Exception("Tokenizer", "unexpected " + printAsChar(c))
        }
      }
    }
  }

}
