package smrsc.counters

object Synapse extends CountersWorld {

  val start: C =
    List(W, 0, 0)

  val rules: List[Rule] = List(
    {
      case List(i, d, v) if i >= 1 =>
        List(i + d - 1, 0, v + 1)
    }, {
      case List(i, d, v) if v >= 1 =>
        List(i + d + v - 1, 1, 0)
    }, {
      case List(i, d, v) if i >= 1 =>
        List(i + d + v - 1, 1, 0)
    }
  )

  val isUnsafe: C => Boolean = {
    case List(i, d, v) if d >= 1 && v >= 1 => true
    case List(i, d, v) if d >= 2 => true
    case _ => false
  }
}

object MSI extends CountersWorld {

  val start: C =
    List(W, 0, 0)

  val rules: List[Rule] = List(
    {
      case List(i, m, s) if i >= 1 =>
        List(i + m + s - 1, 1, 0)
    }, {
      case List(i, m, s) if s >= 1 =>
        List(i + m + s - 1, 1, 0)
    }, {
      case List(i, m, s) if i >= 1 =>
        List(i - 1, 0, m + s + 1)
    })

  val isUnsafe: C => Boolean = {
    case List(i, m, s) => (m >= 1 && s >= 1) || (m >= 2)
  }
}

object MOSI extends CountersWorld {

  val start: C =
    List(W, 0, 0, 0)

  val rules: List[Rule] = List(
    {
      case List(i, o, s, m) if i >= 1 =>
        List(i - 1, m + o, s + 1, 0)
    }, {
      case List(i, o, s, m) if o >= 1 =>
        List(i + o + s + m - 1, 0, 0, 1)
    }, { // wI
      case List(i, o, s, m) if i >= 1 =>
        List(i + o + s + m - 1, 0, 0, 1)
    }, { // wS
      case List(i, o, s, m) if s >= 1 =>
        List(i + o + s + m - 1, 0, 0, 1)
    }, { // se
      case List(i, o, s, m) if s >= 1 =>
        List(i + 1, o, s - 1, m)
    }, { // wbm
      case List(i, o, s, m) if m >= 1 =>
        List(i + 1, o, s, m - 1)
    }, { // wbo
      case List(i, o, s, m) if o >= 1 =>
        List(i + 1, o - 1, s, m)
    })

  val isUnsafe: C => Boolean = {
    case List(i, o, s, m) if o >= 2 => true
    case List(i, o, s, m) if m >= 2 => true
    case List(i, o, s, m) if s >= 1 && m >= 1 => true
    case _ => false
  }
}

object ReaderWriter extends CountersWorld {

  val start: C =
    List(1, 0, 0, W, 0, 0)

  val rules: List[Rule] = List(
    { // r1
      case List(x2, x3, x4, x5, x6, x7) if x2 >= 1 && x4 === 0 && x7 >= 1 =>
        List(x2 - 1, x3 + 1, 0, x5, x6, x7)
    }, { // r2
      case List(x2, x3, x4, x5, x6, x7) if x2 >= 1 && x6 >= 1 =>
        List(x2, x3, x4 + 1, x5, x6 - 1, x7)
    }, { // r3
      case List(x2, x3, x4, x5, x6, x7) if x3 >= 1 =>
        List(x2 + 1, x3 - 1, x4, x5 + 1, x6, x7)
    }, { // r4
      case List(x2, x3, x4, x5, x6, x7) if x4 >= 1 =>
        List(x2, x3, x4 - 1, x5 + 1, x6, x7)
    }, { // r5
      case List(x2, x3, x4, x5, x6, x7) if x5 >= 1 =>
        List(x2, x3, x4, x5 - 1, x6 + 1, x7)
    }, { // r6
      case List(x2, x3, x4, x5, x6, x7) if x5 >= 1 =>
        List(x2, x3, x4, x5 - 1, x6, x7 + 1)
    })

  val isUnsafe: C => Boolean = {
    case List(x2, x3, x4, x5, x6, x7) if x3 >= 1 && x4 >= 1 => true
    case _ => false
  }
}

object MESI extends CountersWorld {

  val start: C =
    List(W, 0, 0, 0)

  val rules: List[Rule] = List(
    {
      case List(i, e, s, m) if i >= 1 =>
        List(i - 1, 0, s + e + m + 1, 0)
    }, {
      case List(i, e, s, m) if e >= 1 =>
        List(i, e - 1, s, m + 1)
    }, {
      case List(i, e, s, m) if s >= 1 =>
        List(i + e + s + m - 1, 1, 0, 0)
    }, {
      case List(i, e, s, m) if i >= 1 =>
        List(i + e + s + m - 1, 1, 0, 0)
    })

  val isUnsafe: C => Boolean = {
    case List(i, e, s, m) if m >= 2 => true
    case List(i, e, s, m) if s >= 1 && m >= 1 => true
    case _ => false
  }
}

object MOESI extends CountersWorld {

  val start: C = List(W, 0, 0, 0, 0)

  val rules: List[Rule] = List(
    { // rm
      case List(i, m, s, e, o) if i >= 1 =>
        List(i - 1, 0, s + e + 1, 0, o + m)
    }, { //wh2
      case List(i, m, s, e, o) if e >= 1 =>
        List(i, m + 1, s, e - 1, o)
    }, { // wh3
      case List(i, m, s, e, o) if s + o >= 1 =>
        List(i + m + s + e + o - 1, 0, 0, 1, 0)
    }, { // wm
      case List(i, m, s, e, o) if i >= 1 =>
        List(i + m + s + e + o - 1, 0, 0, 1, 0)
    })

  val isUnsafe: C => Boolean = {
    case List(i, m, s, e, o) if m >= 1 && (e + s + o) >= 1 => true
    case List(i, m, s, e, o) if m >= 2 => true
    case List(i, m, s, e, o) if e >= 2 => true
    case _ => false
  }
}

object Illinois extends CountersWorld {

  val start: C =
    List(W, 0, 0, 0)

  val rules: List[Rule] = List(
    { // r2
      case List(i, e, d, s) if i >= 1 && e === 0 && d === 0 && s === 0 =>
        List(i - 1, 1, 0, 0)
    }, { // r3
      case List(i, e, d, s) if i >= 1 && d >= 1 =>
        List(i - 1, e, d - 1, s + 2)
    }, { // r4
      case List(i, e, d, s) if i >= 1 && s + e >= 1 =>
        List(i - 1, 0, d, s + e + 1)
    }, { // r6
      case List(i, e, d, s) if e >= 1 =>
        List(i, e - 1, d + 1, s)
    }, { // r7
      case List(i, e, d, s) if s >= 1 =>
        List(i + s - 1, e, d + 1, 0)
    }, { // r8
      case List(i, e, d, s) if i >= 1 =>
        List(i + e + d + s - 1, 0, 1, 0)
    }, { // r9
      case List(i, e, d, s) if d >= 1 =>
        List(i + 1, e, d - 1, s)
    }, { // r10
      case List(i, e, d, s) if s >= 1 =>
        List(i + 1, e, d, s - 1)
    }, { // r11
      case List(i, e, d, s) if e >= 1 =>
        List(i + 1, e - 1, d, s)
    })

  val isUnsafe: C => Boolean = {
    case List(i, e, d, s) if d >= 1 && s >= 1 => true
    case List(i, e, d, s) if d >= 2 => true
    case _ => false
  }
}

object Berkley extends CountersWorld {

  val start: C =
    List(W, 0, 0, 0)

  val rules: List[Rule] = List(
    { // rm
      case List(i, n, u, e) if i >= 1 =>
        List(i - 1, n + e, u + 1, 0)
    }, { // wm
      case List(i, n, u, e) if i >= 1 =>
        List(i + n + u + e - 1, 0, 0, 1)
    }, { // wh1
      case List(i, n, u, e) if n + u >= 1 =>
        List(i + n + u - 1, 0, 0, e + 1)
    })

  val isUnsafe: C => Boolean = {
    case List(i, n, u, e) if e >= 1 && u + n >= 1 => true
    case List(i, n, u, e) if e >= 2 => true
    case _ => false
  }
}

case object Firefly extends CountersWorld {

  val start: C =
    List(W, 0, 0, 0)

  val rules: List[Rule] = List(
    { // rm1
      case List(i, e, s, d) if i >= 1 && d === 0 && s === 0 && e === 0 =>
        List(i - 1, 1, 0, 0)
    }, { // rm2
      case List(i, e, s, d) if i >= 1 && d >= 1 =>
        List(i - 1, e, s + 2, d - 1)
    }, { // rm3
      case List(i, e, s, d) if i >= 1 && s + e >= 1 =>
        List(i - 1, 0, s + e + 1, d)
    }, { // wh2
      case List(i, e, s, d) if e >= 1 =>
        List(i, e - 1, s, d + 1)
    }, { // wh3
      case List(i, e, s, d) if s === 1 =>
        List(i, e + 1, 0, d)
    }, { // wm
      case List(i, e, s, d) if i >= 1 =>
        List(i + e + d + s - 1, 0, 0, 1)
    })

  val isUnsafe: C => Boolean = {
    case List(i, e, s, d) if d >= 1 && s + e >= 1 => true
    case List(i, e, s, d) if e >= 2 => true
    case List(i, e, s, d) if d >= 2 => true
    case _ => false
  }
}

object Futurebus extends CountersWorld {

  val start: C = List(W, 0, 0, 0, 0, 0, 0, 0, 0)

  val rules: List[Rule] = List(
    { // r2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if i >= 1 && pW === 0 =>
        List(i - 1, 0, 0, 0, pR + 1, pW, pEMR + eM, pEMW, pSU + sU + eU)
    }, { // r3
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMR >= 1 =>
        List(i, sU + pR + 1, eU, eM, 0, pW, pEMR - 1, pEMW, pSU)
    }, { // r4
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pSU >= 1 =>
        List(i, sU + pR + pSU, eU, eM, 0, pW, pEMR, pEMW, 0)
    }, { // r5
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR >= 2 && pSU === 0 && pEMR === 0 =>
        List(i, sU + pR, eU, eM, 0, pW, 0, pEMW, 0)
    }, { // r6
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR === 1 && pSU === 0 && pEMR === 0 =>
        List(i, sU, eU + 1, eM, 0, pW, 0, pEMW, 0)
    }, { // wm1
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if i >= 1 & pW === 0 =>
        List(i + eU + sU + pSU + pR + pEMR - 1, 0, 0, 0, 0, 1, 0, pEMW + eM, 0)
    }, { // wm2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMW >= 1 =>
        List(i + 1, sU, eU, eM + pW, pR, 0, pEMR, pEMW - 1, pSU)
    }, { // wm3
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pEMW === 0 =>
        List(i, sU, eU, eM + pW, pR, 0, pEMR, 0, pSU)
    }, { // wh2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if eU >= 1 =>
        List(i, sU, eU - 1, eM + 1, pR, pW, pEMR, pEMW, pSU)
    }, { // wh2
      case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if sU >= 1 =>
        List(i + sU - 1, 0, eU, eM + 1, pR, pW, pEMR, pEMW, pSU)
    })

  val isUnsafe: C => Boolean = {
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if sU >= 1 && eU + eM >= 1 => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if eU + eM >= 2 => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pR >= 1 && pW >= 1 => true
    case List(i, sU, eU, eM, pR, pW, pEMR, pEMW, pSU) if pW >= 2 => true
    case _ => false
  }
}

object Xerox extends CountersWorld {
  val start: C =
    List(W, 0, 0, 0, 0)

  val rules: List[Rule] = List(
    { // (1) rm1
      case List(i, sc, sd, d, e) if i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0 =>
        List(i - 1, 0, 0, 0, 1)
    }, { // (2) rm2
      case List(i, sc, sd, d, e) if i >= 1 && d + sc + e + sd >= 1 =>
        List(i - 1, sc + e + 1, sd + d, 0, 0)
    }, { // (3) wm1
      case List(i, sc, sd, d, e) if i >= 1 && d === 0 && sc === 0 && sd === 0 && e === 0 =>
        List(i - 1, 0, 0, 1, 0)
    }, { // (4) wm2
      case List(i, sc, sd, d, e) if i >= 1 && d + sc + e + sd >= 1 =>
        List(i - 1, sc + e + 1 + sd + d, sd, 0, 0)
    }, { // (5) wh1
      case List(i, sc, sd, d, e) if d >= 1 =>
        List(i + 1, sc, sd, d - 1, e)
    }, { // (6) wh2
      case List(i, sc, sd, d, e) if sc >= 1 =>
        List(i + 1, sc - 1, sd, d, e)
    }, { // (7) wh3
      case List(i, sc, sd, d, e) if sd >= 1 =>
        List(i + 1, sc, sd - 1, d, e)
    }, { // (8) wh4
      case List(i, sc, sd, d, e) if e >= 1 =>
        List(i + 1, sc, sd, d, e - 1)
    })

  val isUnsafe: C => Boolean = {
    case List(i, sc, sd, d, e) if d >= 1 && (e + sc + sd) >= 1 => true
    case List(i, sc, sd, d, e) if e >= 1 && (sc + sd) >= 1 => true
    case List(i, sc, sd, d, e) if d >= 2 => true
    case List(i, sc, sd, d, e) if e >= 2 => true
    case _ => false
  }
}

object DataRace extends CountersWorld {

  val start: C =
    List(W, 0, 0)

  val rules: List[Rule] = List(
    { // 1
      case List(out, cs, scs) if out >= 1 && cs === 0 && scs === 0 =>
        List(out - 1, 1, 0)
    }, { // 2
      case List(out, cs, scs) if out >= 1 && cs === 0 =>
        List(out - 1, 0, scs + 1)
    }, { // 3
      case List(out, cs, scs) if cs >= 1 =>
        List(out + 1, cs - 1, scs)
    }, { // 4
      case List(out, cs, scs) if scs >= 1 =>
        List(out + 1, cs, scs - 1)
    })

  val isUnsafe: C => Boolean = {
    case List(out, cs, scs) if cs >= 1 && scs >= 1 => true
    case _ => false
  }
}
